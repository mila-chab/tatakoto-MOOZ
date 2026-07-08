library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library("readxl")

retrieve.raw.data <- function(root_folder, folder_path, output_path, run,
                              skip = 136, save_data = FALSE) {
  path <- paste(root_folder, folder_path, sep = "")
  column_names <- c(
    "Date",
    "Time",
    "Duration.s",
    as.vector(rbind(paste0("Ox.", 1:8),
                    paste0("Temp.", 1:8)))
  )

  files_list <- list.files(path = path,
                           pattern = "*.txt")

  file <- files_list[1]
  table <- read.table(file = paste(path, file, sep = "/"),
                      sep = "\t", header = TRUE, fill = TRUE,  skip = skip) |>
    clean_column_names(column_names)

  if (save_data) {
    write.csv(table,
              paste(output_path, "/", run, ".csv", sep = ""),
              sep = ";", dec = ".", row.names = FALSE)
  }
}


extract.metadata <- function(root_folder, date = NULL) {
  resp <- read_excel(paste(root_folder, "metadata/metadata.xlsx", sep = ""),
                     sheet = "resp")
  df <- read_excel(paste(root_folder, "metadata/metadata.xlsx", sep = ""),
                   sheet = "metadata")

  resp <- resp |>
    mutate(Start_Time_Day = hms::as_hms(Start_Time_Day),
           Close_Time_Day = hms::as_hms(Close_Time_Day),
           Start_Time_Night = hms::as_hms(Start_Time_Night),
           Close_Time_Night = hms::as_hms(Close_Time_Night),
           Temperature.C = as.numeric(gsub(",", ".", Temperature.C)),
           Blanc = as.logical(Blanc))

  if (!is.null(date)) {
    df <- df |> filter(Date == as.Date(date))
    resp <- resp |> filter(Date == as.Date(date))
  }

  list(metadata = df, resp = resp)
}


clean_column_names <- function(df, column_names) {
  columns_reference <- as.vector(
    c(1:3,
      sapply(0:3,
             function(i) c(4 + 18 * i, 12 + 18 * i)),
      sapply(0:3,
             function(i) c(4 + 18 * i + 77, 12 + 18 * i + 77))
    )
  )

  df <- df[, columns_reference]
  colnames(df) <- column_names
  df
}


df_to_long_series <- function(df) {
  df_long <- df |>
    pivot_longer(
      cols = c(starts_with("Ox."), starts_with("Temp.")),
      names_to = c(".value", "Channel"),
      names_sep = "\\."
    ) |>
    mutate(Channel = as.numeric(Channel))

  df_long
}


attribute.temperature.to.timeseries <- function(data, metadata) {
  data$Temp <- NA

  for (i in seq_len(nrow(metadata))) {
    start <- metadata[i, "Start_Time_Day"]
    end <- metadata[i, "Close_Time_Day"]
    idx_day <- data$Time >= start & data$Time <= end

    start_night <- metadata[i, "Start_Time_Night"]
    end_night <- metadata[i, "Close_Time_Night"]
    idx_night <- data$Time >= start_night & data$Time <= end_night

    data <- data |> mutate(case_when(
      idx_day & Phase == "Day" ~ data |>
        filter(idx_day) |>
        pull(Temp) |>
        mean(na.rm = TRUE),
      idx_night & Phase == "Night" ~ data |>
        filter(idx_night) |>
        pull(Temp) |>
        mean(na.rm = TRUE),
      TRUE ~ Temp
    ))
  }
  data
}


calculate.slopes <- function(metadata, resp, data, output_path,
                             boutures_id, run,
                             save_data = FALSE,
                             waiting.time = 60, end.discard = 60) {
  #' Calculate slopes for each bouture and temperature combination.
  #' Data must be in long format with columns: Date, Time, Duration.s, Ox, Temp, Channel.
  #'  - metadata: data frame containing metadata information (ID, Channel, V.L, Volume.chamber)
  #'  - resp: data frame containing respiration information (Temperature.C, Start_Time_Day, Close_Time_Day, Start_Time_Night, Close_Time_Night, Blanc)
  #'  - data_path: path to the CSV file containing the long format data

  #' Note: If Error is :
  #'   Error in read.table(file = data_path, sep = ";", dec = ".", fill = TRUE, : plus de colonnes que de noms de colonnes
  #' It probably means that the csv file have to be opened with (sep = ";", dec = ",") instead of (sep = ";", dec = ".") or invertly.
  #' You can change it in the first line of this function.

  # data <- read.table(file = data_path, sep = ",", dec = ".",
  #                    fill = TRUE, header = TRUE) |>
  #   arrange(Time)

  # Result Table
  result <- create.result.frame(resp, metadata, boutures_id,
                                date = as.character(data$Date[1]))

  for (id in boutures_id) {
    channel <- metadata |> filter(ID == id) |> pull(Channel)
    data_channel <- data |> filter(Channel == channel)

    for (temp in resp |> filter(!Blanc) |> pull(Temperature.C)) {
      for (phase in c("Day", "Night")) {
        start_time <- resp |>
          filter(Temperature.C == temp, !Blanc) |>
          pull(paste("Start_Time", phase, sep = "_"))
        close_time <- resp |>
          filter(Temperature.C == temp, !Blanc) |>
          pull(paste("Close_Time", phase, sep = "_"))

        result <- result |> result.slopes(data_channel, id, temp, phase,
                                          metadata,
                                          start_time = start_time,
                                          close_time = close_time,
                                          waiting.time = waiting.time,
                                          end.discard = end.discard)
      }
    }

    # Calculate linear regression for the blank phases
    blank_temps <- resp |> filter(Blanc) |> pull(Temperature.C)
    for (temp in blank_temps) {
      start_time <- resp |>
        filter(Blanc, Temperature.C == temp) |>
        pull(Start_Time_Day)
      close_time <- resp |>
        filter(Blanc, Temperature.C == temp) |>
        pull(Close_Time_Day)

      result <- result |> result.slopes(data_channel, id, temp, "Blanc",
                                        metadata,
                                        start_time = start_time,
                                        close_time = close_time,
                                        waiting.time = waiting.time,
                                        end.discard = end.discard)
    }
  }

  if (save_data) {
    write.csv(result,
              paste(output_path, "/", "results_", run, ".csv", sep = ""),
              sep = ";", dec = ".", row.names = FALSE)
  }
  result
}


create.result.frame <- function(resp, metadata, boutures_id, date) {
  temps <- resp |> filter(!Blanc) |> pull(Temperature.C)

  res <- expand.grid(ID = boutures_id, Temp = temps,
                     Phase = c("Day", "Night"),
                     stringsAsFactors = FALSE) |>
    arrange(ID, Temp, Phase) |>
    mutate(Date = date)

  num_cols <- c("Variance.Temp", "RawSlope", "Rsquared", "Slope")
  res[num_cols] <- NA_real_

  blanc_res <- expand.grid(ID = boutures_id,
                           Temp = resp |> filter(Blanc) |> pull(Temperature.C),
                           Phase = "Blanc",
                           stringsAsFactors = FALSE) |>
    arrange(ID, Temp, Phase) |>
    mutate(Date = date)

  blanc_res[num_cols] <- NA_real_

  result <- rbind(res, blanc_res) |> arrange(ID, Temp, Phase) |>
    left_join(metadata[, c("ID", "Channel", "V.coral.L",
                           "V.chamber.L", "Surface.m2")],
              by = "ID")

  result <- result[, c("ID", "Date", "Channel", "Phase", "Temp",
                       num_cols,
                       "V.coral.L", "V.chamber.L", "Surface.m2")]
  result
}


result.slopes <- function(result, data, id, temp, phase,
                          metadata,
                          start_time, close_time,
                          waiting.time = 60, end.discard = 60) {
  data_id_filtered <- data |>
    filter(
      as.numeric(hms(Time)) > as.numeric(hms(start_time)) + waiting.time &
        as.numeric(hms(Time)) < as.numeric(hms(close_time)) - end.discard
    )
  temp_mean <- data_id_filtered |> pull(Temp) |> mean(na.rm = TRUE)
  temp_var <- data_id_filtered |> pull(Temp) |> var(na.rm = TRUE)

  v.coral <- metadata |> filter(ID == id) |> pull(V.coral.L)
  v.chamber <- metadata |> filter(ID == id) |> pull(V.chamber.L)
  s.coral <- metadata |> filter(ID == id) |> pull(Surface.m2)

  model <- lm(Ox ~ Duration.s, data = data_id_filtered)

  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "RawSlope"] <- model$coefficients[2]
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Rsquared"] <- summary(model)$r.squared
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Slope"] <- model$coefficients[2] *
    (v.chamber - v.coral) * 3600 / s.coral
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Variance.Temp"] <- temp_var
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Temp"] <- temp_mean

  result
}


raw.plot <- function(data, resp, unit, wayout = NULL,
                     save_image = TRUE) {
  for (i in 1:8) {
    channel_data <- data |>
      filter(Channel == i) |>
      mutate(Time = hms::as.hms(Time))

    c <- ggplot(channel_data, environment = environment()) +
      geom_point(aes(
        x = Time,
        y = as.numeric(as.character(Ox))
      )) +
      ylab(paste("Ox.", i, unit, sep = "")) +
      xlab("Time (sec)") +
      scale_x_discrete(
        breaks = channel_data$Time[seq(1, nrow(channel_data), by = 2000)]
      ) +
      scale_x_discrete(
        breaks = channel_data$Time[seq(1, nrow(channel_data), by = 2200)]
      ) +
      geom_rect(data = resp |> filter(Date == as.Date(date)),
                aes(xmin = Start_Time_Day,
                    xmax = Close_Time_Day,
                    ymin = - Inf,
                    ymax = Inf,
                    fill = "darkred"),
                alpha = 0.3) +
      geom_rect(data = resp |> filter(Date == as.Date(date)),
                aes(xmin = Start_Time_Night,
                    xmax = Close_Time_Night,
                    ymin = - Inf,
                    ymax = Inf,
                    fill = "darkblue"),
                alpha = 0.3) +
      scale_fill_manual(
        values = c("darkred" = "darkred", "darkblue" = "darkblue"),
        labels = c("darkred" = "Jour", "darkblue" = "Nuit")
      )

    print(c)

    if (save_image) {
      ggsave(c,
        filename = paste("Ox.", i, ".pdf", sep = ""),
        path = wayout, width = 20, height = 4
      )
    }
  }
}


plot_channel_n <- function(data, date, phase = "Blank", temp, unit = "mg/s", wayout) {
  plot_channel <- ggplot(data, environment = environment()) +
    geom_point(aes(x = Time, y = Ox)) +
    ylab(paste("Concentration en Ox. ", unit)) +
    xlab("Time (sec)") +
    ggtitle(paste("Phase :", phase, "- Temp :", temp)) +
    facet_wrap(~ ID)

  print(plot_channel)
  ggsave(plot_channel,
    filename = paste(phase, "_", round(temp), "C.pdf", sep = ""),
    path = wayout, width = 20, height = 4
  )
}


create.temp.frame <- function(resp, date) {
  temps <- r |> filter(!Blanc) |> pull(Temperature.C)

  res <- expand.grid(Date = as.Date(date), Temp = temps,
                     Phase = c("Day", "Night"),
                     stringsAsFactors = FALSE) |>
    arrange(Temp, Phase)

  blanc_res <- expand.grid(Date = as.Date(date),
                           Temp = r |> filter(Blanc) |> pull(Temperature.C),
                           Phase = "Blanc",
                           stringsAsFactors = FALSE) |>
    arrange(Temp, Phase)

  temp.results <- rbind(res, blanc_res) |>
    arrange(Date, Temp, Phase) |>
    mutate(Variance.Temp = NA)

  temp.results
}

calculate.var.temp <- function(resp, data, date,
                               waiting.time = 60, end.discard = 60) {
  temp.results <- create.temp.frame(resp, as.Date(date))

  for (temp in resp |> filter(!Blanc) |> pull(Temperature.C)) {
    for (phase in c("Day", "Night")) {
      start_time <- resp |>
        filter(Temperature.C == temp, !Blanc) |>
        pull(paste("Start_Time", phase, sep = "_"))
      close_time <- resp |>
        filter(Temperature.C == temp, !Blanc) |>
        pull(paste("Close_Time", phase, sep = "_"))

      phase_data <- data |>
        filter(
          as.numeric(hms(Time)) > as.numeric(hms(start_time)) + waiting.time &
            as.numeric(hms(Time)) < as.numeric(hms(close_time)) - end.discard
        )
      temp_var <- phase_data |> pull(Temp) |> var(na.rm = TRUE)

      temp.results[(temp.results$Date == date) & (temp.results$Temp == temp) &
                    (temp.results$Phase == phase),
                  "Variance.Temp"] <- temp_var
    }
  }

  for (temp in r |> filter(Blanc) |> pull(Temperature.C)) {
    start_time <- r |>
      filter(Temperature.C == temp, Blanc) |>
      pull("Start_Time_Day")
    close_time <- r |>
      filter(Temperature.C == temp, Blanc) |>
      pull("Close_Time_Day")

    phase_data <- data |>
      filter(
        as.numeric(hms(Time)) > as.numeric(hms(start_time)) + waiting.time &
          as.numeric(hms(Time)) < as.numeric(hms(close_time)) - end.discard
      )
    temp_var <- phase_data |> pull(Temp) |> var(na.rm = TRUE)

    temp.results[(temp.results$Date == date) & (temp.results$Temp == temp) &
                    (temp.results$Phase == "Blanc"),
                  "Variance.Temp"] <- temp_var
  }

  temp.results
}