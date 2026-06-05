library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library("readxl")

retrieve.raw.data <- function(root_folder, folder_path, output_path, run,
                              skip = 136, save_data = FALSE) {
  path <- paste(root_folder, folder_path, sep = "/")
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
           Temperature.C = as.numeric(gsub(",", ".", Temperature.C)))

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


calculate.slopes <- function(metadata, resp, data_path, output_path,
                             boutures_id, run,
                             save_data = FALSE,
                             waiting.time = 60, end.discard = 60) {
  "
  Calculate slopes for each bouture and temperature combination.
  Data must be in long format with columns: Date, Time, Duration.s, Ox, Temp, Channel.
   - metadata: data frame containing metadata information (ID, Channel, V.L, Volume.chamber)
   - resp: data frame containing respiration information (Temperature.C, Start_Time_Day, Close_Time_Day, Start_Time_Night, Close_Time_Night, Blanc)
   - data_path: path to the CSV file containing the long format data
  "
  data <- read.table(file = data_path, sep = ",", dec = ".",
                     fill = TRUE, header = TRUE) |>
    arrange(Time)

  # Result Table
  result <- create.result.frame(resp, boutures_id,
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


create.result.frame <- function(resp, boutures_id, date) {
  temps <- resp |> filter(!Blanc) |> pull(Temperature.C)

  res <- expand.grid(ID = boutures_id, Temp = temps,
                     Phase = c("Day", "Night"),
                     stringsAsFactors = FALSE) |>
    arrange(ID, Temp, Phase) |>
    mutate(Date = date)
  res <- res[, c("ID", "Date", "Temp", "Phase")]

  num_cols <- c("RawSlope", "Rsquared", "Slope")
  res[num_cols] <- NA_real_

  blanc_res <- expand.grid(ID = boutures_id,
                           Temp = resp |> filter(Blanc) |> pull(Temperature.C),
                           Phase = "Blanc",
                           stringsAsFactors = FALSE) |>
    arrange(ID, Temp, Phase) |>
    mutate(Date = date)
  blanc_res <- blanc_res[, c("ID", "Date", "Temp", "Phase")]

  num_cols <- c("RawSlope", "Rsquared", "Slope")
  blanc_res[num_cols] <- NA_real_

  result <- rbind(res, blanc_res) |> arrange(ID, Temp, Phase) #|>
    # left_join(metadata[, c("ID", "Channel", "V.L", "Volume.chamber")], by = "ID")
  result
}


result.slopes <- function(result, data, id, temp, phase,
                          start_time, close_time,
                          waiting.time = 60, end.discard = 60) {
  data_id_filtered <- data |>
    filter(
      as.numeric(hms(Time)) > as.numeric(hms(start_time)) + waiting.time &
        as.numeric(hms(Time)) < as.numeric(hms(close_time)) - end.discard
    )
  temp_mean <- data_id_filtered |> pull(Temp) |> mean(na.rm = TRUE)

  model <- lm(Ox ~ Duration.s, data = data_id_filtered)

  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "RawSlope"] <- model$coefficients[2]
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Rsquared"] <- summary(model)$r.squared
  # TODO : modifier en fonction de la V.L et du volume de la chambre
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Slope"] <- model$coefficients[2] * 3600
  result[(result$ID == id) & (result$Temp == temp) & (result$Phase == phase),
         "Temp"] <- temp_mean

  result
}
