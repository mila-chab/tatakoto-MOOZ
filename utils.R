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
           Temperature = as.numeric(gsub(",", ".", Temperature.C)))

  if (!is.null(date)) {
    df <- df |> filter(Date == date)
    resp <- resp |> filter(Date == date)
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
  df |>
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


calculate.slopes <- function(root_folder, data_path, output_path,
                             run, save_data = FALSE) {
  # Extract metadata
  frames <- extract.metadata(root_folder)
  metadata <- frames$metadata
  resp <- frames$resp
  print(head(metadata))
  print(head(resp))

  # ID <- metadata$ID

  data <- read.table(file = data_path, sep = ",", fill = TRUE,
                      header = TRUE) |>
          arrange(Time)

  # Result Table
  result <- data.frame(matrix(ncol = 26, nrow = length(metadata$ID)))
  colnames(result) <- c(
    "ID", "Date", "Phase", "Temp", "RawSlope", "Rsquared", "Slope"
  )
  result <- result |> mutate(ID = metadata |> pull(ID),
                             Date = as.character(data$Date[[1]])) #|>
    # left_join(metadata[, c("ID", "Chamber", "V.L", "Volume.chamber")], by = c("ID" = "ID"))

  for (id in metadata$ID) {
    channel <- metadata[which(metadata$ID == id), "Channel"]
    data_channel <- data |> filter(Channel == channel)

    for (temp in metadata$Temp) {
      start_time <- metadata |>
        filter(Temp == temp) |>
        pull(Start_Time_Day)
      close_time <- metadata |>
        filter(Temp == temp) |>
        pull(Close_Time_Day)

      data_id_filtered <- data_channel |>
        filter(
          as.numeric(hms(Time)) > as.numeric(hms(start_time)) + waittime &
            as.numeric(hms(Time)) < as.numeric(hms(start_time)) +
              (close_time - enddiscard)
        ) |>
        mutate(Ox = as.numeric(as.character(Ox)))

      b <- lm(
        data_id_filtered |> pull(Ox) ~ data_id_filtered |> pull(Time.s)
      )

      result[result$ID == id, "RawSlope"] <- b$coefficients[2]
      result[result$ID == id, "Rsquared"] <- summary(b)$r.squared
      # TODO : modifier en fonction de la V.L et du volume de la chambre
      result[result$ID == id, "Slope"] <- b$coefficients[2]
    }
  }
  # final : Date, Temp, ID, Phase, RawSlope, Rsquared, Slope
  result
}
