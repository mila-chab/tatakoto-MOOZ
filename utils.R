library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

retrieve.raw.data <- function(root_folder, folder_path, output_path, run, skip = 136, save_data = FALSE) {
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
  table <- read.table(file = paste(path, file, sep = "/"), sep = "\t", header = TRUE, fill = TRUE,  skip = skip) |>
    clean_column_names(column_names)

  if (save_data) {
    write.csv(table,
              paste(output_path, "/", run, ".csv", sep = ""),
              sep = ";", dec = ".", row.names = FALSE)
  }
}


extract.metadata <- function(root_folder) {
  metadata <- read.table(file = paste(root_folder, "metadata.csv", sep = ""),
                         sep = ";", fill = TRUE, header = TRUE)
  temp_metadata <- read.table(file = paste(root_folder, "temperature_metadata.csv", sep = ""),
                              sep = ";", fill = TRUE, header = TRUE)

  temp_metadata <- temp_metadata |>
    mutate(Start_Time_Day = hms::as_hms(Start_Time_Day),
           Close_Time_Day = hms::as_hms(Close_Time_Day),
           Start_Time_Night = hms::as_hms(Start_Time_Night),
           Close_Time_Night = hms::as_hms(Close_Time_Night),
           Temperature = as.numeric(gsub(",", ".", Temperature.C)))

  list(metadata = metadata, temp_metadata = temp_metadata)
}

clean_column_names <- function(df, column_names) {
  columns_reference <- as.vector(
        c(1:3, sapply(
            0:3,
            function(i) c(4 + 18*i, 12 + 18*i))
        ,
        sapply(
            0:3,
            function(i) c(4 + 18*i + 77, 12 + 18*i + 77)
        ))
    )

  df <- df[, columns_reference]
  colnames(df) <- column_names
  df
}


df_to_long_series <- function(df) {
  df_long <- df |>
    pivot_longer(
      cols = starts_with("Ox."),
      names_to = c(".value", "Channel"),
      names_sep = "\\."
    ) |>
    mutate(Channel = as.numeric(Channel))

  df_long
}
