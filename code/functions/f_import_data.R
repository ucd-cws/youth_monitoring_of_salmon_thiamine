# Read Data and plot

library(here) # to set directory to project location
library(janitor) # to clean column names
library(dplyr) # to wrangle and clean data
library(ggplot2) # visualize
library(lubridate) # to fix dates and times
library(glue) # to paste names together
library(readr) # to import/export data
library(fs) # files and folders functions

f_import_data <- function(){

  # get paths for files
  (g_files <- fs::dir_ls("data_raw", type="file", regexp = "*gsheet*"))

  # show most recent file
  sort(g_files, decreasing = TRUE)[1]

  # read in data (df=dataframe)
  df <- read_csv(sort(g_files, decreasing = TRUE)[1],
                       show_col_types = FALSE)

  print(glue("Data successfully imported, using {sort(g_files, decreasing=TRUE)[1]}"))

  # Tidy Data -----

  # drop columns that are comments/notes for now
  df_select <- df %>% select(site:how_many_if_any_salmon_died_since_last_reporting)

  # Write out Data ------------------------------
  write_csv(df_select, glue("data_clean/clean_sample_data.csv"))
  print(glue("Data saved to data_clean."))

}



