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

  # make some shorter names
  df <- df %>%
    rename(datetime = date_time_of_observation,
           water_temp_f = water_temperature_in_farenheit,
           eggs_hatched = number_of_eggs_hatched,
           swimming_up = of_the_eggs_that_have_hatched_how_many_fish_are_swimming_up,
           laying_on_side = of_the_fish_attempting_to_swim_how_many_if_any_fish_are_laying_on_their_side,
           spinning = of_the_fish_attempting_to_swim_how_many_if_any_fish_are_spinning,
           dead = how_many_if_any_salmon_died_since_you_began_reporting,
           started_feeding_date = have_you_started_feeding_if_yes_please_report_date,
           questions = do_you_have_any_questions,
           comments = do_you_have_additional_observations_thoughts_or_comments) %>%
    # make a date column
    mutate(date = as.Date(datetime), .after="datetime")

  print(glue("Data successfully imported, using {sort(g_files, decreasing=TRUE)[1]}"))

  # Tidy Data -----

  # drop columns that are comments/notes for now
  df_select <- df %>% select(site:dead, tank_number,
                             date_of_release,
                             number_of_salmon_released, comments, questions)

  # Write out Data ------------------------------
  write_csv(df_select, glue("data_clean/clean_salmon_thiamine_data_current.csv"))
  print(glue("Data saved to data_clean."))
  return(df_select)

}



