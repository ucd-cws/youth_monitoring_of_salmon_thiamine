# Read Data from googlesheets

library(here) # to set directory to project location
library(googlesheets4) # to read from googlesheet
library(janitor) # to clean column names
library(dplyr) # to wrangle and clean data
library(lubridate) # to fix dates and times
library(glue) # to paste names together
library(readr) # to write data back out as csv

f_download_data <- function(deauth=FALSE){

  # First time you run this it will open a browser and
  # request access. Check "see all spreadsheets" and continue.
  # It requires the email linked to your google account with access to gsheet
  # make sure .secrets is in your .gitignore file
  if(deauth==TRUE){

    # turn off auth (e.g., if using publically shared sheet shared with anyone)
    gs4_deauth()
  } else {

    # Authorize Googlesheets
    options(gargle_oauth_cache = here::here(".secrets"))

    # auth for sheets for readonly
    gs4_auth(email = Sys.getenv("MYEMAIL"), cache = here::here(".secrets"),
             scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

    # Read in Data ----------------------------

    # the form: https://docs.google.com/forms/d/e/1FAIpQLSco575CkSIVTov7hyi4ajf3hbzt0rOGM0ubjFAlq8peIBZEuw/viewform

    # the datasheet
    # https://docs.google.com/spreadsheets/d/1sLqzWvos3etI74PW_V_uGDZ3QsCqM5nxB4gGK6JcZ3s/edit?resourcekey#gid=787935715

    # set the ID of the sheet
    gs_id <- "1sLqzWvos3etI74PW_V_uGDZ3QsCqM5nxB4gGK6JcZ3s"
    wb_id <- gs4_get(gs_id) # here we load the id

    # now check names of worksheets (tabs):
    (wb_names <- googlesheets4::sheet_names(wb_id)) # 'Form Responses 1' is default

    # import data from workbook (wb) and sheet 1 (can use full tab name too)
    wb_data <- read_sheet(wb_id, sheet = 1) %>% clean_names()

    print(glue("Data successfully imported from googlesheet!"))
    # Tidy Data ------------------------------------

    # do some data scrubbing/cleaning to remove emails and timestamps
    scrubbed_data <- wb_data %>%
      select(-c(timestamp, email_address)) %>%
      mutate(date_time_of_observation = ymd_hms(date_time_of_observation))

    print(glue("Data successfully scrubbed. There are {nrow(scrubbed_data)} records."))

    # Write out Data ------------------------------
    write_csv(scrubbed_data, glue("data_raw/raw_gsheet_downloaded_{Sys.Date()}.csv"))
    print(glue("Data saved to data_raw."))

  }
}


