# sim data

library(lubridate)
library(dplyr)

# make some fake data for now
f_make_sim_data <- function(){
  schools <- rep(c("Forest HS","Elven HS", "Hobbiton HS", "Chinook HS"),7)
  datetimes <- sort(rep(seq(ymd_hm('2021-11-07 12:00',
                              tz = "America/Los_Angeles"),
                       ymd_hm('2021-12-24 12:00',
                              tz = "America/Los_Angeles"),
                       by='weeks'), 4))
  temps <- round(sample(rnorm(100, 53, sd = 8), size = 28),1)
  clarity <- sample(c("Water is clear, pump functioning well",
                      "Water is clear, but unsure of pump functioning",
                      "Water is clouding"), 28, replace = TRUE)
  eggs_hatched <- sort(round(abs(sample(rnorm(100, 17, 6), 28, replace = FALSE)),0))
  swimming_up <- sort(round(eggs_hatched * rnorm(5, mean = 0.3, sd = 0.1), 0))
  swimming_up_but_down <- round(swimming_up * 0.4, 0)
  swimming_up_but_spinning <- round(swimming_up * 0.6, 0)
  dead <- round(eggs_hatched * .1)

  sim_df <- tibble(schools, datetimes, temps, clarity,
                   eggs_hatched, swimming_up,
                   swimming_up_but_down,
                   swimming_up_but_spinning, dead)
  return(sim_df)

}
