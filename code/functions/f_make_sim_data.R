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
  eggs_hatched <- sort(round(abs(sample(rnorm(100, 19, 6), 28, replace = FALSE)),0))
  swimming_up <- sort(round(eggs_hatched * abs(rnorm(5, mean = 0.2, sd = 0.1)), 0))
  swimming_up_but_down <- round(swimming_up * 0.4, 0)
  swimming_up_but_spinning <- round(swimming_up * 0.6, 0)
  dead <- round(eggs_hatched * .1)

  sim_df <- tibble(schools, datetimes, temps, clarity,
                   eggs_hatched, swimming_up,
                   swimming_up_but_down,
                   swimming_up_but_spinning, dead)

  sim_df$surv_prcnt <- (sim_df$eggs_hatched/35)*100
  #sim_df$survival <- sim_df$eggs_hatched/ifelse(sim_df$dead==0, NA_real_, sim_df$dead)
  #sim_df$surv_prcntle <- round(ecdf(sim_df$survival)(sim_df$survival),4)*100
  # make fake thiamine based on percentile
  sim_df <- sim_df %>%
    mutate(thiamine = case_when(
      surv_prcnt < 50 ~ sample(round(runif(50,min=0.01, max=3),2), nrow(sim_df)),
      surv_prcnt >=50 ~ sample(round(runif(50,min=5.5, max=12),2), nrow(sim_df))))

  return(sim_df)

}
