# plots



# Load Libraries ----------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(scales)
library(glue)
library(here)
library(cowplot)

# Load Data ---------------------------------------------------------------

source("code/functions/f_make_sim_data.R")
df <- f_make_sim_data() # warning ok

# make date col
df$date <- as.Date(df$datetimes)


# Barplot of Egg Status by Class ---------------------------

df_class <- df %>%
  select(schools, date, eggs_hatched, dead) %>%
  group_by(schools, date) %>%
  tidyr::pivot_longer(c(eggs_hatched, dead), names_to = "egg_status", values_to = "count") %>%
  group_by(schools, date) %>%
  mutate(unhatched = 35 - sum(count)) %>%
  group_by(schools, date) %>%
  tidyr::pivot_wider(id_cols = c(schools, date, egg_status),names_from =c( egg_status), values_from = c(count, unhatched)) %>%
select(-unhatched_dead) %>%
  tidyr::pivot_longer(cols = c(count_eggs_hatched, count_dead, unhatched_eggs_hatched), names_to = "status", values_to = "values") %>%
  ungroup() %>%
  mutate(status2 = case_when(
    status == "count_eggs_hatched" ~ "Hatched",
    status == "count_dead" ~ "Dead",
    status == "unhatched_eggs_hatched" ~ "Unhatched",
    TRUE ~ status))


# eggs hatched by status type
ggplot() +
  geom_col(data=df_class, aes(x=date, y=values, fill=status2),
            show.legend=TRUE)+
  ylim(c(0,35)) +
  facet_wrap(.~schools) +
  theme_cowplot() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Simulated data for eggs status by class",
       y="Eggs Hatched", x="Obs Date")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))


# Eggs Hatched over time by  Class ---------------------------

# eggs hatched
ggplot() +
  geom_line(data=df, aes(x=date, y=eggs_hatched, fill=schools),
            show.legend=FALSE)+
  geom_point(data=df, aes(x=date, y=eggs_hatched, fill=schools),
             show.legend =FALSE)+
  ylim(c(0,35)) +
  facet_wrap(.~schools) +
  theme_cowplot() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Simulated data for eggs hatched through time",
       y="Eggs Hatched", x="Obs Date")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))
