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
g1 <- ggplot() +
  geom_col(data=df_class, aes(x=date, y=values, fill=status2),
            show.legend=TRUE)+
  ylim(c(0,35)) +
  facet_wrap(.~schools) +
  theme_cowplot() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_fill_colorblind("Status") +
  labs(subtitle = "Simulated data for eggs status by HS",
       y="Eggs Hatched", x="Obs Date")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

g1

# Eggs Hatched over time by HS ---------------------------

# eggs hatched
g2 <- ggplot() +
  geom_line(data=df, aes(x=date, y=eggs_hatched),
            show.legend=FALSE) +
  geom_point(data=df, aes(x=date, y=eggs_hatched),
             show.legend =FALSE) +
  geom_point(data=df %>% group_by(schools) %>% filter(date==max(date)), aes(x=date, y=eggs_hatched), pch=21, size=4, fill="maroon") +
  geom_text(data=df %>% group_by(schools) %>% filter(date==max(date)), aes(x=date, y=eggs_hatched), label="Current", nudge_x = -5, color="maroon") +
  ylim(c(0,35)) +
  facet_wrap(.~schools) +
  theme_cowplot() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(subtitle = "Simulated data for eggs hatched through time",
       y="Eggs Hatched", x="Obs Date")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))
g2


# Survival Curve ----------------------------------------------------------

# survival
g3 <- ggplot() +
  geom_point(data=df, aes(x=thiamine, y=surv_prcnt, fill=schools),
            show.legend=TRUE, pch=21, size=3) +
  geom_smooth(data=df, aes(x=thiamine, y=surv_prcnt),method = "gam", color="orange") +
  #geom_smooth(data=df, aes(x=thiamine, y=surv_prcnt), method = "loess", span=1.5)+
  theme_cowplot() +
  scale_fill_colorblind("School") +
  labs(title = "Simulated data: survival vs. thiamine",
       y="% Survival (# alive / total)", x="Female egg thiamine concentration (\u03BCg/g)")

g3


# Patchwork -----------------------------------------------------------

library(patchwork)
(g1 + g2) / g3

ggsave(filename = "figures/simulated_plots_of_data.png", width = 10, height = 8, dpi=300)
