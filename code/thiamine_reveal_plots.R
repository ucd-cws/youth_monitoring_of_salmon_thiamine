# thiamine plots

library(tidyverse)
library(ggthemes)
library(scales)
library(glue)
library(cowplot)

# Data --------------------------------------------------------------------

th_df <- read_csv("data_raw/raw_eggs_allotted_downloaded_2022-02-04.csv") %>%
  select(school:total_egg_count)

dat <- read_csv("data_clean/clean_salmon_thiamine_data_current.csv")

# Tidy --------------------------------------------------------------------

# Join the Original Number of Eggs Provided -------------------------------

# add initial timestamp
df_all <- left_join(dat, th_df) %>%
  filter(!tank_number %in% c(10:13),
         !is.na(tank_number))

rm(dat)

# Make Egg Status Summary By Class -----------------------------------

df_status_prop <- df_all %>%
  mutate(unhatched = total_egg_count - eggs_hatched, .after=eggs_hatched) %>%
  # select cols of interest
  select(site, date, tank_number, total_egg_count, eggs_hatched:dead) %>%
  # group by date and site
  group_by(site, tank_number) %>%
  # now add proportions for comparisons and plots
  mutate(across(unhatched:dead, .fns = ~(.x/total_egg_count))) %>%
  # now make the data longer for plotting purposes
  tidyr::pivot_longer(c(total_egg_count, unhatched, eggs_hatched:dead),
                      names_to = "status",
                      values_to = "prop") %>%
  ungroup() %>%
  mutate(status2 = case_when(
    status == "eggs_hatched" ~ "Hatched",
    status == "unhatched" ~ "Unhatched",
    status == "total_egg_count" ~ "Total",
    status == "dead" ~ "Dead",
    status == "laying_on_side" ~ "Laying on side",
    status == "spinning" ~ "Spinning",
    status == "swimming_up" ~ "Swimming up",
    TRUE ~ status)) %>%

  # re-add thiamine status
  left_join(., th_df %>% select(tank_number, th_avg=avg_th, th_status=status),
            by="tank_number")

# Prelim Plots ------------------------------------------------------------

th_status <- df_status_prop %>%
  filter(!is.na(th_avg)) %>%
  filter(!status2 %in% c("Hatched", "Unhatched", "Total"))

# now plot
ggplot() +
  geom_point(data=th_status,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=th_status, aes(x=date, y=prop, color=th_status, group=th_status),
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  #facet_wrap(site~tank_number) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")

ggsave(filename = "figures/thiamine_vs_status_prelim.png",
       width = 11, height = 8.5, dpi=300)

# now plot just each status against actual thiamine
ggplot() +
  geom_point(data=th_status,
             aes(x=th_avg, y=prop, color=th_avg, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=th_status, aes(x=th_avg, y=prop), col="gray40",
              method="glm", se = FALSE, formula = y ~ poly(x, 3)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  #facet_grid(.~status2,scales = "free_y") +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  #scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_viridis_c("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="Thiamine")

ggsave(filename = "figures/thiamine_vs_status_all.png",
       width = 11, height = 8.5, dpi=300)

# now plot just each status against actual thiamine
ggplot() +
  geom_point(data=th_status,# %>% filter(status2!="Dead"),
             aes(y=th_avg, x=prop, color=th_avg),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=th_status,# %>% filter(status2!="Dead"),
              aes(y=th_avg, x=prop), col="gray40",
              method="lm", formula = y ~ poly(x, 3), se = FALSE) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_color_viridis_c("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       x="Proportion (Spinning/Swimming Up/On Side/Dead)", y="Thiamine")

ggsave(filename = "figures/thiamine_vs_status_all.png",
       width = 11, height = 8.5, dpi=300)

# now boxplots
ggplot() +
  geom_col(data=th_status,
             aes(x=th_status, y=prop, fill=th_status, group=th_status),
             alpha=0.8, show.legend=TRUE) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  #scale_color_viridis_c("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       x="Proportion (Spinning/Swimming Up/On Side/Dead)", y="Thiamine")




