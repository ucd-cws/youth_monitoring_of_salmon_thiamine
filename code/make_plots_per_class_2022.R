## Code to Pull Data, Wrangle, and Visualize!
## written by R. Peek 2021

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(scales)
library(glue)
library(lubridate)
library(here)
library(cowplot)

# Download Data ---------------------------------------------------------------

source("code/functions/f_download_data_rap.R")
f_download_data()

source("code/functions/f_download_eggs_allotted.R")
f_download_eggs_allotted()

# Load Data ---------------------------------------------------------------

source("code/functions/f_import_data.R")
df <- f_import_data()

# read in number of eggs per tank and join:
eggs_lookup <- read_csv("data_raw/raw_eggs_allotted_downloaded_2022-01-21.csv") %>%
  select(site=school, tank_number, total_egg_count, avg_th, status)

summary(eggs_lookup)

# Check Data -----------------------------------------------------

# filter out anything with Test/Testing
df_orig <- nrow(df)

df <- df %>%
  filter(!grepl("Test|Testing|test", comments)) %>%
  filter(tank_number %in% c(1:8, 16, 17, 18)) # filter to only tanks of current exp

glue("Full data had {df_orig} rows, {df_orig - nrow(df)} dropped")

# if dead in previous time they need to stay dead
df <- df %>%
  arrange(site, date) %>%
  group_by(site, tank_number) %>%
  mutate(dead_qa = case_when(
    lag(dead) - dead > 0 ~ "CHECK NUMBERS",
    TRUE ~ "OK"
  ), .after=dead)

# any issues?
table(df$dead_qa) # should all be OK

# Join the Original Number of Eggs Provided -------------------------------

# add initial timestamp
df <- left_join(df, eggs_lookup)

# Make Egg Status Summary By Class -----------------------------------

df_status <- df %>%
  select(site, date, tank_number, total_egg_count, eggs_hatched, dead) %>%
  group_by(site, date, tank_number) %>%
  mutate(unhatched = total_egg_count - eggs_hatched, .after=eggs_hatched) %>%
  tidyr::pivot_longer(c(total_egg_count, eggs_hatched, unhatched, dead), names_to = "egg_status", values_to = "count") %>%
  ungroup() %>%
  mutate(status = case_when(
    egg_status == "total_egg_count" ~ "Total",
    egg_status == "eggs_hatched" ~ "Hatched",
    egg_status == "dead" ~ "Dead",
    egg_status == "unhatched" ~ "Unhatched"))

df_status_prop <- df %>%
  mutate(unhatched = total_egg_count - eggs_hatched, .after=eggs_hatched) %>%
  # select cols of interest
  select(site, date, tank_number, total_egg_count, unhatched, eggs_hatched:dead) %>%
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
    TRUE ~ status))

# Barplot of Egg Status by Class ---------------------------

# eggs hatched by status type
g1 <- ggplot() +
  geom_hline(yintercept = 1, color="gray", lty=2) +
  geom_col(data=df_status_prop %>%
             filter(status2 %in% c("Hatched", "Unhatched", "Dead")),
           aes(x=date, y=prop, fill=status2),
            show.legend=TRUE, position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_cowplot() +
  scale_x_date(date_labels = "%m-%d") +
  scale_fill_colorblind("Status") +
  cowplot::background_grid("y") +
  #scale_fill_few("Medium", "Status") +
  labs(subtitle = "Eggs status by Site",
       y="Proportion Eggs Hatched", x="")+
  facet_wrap(tank_number~site, scales = "free_x") +
  theme(axis.text.x = element_text(angle=70, vjust = 0.5),
        plot.background = element_rect(fill="white"))

g1


# eggs hatched by status type
g1b <- ggplot() +
  geom_hline(yintercept = 1, color="gray", lty=2) +
  geom_ribbon(data=df_status_prop %>%
                filter(status2 %in% c("Hatched", "Unhatched")),
           aes(x=date, ymax=prop, ymin=0, fill=status2),
           show.legend=TRUE, alpha=0.7) +
  geom_line(data=df_status_prop %>%
              filter(status2 %in% c("Hatched", "Unhatched")),
              aes(x=date, y=prop, color=status2),
              show.legend=TRUE, lwd=1.7) +
  geom_point(data=df_status_prop %>%
               filter(status2 %in% c("Hatched", "Unhatched")),
             aes(x=date, y=prop, fill=status2),
             show.legend=TRUE, alpha=0.7, pch=21) +
  # only dead
  geom_line(data=df_status_prop %>%
              filter(status2 %in% c("Dead")),
            aes(x=date, y=prop, color=status2),
            show.legend=FALSE, alpha=0.9, lwd=0.6, lty=4) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_cowplot() +
  scale_x_date(date_labels = "%m-%d") +
  cowplot::background_grid("y") +
  scale_fill_manual("Status", values = c("Hatched"="#0072B2",
                                          "Unhatched" = "#E69F00",
                                          "Dead"= "black")) +
  scale_color_manual("Status", values = c("Hatched"="#0072B2",
                                          "Unhatched" = "#E69F00",
                                          "Dead"= "black")) +
  #scale_fill_few("Medium", "Status") +
  labs(subtitle = "Eggs status by school and site",
       y="Proportion Eggs Hatched", x="")+
  facet_wrap(site~tank_number, scales = "free_x") +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.background = element_rect(fill="white"))

g1b

# now by only hatched eggs:

# eggs hatched by status type
df_filt <- df_status_prop %>%
  filter(!status2 %in% c("Total", "Dead"))
g2 <- ggplot() +
  # hatched
  geom_line(data=df_filt %>% filter(status2 %in% c("Hatched", "Unhatched")),
            aes(x=date, y=prop, group=status2), color="gray40", alpha=0.6) +
  geom_point(data=df_filt %>% filter(status2 %in% c("Hatched", "Unhatched")),
             aes(x=date, y=prop, fill=status2),
             alpha=0.8, pch=21, show.legend=TRUE, size=2.5) +

  # everything else
  geom_line(data=df_filt %>% filter(!status2 %in% c("Hatched", "Unhatched")),
             aes(x=date, y=prop, color=status2),
             alpha=0.8, show.legend=FALSE) +
  geom_point(data=df_filt %>% filter(!status2 %in% c("Hatched", "Unhatched")),
             aes(x=date, y=prop, color=status2, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(site~tank_number) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Status") +
  scale_fill_manual(NULL, values=c("gray40", "gray90")) +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="",
       caption = "No effect observed if status is only = 'Hatched'")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.background = element_rect(fill="white")) +
  guides(fill=guide_legend(override.aes =
                             list(fill=c("gray40", "gray90"),
                                  shape=c(21,21), size=c(4,4))))
g2

# eggs hatched by status type: Laying on Side
# g2b <- df_status_prop %>%
#   filter(status2 %in% c("Laying on side")) %>%
#   ggplot() +
#   geom_line(aes(x=date, y=prop, group=tank_number), show.legend=FALSE, color="gray70")+
#   geom_point(aes(x=date, y=prop,fill=site, group=tank_number), pch=21, size=4, show.legend=TRUE) +
#   facet_grid(tank_number~., scales = "free_y") +
#   scale_y_continuous(labels = scales::percent_format(scale = 100)) +
#   #facet_wrap(site~tank_number) +
#   theme_cowplot() +
#   cowplot::background_grid("y") +
#   scale_x_date(date_labels = "%m-%d-%y") +
#   scale_fill_viridis_d("Site") +
#   labs(subtitle = "Status: Proportion laying on side",
#        y="Proportion of Total Hatched Eggs Laying on Side", x="")+
#   theme(axis.text.x = element_text(angle=90, vjust = 0.5),
#         plot.background = element_rect(fill="white"))
#
# g2b


# Eggs Hatched over time by HS ---------------------------

# eggs hatched
g3 <- ggplot() +
  geom_hline(data = df, aes(yintercept = total_egg_count), color="gray", lty=2, lwd=1) +
  geom_line(data=df, aes(x=date, y=eggs_hatched),
            show.legend=FALSE) +
  geom_point(data=df, aes(x=date, y=eggs_hatched),
             show.legend =FALSE) +
  geom_point(data=df %>% group_by(site, tank_number) %>% filter(date==max(date)), aes(x=date, y=eggs_hatched), pch=21, size=4, fill="maroon") +
  facet_wrap(tank_number + site ~.) +
  theme_cowplot() +
  cowplot::background_grid("y") +
  #scale_x_date(date_labels = "%m-%d-%y") +
  #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d-%y") +
  labs(subtitle = "Eggs hatched through time (current = maroon)",
       y="Eggs Hatched", x="",
       caption = "Gray dashed line = total eggs provided in tank") +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.background = element_rect(fill="white"))
g3


# Patchwork -----------------------------------------------------------

ggsave(g1b, filename = glue("figures/summary_2022_eggs_hatched_updated_{Sys.Date()}.png"), width = 11, height = 8, dpi=300)

ggsave(g2, filename = glue("figures/summary_2022_egg_status_hatched_updated_{Sys.Date()}.png"), width = 11, height = 8, dpi=300)

ggsave(g3, filename = glue("figures/summary_2022_eggs_hatched_over_time_updated_{Sys.Date()}.png"), width = 11, height = 7, dpi=300)

# library(patchwork)
# (g1 + g2) / g3
#
# ggsave(filename = glue("figures/summary_plots_updated_{Sys.Date()}.png"), width = 11, height = 8, dpi=300)
