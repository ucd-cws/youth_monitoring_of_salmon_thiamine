# thiamine plots

library(tidyverse)
library(ggthemes)
library(scales)
library(glue)
library(cowplot)

# Data --------------------------------------------------------------------

th_df <- read_csv("data_raw/raw_eggs_allotted_downloaded_2022-03-07.csv") %>%
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


# EGG STATUS BY DATE ------------------------------------------------------

# now plot ALL
ggplot() +
  geom_point(data=th_status,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=th_status,
              aes(x=date, y=prop, color=th_status, group=th_status),
              method = "glm",
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  #facet_wrap(status2~th_status) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")+
  theme(plot.background = element_rect(fill="white"))

#ggsave(filename = "figures/thiamine_vs_status_prelim.png",
#       width = 11, height = 8.5, dpi=300)


# PLOT BY SPECIFIC STATUS -------------------------------------------------

# split out by status (status2)
stat_dead <- filter(th_status, status2=="Dead")
stat_side <- filter(th_status, status2=="Laying on side")
stat_swim <- filter(th_status, status2=="Swimming up")
stat_spin <- filter(th_status, status2=="Spinning")


# EGG STATUS BY DATE: SIDE --------------------------------------------

# on side
ggplot() +
  geom_point(data=stat_side,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=stat_side,
              aes(x=date, y=prop, color=th_status, group=th_status),
              #method = "gam", #formula=(y~x)),
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(status2~th_status) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")+
  theme(plot.background = element_rect(fill="white"))


# EGG STATUS BY DATE: DEAD ------------------------------------------------

# dead
ggplot() +
  geom_point(data=stat_dead,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=stat_dead,
              aes(x=date, y=prop, color=th_status, group=th_status),
              #method = "glm", #formula=(y~x)),
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(status2~th_status) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")+
  theme(plot.background = element_rect(fill="white"))



# EGG STATUS BY DATE: SPINNING --------------------------------------------

# spinning
ggplot() +
  geom_point(data=stat_spin,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=stat_spin,
              aes(x=date, y=prop, color=th_status, group=th_status),
              method = "glm", #formula=(y~x)),
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(status2~th_status) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")+
  theme(plot.background = element_rect(fill="white"))

# EGG STATUS BY DATE: SWMMING UP --------------------------------------------

# swimming
ggplot() +
  geom_point(data=stat_swim,
             aes(x=date, y=prop, color=th_status, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=stat_swim,
              aes(x=date, y=prop, color=th_status, group=th_status),
              method = "gam", #formula=(y~x)),
              se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(status2~th_status) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status") +
  scale_color_colorblind("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="")+
  theme(plot.background = element_rect(fill="white"))

# now plot just each status against actual thiamine
ggplot() +
  # low
  geom_rect(aes(xmin=0, xmax=5, ymin=0, ymax=1 ), fill="red2", alpha=0.2) +
  geom_text(label="Low", aes(x=2, y=0.4), color="maroon", size=10, family="Bebas Neue", alpha=0.5)+
  # med
  geom_rect(aes(xmin=5, xmax=8, ymin=0, ymax=1 ), fill="orange2", alpha=0.2) +
  geom_text(label="MED", aes(x=6.5, y=0.63), color="darkorange", size=10, family="Bebas Neue", alpha=0.5)+
  # high/good
  geom_rect(aes(xmin=8, xmax=Inf, ymin=0, ymax=1 ), fill="seagreen", alpha=0.2) +
  geom_text(label="HIGH", aes(x=11, y=0.86), color="forestgreen", size=10, family="Bebas Neue", alpha=0.5)+

  geom_jitter(data=th_status,
             aes(x=th_avg, y=prop, color=status2, shape=status2),
             alpha=0.8, show.legend=TRUE, size=4) +
  geom_smooth(data=th_status, aes(x=th_avg, y=prop, group=status2, color=status2), show.legend = FALSE,
              method="lm", se = FALSE, formula = y ~ poly(x, 2)) +

  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  #facet_grid(.~status2,scales = "free_y") +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  #scale_x_date(date_labels = "%m-%d-%y") +
  scale_shape_discrete("Status", breaks=c("Dead"=21, "Laying on Side"=22, "Spinning"=23, "Swimming up"=25)) +
  scale_color_brewer("Status", palette = "Set1") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="Thiamine")+
  theme(plot.background = element_rect(fill="white"))

#ggsave(filename = "figures/thiamine_vs_status_all.png",
#       width = 11, height = 8.5, dpi=300)

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
       x="Proportion (Spinning/Swimming Up/On Side/Dead)", y="Thiamine")+
  theme(plot.background = element_rect(fill="white"))

#ggsave(filename = "figures/thiamine_vs_status_all.png",
#       width = 11, height = 8.5, dpi=300)

# now boxplots
ggplot() +
  geom_col(data=th_status,
             aes(x=th_status, y=prop, fill=th_status, group=th_status),
             alpha=0.8, show.legend=TRUE) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  #scale_color_viridis_c("Thiamine") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       x="Proportion (Spinning/Swimming Up/On Side/Dead)", y="Thiamine")+
  theme(plot.background = element_rect(fill="white"))


# thresholds
# < 5 nmoles/gram (LOW)
# 5-8 nmoles/gram (MED)
# > 8 GOOD



# Fancy Fish Plot ---------------------------------------------------------

#devtools::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)

# Fancy
# remove "swimming up"
ggplot() +
  # low
  geom_rect(aes(xmin=0, xmax=5, ymin=0, ymax=Inf ), fill="red2", alpha=0.2) +
  geom_text(label="Low", aes(x=2, y=0.4), color="maroon", size=10, family="Bebas Neue", alpha=0.5)+
  # med
  geom_rect(aes(xmin=5, xmax=8, ymin=0, ymax=Inf ), fill="orange2", alpha=0.2) +
  geom_text(label="MED", aes(x=6.5, y=0.63), color="darkorange", size=10, family="Bebas Neue", alpha=0.5)+
  # high/good
  geom_rect(aes(xmin=8, xmax=Inf, ymin=0, ymax=Inf ), fill="seagreen", alpha=0.2) +
  geom_text(label="HIGH", aes(x=11, y=0.86), color="forestgreen", size=10, family="Bebas Neue", alpha=0.5)+

  # points
  geom_jitter(data=th_status %>% filter(status2!="Swimming up"),
             aes(x=th_avg, y=prop, fill=status2, shape=status2),
             alpha=0.8, color="gray30", size=4) +
  # line poly
  geom_smooth(data=th_status %>% filter(status2!="Swimming up"),
              aes(x=th_avg, y=prop, group=status2, color=status2, fill=status2),
              show.legend = FALSE,
              method="lm", se = FALSE, formula = y ~ poly(x, 2)) +
  # line logistic
  # stat_smooth(data=th_status, aes(x=th_avg, y=prop, group=status2, color=status2),
  #            show.legend = FALSE, method="glm", se=FALSE,
  #            method.args = list(family = "binomial")) +

  # # smooth just points?
  # stat_summary_bin(data=th_status, aes(x=th_avg, y=prop, group=status2,
  #                                      color=status2),
  #                  geom = "point", fun = mean, size=3, pch=13) +
  # add a fish:
  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
              xmin = 10, xmax = 13, ymin = 0.55, ymax = 0.75,
              fill = "gray40",
              alpha = 0.8) +

  # scales
  # colors: RColorBrewer::brewer.pal(n = 4, name = "Set1")
  scale_shape_manual("Status", values=c("Dead"=21, "Laying on side"=22, "Spinning"=23))+#, "Swimming up"=25)) +
  scale_fill_brewer("Status", palette = "Set1") +
  scale_color_brewer("Status", palette = "Set1") +
  guides(fill=guide_legend(overide.aes=list(
    fill=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
    shape=c(21,22,23,25),
    color=c("gray30")))) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Proportion of Total Hatched Eggs", x="Thiamine") +
  # themes
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  theme(plot.background = element_rect(fill="white"))



ggsave(filename = "figures/thiamine_vs_status_all_rev.png",
       width = 11, height = 8.5, dpi=300)


