# thiamine plots

library(tidyverse)
library(ggthemes)
library(scales)
library(glue)
library(cowplot)
library(fishualize)
library(ggtext)


# Data --------------------------------------------------------------------

th_df <- read_csv("data_raw/raw_eggs_allotted_downloaded_2023-01-10.csv") %>%
  select(school:total_egg_count)

dat <- read_csv("data_clean/clean_salmon_thiamine_data_current.csv")

# Tidy --------------------------------------------------------------------

# Join the Original Number of Eggs Provided -------------------------------

# add initial timestamp
df_all <- left_join(dat, th_df) #%>%
  #filter(!tank_number %in% c(10:13),
         #!is.na(tank_number))

rm(dat)

# get total survived
tot_surv <- df_all %>% filter(!is.na(number_of_salmon_released)) %>%
  select(-c(water_temp_f, water_clarity, comments:questions)) %>%
  mutate(survival = number_of_salmon_released/ total_egg_count) #%>%
  # label for plots
  #mutate(class_label = case_when(
    #grepl("Willows High",school) ~ "Willows",
    #grepl("Orland", school) ~ "Orland",
    #grepl("Pierce", school) ~ "Pierce",
    #grepl("Samuel Jackman", school) ~ "Samuel Jackman",
    #grepl("Red Bluff", school) ~ "Red Bluff",
    #grepl("Edward Harris Jr.", school) ~ "Edward Harris Jr.",
    #TRUE ~ school))

# fix order
tot_surv$status <- factor(tot_surv$status, levels = c("low", "intermediate", "high"))
levels(tot_surv$status)

# Boxplot of Classroom Status ---------------------------------------------

# now boxplots
ggplot() +
  geom_bar(data=tot_surv,
           aes(x=status, fill=status),
           alpha=0.8, show.legend=FALSE) +
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  scale_fill_manual("Classrooms", values = c("low"="red2", "intermediate"="darkorange", "high"="seagreen")) +
  labs(subtitle = "2023 Thiamine Concentrations by Classroom",
       x="", y="Number of Classrooms")+ #coord_flip() +
  theme(plot.background = element_rect(fill="white")) +
  ggtext::geom_textbox(data=tot_surv, x="intermediate", y=5, label="The majority of classroom tanks had **High** thiamine levels. What level do you think your tank was?", family="Roboto")

ggsave(filename = "figures/thiamine_by_classroom_barplot.png", dpi=300, width = 10, height = 8)

# Plot By Thiamine Value WITHOUT CLASS --------------------------------------------------

# by thiamine value (without classroom ID)
set.seed(111)
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
  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
              xmin = 10, xmax = 13, ymin = 0.55, ymax = 0.75,
              fill = "gray40",
              alpha = 0.8) +
  geom_jitter(data=tot_surv, aes(y=survival, x=avg_th, fill=status), pch=21, size=4.5, show.legend = FALSE) +
  #ggtext::geom_textbox(data=tot_surv, x=12, y=0.25, label="Thiamine concentration groups (*LOW/MED/HIGH*) as shown here are generalized, they may be different for different populations, regions, or life-history strategies. There's much we don't know!", family="Roboto", color="gray40") +
  labs(subtitle = "2023 Survival vs. Thiamine Concentration",
       y="Survival (Released / Total Eggs)", x="Thiamine (nmol/g)") +
  scale_fill_manual("", values = c("Low"="red2", "Intermediate"="darkorange", "High"="seagreen")) +
  # themes
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  theme(plot.background = element_rect(fill="white"))

ggsave(filename = "figures/thiamine_by_survival_noclassid.png", width = 10, height = 8, dpi=300)

# Plot By Thiamine Value WITH CLASS --------------------------------------------------

# by thiamine value (with classroom ID)
set.seed(111)
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
  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
              xmin = 10, xmax = 13, ymin = 0.55, ymax = 0.75,
              fill = "gray40",
              alpha = 0.8) +
  ggrepel::geom_text_repel(data=tot_surv, aes(y=survival, x=avg_th, label=glue("{school}-{tank_number}")), color="gray40",
                           segment.color="gray50", segment.alpha=0.5,
                           point.padding = 0.2, seed=111,
                           min.segment.length = .1,force = 1.3,
                           nudge_y = -0.1, nudge_x = -0.1
  ) +
  geom_jitter(data=tot_surv, aes(y=survival, x=avg_th, fill=status), pch=21, size=4.5, show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0,60,3))) +
  labs(subtitle = "2023 Survival vs. Thiamine Concentration",
       y="Survival (Released / Total Eggs)", x="Thiamine (nmol/g)") +
  scale_fill_manual("", values = c("Low"="red2", "Intermediate"="darkorange", "High"="seagreen")) +
  # themes
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  theme(plot.background = element_rect(fill="white"))

ggsave(filename = "figures/thiamine_by_survival_wclassid.png", width = 10, height = 8, dpi=300)

# Plot By Thiamine Value WITH DATA --------------------------------------------------

thidat<-read_csv("data_raw/WRCS_Survival_Data_HBell.csv") %>%
  janitor::clean_names()

# by thiamine value (with classroom ID)
set.seed(111)
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
  # now add exp data underneath!
  stat_smooth(data=thidat, aes(y=survival_proportion, x=total_thiamine_nmol_g), method="glm", se=FALSE, method.args = list(family="binomial"), color="gray40", lwd=0.5) +
  geom_point(data=thidat, aes(y=survival_proportion, x=total_thiamine_nmol_g), pch=21, size=2.5, fill="gray10", alpha=0.7) +

  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
              xmin = 10, xmax = 13, ymin = 0.55, ymax = 0.75,
              fill = "gray40",
              alpha = 0.8) +
  ggrepel::geom_text_repel(data=tot_surv, aes(y=survival, x=avg_th, label=glue("{class_label}-{tank_number}")), color="gray40",
                           segment.color="gray50", segment.alpha=0.5,
                           point.padding = 0.2, seed=111,
                           min.segment.length = .1,force = 1.3,
                           nudge_y = -0.1, nudge_x = -0.1
  ) +
  geom_jitter(data=tot_surv, aes(y=survival, x=avg_th, fill=status), pch=21, size=4.5, show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0,60,3))) +
  labs(subtitle = "Survival vs. Thiamine Concentration",
       y="Proportion Survived", x="Thiamine (nmol/g)") +
  scale_fill_manual("", values = c("Low"="red2", "Intermediate"="darkorange", "High"="seagreen")) +
  # themes
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  theme(plot.background = element_rect(fill="white"))


#save
ggsave(filename = "figures/thiamine_by_survival_wdata_class_w_curve.png", width = 10, height = 8, dpi=300)



# TRUNCATE CONCENTRATION THRESHOLD ----------------------------------------

# Plot By Thiamine Value WITH DATA --------------------------------------------------

# by thiamine value (with classroom ID)
set.seed(111)
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
  # now add exp data underneath!
  stat_smooth(data=thidat, aes(y=survival_proportion, x=total_thiamine_nmol_g), method="glm", se=FALSE, method.args = list(family="binomial"), color="gray40", lwd=0.5) +
  geom_point(data=thidat, aes(y=survival_proportion, x=total_thiamine_nmol_g), pch=21, size=2.5, fill="gray10", alpha=0.7) +

  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
              xmin = 10, xmax = 13, ymin = 0.55, ymax = 0.75,
              fill = "gray40",
              alpha = 0.8) +
  ggrepel::geom_text_repel(data=tot_surv, aes(y=survival, x=avg_th, label=glue("{class_label}-{tank_number}")), color="gray10",
                           segment.color="gray50", segment.alpha=0.5,
                           point.padding = 0.2, seed=111,
                           min.segment.length = .1,force = 1.3,
                           nudge_y = -0.1, nudge_x = -0.1
  ) +
  geom_jitter(data=tot_surv, aes(y=survival, x=avg_th, fill=status), pch=21, size=4.5, show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0,60,3))) +
  labs(subtitle = "Survival vs. Thiamine Concentration",
       y="Proportion Survived", x="Thiamine (nmol/g)") +
  scale_fill_manual("", values = c("Low"="red2", "Intermediate"="darkorange", "High"="seagreen")) +
  xlim(c(0,18)) +
  # themes
  theme_cowplot(font_family = "Roboto Condensed") +
  cowplot::background_grid("y") +
  theme(plot.background = element_rect(fill="white"))


#save
ggsave(filename = "figures/thiamine_by_survival_wdata_class_w_curve_trunc.png", width = 10, height = 8, dpi=300)

