# plots

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

# Load Data ---------------------------------------------------------------

source("code/functions/f_import_data.R")
df <- f_import_data()

# Check for Test Data -----------------------------------------------------

# filter out anything with Test/Testing
df_orig <- nrow(df)

df <- df %>%
  filter(!grepl("Test|Testing|test", comments))

glue("Full data had {df_orig} rows, {df_orig - nrow(df)} dropped")

# if dead in previous time they need to stay dead
# need to fix dead that increases...
df <- df %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(dead_qa = case_when(
    lag(dead) - dead > 0 ~ "CHECK NUMBERS",
    TRUE ~ "OK"
  ), .after=dead)

# any issues?
table(df$dead_qa)


# Make Egg Status Summary By Class -----------------------------------

df_status <- df %>%
  select(site, date, eggs_hatched, dead, tank_number) %>%
  group_by(site, date, tank_number) %>%
  tidyr::pivot_longer(c(eggs_hatched, dead), names_to = "egg_status", values_to = "count") %>%
  group_by(site, date) %>%
  mutate(unhatched = 35 - sum(count[egg_status=="eggs_hatched" | egg_status=="dead"])) %>%
  group_by(site, date) %>%
  tidyr::pivot_wider(id_cols = c(site, date, egg_status),names_from =c( egg_status), values_from = c(count, unhatched)) %>%
select(-unhatched_dead) %>%
  tidyr::pivot_longer(cols = c(count_eggs_hatched, count_dead, unhatched_eggs_hatched), names_to = "status", values_to = "values") %>%
  ungroup() %>%
  mutate(status2 = case_when(
    status == "count_eggs_hatched" ~ "Hatched",
    status == "count_dead" ~ "Dead",
    status == "unhatched_eggs_hatched" ~ "Unhatched",
    TRUE ~ status))

df_status_detail <- df %>%
  # select cols of interest
  select(site, date, eggs_hatched:dead) %>%
  # group by date and site
  group_by(site, date) %>%
  # now make the data longer for plotting purposes
  tidyr::pivot_longer(c(eggs_hatched:dead), names_to = "egg_status", values_to = "count") %>%
  group_by(site, date) %>%
  # this only sums egg_hatched and dead to calc unhatched
  mutate(unhatched = 35 - sum(count[egg_status=="eggs_hatched" | egg_status=="dead"])) %>%
  group_by(site, date) %>%
  # now pivot wider again to merge the unhatched column back in
  tidyr::pivot_wider(id_cols = c(site, date, egg_status), names_from =c( egg_status), values_from = c(count, unhatched)) %>%
  select(-c(unhatched_eggs_hatched:unhatched_spinning)) %>%
  rename(unhatched = unhatched_dead) %>%
  rename_with(~str_remove(., "count_")) %>%
  # now pivot long again and reclassify
  tidyr::pivot_longer(cols = c(eggs_hatched:unhatched), names_to = "status", values_to = "values") %>%
  ungroup() %>%
  mutate(status2 = case_when(
    status == "eggs_hatched" ~ "Hatched",
    status == "dead" ~ "Dead",
    status == "unhatched" ~ "Unhatched",
    status == "laying_on_side" ~ "Laying on side",
    status == "spinning" ~ "Spinning",
    status == "swimming_up" ~ "Swimming up",
    TRUE ~ status))

# view?
# View(df_status_detail)

# Barplot of Egg Status by Class ---------------------------

# eggs hatched by status type
g1 <- ggplot() +
  geom_hline(yintercept = 35, color="gray", lty=2) +
  geom_col(data=df_status, aes(x=date, y=values, fill=status2),
            show.legend=TRUE) +
  scale_y_continuous(limits=c(0,35), breaks = c(seq(0,35,5))) +
  facet_wrap(.~site) +
  theme_cowplot() +
  scale_x_date(date_labels = "%m-%d-%y") + #date_breaks = "1 week",
  scale_fill_colorblind("Status") +
  labs(subtitle = "Eggs status by Site",
       y="Eggs Hatched", x="Obs Date")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

g1

# now by only hatched eggs:
#status_details <- c("Laying on side","Spinning", "Swimming up", "Hatched")
# eggs hatched by status type
g2 <- df_status_detail %>%
  filter(!status2 == "Unhatched") %>%
  ggplot() +
  geom_hline(yintercept = 35, color="gray", lty=2) +
  geom_col(aes(x=date, y=values, fill=status2), show.legend=TRUE)+
  scale_y_continuous(limits=c(0,35), breaks = c(seq(0,35,5))) +
  facet_wrap(.~site) +
  theme_cowplot() +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_fill_colorblind("Status") +
  labs(subtitle = "Eggs status by site for all hatched eggs",
       y="Hatched Egg Status", x="",
       caption = "No effect observed if status is 'Hatched'")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

g2



# Eggs Hatched over time by HS ---------------------------

# eggs hatched
g3 <- ggplot() +
  geom_hline(yintercept = 35, color="gray", lty=2) +
  annotate("text",label="Total possible", x=as.Date("2021-12-01"), y=34, color="gray50") +
  geom_line(data=df, aes(x=date, y=eggs_hatched),
            show.legend=FALSE) +
  geom_point(data=df, aes(x=date, y=eggs_hatched),
             show.legend =FALSE) +
  geom_point(data=df %>% group_by(site) %>% filter(date==max(date)), aes(x=date, y=eggs_hatched), pch=21, size=4, fill="maroon") +
  geom_text(data=df %>% group_by(site) %>% filter(date==max(date)), aes(x=date, y=eggs_hatched), label="Current", nudge_y = -1, nudge_x = -1, color="maroon") +
  scale_y_continuous(limits=c(0,35), breaks = c(seq(0,35,5))) +
  facet_wrap(.~site) +
  theme_cowplot() +
  scale_x_date(date_labels = "%m-%d-%y") +
  #scale_x_date(date_breaks = "1 week", date_labels = "%m-%d-%y") +
  labs(subtitle = "Eggs hatched through time",
       y="Eggs Hatched", x="") +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))
g3


# Survival Curve ----------------------------------------------------------

# # survival
# g3 <- ggplot() +
#   geom_point(data=df, aes(x=thiamine, y=surv_prcnt, fill=site),
#             show.legend=TRUE, pch=21, size=3) +
#   geom_smooth(data=df, aes(x=thiamine, y=surv_prcnt),method = "gam", color="orange") +
#   #geom_smooth(data=df, aes(x=thiamine, y=surv_prcnt), method = "loess", span=1.5)+
#   theme_cowplot() +
#   scale_fill_colorblind("School") +
#   labs(title = "Simulated data: survival vs. thiamine",
#        y="% Survival (# alive / total)", x="Female egg thiamine concentration (\u03BCg/g)")
#
# g3


# Patchwork -----------------------------------------------------------

library(patchwork)
(g1 + g2) / g3

ggsave(filename = glue("figures/summary_plots_updated_{Sys.Date()}.png"), width = 11, height = 8, dpi=300)
