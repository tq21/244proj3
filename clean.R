library(dplyr)
library(tidyverse)
library(ggplot2)
library(dygraphs)
library(xts)
library(lubridate)

# US daily cases
df_us_case <- read.csv("RDSC-07-30-Update/New_York_Times/covid-19-state-level-data.csv")
df_us_case <- df_us_case %>% 
  group_by(date) %>% 
  summarize(total = sum(cases))
df_us_case$date <- as.Date(df_us_case$date)
df_us_case <- df_us_case %>% 
  filter(date >= as.Date("2020-03-01"),
         date <= as.Date("2020-06-30"))
tmp <- c(df_us_case$total[1], diff(df_us_case$total))
df_us_case <- df_us_case %>% 
  mutate(total = tmp)

# UK daily cases
df_uk_case <- read.csv("RDSC-07-30-Update/coronadatascraper/coronadatascraper-timeseries.csv")
df_uk_case <- df_uk_case %>% 
  filter(country == "United Kingdom") %>% 
  group_by(date) %>% 
  summarize(total = sum(cases)) 
df_uk_case$date <- as.Date(df_uk_case$date)
df_uk_case <- df_uk_case %>% 
  filter(date >= as.Date("2020-03-01"),
         date <= as.Date("2020-06-30"))
tmp <- c(df_uk_case$total[1], diff(df_uk_case$total))
df_uk_case <- df_uk_case %>% 
  mutate(total = abs(tmp))

# US mobility
df_mob <- read.csv("RDSC-07-30-Update/apple_mobility_trends/mobility-trends.csv")
df_mob$date <- as.Date(df_mob$date)
df_us_mob <- df_mob %>% 
  drop_na() %>% 
  filter(region == "United States") %>% 
  group_by(date) %>% 
  summarize(mob = mean(value)) %>% 
  filter(date >= as.Date("2020-03-01"),
         date <= as.Date("2020-06-30"))

# UK mobility
df_uk_mob <- df_mob %>% 
  drop_na() %>% 
  filter(region == "United Kingdom") %>% 
  group_by(date) %>% 
  summarize(mob = mean(value)) %>% 
  filter(date >= as.Date("2020-03-01"),
         date <= as.Date("2020-06-30"))

# US all
df_us <- df_us_mob %>% 
  left_join(df_us_case, by = "date") %>% 
  mutate(mob = (mob - mean(mob)) / sd(mob),
         total = (total - mean(total)) / sd(total))

# UK all
df_uk <- df_uk_mob %>% 
  left_join(df_uk_case, by = "date") %>% 
  mutate(mob = (mob - mean(mob)) / sd(mob),
         total = (total - mean(total)) / sd(total))

us_xts <- xts(x = df_us[,-1], order.by = df_us$date)
uk_xts <- xts(x = df_uk[,-1], order.by = df_uk$date)
dygraph(us_xts)
dygraph(uk_xts)







res <- matrix(NA, nrow = 2, ncol = 6)
for (d in 1:6) {
  df_us_tmp <- filter(df_us, days == d)
  df_uk_tmp <- filter(df_uk, days == d)
  res[1,d] <- cor(df_us_tmp$mob, df_us_tmp$total)
  res[2,d] <- cor(df_uk_tmp$mob, df_uk_tmp$total)
}






us_xts <- xts(x = df_us_mob[,-1], order.by = df_us_mob$date)
dygraph(us_xts)



df_uk_mob <- df_mob %>% 
  filter(region == "United Kingdom") %>% 
  drop_na() %>% 
  select(transportation_type, date, value)

df_jp_mob <- df_mob %>% 
  filter(region == "Japan") %>% 
  drop_na() %>% 
  select(transportation_type, date, value)

df_us_mob_drive <- df_us_mob %>% 
  filter(transportation_type == "driving") %>% 
  select(-transportation_type)

df_us_mob_transit <- df_us_mob %>% 
  filter(transportation_type == "transit") %>% 
  select(-transportation_type)

df_us_mob_walk <- df_us_mob %>% 
  filter(transportation_type == "walking") %>% 
  select(-transportation_type)

df_us_mob <- df_us_mob_drive %>% 
  rename(drive = value) %>% 
  left_join(df_us_mob_transit, by = "date") %>% 
  rename(transit = value) %>% 
  left_join(df_us_mob_walk, by = "date") %>% 
  rename(walk = value)

df_us_mob$date <- as.Date(df_us_mob$date)



