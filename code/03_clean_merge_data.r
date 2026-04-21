#Install packages if needed
#install.packages("tidyverse")

#load Packages
library(tidyverse)

#load data
load("data_raw/olympics_all.RData")
load("data_raw/world_bank_raw.RData")

#----------------------------
# Cleaning Data Set
#---------------------------

#Selecting data range from most complete years

# Count non-missing indicators per year
year_completeness <- olympics_indicators %>%
  group_by(year) %>%
  summarise(
    non_missing = sum(!is.na(across(where(is.numeric)))),
    total_values = n() * ncol(select(., where(is.numeric))),
    completeness_ratio = non_missing / total_values
  ) %>%
  arrange(year)

#Find where completeness increases a lot
year_completeness <- year_completeness %>%
  mutate(change = completeness_ratio - lag(completeness_ratio))

start_year <- year_completeness %>%
  filter(change == max(change, na.rm = TRUE)) %>%
  pull(year)

#filter data set to most complete years
olympics_final <- olympics_indicators %>%
  filter(year >= start_year)

#Check how many years were kept
summary(olympics_final$year)

#save clean data
save(olympics_final, file = "data_clean/olympics_final.RData")
