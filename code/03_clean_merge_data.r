
#load Packages
library(tidyverse)

#load data
load("data_raw/olympics_raw.RData")
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


#--------------------------
#Data log Transformation
#--------------------------
#Normalizing to improve is right skewed - Log transformation
olympics_viz <- olympics_final %>%
  mutate(
    medals_per_million = total / (population_total / 1e6),
    log_gdp = log(gdp_per_capita_ppp),
    log_population = log(population_total)
  )

#removing NAs since log can't have zeros
olympics_viz_clean <- olympics_viz %>%
  filter(
    !is.na(log_population),
    !is.na(medals_per_million),
    is.finite(log_population),
    is.finite(medals_per_million)
  )

#save clean data
save(olympics_viz_clean, file = "data_clean/olympics_viz_clean.RData")
