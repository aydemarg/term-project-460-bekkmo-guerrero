#Install packages if needed
#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("janitor")

# Packages
library(tidyverse)
library(rvest)
library(janitor)
#Web Scraping

#-----------------------------------
#Summer Olympics
#------------------------------------

#scrape the category page, get all links
url = "https://en.wikipedia.org/wiki/Category:Summer_Olympics_medal_tables"
page = read_html(url)

links = page |>
  html_nodes("#mw-pages a") |>
  html_attr("href") |>
  unique()

links = links[str_detect(links, "/wiki/\\d{4}_Summer_Olympics_medal_table$")]

links = paste0("https://en.wikipedia.org", links)

# links

#function to inspect where medal table is
get_medal_table = function(link) {
  page = read_html(link)
  year = str_extract(link, "\\d{4}")
  
  tables = page |>
    html_table(fill = TRUE)
  
  medal_table = NULL
  
  for (tbl in tables) {
    names(tbl) = names(tbl) |>
      str_replace_all("\\[.*?\\]", "") |>
      str_trim() |>
      make_clean_names()
    
    # check if first column is rank-like
    first_col = names(tbl)[1]
    
    has_rank = str_detect(first_col, "rank")
    has_gold = any(str_detect(names(tbl), "gold"))
    has_silver = any(str_detect(names(tbl), "silver"))
    has_bronze = any(str_detect(names(tbl), "bronze"))
    has_total = any(str_detect(names(tbl), "total"))
    
    if (has_rank & has_gold & has_silver & has_bronze & has_total) {
      medal_table = tbl
      break
    }
  }
  
  if (is.null(medal_table)) {
    message("No medal table found for: ", link)
    return(NULL)
  }
  
  nation_col = names(medal_table)[str_detect(names(medal_table), "nation|team|noc")][1]
  gold_col   = names(medal_table)[str_detect(names(medal_table), "gold")][1]
  silver_col = names(medal_table)[str_detect(names(medal_table), "silver")][1]
  bronze_col = names(medal_table)[str_detect(names(medal_table), "bronze")][1]
  total_col  = names(medal_table)[str_detect(names(medal_table), "total")][1]
  
  medal_table |>
    transmute(
      country = .data[[nation_col]] |>
        str_remove_all("\\[.*?\\]") |>
        str_remove_all("[^A-Za-z ]") |>
        str_squish(),
      gold = as.numeric(.data[[gold_col]]),
      silver = as.numeric(.data[[silver_col]]),
      bronze = as.numeric(.data[[bronze_col]]),
      total = as.numeric(.data[[total_col]]),
      year = as.numeric(year)
    ) |>
    filter(!is.na(country)) |>
    filter(country != "") |>
    filter(!str_detect(country, "Totals|Total"))
}

#Run funtion in all links
olympics_list = map(links, get_medal_table)

olympics_list = olympics_list[!sapply(olympics_list, is.null)]

olympics_raw = bind_rows(olympics_list)

#Clean data
olympics_clean = bind_rows(olympics_list) |>
  mutate(
    country = str_squish(country),
    country = case_when(
      country == "Great Britain" ~ "United Kingdom",
      country == "Mixed team" ~ NA_character_,
      TRUE ~ country
    )
  ) |>
  filter(!is.na(country), country != "")

#Make sure medal columns are numeric
glimpse(olympics_clean)

#check uniqueness for tidy
olympics_clean |>
  count(country, year) |>
  filter(n > 1)

#-----------------------------------
#Winter Olympics
#------------------------------------

# Winter Olympics links
url_winter = "https://en.wikipedia.org/wiki/Category:Winter_Olympics_medal_tables"
page_winter = read_html(url_winter)

links_winter = page_winter |>
  html_nodes("#mw-pages a") |>
  html_attr("href") |>
  unique()

links_winter = links_winter[str_detect(links_winter, "/wiki/\\d{4}_Winter_Olympics_medal_table$")]

links_winter = paste0("https://en.wikipedia.org", links_winter)

# Run existing function on winter links
winter_list = map(links_winter, get_medal_table)

winter_list = winter_list[!sapply(winter_list, is.null)]

winter_raw = bind_rows(winter_list)

# Clean winter data
winter_clean = bind_rows(winter_list) |>
  mutate(
    country = str_squish(country),
    country = case_when(
      country == "Great Britain" ~ "United Kingdom",
      country == "Mixed team" ~ NA_character_,
      TRUE ~ country
    )
  ) |>
  filter(!is.na(country), country != "")

# Check uniqueness
winter_clean |>
  count(country, year) |>
  filter(n > 1)

#join summer and winter
summer_clean = olympics_clean |>
  mutate(season = "Summer")

winter_clean = winter_clean |>
  mutate(season = "Winter")

olympics_all = bind_rows(summer_clean, winter_clean)

#make sure full data set is unique

olympics_all |>
  count(country, year, season) |>
  filter(n > 1)

#save raw data
save(olympics_all, file = "data_raw/olympics_all.RData")

message("Script 1 complete.")
