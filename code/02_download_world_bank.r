#Install packages if needed
#install.packages("tidyverse")
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("furrr")

#Libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(furrr)
plan(multisession)


#-----------------------------------
#World Bank Data
#------------------------------------

# Define API request
get_wb_indicator <- function(indicator, var_name) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator
  )
  params <- list(
    format = "json",
    per_page = 20000
  )
  
  # Make API request
  response <- GET(url = url, query = params)
  
  # Parse JSON
  json <- response |>
    httr::content("text") |>
    fromJSON(flatten = TRUE) # convert from JSON into an R list
  
  # Convert JSON data into a tidy tibble
  df <- as_tibble(json[[2]]) |>
    transmute(
      country = country.value,
      year    = as.numeric(date),
      value   = value
    ) |>
    rename(!!var_name := value) # rename 'value' column to var_name
  
  # Return the cleaned data frame
  return(df)
}

# Create a list of World Bank indicators to download
indicators <- c(
  # GDP/macro
  gross_capital_formation = "NE.GDI.TOTL.ZS",
  gross_savings = "NY.GNS.ICTR.ZS",
  final_consumption = "NE.CON.TOTL.ZS",
  household_consumption = "NE.CON.PRVT.ZS",
  government_consumption = "NE.CON.GOVT.ZS",
  gdp_constant_usd = "NY.GDP.MKTP.KD",
  gdp_ppp = "NY.GDP.MKTP.PP.CD",
  gdp_per_capita_ppp = "NY.GDP.PCAP.PP.CD",
  
  # inflation/prices
  gdp_deflator = "NY.GDP.DEFL.KD.ZG",
  cpi_all_items = "FP.CPI.TOTL",
  
  # labor market
  unemployment_total = "SL.UEM.TOTL.ZS",
  unemployment_female = "SL.UEM.TOTL.FE.ZS",
  unemployment_male = "SL.UEM.TOTL.MA.ZS",
  youth_unemployment = "SL.UEM.1524.ZS",
  labor_force_total = "SL.TLF.CACT.ZS",
  labor_force_female = "SL.TLF.CACT.FE.ZS",
  employment_to_population = "SL.EMP.TOTL.SP.ZS",
  vulnerable_employment = "SL.EMP.VULN.ZS",
  
  # demographics
  population_total = "SP.POP.TOTL",
  population_growth = "SP.POP.GROW",
  urban_population = "SP.URB.TOTL.IN.ZS",
  rural_population = "SP.RUR.TOTL.ZS",
  life_expectancy = "SP.DYN.LE00.IN",
  life_expectancy_female = "SP.DYN.LE00.FE.IN",
  life_expectancy_male = "SP.DYN.LE00.MA.IN",
  fertility_rate = "SP.DYN.TFRT.IN",
  dependency_ratio = "SP.POP.DPND",
  age_dependency_young = "SP.POP.DPND.YG",
  age_dependency_old = "SP.POP.DPND.OL",
  
  # education
  primary_enrollment = "SE.PRM.ENRR",
  secondary_enrollment = "SE.SEC.ENRR",
  tertiary_enrollment = "SE.TER.ENRR",
  education_spending = "SE.XPD.TOTL.GD.ZS",
  government_education_spending = "SE.XPD.TOTL.GB.ZS",
  literacy_rate = "SE.ADT.LITR.ZS",
  expected_years_schooling = "SE.SCH.LIFE",
  mean_years_schooling = "SE.ADT.1524.LT.ZS",
  
  # trade
  trade_openness = "NE.TRD.GNFS.ZS",
  exports_goods_services = "NE.EXP.GNFS.ZS",
  imports_goods_services = "NE.IMP.GNFS.ZS",
  fdi_inflow = "BX.KLT.DINV.CD.WD",
  fdi_outflow = "BM.KLT.DINV.CD.WD",
  portfolio_inflow = "BX.PEF.TOTL.CD.WD",
  
  # finance
  private_credit = "FS.AST.PRVT.GD.ZS",
  broad_money = "FM.LBL.BMNY.GD.ZS",
  money_growth = "FM.LBL.BMNY.ZG",
  lending_rate = "FR.INR.LEND",
  deposit_rate = "FR.INR.DPST",
  real_interest_rate = "FR.INR.RINR",
  
  # government
  tax_revenue = "GC.TAX.TOTL.GD.ZS",
  government_expenditure = "GC.XPN.TOTL.GD.ZS",
  government_revenue = "GC.REV.XGRT.GD.ZS",
  public_debt = "GC.DOD.TOTL.GD.ZS",
  
  # inequality/poverty
  gini = "SI.POV.GINI",
  poverty_2usd = "SI.POV.DDAY",
  poverty_3_2usd = "SI.POV.UMIC",
  income_share_top10 = "SI.DST.10TH.10",
  poverty_national = "SI.POV.NAHC",
  
  # governance
  rule_of_law = "RL.EST",
  control_corruption = "CC.EST",
  gov_effectiveness = "GE.EST",
  regulatory_quality = "RQ.EST",
  political_stability = "PV.EST",
  voice_accountability = "VA.EST",
  
  # environment
  renewable_energy = "EG.FEC.RNEW.ZS",
  energy_use = "EG.USE.PCAP.KG.OE",
  electricity_access = "EG.ELC.ACCS.ZS",
  forest_area = "AG.LND.FRST.ZS",
  
  # health
  life_expectancy = "SP.DYN.LE00.IN",
  infant_mortality = "SP.DYN.IMRT.IN",
  child_mortality = "SH.DYN.MORT",
  health_expenditure = "SH.XPD.CHEX.GD.ZS",
  physicians = "SH.MED.PHYS.ZS",
  hospital_beds = "SH.MED.BEDS.ZS",
  immunization_dpt = "SH.IMM.IDPT",
  immunization_measles = "SH.IMM.MEAS"
)

# Download indicators into a list of data frames
# help from AI with the prompt: how can I use this list of indicators to grab data from the world bank API and combine into one data frame?
safe_get <- purrr::possibly(get_wb_indicator, otherwise = NULL)

# use future_imap() to cut run time
indicators_list <- future_imap(indicators, safe_get)
indicators_list <- indicators_list[!sapply(indicators_list, is.null)]

# Combine indicators data sets into one wide dataset
# reduce() combined two elements of a list at a time until one object is left
indicators_final <- reduce(indicators_list, left_join, by = c("country", "year"))

# load olympics raw data
load("data_raw/olympics_raw.RData")
olympics_indicators <- left_join(olympics_all, indicators_final, by = c("country", "year"))

#save raw data
save(olympics_indicators, file = "data_raw/world_bank_raw.RData")

message("Script 2 complete.")