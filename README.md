# term-project-406-bekkmo-guerrero
How well can GDP per capita, population size, and health expenditure predict Olympic medal counts across countries in the Winter Olympics?

# Tentative Analysis Plan

We will conduct a descriptive and predictive analysis of Olympic performance across countries.

First, we will collect Olympic medal data by country and year. Then we will merge this dataset with economic and demographic indicators such as GDP per capita, population, and health expenditure.

The analysis will include:

Exploratory data analysis and visualizations

Scatterplots showing relationships between GDP per capita and medal counts

Comparisons of medal counts across countries with different population sizes

A regression model predicting medal counts using GDP per capita, population, and health expenditure

The goal is to understand how well economic and demographic factors can predict Olympic success.

# Challenge

Although we previously worked with basic HTML scraping in rvest and simple API requests using httr, our challenge for this project is to use a more advanced data collection workflow. We plan to retrieve country-level indicators through the World Bank API using httr2, including multiple variables and years, and combine those data with Olympic medal tables collected from the web.

# Datasets
Dataset 1: Olympic Medal Tables

Source:
[https://en.wikipedia.org/wiki/2020_Summer_Olympics_medal_table](https://en.wikipedia.org/wiki/Category:Winter_Olympics_medal_tables)

Description:
This dataset contains medal tables for each Winter Olympic Games. The webpage lists a separate page for every Olympic year (for example 2018, 2022, 2026), and each page includes a table showing the number of gold, silver, and bronze medals won by each country.

To build our dataset, we will collect the medal table from each Olympic year by visiting each page and extracting the medal counts for every country. Each table includes information such as the country name and the number of gold, silver, bronze, and total medals.

Main variables include:

country

gold medals

silver medals

bronze medals

total medals

Timespan:
Last 10 Winter Olympic Games (1992–2026).

Spatial coverage:
All countries participating in the Winter Olympic Games.

Dataset 2: World Bank Economic Indicators

Source:
https://data.worldbank.org

Description:
This dataset contains economic and demographic indicators for countries.

Main variables include:

GDP per capita

total population

health expenditure per capita

Timespan:
1992–2026

Spatial coverage:
Countries worldwide that participated in the Winter Olympics Games.

# Relationship Between Datasets

The datasets will be merged by country name and year.

The Olympic dataset provides the outcome variable (medal counts), while the World Bank dataset provides economic and demographic predictors such as GDP per capita, population, and health expenditure.

Combining these datasets allows us to analyze how country-level economic factors relate to Olympic performance.
