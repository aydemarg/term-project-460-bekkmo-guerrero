# term-project-406-bekkmo-guerrero
How well can GDP per capita, population size, health expenditure, and other variables predict Olympic medal counts across countries in the Summer and Winter Olympics?
  
Improved Question: How well can country-level characteristics predict Olympic medal performance across nations?

# Tentative Analysis Plan

We will conduct a descriptive and predictive analysis of Olympic performance across countries.

First, we will collect Olympic medal data by country and year. Then we will merge this dataset with economic and demographic indicators such as GDP per capita, population, and health expenditure. We also plan to add more variables based on available data.

The analysis will include:

Exploratory data analysis and visualizations

Scatterplots showing relationships between GDP per capita and medal counts

Comparisons of medal counts across countries with different population sizes

A regression model predicting medal counts using GDP per capita, population, health expenditure, and more variables

The goal is to understand how well economic and demographic factors can predict Olympic success.

# Challenge

Building on the web scraping techniques learned in class, we will collect data using multiple approaches. We will extract data from HTML tables using rvest, retrieve API data using httr, and process JSON responses with jsonlite. These methods will allow us to combine structured data from APIs with information gathered directly from web pages for our analysis.

While variables such as GDP per capita, population size, and health expenditure are  indicators of Olympic success, their predictive power may be limited. Our challenge for this project is to learn some additional machine learning algorithms that we could use, run different machine learning models, and compare them with basic ones or the ones that we learned in class in order to improve predictive accuracy.

# Datasets
**Dataset 1: Olympic Medal Tables**

Source:
https://en.wikipedia.org/wiki/Category:Summer_Olympics_medal_tables and https://en.wikipedia.org/wiki/Category:Winter_Olympics_medal_tables

Description:
This dataset contains medal tables for each Summer and Winter Olympic Games. The webpage lists a separate page for every Olympic year (for example: 2016, 2018, 2020, 2022, 2024, 2026), and each page includes a table showing the number of gold, silver, and bronze medals won by each country.

To build our dataset, we will collect the medal table from each Olympic year by visiting each page and extracting the medal counts for every country. Each table includes information such as the country name and the number of gold, silver, bronze, and total medals.

Main variables include:

country

gold medals

silver medals

bronze medals

total medals

Timespan:
Last 10 Summer Olympic Games and Last 10 Winter Olympic Games (1988–2026).

Spatial coverage:
All countries participating in the Summer and Winter Olympic Games.

**Dataset 2: World Bank Economic Indicators**

Source:
https://data.worldbank.org

Description:
This dataset contains economic and demographic indicators for countries.

Main variables include:

GDP per capita

total population

health expenditure per capita

*additional indicators*

Timespan:
1992–2026

Spatial coverage:
Countries worldwide that participated in the Summer and Winter Olympic Games.

# Relationship Between Datasets

The datasets will be merged by country name and year.

The Olympic dataset provides the outcome variable (medal counts), while the World Bank dataset provides economic and demographic predictors, including, but not limited to, GDP per capita, population, and health expenditure.
