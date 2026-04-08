#Packages


#Load Data
load("data_clean/olympics_viz_clean.RData")

#Check raw data plots

#Medals Distribution
p1 = ggplot(olympics_final, aes(x = total)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Raw Distribution of Olympic Medals",
    x = "Total Medals",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()
ggsave("output/figures/fig1_raw_medal_distribution.png", plot = p1, width = 8, height = 5, dpi = 300)

#GDP per capital
p2 = ggplot(olympics_final, aes(x = gdp_per_capita_ppp)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Raw Distribution of GDP per Capita",
    x = "GDP per Capita (PPP)",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()

ggsave("output/figures/fig2_raw_dist_GDPpercap.png", plot = p2, width = 8, height = 5, dpi = 300)

#population
p3 = ggplot(olympics_final, aes(x = population_total)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Raw Distribution of Population",
    x = "Population",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()

ggsave("output/figures/fig3_raw_dist_GDPpercap.png", plot = p3, width = 8, height = 5, dpi = 300)


#--------------------------
#Data log Transformation
#--------------------------

#Plots after transformation
#Medals Distribution 
p4 = ggplot(olympics_viz_clean, aes(x = medals_per_million)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 10)) +
  labs(
    title = "Distribution of Medals per Million People (Zoomed In)",
    x = "Medals per Million",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()

ggsave("output/figures/fig4_log_dist.png", plot = p4, width = 8, height = 5, dpi = 300)

#GDP per capital
p5 = ggplot(olympics_viz_clean, aes(x = log_gdp)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Log-Transformed GDP per Capita Distribution",
    x = "Log GDP per Capita",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()

ggsave("output/figures/fig5_log_dist_GDPpercap.png", plot = p5, width = 8, height = 5, dpi = 300)

#Population
p6 = ggplot(olympics_viz_clean, aes(x = log_population)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Log-Transformed Population Distribution",
    x = "Log Population",
    y = "Number of Country-Year Observations"
  ) +
  theme_minimal()

ggsave("output/figures/fig6_log_dist_pop.png", plot = p6, width = 8, height = 5, dpi = 300)

#Understand relationships between variables

#relationship between GDP and Medals
p7 = ggplot(olympics_viz_clean, aes(x = log_gdp, y = medals_per_million)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between GDP per Capita and Olympic Success",
    x = "Log GDP per Capita",
    y = "Medals per Million"
  ) +
  theme_minimal()

ggsave("output/figures/fig7_GDP_medals.png", plot = p7, width = 8, height = 5, dpi = 300)

#relationship 
p8 = ggplot(olympics_viz_clean, aes(x = log_population, y = medals_per_million)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Population and Olympic Success",
    x = "Log Population",
    y = "Medals per Million"
  ) +
  theme_minimal()

ggsave("output/figures/fig8_pop_oly_success.png", plot = p8, width = 8, height = 5, dpi = 300)

# GDP vs medals per million (binned median trend)
p9 = ggplot(olympics_viz_clean, aes(x = log_gdp, y = medals_per_million)) +
  geom_point(alpha = 0.12) +
  geom_smooth(se = FALSE, linewidth = 1.2, color = "steelblue")+
  coord_cartesian(ylim = c(0, 5)) +
  labs(
    title = "Relationship Between GDP per Capita and Medals per Million",
    x = "Log GDP per Capita",
    y = "Medals per Million"
  ) +
  theme_minimal()

ggsave("output/figures/fig9_GPD_medals_per_millp.png", plot = p9, width = 8, height = 5, dpi = 300)

#Total medals over time for a topmajor countries
 top_countries_total <- olympics_final %>%
  group_by(country) %>%
  summarise(total_medals_all_years = sum(total, na.rm = TRUE)) %>%
  slice_max(total_medals_all_years, n = 10)

 p10 = ggplot(top_countries_total,
       aes(x = reorder(country, total_medals_all_years),
           y = total_medals_all_years)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Total Olympic Medals",
    x = NULL,
    y = "Total Medals"
  ) +
  theme_minimal()

ggsave("output/figures/fig10_top10_total_medals.png", plot = p10, width = 8, height = 5, dpi = 300)


#Choropleth world map with a sequential color scale

# -----------------------------------
# Get world map data
# -----------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# -----------------------------------
# Aggregate Olympic medals by country and Fix country names to improve matching
# -----------------------------------

medals_map_clean <- olympics_final %>%
  group_by(country, season) %>%
  summarise(total_medals = sum(total, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country = case_when(
      country == "United States" ~ "United States of America",
      country == "Russia" ~ "Russian Federation",
      country == "South Korea" ~ "Republic of Korea",
      country == "Republic of Korea" ~ "South Korea",
      country == "North Korea" ~ "Dem. Rep. Korea",
      country == "Dem. Rep. Korea" ~ "North Korea",
      country == "Iran" ~ "Iran",
      country == "Venezuela" ~ "Venezuela",
      country == "Czech Republic" ~ "Czechia",
      country == "Slovakia" ~ "Slovakia",
      country == "Moldova" ~ "Moldova",
      country == "Syria" ~ "Syria",
      country == "Bolivia" ~ "Bolivia",
      country == "Tanzania" ~ "Tanzania",
      country == "Laos" ~ "Laos",
      country == "Vietnam" ~ "Vietnam",
      country == "Brunei" ~ "Brunei",
      country == "Cape Verde" ~ "Cabo Verde",
      country == "Ivory Coast" ~ "Côte d'Ivoire",
      country == "Macedonia" ~ "North Macedonia",
      country == "Chinese Taipei" ~ "Taiwan",
      #Adding medals to Russia when competed with different team names
      country %in% c("Russian Federation", "ROC", "Olympic Athletes from Russia") ~ "Russia",
      TRUE ~ country
    )
  ) %>%
  filter(!country %in% c(
    "Czechoslovakia",
    "FR Yugoslavia",
    "Independent Olympic Athletes",
    "Independent Olympic Participants",
    "Individual Neutral Athletes",
    "Refugee Olympic Team",
    "Serbia and Montenegro",
    "Unified Team"
  ))

# -----------------------------------
# Ensure all countries exist for both seasons
# -----------------------------------

medals_map_complete <- medals_map_clean %>%
  tidyr::complete(
    country = unique(world$name),
    season = c("Summer", "Winter")
  )

# -----------------------------------
# Join map with medal data
# -----------------------------------

world_medals <- world %>%
  left_join(medals_map_complete, by = c("name" = "country"))

# -----------------------------------
# Plot
# -----------------------------------

p11 = ggplot(world_medals) +
  geom_sf(aes(fill = total_medals), color = "white", linewidth = 0.2) +
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    trans = "sqrt",
    na.value = "gray90",
    name = "Total medals",
    breaks = c(10, 50, 100, 200, 400, 600, 800)
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "left",
      title.hjust = 0.5,
      barwidth = unit(12, "cm"),
      barheight = unit(0.7, "cm")
    )
  ) +
  labs(
    title = "Olympic Medal Totals Across Participating Countries",
    subtitle = "Data covers Olympic Games from 1996 to 2024 \nCountries in gray do not appear in the dataset",
    caption = "Lighter colors indicate lower total medal counts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  ) +
  facet_wrap(~season)

ggsave("output/figures/fig11_worldmap_participation.png", plot = p11, width = 8, height = 5, dpi = 300)


#Countries That Overperform in Olympic Success
model <- lm(medals_per_million ~ log_gdp, data = olympics_viz_clean)

olympics_model <- olympics_viz_clean %>%
  mutate(
    expected = predict(model, newdata = olympics_viz_clean),
    residual = medals_per_million - expected
  )

top_overperformers <- olympics_model %>%
  group_by(country) %>%
  summarise(avg_residual = mean(residual, na.rm = TRUE)) %>%
  slice_max(avg_residual, n = 10)

p12 = ggplot(top_overperformers,
       aes(x = reorder(country, avg_residual), y = avg_residual)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Countries That Overperform in Olympic Success",
    subtitle = "Relative to GDP per capita expectations",
    x = NULL,
    y = "Average Overperformance (Residual)"
  ) +
  theme_minimal()

ggsave("output/figures/fig12_overperform_countries.png", plot = p12, width = 8, height = 5, dpi = 300)


#Distribution of Log GDP per Capita by Olympic Success Level

p13 = ggplot(olympics_viz_clean ,aes(x = total, y = log_gdp)) +
  geom_rug(color = "steelblue") +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(
    title = "Distribution of Log GDP per Capita by Olympic Success Level",
    x = "Total Medals",
    y = "Log GDP per Capita"
  ) +
  theme_minimal() +
  facet_wrap(~season)

ggsave("output/figures/fig13_dist_GDP_sucesslevel.png", plot = p13, width = 8, height = 5, dpi = 300)


#GDP per Capita and Olympic Medal Types
medals_long <- olympics_viz_clean %>%
  select(country, year, log_gdp, gold, silver, bronze) %>%
  pivot_longer(
    cols = c(gold, silver, bronze),
    names_to = "medal_type",
    values_to = "medals"
  )

p14 = ggplot(medals_long, aes(x = log_gdp, y = medals, color = medal_type)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  facet_wrap(~ medal_type) +
  scale_color_manual(
    values = c(
      gold = "#D4AF37",
      silver = "#C0C0C0",
      bronze = "#CD7F32"
    )
  ) +
  guides(color = "none") +
  labs(
    title = "GDP per Capita and Olympic Medal Types",
    x = "Log GDP per Capita",
    y = "Medals",
    color = ""
  ) +
  theme_minimal()

ggsave("output/figures/fig14_medals_type.png", plot = p14, width = 8, height = 5, dpi = 300)

#Top 10 Olympic Medal Composition by Country

country_medals <- olympics_final %>%
  group_by(country) %>%
  summarise(
    gold = sum(gold, na.rm = TRUE),
    silver = sum(silver, na.rm = TRUE),
    bronze = sum(bronze, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  )

norway <- country_medals %>%
  filter(country == "Norway") %>%
  select(gold, silver, bronze)

waffle(
  c(
    Gold = norway$gold,
    Silver = norway$silver,
    Bronze = norway$bronze
  ),
  rows = 10,
  colors = c("#D4AF37", "#C0C0C0", "#CD7F32"),
  title = "Norway Olympic Medals",
  xlab = "1 square = 1 medal"
)

top10 <- country_medals %>%
  slice_max(total, n = 10)

medals_long_counts <- top10 %>%
  pivot_longer(
    cols = c(gold, silver, bronze),
    names_to = "medal",
    values_to = "count"
  )

p15 = ggplot(medals_long_counts, aes(fill = medal, values = count)) +
  geom_waffle(
    n_rows = 10,
    color = "white",
    size = 0.25
  ) +
  facet_wrap(~ country, ncol = 2) +
  scale_fill_manual(
    values = c(
      gold = "#D4AF37",
      silver = "#C0C0C0",
      bronze = "#CD7F32"
    )
  ) +
  theme_minimal() +
  labs(title = "Olympic Medal Composition by Country",
       subtitle = "1 square = 1 medal",
       caption = "For Top 10 Countries in Total Medal Count") +
  theme(plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5) ,
        plot.subtitle = element_text(hjust = 0.5) )

ggsave("output/figures/fig15_top10_medal_comp.png", plot = p15, width = 8, height = 5, dpi = 300)







