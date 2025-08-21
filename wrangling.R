library(ggplot2)
library(dplyr)
library(countrycode)
library(stringr)

# Criteria 1: Stop follow up after 2020
life_expectancy <- life_expectancy |>
  filter(Year < 2020)
life_expectancy_different_ages <- life_expectancy_different_ages |>
  filter(Year < 2020)
life_expectancy_female_male <- life_expectancy_female_male |>
  filter(Year < 2020)

# Stop follow-up after 2020
# Create Decade column
# Remove NAs
life_expectancy <- life_expectancy |>
  mutate(Decade = case_when(Year >= 1950 & Year < 1960 ~ 1950,
                            Year >= 1960 & Year < 1970 ~ 1960,
                            Year >= 1970 & Year < 1980 ~ 1970,
                            Year >= 1980 & Year < 1990 ~ 1980,
                            Year >= 1990 & Year < 2000 ~ 1990,
                            Year >= 2000 & Year < 2010 ~ 2000,
                            Year >= 2010 & Year < 2020 ~ 2010)) |>
  filter(!is.na(Decade))

# Add region and continent, remove countries with no assigned region
life_expectancy_region <- life_expectancy |>
  filter(!Entity %in% c("Africa", "Asia", "Europe", "Northern America", "Americas", "Oceania")) |>
  mutate(
    Continent = countrycode(sourcevar = Entity,
                            origin = "country.name",
                            destination = "continent"),
    Region = countrycode(sourcevar = Entity,
                         origin = "country.name",
                         destination = "region")
  ) |>
  filter(!is.na(Region))


 # income groups data
income_groups <- life_expectancy |>
  filter(str_detect(Entity, regex("income", ignore_case = TRUE))) |>
  select(-Code)

# Adding continent, region and decade to life_expectancy_female_male

life_expectancy_female_male_regions <- life_expectancy_female_male |>
  mutate(Decade = case_when(Year >= 1950 & Year < 1960 ~ 1950,
                            Year >= 1960 & Year < 1970 ~ 1960,
                            Year >= 1970 & Year < 1980 ~ 1970,
                            Year >= 1980 & Year < 1990 ~ 1980,
                            Year >= 1990 & Year < 2000 ~ 1990,
                            Year >= 2000 & Year < 2010 ~ 2000,
                            Year >= 2010 & Year < 2020 ~ 2010)) |>
  filter(!is.na(Decade)) |>
  filter(!Entity %in% c("Africa", "Asia", "Europe", "Northern America", "Americas", "Oceania")) |>
  mutate(
    Continent = countrycode(sourcevar = Entity,
                            origin = "country.name",
                            destination = "continent"),
    Region = countrycode(sourcevar = Entity,
                         origin = "country.name",
                         destination = "region")
  ) |>
  filter(!is.na(Region))




