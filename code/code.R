## Title: Seafood Sustainability and GDP Per Capita
## Authors: Clarissa Gallo, Nicole Perez, Amber Waltz
## Research question: Does GDP per capita impact a country's level of aquaculture over time?

## read in data

farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')
worldbank <- readr::read_csv('https://raw.githubusercontent.com/awaltz001/winter-inst-2022-seafood/main/data/World%20Bank%20Indicators_GDP%20Per%20Capita.csv')

## load packages

library(tidyverse)

## join the data

wb <- worldbank %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "Year",
    values_to = "gdp_per_capita",
    values_drop_na = TRUE
  )

wb$year_only <- as.numeric(substr(wb$Year, 1,4))

df <- merge(x = wb,
            y = captured_vs_farmed,
            by.x = c("Country Code","year_only"),
            by.y = c("Code","Year"),
            all.x = TRUE)

## explore data

df$aquaprod <- df$`Aquaculture production (metric tons)` 

df$shareaquaprod <- df$aquaprod /
  (df$`Capture fisheries production (metric tons)` + df$aquaprod)

ggplot(data=df, aes(x=year_only, y=aquaprod, group=1)) +
  geom_point()

share_of_aqua_line <- ggplot(data=df, aes(x=year_only, y=shareaquaprod, group=1)) +
  geom_smooth()

df$gdp_nulls_removed <- as.numeric(na_if(df$gdp_per_capita, ".."))
