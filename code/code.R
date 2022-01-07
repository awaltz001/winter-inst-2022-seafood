## Title: Seafood Sustainability and GDP Per Capita
## Authors: Clarissa Gallo, Nicole Perez, Amber Waltz
## Research question: What impacts a country's proportion of aquaculture over time?
## 

## read in data

farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')
worldbank_gdp <- readr::read_csv('https://raw.githubusercontent.com/awaltz001/winter-inst-2022-seafood/main/data/World%20Bank%20Indicators_GDP%20Per%20Capita.csv')
worldbank_co2 <- readr::read_csv('https://raw.githubusercontent.com/awaltz001/winter-inst-2022-seafood/main/data/World%20Bank%20CO2%20Emissions%20Per%20Capita%20(mt).csv')

## load packages

install.packages("tidymodels")
library(tidyverse)

## join the data

wb_gdp <- worldbank_gdp %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "Year",
    values_to = "gdp_per_capita",
    values_drop_na = TRUE
  )

wb_co2 <- worldbank_co2 %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "Year",
    values_to = "co2_emissions_per_capita",
    values_drop_na = TRUE
  )

wb_gdp$year_only <- as.numeric(substr(wb_gdp$Year, 1,4))
wb_co2$year_only <- as.numeric(substr(wb_co2$Year, 1,4))

merge1 <- merge(x = wb_gdp,
            y = captured_vs_farmed,
            by.x = c("Country Code","year_only"),
            by.y = c("Code","Year"),
            all.x = TRUE)

df <- merge(x = wb_co2,
            y = merge1,
            by.x = c("Country Code","year_only"),
            by.y = c("Country Code","year_only"),
            all.x = TRUE)

df <- merge(x = df,
            y = consumption,
            by.x = c("Country Code","year_only"),
            by.y = c("Code","Year"),
            all.x = TRUE)

df <- merge(x = df,
            y = fishery,
            by.x = c("Country Code","year_only"),
            by.y = c("Code","Year"),
            all.x = TRUE)

machinedata <- df[ -c(1, 3:6, 8:11, 13, 16, 18:20, 22:23) ]

machinedata$co2_emissions_per_capita <- as.numeric(machinedata$co2_emissions_per_capita)

## clean data

machinedata$aquaprod <- df$`Aquaculture production (metric tons)` 
machinedata$captureprod <- machinedata$`Capture fisheries production (metric tons)`
machinedata$consumption <- machinedata$`Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`
machinedata$totalprod <- machinedata$aquaprod + machinedata$captureprod
machinedata$proportion_aquaprod <- machinedata$aquaprod / machinedata$totalprod
machinedata$largescale <- machinedata$`Industrial (large-scale commercial)`
machinedata$proportion_largescale <- machinedata$largescale / machinedata$totalprod
machinedata$country_code <- machinedata$`Country Code`
machinedata$country_name <- machinedata$`Country Name.x`
machinedata$gdp_nulls_removed <- as.numeric(na_if(df$gdp_per_capita, ".."))

machinedata <- machinedata[ -c(1,3:9,11,13) ]

## explore data - note to self: edit these graphs to filter!

ggplot(data=machinedata, aes(x=year_only, y=aquaprod, group=1)) +
  geom_point()

ggplot(data=machinedata, aes(x=year_only, y=shareaquaprod, group=1)) +
  geom_point()

ggplot(data=machinedata, aes(x=year_only, y=gdp_nulls_removed, group=1)) +
  geom_smooth()

## start machine learning

library(rsample)
library(tidymodels)

machinedata_split <- initial_split(machinedata)
machinedata_train <- training(machinedata_split)
machinedata_test <- testing(machinedata_split)

library(recipes)

machinedata_recip <- recipe(proportion_aquaprod ~ .,
                            data = machinedata_train)

machinedata_recip <- machinedata_recip %>%
  step_dummy(all_nominal(), -all_outcomes())

machinedata_recip <- machinedata_recip %>%
  step_zv(all_predictors())

machinedata_recip <- machinedata_recip %>%
  step_center(all_predictors(), -proportion_aquaprod)

machinedata_recip <- machinedata_recip %>%
  step_interact(terms = ~
                  proportion_largescale:all_predictors() +
                co2_emissions_per_capita:all_predictors() +
                gdp_nulls_removed:all_predictors())

machinedata_recip %>% 
  prep()

machinedata_train_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = NULL)

machinedata_test_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = machinedata_test)

machinedata_test_processed

write.csv(machinedata_test_processed,"C:\\Users\\amberwaltz\\Documents\\Machine Data Test.csv", row.names = FALSE)
