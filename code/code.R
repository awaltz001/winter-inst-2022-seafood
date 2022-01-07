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
library(stargazer)

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

df_cleaned <- df[ -c(1, 4:6, 8:11, 13, 16, 18:20, 22:23) ]

df_cleaned$co2_emissions_per_capita <- as.numeric(machinedata$co2_emissions_per_capita)

## clean data

df_cleaned$aquaprod <- df$`Aquaculture production (metric tons)` 
df_cleaned$captureprod <- df$`Capture fisheries production (metric tons)`
df_cleaned$consumption <- df$`Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`
df_cleaned$totalprod <- df_cleaned$aquaprod + df_cleaned$captureprod
df_cleaned$proportion_aquaprod <- df_cleaned$aquaprod / df_cleaned$totalprod
df_cleaned$largescale <- df$`Industrial (large-scale commercial)`
df_cleaned$proportion_largescale <- df_cleaned$largescale / df_cleaned$totalprod
df_cleaned$country_code <- df$`Country Code`
df_cleaned$country_name <- df$`Country Name.x`
df_cleaned$gdp_nulls_removed <- as.numeric(na_if(df$gdp_per_capita, ".."))

sum(is.na(df_cleaned$aquaprod))

machinedata <- df_cleaned[ -c(2,4:10,12,14,16) ]


## explore data - note to self: edit these graphs to filter! and may want to take these out later


ggplot(data=df_cleaned, aes(x=year_only, y=proportion_aquaprod, group=1)) +
  geom_smooth()

ggplot(data=df_cleaned, aes(x=year_only, y=gdp_nulls_removed, group=1)) +
  geom_smooth()

hist(machinedata$proportion_aquaprod)


## start machine learning

library(rsample)
library(tidymodels)

set.seed(123)

machinedata_split <- initial_split(machinedata)
machinedata_train <- training(machinedata_split)
machinedata_test <- testing(machinedata_split)

library(recipes)

machinedata_recip <- recipe(proportion_aquaprod ~ .,
                            data = machinedata_train)

machinedata_recip <- machinedata_recip %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -proportion_aquaprod) %>%
  step_interact(terms = ~
                consumption:gdp_nulls_removed) %>%
  step_impute_knn(all_predictors()) 

machinedata_recip %>% 
  prep()

machinedata_train_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = machinedata_train)

machinedata_train_processed


## linear model from machine learning training data

fit1 <- lm (proportion_aquaprod ~ .,
            data= machinedata_train_processed)

summary(fit1)

machinedata_test_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = machinedata_test)

machinedata_test_processed

fit2 <- lm (proportion_aquaprod ~ .,
            data= machinedata_test_processed)

summary(fit2)


## model: coefficients will be year, consumption, and GDP

aquaprod_lm <- lm(proportion_aquaprod ~ 
                  year_only + consumption + gdp_nulls_removed,
                  data = df_cleaned)

summary(aquaprod_lm)

stargazer(aquaprod_lm)

pairs(machinedata)

write.csv(machinedata_test_processed,
          "C:\\Users\\amberwaltz\\Documents\\Machine Data Test.csv",
          row.names = FALSE)
