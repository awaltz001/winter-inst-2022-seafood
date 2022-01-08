## Title: Seafood Sustainability and GDP Per Capita
## Authors: Clarissa Gallo, Nicole Perez, Amber Waltz
## Research question: Does 
## Hypothesis: We believe that there is a positive correlation between aquaculture and
## freshwater fish production.

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


write.csv(production,
          "C:\\Users\\amberwaltz\\Documents\\production.csv",
          row.names = FALSE)

## load packages

library(tidymodels)
library(tidyverse)
library(stargazer)
library(rsample)

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

df <- merge(x = df,
            y = production,
            by.x = c("Country Code","year_only"),
            by.y = c("Code","Year"),
            all.x = TRUE)



df_cleaned <- df[ -c(1, 4:6, 8:11, 13, 16, 18:20, 22:28, 30:31) ]
df_cleaned <- select(df, "year_only", "Country Name.x", "Country Code",
                     "co2_emissions_per_capita", "Aquaculture production (metric tons)", "Capture fisheries production (metric tons)", "Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Freshwater Fish - 2761 - Production - 5510 - tonnes",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Pelagic Fish - 2763 - Production - 5510 - tonnes",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Crustaceans - 2765 - Production - 5510 - tonnes", "Artisanal (small-scale commercial)",
                     "Industrial (large-scale commercial)", "Discards", "Recreational", "Subsistence",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Cephalopods - 2766 - Production - 5510 - tonnes",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Demersal Fish - 2762 - Production - 5510 - tonnes",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Molluscs, Other - 2767 - Production - 5510 - tonnes",
                     "Commodity Balances - Livestock and Fish Primary Equivalent - Marine Fish, Other - 2764 - Production - 5510 - tonnes")

df_cleaned$co2_emissions_per_capita <- as.numeric(df_cleaned$co2_emissions_per_capita)

## clean data

df_cleaned$aquaprod <- df_cleaned$`Aquaculture production (metric tons)` 
df_cleaned$captureprod <- df_cleaned$`Capture fisheries production (metric tons)`
df_cleaned$consumption <- df_cleaned$`Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`
df_cleaned$totalprod <- df_cleaned$aquaprod + df_cleaned$captureprod
df_cleaned$proportion_aquaprod <- df_cleaned$aquaprod / df_cleaned$totalprod
df_cleaned$country_code <- df_cleaned$`Country Code`
df_cleaned$country_name <- df_cleaned$`Country Name.x`
df_cleaned$freshwater <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Freshwater Fish - 2761 - Production - 5510 - tonnes`
df_cleaned$molluscs <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Molluscs, Other - 2767 - Production - 5510 - tonnes`
df_cleaned$pelagic <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Pelagic Fish - 2763 - Production - 5510 - tonnes`
df_cleaned$crustacean <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Crustaceans - 2765 - Production - 5510 - tonnes`
df_cleaned$cephalopods <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Cephalopods - 2766 - Production - 5510 - tonnes`
df_cleaned$marine <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Marine Fish, Other - 2764 - Production - 5510 - tonnes`
df_cleaned$demersal <- df_cleaned$`Commodity Balances - Livestock and Fish Primary Equivalent - Demersal Fish - 2762 - Production - 5510 - tonnes`
df_cleaned$artisanal <- df_cleaned$`Artisanal (small-scale commercial)`
df_cleaned$discards <- df_cleaned$Discards

df_cleaned %>%
  filter(country_name != "China") %>%
  ggplot(aes(proportion_aquaprod, freshwater)) + geom_point(color = country_name) 

df_cleaned %>%
  filter(country_name != "China") %>%
  ggplot(aes(proportion_aquaprod, freshwater)) + geom_smooth()

ggplot(data = df_cleaned) +
  geom_smooth(mapping = aes(x = year_only, y = freshwater, group = country_name))

ggplot(data = df_cleaned, mapping = aes(x=year_only, y=freshwater, color=country_name)) +
  geom_smooth(mapping = aes(linetype =country_name))

ggplot(df_cleaned, aes(year_only)) +   
  geom_line(aes(aquaprod), colour="red") +
  geom_line(aes(captureprod), colour="green")

ggplot(data = df_cleaned, mapping =aes(x=year_only, y=totalprod, color = aquaprod) ) +
  geom_smooth(se=FALSE)

machinedata <- select(df_cleaned, "year_only", "country_name", "proportion_aquaprod",
                      "consumption", "freshwater", "co2_emissions_per_capita", "totalprod")

machinedata$co2_emissions_per_capita <- as.numeric(machinedata$co2_emissions_per_capita)

## explore data - note to self: edit these graphs to filter! and may want to take these out later

ggplot(data=df_cleaned, aes(x=year_only, y=proportion_aquaprod, group=1)) +
  geom_smooth()

ggplot(data=df_cleaned, aes(x=year_only, y=gdp_nulls_removed, group=1)) +
  geom_smooth()

hist(machinedata$proportion_aquaprod)

p<-ggplot(data=df, aes(x=, y=len)) +
  geom_bar(stat="identity")

ggplot(data=df_cleaned, aes(x=year_only, y=proportion_aquaprod, group=1)) +
  geom_smooth()


## start machine learning


set.seed(456)

machinedata_split <- initial_split(machinedata)
machinedata_train <- training(machinedata_split)
machinedata_test <- testing(machinedata_split)

library(recipes)

machinedata_recip <- recipe(proportion_aquaprod ~ .,
                            data = machinedata_train)

machinedata_recip <- machinedata_recip %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -proportion_aquaprod)

machinedata_recip %>% 
  prep()

machinedata_train_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = machinedata_train)

machinedata_test_processed <- machinedata_recip %>%
  prep() %>%
  bake(new_data = machinedata_test)

## linear model from machine learning test

fit2 <- lm (proportion_aquaprod ~ .,
            data= machinedata_test_processed)

summary(fit2)

## model: coefficients will be year, consumption, and GDP

aquaprod_lm <- lm(proportion_aquaprod ~ 
                  year_only + consumption + freshwater + co2_emissions_per_capita,
                  data = df_cleaned)

summary(aquaprod_lm)


stargazer(aquaprod_lm)

df %>%
  filter(Entity.x != "China") %>%
  ggplot(aes(proportion_aquaprod, freshwater)) + geom_point() 



df_cleaned %>%
  filter(country_name != "China") %>%
  ggplot(aes(proportion_aquaprod, freshwater)) + geom_point() 

df_cleaned %>%
  filter(country_name != "China") %>%
  ggplot(aes(proportion_aquaprod, consumption)) + geom_point() 

freshwatersp %>% filter(country_name != "China")


write.csv(machinedata_test_processed,
          "C:\\Users\\amberwaltz\\Documents\\Machine Data Test.csv",
          row.names = FALSE)
