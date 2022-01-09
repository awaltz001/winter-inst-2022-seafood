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


## load packages

library(tidymodels)
library(tidyverse)
library(stargazer)
library(rsample)
library(recipes)
library(glmnet)

## merge the data

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
df_cleaned$proportion_freshwater <- df_cleaned$freshwater/ df_cleaned$totalprod
df_cleaned$proportion_molluscs <- df_cleaned$molluscs/ df_cleaned$totalprod
df_cleaned$proportion_pelagic <- df_cleaned$pelagic/ df_cleaned$totalprod
df_cleaned$proportion_crustacean <- df_cleaned$crustacean/ df_cleaned$totalprod
df_cleaned$proportion_cephalopods <- df_cleaned$cephalopods/ df_cleaned$totalprod
df_cleaned$proportion_marine <- df_cleaned$marine/ df_cleaned$totalprod
df_cleaned$proportion_demersal <- df_cleaned$demersal/ df_cleaned$totalprod
df_cleaned$proportion_artisanal <- df_cleaned$artisanal/ df_cleaned$totalprod
df_cleaned$proportion_discards <- df_cleaned$discards/ df_cleaned$totalprod

machinedata <- select(df_cleaned, "year_only", "country_name", "proportion_aquaprod",
                      "consumption", "proportion_freshwater", "proportion_molluscs", "proportion_pelagic", "proportion_crustacean",
                      "proportion_cephalopods", "proportion_marine", "proportion_demersal",
                      "co2_emissions_per_capita", "totalprod")

machinedata$co2_emissions_per_capita <- as.numeric(machinedata$co2_emissions_per_capita)


## start machine learning

set.seed(789)

trim_machinedata <- na.omit(machinedata)

machinedata_split  <- initial_split(trim_machinedata)
machinedata_train <- training(machinedata_split)
machinedata_test <- testing(machinedata_split)

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


X <- machinedata_train_processed %>% select(!proportion_aquaprod)

X <- as.matrix(X)
  
y <- machinedata_train_processed %>% select(proportion_aquaprod) %>%
  unlist %>% as.numeric()

lasso_out <- glmnet(X, y, alpha=1)

cv_lasso_out <- cv.glmnet(X, y, alpha=1)

coefs_lasso <- coef(cv_lasso_out, s = "lambda.1se")

## linear model to compare coefficients

fit1 <- lm (proportion_aquaprod ~ .,
            data= machinedata_train_processed)

coefs_fit1 <- coef(fit1)

summary(fit1)

## compare coefficients

cbind(coefs_lasso, coefs_fit1)

## find X and predict coefs for test data 

X_test <- machinedata_test_processed %>% select(!proportion_aquaprod)

X_test_predictors <- as.matrix(X_test)

X_test <- cbind(1, X_test_predictors)

y_hat_lasso <- X_test %*% coefs_lasso

y_test <- machinedata_test_processed %>% select(proportion_aquaprod) %>%
  unlist %>% as.numeric()

residuals_lasso <- y_test - y_hat_lasso

rmse_lasso <- sqrt(mean((residuals_lasso)^2))

rmse_lasso

## calculate rmse with lm

set.seed(789)

y_hat_lm <- X_test %*% coefs_fit1

residuals_lm <- y_test - y_hat_lm

residuals_lm

rmse_lm <- sqrt(mean((residuals_lm)^2))

rmse_lm

# calculate rmse with lasso, lambda = min

set.seed(789)

coefs_lasso_lambdamin <- coef(cv_lasso_out, s = "lambda.min")

cbind(coefs_lasso_lambdamin, coefs_fit1)

y_hat_lasso_lambdamin <- X_test %*% coefs_lasso_lambdamin

residuals_lasso_lambdamin <- y_test - y_hat_lasso_lambdamin

rmse_lasso_lambdamin <- sqrt(mean((residuals_lasso_lambdamin)^2))

rmse_lasso_lambdamin

## calculate rmse with lasso, alpha = .5

set.seed(789)

cv_lasso_out_alpha5 <- cv.glmnet(X_test_predictors, y_test, alpha=.5)

coefs_lasso_alpha5 <- coef(cv_lasso_out_alpha5, s = "lambda.min")

y_hat_lasso_alpha5 <- X_test %*% coefs_lasso_alpha5

residuals_lasso_alpha5 <- y_test - y_hat_lasso_alpha5

rmse_lasso_alpha5 <- sqrt(mean((residuals_lasso_alpha5)^2))

rmse_lasso_alpha5


coefs_lasso_alpha5

confint(fit1)


write.csv(captured_vs_farmed, "C:\\Users\\amberwaltz\\Documents\\captured.csv", row.names = FALSE)
