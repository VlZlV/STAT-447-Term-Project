library(rstan)
library(dplyr)
library(tidyr)


df <- read.csv("v50_PM10_1970_2015 (1)(TOTALS BY COUNTRY).csv") %>%
  # Remove all auto-generated empty columns(weird problem)
  select(-matches("^X(\\.?\\d+)?$")) %>%
  # Convert and pivot
  mutate(across(starts_with("Y_"), as.numeric)) %>%
  #reshaping the data into a new,long format for easier future processing
  pivot_longer(
    cols = starts_with("Y_"), # Columns convert to pivots
    names_to = "year", # Create a new column containing the years
    values_to = "pm25", # Create a new column containing the pm2.5 values
    values_drop_na = TRUE # Remove NA pm2.5 rows
  ) %>%
  # Convert "Y_1990" → 1990
  mutate(year = as.numeric(gsub("Y_", "", year)))

stan_data <- list(
  N = nrow(df), # Total observations
  pm25 = df$pm25,
  year = df$year - min(df$year), # Centered year (1970-2015 becomes 0-45)
  annex = ifelse(df$IPCC.Annex == "Annex_I", 1, 0), # Binary annex status
  country_id = as.numeric(factor(df$ISO_A3)), # Unique country IDs
  region_id = as.numeric(factor(df$World.Region)), # Unique region IDs
  J = length(unique(df$ISO_A3)), # Number of countries
  K = length(unique(df$World.Region)) # Number of regions
)


spatial_model <- stan(
  file = "spatial_model_rao.stan",
  data = stan_data,
  chains = 4,
  iter = 2000,
  seed = 1
)

print(spatial_model, pars = c("alpha", "beta_year", "beta_annex", "beta_interaction"))
