# 1. Load Libraries
library(tidyverse)
library(forecast)
library(lubridate)
library(readr) # Explicitly load readr

# 2. Load and Prepare Data
# Assuming 'PM2.5_long.xlsx - Sheet1.csv' is the main data file
data_long <- PM2_5_long

data_processed <- data_long %>%
  rename(
    ID_Date = ID_Date,
    Station_ID = Type_of_station,
    PM25 = PM2.5
  ) %>%
  # Convert date column
  mutate(ID_Date = ymd(ID_Date)) %>%
  # Sort by date
  arrange(ID_Date) %>%
  # Create a factor for station type. R will create a dummy variable.
  # Station_ID = 1 (Industriel) will be the baseline.
  # Station_ID = 2 (Urbain) will have an associated coefficient (beta).
  mutate(Station_Factor = factor(Station_ID))

# Create the time series object (using weekly seasonality, m=7)
# Change 'frequency = 7' to 'frequency = 365' for annual seasonality if desired.
ts_data <- ts(data_processed$PM25, frequency = 7)

# Create the exogenous variable matrix (xreg)
# The factor 'Station_Factor' is converted to a design matrix.
# We remove the intercept column because the ARIMA model already has an intercept/drift.
# The remaining column 'Station_Factor2' acts as the dummy variable for Station Type 2.
xreg_matrix <- model.matrix(~ Station_Factor, data = data_processed)
xreg_matrix <- xreg_matrix[, -1, drop = FALSE] # Remove the intercept column
colnames(xreg_matrix) <- 'Station_Type_2_Effect'

# 3. Train SARIMAX Model (using auto.arima for order selection)
# The 'xreg' argument includes the Type_of_station covariate.
sarimax_fit <- auto.arima(
  y = ts_data,
  xreg = xreg_matrix,
  stepwise = TRUE,
  approximation = FALSE,
  seasonal = TRUE
)

# Display the model summary (includes selected order, AIC, etc.)
cat('\n### SARIMAX Model Summary ###\n')
print(summary(sarimax_fit))

# --------------------------------------------------

# 4. Extracting Coefficients (Betas) for Type of Station
cat('\n### Betas (Coefficients) for Each Type of Station ###\n')
betas <- sarimax_fit$coef

# The coefficient for the covariate is the 'Station_Type_2_Effect' beta.
betas_covariates <- betas[names(betas) %in% colnames(xreg_matrix)]
print(betas_covariates)

# Interpretation:
# - The 'Station_Type_2_Effect' value is the estimated **difference** in the mean PM2.5 level
#   of Station Type 2 compared to Station Type 1 (the baseline), after accounting for the time series components.
# - To get the total baseline level for Type 1, you would need to combine the model's 'intercept' or 'drift' term with the PM2.5 mean, but the 'Station_Type_2_Effect' is the primary $\beta$ of interest here.

# --------------------------------------------------

# 5. Model Evaluation: RMSE and MAE (In-sample accuracy)
cat('\n### RMSE and MAE (In-sample accuracy) ###\n')
# The 'accuracy' function provides various metrics, including RMSE and MAE.
accuracy_metrics <- accuracy(sarimax_fit)

# The first row of the accuracy matrix contains the in-sample metrics.
rmse <- accuracy_metrics[1, 'RMSE']
mae <- accuracy_metrics[1, 'MAE']

cat(paste('RMSE:', round(rmse, 4), '\n'))
cat(paste('MAE:', round(mae, 4), '\n'))

# --------------------------------------------------

# 6. Forecast
cat('\n### Forecast (h=30 steps) ###\n')
# To generate a forecast, you must provide future values for the exogenous variable (xreg).
h_steps <- 2555 # Number of steps to forecast

# Scenario A: Forecast for Station Type 1 (Industriel)
# 'Station_Type_2_Effect' is 0 for Type 1 (baseline)
new_xreg_type1 <- matrix(0, nrow = h_steps, ncol = 1)
colnames(new_xreg_type1) <- 'Station_Type_2_Effect'

forecast_type1 <- forecast(sarimax_fit, xreg = new_xreg_type1, h = h_steps)
cat('\nForecast for Type 1 Station (next 30 steps):\n')
print(head(forecast_type1$mean)) # Show the first 6 forecast means

# Scenario B: Forecast for Station Type 2 (Urbain)
# 'Station_Type_2_Effect' is 1 for Type 2
new_xreg_type2 <- matrix(1, nrow = h_steps, ncol = 1)
colnames(new_xreg_type2) <- 'Station_Type_2_Effect'

forecast_type2 <- forecast(sarimax_fit, xreg = new_xreg_type2, h = h_steps)
cat('\nForecast for Type 2 Station (next 30 steps):\n')
print(head(forecast_type2$mean)) # Show the first 6 forecast means

# Plot the forecasts
plot(forecast_type1, main = 'SARIMAX Forecast for Type 1 Station (h= 3652)')
plot(forecast_type2, main = 'SARIMAX Forecast for Type 2 Station (h= 3652)')