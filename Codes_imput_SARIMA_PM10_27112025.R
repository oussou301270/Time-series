# Load required libraries
library(dplyr)
library(tidyr)
library(forecast)
library(zoo)

# Read the data
data <- data_PM10_long

# Convert date to proper format
data$ID_Date <- as.Date(data$ID_Date, format = "%d-%b-%Y")

# Create complete date-station grid
all_dates <- seq(min(data$ID_Date), max(data$ID_Date), by = "day")
stations <- unique(data$Station)
complete_grid <- expand.grid(ID_Date = all_dates, Station = stations)

# Merge with original data
complete_data <- merge(complete_grid, data, by = c("ID_Date", "Station"), all.x = TRUE)

# Function to impute missing values using SARIMA
impute_sarima <- function(x) {
  # Create time series object (daily data, no explicit seasonality)
  ts_data <- ts(x, frequency = 1)
  
  # Only impute if there are non-NA values
  if (sum(!is.na(ts_data)) > 10) {
    tryCatch({
      # Auto-fit SARIMA model
      fit <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
      # Impute missing values
      imputed <- na.kalman(ts_data, model = fit$model)
      return(imputed)
    }, error = function(e) {
      # If SARIMA fails, use linear interpolation
      return(na.approx(ts_data, rule = 2))
    })
  } else {
    # If too few observations, use linear interpolation
    return(na.approx(ts_data, rule = 2))
  }
}

# Apply imputation by station
imputed_data <- complete_data %>%
  group_by(Station) %>%
  arrange(ID_Date) %>%
  mutate(PM10_Imputed = impute_sarima(PM10)) %>%
  ungroup()

# Create final dataset (use original values where available, imputed where missing)
final_data <- imputed_data %>%
  mutate(PM10_Final = ifelse(is.na(PM10), PM10_Imputed, PM10)) %>%
  select(ID_Date, Station, PM10 = PM10_Final)

# View summary of imputed values
summary(final_data)

# Check station-wise missing values before/after
original_missing <- data %>% group_by(Station) %>% summarise(Original_NA = sum(is.na(PM10)))
final_missing <- final_data %>% group_by(Station) %>% summarise(Final_NA = sum(is.na(PM10)))

print(original_missing)
print(final_missing)

# Save imputed dataset
write.csv(final_data, "C:/Users/Lenovo/Documents/Trip_UGANDA_APHRC/Air_Pollution/Final_Data_Air_Pollution/data_PM10_imputed.csv", row.names = FALSE)