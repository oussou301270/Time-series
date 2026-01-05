# Load required libraries
library(forecast)
library(zoo)
library(dplyr)
library(lubridate)

# Prepare the data
df <- PM2_5_data_stations  # Load your data
df$ID_Date <- as.Date(df$ID_Date, format = "%d-%b-%Y")

# Create time series objects
belair_ts <- ts(df$BelAir, frequency = 365, start = c(2010, 1))
bdrep_ts <- ts(df$BdRep, frequency = 365, start = c(2010, 1))

# Function to impute missing values using SARIMA
impute_sarima <- function(ts_data, series_name) {
  cat("Imputing", series_name, "...\n")
  
  # Find missing values
  missing_indices <- which(is.na(ts_data))
  cat("Number of missing values:", length(missing_indices), "\n")
  
  if (length(missing_indices) == 0) {
    cat("No missing values found.\n")
    return(ts_data)
  }
  
  # Fit SARIMA model on available data
  tryCatch({
    # Auto-select SARIMA parameters
    sarima_model <- auto.arima(ts_data, seasonal = TRUE, 
                               stepwise = TRUE, approximation = FALSE,
                               trace = FALSE)
    
    cat("Selected SARIMA model:", arimaorder(sarima_model), "\n")
    
    # Forecast missing values
    if (length(missing_indices) > 0) {
      # Create a complete time series for imputation
      ts_complete <- ts_data
      
      for (i in missing_indices) {
        # Fit model on all data except the current missing value
        train_data <- ts_data
        train_data[i] <- NA
        
        # Use Kalman filter for imputation (handled by Arima)
        impute_model <- Arima(train_data, model = sarima_model)
        ts_complete[i] <- fitted(impute_model)[i]
      }
      
      cat("Imputation completed.\n")
      return(ts_complete)
    }
    
  }, error = function(e) {
    cat("Error in SARIMA imputation:", e$message, "\n")
    cat("Using linear interpolation as fallback...\n")
    return(na.approx(ts_data, rule = 2))
  })
}

# Impute missing values for both stations
belair_imputed <- impute_sarima(belair_ts, "BelAir")
bdrep_imputed <- impute_sarima(bdrep_ts, "BdRep")

# Create results dataframe
results <- data.frame(
  ID_Date = df$ID_Date,
  BelAir_Original = df$BelAir,
  BelAir_Imputed = as.numeric(belair_imputed),
  BdRep_Original = df$BdRep,
  BdRep_Imputed = as.numeric(bdrep_imputed)
)

# Add flags for imputed values
results$BelAir_Imputed_Flag <- ifelse(is.na(results$BelAir_Original), "Imputed", "Original")
results$BdRep_Imputed_Flag <- ifelse(is.na(results$BdRep_Original), "Imputed", "Original")

# Summary of imputation
cat("\n--- Imputation Summary ---\n")
cat("BelAir: Originally", sum(is.na(df$BelAir)), "missing values\n")
cat("BdRep: Originally", sum(is.na(df$BdRep)), "missing values\n")
cat("BelAir: After imputation", sum(is.na(results$BelAir_Imputed)), "missing values\n")
cat("BdRep: After imputation", sum(is.na(results$BdRep_Imputed)), "missing values\n")

# Visualize results (optional)
par(mfrow = c(2, 2))
plot(belair_ts, main = "BelAir Original", col = "blue", lwd = 1)
plot(belair_imputed, main = "BelAir Imputed", col = "red", lwd = 1)
plot(bdrep_ts, main = "BdRep Original", col = "blue", lwd = 1)
plot(bdrep_imputed, main = "BdRep Imputed", col = "red", lwd = 1)

# Save results
write.csv(results, "PM2_5_imputed_results.csv", row.names = FALSE)

# Compare statistics before and after imputation
summary_stats <- data.frame(
  Station = c("BelAir_Original", "BelAir_Imputed", "BdRep_Original", "BdRep_Imputed"),
  Mean = c(mean(df$BelAir, na.rm = TRUE), mean(results$BelAir_Imputed),
           mean(df$BdRep, na.rm = TRUE), mean(results$BdRep_Imputed)),
  SD = c(sd(df$BelAir, na.rm = TRUE), sd(results$BelAir_Imputed),
         sd(df$BdRep, na.rm = TRUE), sd(results$BdRep_Imputed)),
  Missing = c(sum(is.na(df$BelAir)), 0, sum(is.na(df$BdRep)), 0)
)

print(summary_stats)