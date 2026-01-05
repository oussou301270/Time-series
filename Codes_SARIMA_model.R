#**** PM2.5 **********************************************************************************************

### Préparation des Données

library(readr)
library(forecast)
library(tseries)
library(xts)

### Chargement des données

data_pm25 <- PM2_5_long1

colnames(data_pm25) <- c("Date", "PM2.5")

# Convertir la colonne 'Date' en format Date

data_pm25$Date <- as.Date(data_pm25$Date, format = "%d-%b-%Y")

# Créer l'objet Time Series (TS)

freq_saison <- 365.25 # Fréquence annuelle
start_date <- c(2010, 1) # Année, jour

ts_pm25 <- ts(data_pm25$PM2.5, start = start_date, frequency = freq_saison)

# Aperçu

print(ts_pm25)
plot(ts_pm25, main = "Time serie of PM2.5 with SARIMA imputation method", ylab = "PM2.5")

### Identification des Paramètres du Modèle

model_sarima_auto <- auto.arima(ts_pm25, 
                                seasonal = TRUE, # Important pour activer la saisonnalité
                                stepwise = TRUE, # Recherche plus rapide
                                trace = TRUE)   # Afficher les modèles testés

# Afficher les résultats du meilleur modèle trouvé
print("Meilleur modèle SARIMA (auto.arima) :")
print(model_sarima_auto)

### Prévision (Forecasting)

# --- 6. Prévision ---
# Prévoir les PM2.5 pour les 365 prochains jours (1 an)
nombre_jours_prevision <- 3652 

forecast_pm25 <- forecast(model_sarima_auto, h = nombre_jours_prevision)

# Afficher le tableau des prévisions (moyenne, intervalles de confiance 80% et 95%)
print(forecast_pm25)

# Visualisation graphique de la prévision
plot(forecast_pm25, 
     main = paste("Prévision PM2.5 avec SARIMA", model_sarima_auto$model),
     xlab = "Temps",
     ylab = "Concentration PM2.5 (µg/m³)")

### Calcul du RMSE et du MAE pour l'Évaluation du Modèle SARIMA

# --- 7. Calcul des métriques d'erreur (RMSE, MAE, etc.) ---

metrics_on_training <- accuracy(model_sarima_auto)

# Afficher toutes les métriques
print("Métriques de performance sur les données d'entraînement :")
print(metrics_on_training)

# Extraire spécifiquement le RMSE et le MAE
rmse_value <- metrics_on_training["Training set", "RMSE"]
mae_value <- metrics_on_training["Training set", "MAE"]

cat("\nRMSE (Root Mean Squared Error) :", round(rmse_value, 2), "µg/m³\n")
cat("MAE (Mean Absolute Error) :", round(mae_value, 2), "µg/m³\n")

------------------------------------------------------------------------------------------------------

#**** PM10 **********************************************************************************************
  
### Préparation des Données
  
library(readr)
library(forecast)
library(tseries)
library(xts)

### Chargement des données

data_pm10 <- PM10_long2

colnames(data_pm10) <- c("Date", "PM10")

# Convertir la colonne 'Date' en format Date

data_pm10$Date <- as.Date(data_pm10$Date, format = "%d-%b-%Y")

# Créer l'objet Time Series (TS)

freq_saison <- 365.25 # Fréquence annuelle
start_date <- c(2010, 1) # Année, jour

ts_pm10 <- ts(data_pm10$PM10, start = start_date, frequency = freq_saison)

# Aperçu

print(ts_pm10)
plot(ts_pm10, main = "Time serie of PM10 with SARIMA imputation method", ylab = "PM10")

### Identification des Paramètres du Modèle

model_sarima_auto <- auto.arima(ts_pm10, 
                                seasonal = TRUE, # Important pour activer la saisonnalité
                                stepwise = TRUE, # Recherche plus rapide
                                trace = TRUE)   # Afficher les modèles testés

# Afficher les résultats du meilleur modèle trouvé
print("Meilleur modèle SARIMA (auto.arima) :")
print(model_sarima_auto)

### Prévision (Forecasting)

# --- 6. Prévision ---
# Prévoir les PM10 pour les 365 prochains jours (1 an)
nombre_jours_prevision <- 3652 

forecast_pm10 <- forecast(model_sarima_auto, h = nombre_jours_prevision)

# Afficher le tableau des prévisions (moyenne, intervalles de confiance 80% et 95%)
print(forecast_pm10)

# Visualisation graphique de la prévision
plot(forecast_pm10, 
     main = paste("Prévision PM10 avec SARIMA", model_sarima_auto$model),
     xlab = "Temps",
     ylab = "Concentration PM10 (µg/m³)")

### Calcul du RMSE et du MAE pour l'Évaluation du Modèle SARIMA

# --- 7. Calcul des métriques d'erreur (RMSE, MAE, etc.) ---

metrics_on_training <- accuracy(model_sarima_auto)

# Afficher toutes les métriques
print("Métriques de performance sur les données d'entraînement :")
print(metrics_on_training)

# Extraire spécifiquement le RMSE et le MAE
rmse_value <- metrics_on_training["Training set", "RMSE"]
mae_value <- metrics_on_training["Training set", "MAE"]

cat("\nRMSE (Root Mean Squared Error) :", round(rmse_value, 2), "µg/m³\n")
cat("MAE (Mean Absolute Error) :", round(mae_value, 2), "µg/m³\n")
------------------------------------------------------------------------------------------------------
  
