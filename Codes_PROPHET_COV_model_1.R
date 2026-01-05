# Charger les bibliothèques nécessaires
library(prophet)
library(dplyr)
library(lubridate)
library(ggplot2)

data <- PM2_5_long

data$Type_of_station <- factor(data$Type_of_station, 
                               levels = c(1,2), 
                               labels = c("Industrial", "Urban"))
# Préparation des données
data <- data.frame(
  ds = as.Date(substr(data$ID_Date, 1, 10)),
  y = as.numeric(data$PM2.5),
  Type_of_station = as.factor(data$Type_of_station)
)

# Supprimer les valeurs manquantes
data <- na.omit(data)

# Diviser les données en train (70%) et test (30%)
set.seed(123)
train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Créer le modèle Prophet avec la covariable
model <- prophet(
  growth = 'linear',
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE
)

# Ajouter la covariable Type_of_station
model <- add_regressor(model, 'Type_of_station')

# Ajuster le modèle sur les données d'entraînement
model_fit <- fit.prophet(model, train_data)

# Faire des prédictions sur l'ensemble de test
forecast_test <- predict(model_fit, test_data)

# Calculer les métriques de performance
residuals <- test_data$y - forecast_test$yhat
RMSE <- sqrt(mean(residuals^2))
MAE <- mean(abs(residuals))

cat("RMSE:", RMSE, "\n")
cat("MAE:", MAE, "\n")

# Obtenir les intervalles de confiance des coefficients beta
beta_samples <- model_fit$params$beta
beta_means <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, probs = c(0.025, 0.975))

# Afficher les résultats pour la covariable
cat("\nCoefficient beta pour Type_of_station:\n")
cat("Estimation:", beta_means[1], "\n")
cat("Intervalle de confiance à 95%: [", beta_ci[1, 1], ",", beta_ci[2, 1], "]\n")

# Visualisation des résultats
plot(model_fit, forecast_test) + 
  ggtitle("Prédictions PM2.5 avec Prophet") +
  xlab("Date") + 
  ylab("PM2.5")

# Composantes du modèle
prophet_plot_components(model_fit, forecast_test)

# Créer un dataframe pour les prédictions futures
future_dates <- make_future_dataframe(model_fit, periods = 30)
future_data <- data.frame(
  ds = future_dates$ds,
  Type_of_station = factor(rep(1, nrow(future_dates)), # Exemple avec type 1
                           stringsAsFactors = FALSE
  )
  
  # Prédictions futures
  future_forecast <- predict(model_fit, future_data)
  
  # Résumé des résultats
  results_summary <- data.frame(
    Métrique = c("RMSE", "MAE", "Beta_Type_of_station", "Beta_CI_lower", "Beta_CI_upper"),
    Valeur = c(RMSE, MAE, beta_means[1], beta_ci[1, 1], beta_ci[2, 1])
  )
  
  print(results_summary)
  
  # Analyse de sensibilité pour différents types de stations
  sensitivity_analysis <- data.frame(
    Type_station = c(1, 2),
    Impact_PM25 = c(beta_means[1] * 1, beta_means[1] * 2)
  )
  
  print(sensitivity_analysis)