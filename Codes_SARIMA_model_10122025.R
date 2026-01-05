# --- 1. Installation et Chargement des Packages ---
library(readr)
library(lubridate)
library(forecast)
library(Metrics)
# NB: Assurez-vous d'avoir chargé tous ces packages.

# --- 2. Chargement et Préparation des Données ---

data <- PM10_long1
names(data) <- c("Date", "PM10")

# FIX CRUCIAL: Convertir EXPLICITEMENT la colonne Date en type Date avec le format correct
# Le format de vos dates est "01-JAN-2010" -> "%d-%b-%Y"
data$Date <- as.Date(data$Date, format = "%d-%b-%Y")

# Définir l'objet série temporelle avec la fréquence annuelle (365)
ts_data <- ts(data$PM10, start = c(2010, 1), frequency = 365) 

# --- 3. Définition du Nouvel Horizon de Prévision ---

# La colonne 'data$Date' est maintenant garantie d'être de type Date.
last_date <- tail(data$Date, 1) # Extraction simple d'un objet Date
target_date <- as.Date("2030-12-31")

# Calcul du nombre de jours à prévoir (h).
h <- as.numeric(target_date - last_date) 

cat(sprintf("Horizon de prévision (h) : %d jours (du %s au %s)\n", h, last_date + 1, target_date))


# --- 4. Génération des Termes de Fourier ---

K <- 5 # 5 paires de Fourier pour capturer la saisonnalité de 365 jours

fourier_terms <- fourier(ts_data, K = K)
future_fourier_terms <- fourier(ts_data, K = K, h = h)


# --- 5. Ajustement du Modèle ARIMA avec Régresseurs de Fourier (ARIMAX) ---

model_fourier <- auto.arima(ts_data, 
                            xreg = fourier_terms, 
                            D = 0, # Désactive la différenciation saisonnière (D=0)
                            max.P = 0, max.Q = 0, # Désactive les termes saisonniers (P, Q)
                            stepwise = FALSE,
                            approximation = FALSE)

print("--- Résumé du Modèle ARIMAX avec Fourier (Ordres trouvés) ---")
print(summary(model_fourier))


# --- 6. Prévision Long Terme ---

forecast_fourier_long <- forecast(model_fourier, xreg = future_fourier_terms, h = h)


# --- 7. Calcul des Métriques sur les Données d'Entraînement ---

fitted_values <- fitted(model_fourier)
actual_values <- ts_data

rmse_val <- rmse(actual_values, fitted_values)
mae_val <- mae(actual_values, fitted_values)

print("--- Métriques d'Évaluation (sur l'ensemble de la série) ---")
cat(sprintf("RMSE : %.2f\n", rmse_val))
cat(sprintf("MAE  : %.2f\n", mae_val))


# --- 8. Affichage de la Prévision (Début, Milieu, Fin) ---

forecast_df_long <- data.frame(
  Date = seq(last_date + days(1), by = "day", length.out = h),
  PM10_Forecast = round(as.numeric(forecast_fourier_long$mean), 2),
  Lower_95 = round(as.numeric(forecast_fourier_long$lower[, 2]), 2), 
  Upper_95 = round(as.numeric(forecast_fourier_long$upper[, 2]), 2)  
)

print("--- Prévisions Détaillées (Début de la période) ---")
print(head(forecast_df_long, n=10))

print("--- Prévisions Détaillées (Fin de la période : Année 2030) ---")
print(tail(forecast_df_long, n=10))


# --- 9. Visualisation des Résultats ---

print("--- Graphique de la Prévision Long Terme (10 Ans) ---")

plot(forecast_fourier_long, 
     main = "Prévision de PM2.5 (2020-2030) avec ARIMAX-Fourier",
     ylab = expression("Concentration PM2.5 (" * mu * "g/" * m^3 * ")"),
     xlab = "Temps (Années)")

lines(fitted(model_fourier), col = "blue")

legend("topleft", 
       legend = c("Série Historique", "Prévisions", "Valeurs Ajustées"),
       col = c("black", "red", "blue"), 
       lty = c(1, 1, 1),
       cex = 0.8)

# --- Les étapes 1 à 4 du script précédent (chargement, fourier, auto.arima, prévision court terme) doivent avoir été exécutées.
# L'objet 'forecast_fourier_short' est supposé être disponible.

# --- 5. Construction des Tableaux (Méthode Robuste) ---

# A. Période d'Ajustement (Fit) : 01/01/2010 au 20/11/2020
df_fit <- data.frame(
  Date = data$Date,
  Type = "Ajusté (Fit)",
  PM10_Observé = round(as.numeric(data$PM10), 2),
  PM10_Prédiction = round(as.numeric(fitted(model_fourier)), 2),
  CI_95_Inf = NA, 
  CI_95_Sup = NA
)

# B. Période de Prévision (Forecast) : 21/11/2020 au 31/12/2020
# *** MÉTHODE ROBUSTE POUR EXTRAIRE LES PRÉVISIONS ***
forecast_df_raw <- as.data.frame(forecast_fourier_long)

# Renommer les colonnes pour la clarté
names(forecast_df_raw) <- c("PM10_Prédiction", "CI_80_Inf", "CI_80_Sup", "CI_95_Inf", "CI_95_Sup")

df_forecast <- data.frame(
  Date = seq(last_date + days(1), by = "day", length.out = h),
  Type = "Prévision (Forecast)",
  PM10_Observé = NA,
  PM10_Prédiction = round(forecast_df_raw$PM10_Prédiction, 2),
  CI_95_Inf = round(forecast_df_raw$CI_95_Inf, 2), # Utilise la colonne directement
  CI_95_Sup = round(forecast_df_raw$CI_95_Sup, 2)  # Utilise la colonne directement
)
# *******************************************************

# C. Combinaison et affichage du Tableau 2020
final_table_2020 <- rbind(df_fit, df_forecast)
df_2020_only <- subset(final_table_2020, format(Date, "%Y") == "2020")

print("--- Tableau Complet des Prédictions et CI pour l'Année 2020 (Corrigé) ---")
print("  (Ajustement jusqu'au 20/11/2020, Prévision après)  ")

# Affichage de la zone de transition
print("Période de transition (Novembre/Décembre 2020) :")
print(tail(df_2020_only, n = 15))