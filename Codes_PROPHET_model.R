#************ PM2.5 *************************************************************************************

### Préparation de l'environnement et Chargement des Données

library(prophet)
library(tidyverse)


data_pm25 <- PM2_5_long

data_prophet <- data_pm25 %>%
  rename(ds = ID_Date, y = PM2.5) %>%
  # Conversion de la colonne de date au format YYYY-MM-DD
  mutate(ds = as.Date(ds, format = "%d-%b-%Y"))

### Ajustement et Entraînement du Modèle Prophet

m <- prophet(data_prophet, daily.seasonality=TRUE)

### Préparation du Futur et Prévisions

future <- make_future_dataframe(m, periods = 3652)

# Afficher les dernières lignes du futur dataframe (les nouvelles dates)
tail(future)

# 6. Faire les prévisions
forecast <- predict(m, future)

# Afficher les colonnes de prévisions les plus importantes :
# ds : Date
# yhat : La prévision (point estimate)
# yhat_lower : Intervalle de confiance inférieur (par défaut à 80%)
# yhat_upper : Intervalle de confiance supérieur
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

### Visualisation des Résultats

plot(m, forecast)

### RSME et MAE

# --- Assurez-vous que les librairies sont chargées ---
library(prophet)
library(tidyverse)

# --- Variables de Durée (réutilisation du code précédent) ---
# Durée initiale d'entraînement (e.g., 5 ans)
initial_train_days <- 5 * 365.25
initial_duration <- as.difftime(initial_train_days, units = "days")

# Fréquence entre chaque coupe (e.g., 6 mois)
period_cut_days <- 180
period_duration <- as.difftime(period_cut_days, units = "days")

# Horizon de prédiction à tester (e.g., 1 an)
forecast_horizon_days <- 365
horizon_duration <- as.difftime(forecast_horizon_days, units = "days")
horizon_str <- paste(forecast_horizon_days, "days") # Pour le filtrage final


# --- 1. Exécuter la Validation Croisée (si non fait juste avant) ---
# Assurez-vous que le modèle 'm' a été entraîné précédemment
df_cv <- cross_validation(
  model = m,
  initial = initial_duration,
  period = period_duration,
  horizon = horizon_duration
)

# --- 2. Calculer les Métriques de Performance ---
df_p <- performance_metrics(df_cv)

# --- 1. Indexation Base R pour le Filtrage (SOLUTION) ---
# Ceci est l'équivalent de 'df_p %>% filter(h == horizon_str)'
# mais utilise le Base R (moins sujet aux conflits de packages).

final_metrics_filtered <- df_p[df_p$h == horizon_str, ]


# --- 2. Afficher le RMSE et le MAE ---

# Affichage des résultats
cat("\n--- Résultats pour l'horizon de prévision maximal (", forecast_horizon_days, " jours) ---\n", sep="")
cat("👉 RMSE (Root Mean Squared Error) :", round(final_metrics_filtered$rmse, 3), "\n")
cat("👉 MAE (Mean Absolute Error) :", round(final_metrics_filtered$mae, 3), "\n")


cat("\n--- Résultats (via dernière ligne du tableau) ---\n")
cat("👉 RMSE :", round(final_metrics_last$rmse, 3), "\n")
cat("👉 MAE :", round(final_metrics_last$mae, 3), "\n")

#************ PM10 *************************************************************************************

### Préparation de l'environnement et Chargement des Données

library(prophet)
library(tidyverse)


data_pm10 <- PM10_long1

data_prophet <- data_pm10 %>%
  rename(ds = ID_Date, y = PM10) %>%
  # Conversion de la colonne de date au format YYYY-MM-DD
  mutate(ds = as.Date(ds, format = "%d-%b-%Y"))

### Ajustement et Entraînement du Modèle Prophet

m <- prophet(data_prophet, daily.seasonality=TRUE)

### Préparation du Futur et Prévisions

future <- make_future_dataframe(m, periods = 3652)

# Afficher les dernières lignes du futur dataframe (les nouvelles dates)
tail(future)

# 6. Faire les prévisions
forecast <- predict(m, future)

# Afficher les colonnes de prévisions les plus importantes :
# ds : Date
# yhat : La prévision (point estimate)
# yhat_lower : Intervalle de confiance inférieur (par défaut à 80%)
# yhat_upper : Intervalle de confiance supérieur
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

### Visualisation des Résultats

plot(m, forecast)

### RSME et MAE

# --- Assurez-vous que les librairies sont chargées ---
library(prophet)
library(tidyverse)

# --- Variables de Durée (réutilisation du code précédent) ---
# Durée initiale d'entraînement (e.g., 5 ans)
initial_train_days <- 5 * 365.25
initial_duration <- as.difftime(initial_train_days, units = "days")

# Fréquence entre chaque coupe (e.g., 6 mois)
period_cut_days <- 180
period_duration <- as.difftime(period_cut_days, units = "days")

# Horizon de prédiction à tester (e.g., 1 an)
forecast_horizon_days <- 365
horizon_duration <- as.difftime(forecast_horizon_days, units = "days")
horizon_str <- paste(forecast_horizon_days, "days") # Pour le filtrage final


# --- 1. Exécuter la Validation Croisée (si non fait juste avant) ---
# Assurez-vous que le modèle 'm' a été entraîné précédemment
df_cv <- cross_validation(
  model = m,
  initial = initial_duration,
  period = period_duration,
  horizon = horizon_duration
)

# --- 2. Calculer les Métriques de Performance ---
df_p <- performance_metrics(df_cv)

# --- 1. Indexation Base R pour le Filtrage (SOLUTION) ---
# Ceci est l'équivalent de 'df_p %>% filter(h == horizon_str)'
# mais utilise le Base R (moins sujet aux conflits de packages).

final_metrics_filtered <- df_p[df_p$h == horizon_str, ]


# --- 2. Afficher le RMSE et le MAE ---

# Affichage des résultats
cat("\n--- Résultats pour l'horizon de prévision maximal (", forecast_horizon_days, " jours) ---\n", sep="")
cat("👉 RMSE (Root Mean Squared Error) :", round(final_metrics_filtered$rmse, 3), "\n")
cat("👉 MAE (Mean Absolute Error) :", round(final_metrics_filtered$mae, 3), "\n")

# Option B: Prendre la dernière ligne du tableau df_p (Indexation Base R)
final_metrics_last <- tail(df_p, 1)

cat("\n--- Résultats (via dernière ligne du tableau) ---\n")
cat("👉 RMSE :", round(final_metrics_last$rmse, 3), "\n")
cat("👉 MAE :", round(final_metrics_last$mae, 3), "\n")