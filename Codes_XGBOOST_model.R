library(data.table)
library(lubridate)
library(xgboost)
library(Metrics)
library(ggplot2)
library(readr)

# --- CONFIGURATION ---
# Nom du fichier téléchargé
file_path <- PM10_long1
# Colonne cible
target_col <- "PM10"
# Proportion de la série à utiliser pour le test/forecast (par exemple, les 365 derniers jours)
forecast_horizon <- 365
# ---------------------

## 2. Chargement et Préparation des Données

# Charger le fichier CSV
# L'encodage par défaut peut poser problème avec certains fichiers. Utilisation de read_csv pour plus de robustesse.
data <- file_path

# Convertir en data.table pour une manipulation facile
setDT(data)

# Assurer que la colonne de date est au format Date
# Le format de date semble être "Jour-Mois-Année" avec le mois en abrégé (ex: 01-JAN-2010)
data[, ID_Date := dmy(ID_Date)]

# Trier par date pour s'assurer que l'ordre est correct
setorder(data, ID_Date)

# Vérification simple des données après chargement
head(data)
str(data)

## 3. Feature Engineering

# Création de caractéristiques temporelles (features)
data[, `:=`(
  year = year(ID_Date),
  month = month(ID_Date),
  day = mday(ID_Date),
  wday = wday(ID_Date), # Jour de la semaine (factor)
  yday = yday(ID_Date), # Jour de l'année (1-365)
  quarter = quarter(ID_Date) # Trimestre
)]

# Convertir les variables catégorielles en numérique/factor (XGBoost préfère les numériques)
# Les variables comme 'wday' doivent être gérées. On utilise as.integer(as.factor(...))
data[, wday_num := as.integer(factor(wday, ordered = FALSE))]

# Liste des features à utiliser
features <- c("year", "month", "day", "wday_num", "yday", "quarter")

## 4. Séparation des Ensembles Entraînement et Test

# Déterminer la date de coupure (split)
split_date <- data[.N - forecast_horizon, ID_Date]

# Séparation des ensembles
train_set <- data[ID_Date <= split_date]
test_set <- data[ID_Date > split_date]

cat(sprintf("\n--- Séparation des données ---\n"))
cat(sprintf("Date de début de l'entraînement : %s\n", min(train_set$ID_Date)))
cat(sprintf("Date de fin de l'entraînement   : %s\n", max(train_set$ID_Date)))
cat(sprintf("Date de début du forecast       : %s\n", min(test_set$ID_Date)))
cat(sprintf("Nombre de jours dans l'entraînement : %d\n", nrow(train_set)))
cat(sprintf("Nombre de jours pour le forecast    : %d\n", nrow(test_set)))

# Préparation des matrices DMatrix pour XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[, ..features]), label = train_set[[target_col]])
dtest <- xgb.DMatrix(data = as.matrix(test_set[, ..features]))

## 5. Entraînement du Modèle XGBoost

# Définition des hyperparamètres (vous pouvez les ajuster)
params <- list(
  objective = "reg:squarederror", # Pour la régression
  eta = 0.05,                      # Taux d'apprentissage
  max_depth = 6,                   # Profondeur max de l'arbre
  subsample = 0.7,                 # Ratio d'échantillons à utiliser
  colsample_bytree = 0.7           # Ratio de colonnes à utiliser
)

# Entraînement du modèle (utilisation de toute la série d'entraînement)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500, # Nombre d'itérations/arbres
  verbose = 0 # Désactiver le log pendant l'entraînement
)

cat("\n--- Entraînement du Modèle Terminé ---\n")
# 

## 6. Prédiction (Forecast) et Évaluation

# Prédiction sur l'ensemble de test
test_set[, PM10_Predicted := predict(xgb_model, dtest)]

# Calcul des métriques sur l'ensemble de test
rmse_val <- rmse(test_set[[target_col]], test_set$PM2.5_Predicted)
mae_val <- mae(test_set[[target_col]], test_set$PM2.5_Predicted)

cat("\n--- Résultats des Métriques ---\n")
cat(sprintf("RSME (Root Mean Squared Error) : %.3f\n", rmse_val))
cat(sprintf("MAE (Mean Absolute Error)      : %.3f\n", mae_val))

-------------------------------------------------------------------------------------------------
  # --- 6b. Prédiction sur l'Ensemble d'Entraînement (Fitted Values) ---
  
  # Préparation de la matrice DMatrix pour l'ensemble d'entraînement
  # Nous avons déjà 'dtrain' depuis l'étape 4, mais nous allons le recréer pour être sûr.
dtrain_matrix <- xgb.DMatrix(data = as.matrix(train_set[, ..features]))

# Prédiction sur l'ensemble d'entraînement
train_set[, PM10_Fitted := predict(xgb_model, dtrain_matrix)]

# Calcul des métriques d'ajustement (Goodness of Fit)
rmse_fitted <- rmse(train_set[[target_col]], train_set$PM10_Fitted)
mae_fitted <- mae(train_set[[target_col]], train_set$PM10_Fitted)

cat("\n--- Résultats des Métriques d'Ajustement (Ensemble d'Entraînement) ---\n")
cat(sprintf("RSME d'Ajustement (Fitted RMSE) : %.3f\n", rmse_fitted))
cat(sprintf("MAE d'Ajustement (Fitted MAE)   : %.3f\n", mae_fitted))
cat(sprintf("Comparaison au RMSE de Test (%.3f) : Si le Fitted RMSE est beaucoup plus petit, il y a un risque de surapprentissage.\n", rmse_val))


------------------------------------------------------------------------------------------------------------

  # --- 10. Création des Données Futures pour le Forecast (01/01/2021 au 31/12/2030) ---
  
  # Définition de l'horizon de prévision
start_date_future <- as.Date("2021-01-01")
end_date_future <- as.Date("2030-12-31")

# Création de la séquence de dates futures
future_dates <- seq(from = start_date_future, to = end_date_future, by = "day")

# Création du data.table futur
future_data <- data.table(ID_Date = future_dates)

# --- 11. Feature Engineering sur les Données Futures ---

# Appliquer exactement la même logique de création de features
future_data[, `:=`(
  year = year(ID_Date),
  month = month(ID_Date),
  day = mday(ID_Date),
  wday = wday(ID_Date),
  yday = yday(ID_Date),
  quarter = quarter(ID_Date)
)]

# Convertir la variable catégorielle 'wday' en numérique ('wday_num')
# Il faut s'assurer que les niveaux de facteurs sont les mêmes que pour l'entraînement.
# Nous allons utiliser les mêmes niveaux de facteurs que ceux créés dans l'étape 3.
wday_levels <- levels(data$wday) # Récupérer les niveaux depuis la série d'entraînement

future_data[, wday_num := as.integer(factor(wday, levels = wday_levels, ordered = FALSE))]

# Liste des features (la même que pour l'entraînement)
features <- c("year", "month", "day", "wday_num", "yday", "quarter")

# Préparation de la matrice DMatrix pour XGBoost
dfuture <- xgb.DMatrix(data = as.matrix(future_data[, ..features]))

cat(sprintf("\n--- Forecast Long Terme ---\n"))
cat(sprintf("Période de prévision : %s à %s\n", start_date_future, end_date_future))
cat(sprintf("Nombre de jours à prévoir : %d\n", nrow(future_data)))


# --- 12. Prédiction et Calcul de l'Incertitude ---

# Prédiction sur les données futures
future_data[, PM10_Forecast := predict(xgb_model, dfuture)]

# Récupération de l'erreur standard (RMSE) calculée sur l'ensemble de test
# (Utilisation du RMSE de test comme estimation de la déviation de l'erreur)
sigma_error <- rmse_fitted
Z_score <- 1.96 # Pour un intervalle de prédiction à 95%

# Calcul des intervalles de prédiction à 95%
future_data[, `:=`(
  PM10_Upper_95 = PM10_Forecast + Z_score * sigma_error,
  PM10_Lower_95 = PM10_Forecast - Z_score * sigma_error
)]

# --- 13. Affichage des Résultats (Tableau) ---

# Sélection et formatage des colonnes à afficher
forecast_output <- future_data[, .(
  Date = ID_Date,
  `Prévision PM10` = round(PM10_Forecast, 3),
  `Borne Inf. (95%)` = round(PM10_Lower_95, 3),
  `Borne Sup. (95%)` = round(PM10_Upper_95, 3)
)]