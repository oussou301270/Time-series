# Load required libraries
library(forecast)
library(zoo)
library(dplyr)
library(lubridate)

# --- 1. Préparation des données (Comme dans votre code original) ---
df <- PM10_data_stations  # Assurez-vous que cette variable est chargée
df$ID_Date <- as.Date(df$ID_Date, format = "%d-%b-%Y")

# Créer des objets de séries chronologiques avec la fréquence saisonnière (365 pour les données journalières)
belair_ts <- ts(df$BelAir, frequency = 365, start = c(2010, 1))
bdrep_ts <- ts(df$BdRep, frequency = 365, start = c(2010, 1))
HLM_ts <- ts(df$HLM, frequency = 365, start = c(2010, 1))
Medina_ts <- ts(df$Medina, frequency = 365, start = c(2010, 1))
Yoff_ts <- ts(df$Yoff, frequency = 365, start = c(2010, 1))

# --- 2. Fonction d'Imputation SARIMA Efficace ---
# Utilisation de na.interp() qui fait l'imputation via l'algorithme de lissage de Kalman
# après l'ajustement automatique du modèle SARIMA (auto.arima).
impute_sarima_efficient <- function(ts_data, series_name) {
  cat("Imputing", series_name, "using na.interp()...\n")
  
  missing_count <- sum(is.na(ts_data))
  cat("Number of missing values:", missing_count, "\n")
  
  if (missing_count == 0) {
    cat("No missing values found.\n")
    return(ts_data)
  }
  
  # na.interp() utilise auto.arima par défaut, qui gère la saisonnalité 
  # automatiquement si elle est nécessaire.
  tryCatch({
    # RETRAIT DE L'ARGUMENT 'seasonal = TRUE'
    ts_imputed <- na.interp(ts_data, lambda = NULL) 
    cat("Imputation completed successfully.\n")
    return(ts_imputed)
    
  }, error = function(e) {
    cat("Error in SARIMA imputation (na.interp):", e$message, "\n")
    cat("Using linear interpolation as fallback...\n")
    return(na.approx(ts_data, rule = 2))
  })
}

# --- 3. Imputation des valeurs manquantes pour les deux stations ---
belair_imputed <- impute_sarima_efficient(belair_ts, "BelAir")
bdrep_imputed <- impute_sarima_efficient(bdrep_ts, "BdRep")
HLM_imputed <- impute_sarima_efficient(HLM_ts, "HLM")
Medina_imputed <- impute_sarima_efficient(Medina_ts, "Medina")
Yoff_imputed <- impute_sarima_efficient(Yoff_ts, "Yoff")

# --- 4. Création du dataframe de résultats et Indicateurs d'Imputation ---
results <- data.frame(
  ID_Date <- df$ID_Date,
  BelAir_Original <- df$BelAir,
  BelAir_Imputed <- as.numeric(belair_imputed),
  BdRep_Original <- df$BdRep,
  BdRep_Imputed <- as.numeric(bdrep_imputed),
  HLM_Original <- df$HLM,
  HLM_Imputed <- as.numeric(HLM_imputed),
  Medina_Original <- df$Medina,
  Medina_Imputed <- as.numeric(Medina_imputed),
  Yoff_Original <- df$Yoff,
  Yoff_Imputed <- as.numeric(Yoff_imputed)
  )

# Ajouter des drapeaux pour les valeurs imputées
results$BelAir_Imputed_Flag <- ifelse(is.na(results$BelAir_Original), "Imputed", "Original")
results$BdRep_Imputed_Flag <- ifelse(is.na(results$BdRep_Original), "Imputed", "Original")
results$HLM_Imputed_Flag <- ifelse(is.na(results$HLM_Original), "Imputed", "Original")
results$Medina_Imputed_Flag <- ifelse(is.na(results$Medina_Original), "Imputed", "Original")
results$Yoff_Imputed_Flag <- ifelse(is.na(results$Yoff_Original), "Imputed", "Original")

# --- 5. Résumé et Statistiques de Comparaison ---
cat("\n--- Imputation Summary ---\n")
cat("BelAir: Originally", sum(is.na(df$BelAir)), "missing values\n")
cat("BdRep: Originally", sum(is.na(df$BdRep)), "missing values\n")
cat("HLM: Originally", sum(is.na(df$HLM)), "missing values\n")
cat("Medina: Originally", sum(is.na(df$Medina)), "missing values\n")
cat("Yoff: Originally", sum(is.na(df$Yoff)), "missing values\n")

# Comparer les statistiques avant et après imputation
summary_stats <- data.frame(
  Station = c("BelAir_Original", "BelAir_Imputed", "BdRep_Original", "BdRep_Imputed","HLM_Original", "HLM_Imputed","Medina_Original", "Medina_Imputed","Yoff_Original", "Yoff_Imputed"),
  Mean = c(mean(df$BelAir, na.rm = TRUE), mean(results$BelAir_Imputed),
           mean(df$BdRep, na.rm = TRUE), mean(results$BdRep_Imputed),
           mean(df$HLM, na.rm = TRUE), mean(results$HLM_Imputed),
           mean(df$Medina, na.rm = TRUE), mean(results$Medina_Imputed),
           mean(df$Yoff, na.rm = TRUE), mean(results$Yoff_Imputed)),

  SD = c(sd(df$BelAir, na.rm = TRUE), sd(results$BelAir_Imputed),
         sd(df$BdRep, na.rm = TRUE), sd(results$BdRep_Imputed),
         sd(df$HLM, na.rm = TRUE), sd(results$HLM_Imputed),
         sd(df$Medina, na.rm = TRUE), sd(results$Medina_Imputed),
         sd(df$Yoff, na.rm = TRUE), sd(results$Yoff_Imputed)),
         
  Missing = c(sum(is.na(df$BelAir)), sum(is.na(results$BelAir_Imputed)), 
              sum(is.na(df$BdRep)), sum(is.na(results$BdRep_Imputed)),
              sum(is.na(df$HLM)), sum(is.na(results$HLM_Imputed)),
              sum(is.na(df$Medina)), sum(is.na(results$Medina_Imputed)),
              sum(is.na(df$Yoff)), sum(is.na(results$Yoff_Imputed)))
              )

print(summary_stats)

# --- 6. Visualisation des résultats (Optionnel) ---
par(mfrow = c(2, 2))
plot(belair_ts, main = "BelAir Original", col = "blue", lwd = 1)
# Pour visualiser les points imputés, on peut les marquer sur le graphique imputé
plot(belair_imputed, main = "BelAir Imputed (na.interp)", col = "red", lwd = 1)
points(time(belair_ts)[is.na(belair_ts)], belair_imputed[is.na(belair_ts)], col = "black", pch = 19, cex = 0.5)

plot(bdrep_ts, main = "BdRep Original", col = "blue", lwd = 1)
plot(bdrep_imputed, main = "BdRep Imputed (na.interp)", col = "red", lwd = 1)
points(time(bdrep_ts)[is.na(bdrep_ts)], bdrep_imputed[is.na(bdrep_ts)], col = "black", pch = 19, cex = 0.5)
par(mfrow = c(1, 1)) # Réinitialiser le layout graphique

plot(HLM_ts, main = "HLM Original", col = "blue", lwd = 1)
plot(HLM_imputed, main = "HLM Imputed (na.interp)", col = "red", lwd = 1)
points(time(HLM_ts)[is.na(HLM_ts)], HLM_imputed[is.na(HLM_ts)], col = "black", pch = 19, cex = 0.5)
par(mfrow = c(1, 1)) # Réinitialiser le layout graphique

plot(Medina_ts, main = "Medina Original", col = "blue", lwd = 1)
plot(Medina_imputed, main = "Medina Imputed (na.interp)", col = "red", lwd = 1)
points(time(Medina_ts)[is.na(Medina_ts)], Medina_imputed[is.na(Medina_ts)], col = "black", pch = 19, cex = 0.5)
par(mfrow = c(1, 1)) # Réinitialiser le layout graphique

plot(Yoff_ts, main = "Yoff Original", col = "blue", lwd = 1)
plot(Yoff_imputed, main = "Yoff Imputed (na.interp)", col = "red", lwd = 1)
points(time(Yoff_ts)[is.na(Yoff_ts)], Yoff_imputed[is.na(Yoff_ts)], col = "black", pch = 19, cex = 0.5)
par(mfrow = c(1, 1)) # Réinitialiser le layout graphique


# --- 7. Sauvegarde des résultats (Optionnel) ---
write.csv(results, "C:/Users/Lenovo/Documents/Trip_UGANDA_APHRC/Air_Pollution/Final_Data_Air_Pollution/PM10_imputed_results.csv", row.names = FALSE)