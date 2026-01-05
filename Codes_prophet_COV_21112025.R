##### PM2.5 ########

#============================#
# 1️⃣ Charger les librairies
#============================#
install.packages("remotes")
install.packages("cmdstanr", type = "source")
cmdstanr::install_cmdstan()

library(dplyr)
library(tidyr)
library(prophet)
library(Metrics)

#============================#
# 2️⃣ Préparer les données
#============================#
# Votre dataframe 'data1' doit avoir :
# ds (date), y (valeur à prédire), et colonnes dummy des stations :
# Industrial, Urban, PeriUrban, SubUrban, Background

# Exemple structure :
# ds         y     Industrial Urban PeriUrban SubUrban Background

data = PM2_5_long

data$Type_of_station <- factor(data$Type_of_station, 
                               levels = c(1,2), 
                               labels = c("Industrial", "Urban"))
#============================#
# 3️⃣ Créer le modèle Prophet
#============================#
data1 <- data %>%
  rename(
    ds = ID_Date,
    y = PM2.5
  )
data1 <- data1 %>%
  mutate(Type_of_station = as.factor(Type_of_station)) %>%
  tidyr::pivot_wider(
    names_from = Type_of_station,
    values_from = Type_of_station,
    values_fn = length,
    values_fill = 0
  )

m <- prophet(daily.seasonality = TRUE)  # pas de saisonnalité journalière

# Ajouter les régressions
m <- add_regressor(m, 'Industrial')
m <- add_regressor(m, 'Urban')

# Entraîner le modèle
m <- fit.prophet(m, data1)
print(m$extra_regressors)
betas <- m$params$beta
colnames(betas) <- names(m$extra_regressors)

print(betas)

#============================#
# 4️⃣ Créer le futur
#============================#
future_dates <- make_future_dataframe(m, periods = 3652)

stations <- c("Industrial", "Urban")

future <- future_dates %>%
  slice(rep(1:n(), each = length(stations))) %>%
  mutate(
    Industrial = ifelse(row_number() %% 5 == 1, 1, 0),
    Urban      = ifelse(row_number() %% 5 == 2, 1, 0),
      )

#============================#
# 5️⃣ Faire la prédiction
#============================#
forecast <- predict(m, future)
plot(m, forecast)

#============================#
# 6️⃣ Préparer les données pour calcul métriques
#============================#
# Transformer en format long pour aligner stations
data_long <- data1 %>%
  pivot_longer(cols = Industrial:Urban, names_to = "Station", values_to = "flag") %>%
  filter(flag == 1) %>%
  select(ds, Station, y)

forecast_long <- forecast %>%
  pivot_longer(cols = Industrial:Urban, names_to = "Station", values_to = "flag") %>%
  filter(flag == 1) %>%
  select(ds, Station, yhat)

# Merge réel vs prédit
eval_df <- data_long %>%
  left_join(forecast_long, by = c("ds", "Station"))

#============================#
# 7️⃣ Calculer RMSE, MAE, MAPE par station
#============================#
# Assurez-vous que 'forecast' a exactement le même nombre de lignes que data1
eval_df <- data1 %>%
  mutate(yhat = forecast$yhat[1:nrow(data1)])

# Calcul des métriques par station
smape <- function(y, yhat) {
  mean( 2 * abs(y - yhat) / (abs(y) + abs(yhat)) ) * 100
}

metrics_by_station <- eval_df %>%
  group_by(Type_of_station) %>%
  summarise(
    RMSE = sqrt(mean((y - yhat)^2, na.rm = TRUE)),
    MAE  = mean(abs(y - yhat), na.rm = TRUE),
    MAPE = mean(abs((y - yhat) / y), na.rm = TRUE) * 100
  )

print(metrics_by_station)


# Alignement des prédictions sur les données réelles
eval_df1 <- data1 %>%
  mutate(yhat = forecast$yhat[1:nrow(data1)])  # ne prendre que les lignes historiques

# Calcul des métriques globales
rmse_global <- rmse(eval_df$y, eval_df$yhat)
mae_global  <- mae(eval_df$y, eval_df$yhat)
mape_global <- mape(eval_df$y, eval_df$yhat) * 100  # en pourcentage

# Affichage
cat("Global RMSE  =", round(rmse_global,2), "\n")
cat("Global MAE   =", round(mae_global,2), "\n")
cat("Global MAPE  =", round(mape_global,2), "%\n")

install.packages("writexl")
library(writexl)

write_xlsx(forecast, path = "C:/Users/Lenovo/Documents/Trip_UGANDA_APHRC/Air_Pollution/Final_Data_Air_Pollution/forecast_prophet_cov.xlsx")
write_xlsx(metrics_by_station, path = "C:/Users/Lenovo/Desktop/Data_Air_Pollution/(metricsbystation_prophet_cov.xlsx")

# Extraire les coefficients des régresseurs (stations)
betas <- m$params$beta  # Matrice des coefficients des régresseurs

# Créer un dataframe avec les noms des stations et leurs coefficients
stations_coef <- data.frame(
  Station = names(m$extra_regressors),
  Beta = colMeans(betas),  # Moyenne des coefficients sur toutes les itérations MCMC
  Beta_SD = apply(betas, 2, sd)  # Écart-type pour voir la variabilité
)

# Afficher les coefficients
print(stations_coef)

# Visualisation des coefficients
library(ggplot2)
ggplot(stations_coef, aes(x = Station, y = Beta, fill = Station)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Beta - Beta_SD, ymax = Beta + Beta_SD), width = 0.2) +
  labs(title = "Coefficients des stations dans le modèle Prophet",
       y = "Valeur du coefficient",
       x = "Type de station") +
  theme_minimal()

# Alternative : extraire les informations complètes des régresseurs
print(m$extra_regressors)

# Pour avoir les intervalles de crédibilité (si besoin)
beta_samples <- as.data.frame(betas)
colnames(beta_samples) <- names(m$extra_regressors)

# Résumé statistique des échantillons MCMC
summary(beta_samples)

# Visualisation des distributions
library(tidyr)
beta_samples_long <- pivot_longer(beta_samples, everything(), names_to = "Station", values_to = "Beta")

ggplot(beta_samples_long, aes(x = Beta, fill = Station)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Station, scales = "free") +
  labs(title = "Distributions des coefficients par station",
       x = "Valeur du coefficient",
       y = "Densité") +
  theme_minimal()

regressor_names <- names(m$extra_regressors)

beta_intervals <- data.frame(
  Station = regressor_names,
  Mean = colMeans(beta_samples),
  Median = apply(beta_samples, 2, median),
  SD = apply(beta_samples, 2, sd),
  CI_2.5 = apply(beta_samples, 2, quantile, probs = 0.025),
  CI_97.5 = apply(beta_samples, 2, quantile, probs = 0.975),
  CI_5 = apply(beta_samples, 2, quantile, probs = 0.05),
  CI_95 = apply(beta_samples, 2, quantile, probs = 0.95),
  P_value = apply(beta_samples, 2, function(x) 2 * min(mean(x > 0), mean(x < 0)))  # p-value bayésienne
)
print(beta_intervals)

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

  ##### PM10 ########

#============================#
# 1️⃣ Charger les librairies
#============================#
library(dplyr)
library(tidyr)
library(prophet)
library(Metrics)

#============================#
# 2️⃣ Préparer les données
#============================#
# Votre dataframe 'data1' doit avoir :
# ds (date), y (valeur à prédire), et colonnes dummy des stations :
# Industrial, Urban, PeriUrban, SubUrban, Background

# Exemple structure :
# ds         y     Industrial Urban PeriUrban SubUrban Background

data = PM10_long

data$Type_of_station <- factor(data$Type_of_station, 
                               levels = c(1,2,3,4), 
                               labels = c("Industrial", "Urban", "PeriUrban","SubUrban"))

#============================#
# 3️⃣ Créer le modèle Prophet
#============================#
data1 <- data %>%
  rename(
    ds = ID_Date,
    y = PM10
  )

data1 <- data1 %>%
  mutate(Type_of_station = as.factor(Type_of_station)) %>%
  tidyr::pivot_wider(
    names_from = Type_of_station,
    values_from = Type_of_station,
    values_fn = length,
    values_fill = 0
  )
m <- prophet(daily.seasonality = TRUE)  # pas de saisonnalité journalière

# Ajouter les régressions
m <- add_regressor(m, 'Industrial')
m <- add_regressor(m, 'Urban')
m <- add_regressor(m, 'PeriUrban')
m <- add_regressor(m, 'SubUrban')


# Entraîner le modèle
m <- fit.prophet(m, data1)

#============================#
# 4️⃣ Créer le futur
#============================#
future_dates <- make_future_dataframe(m, periods = 3652)

stations <- c("Industrial", "Urban", "PeriUrban", "SubUrban")

future <- future_dates %>%
  slice(rep(1:n(), each = length(stations))) %>%
  mutate(
    Industrial = ifelse(row_number() %% 5 == 1, 1, 0),
    Urban      = ifelse(row_number() %% 5 == 2, 1, 0),
    PeriUrban  = ifelse(row_number() %% 5 == 3, 1, 0),
    SubUrban   = ifelse(row_number() %% 5 == 4, 1, 0)
    )

#============================#
# 5️⃣ Faire la prédiction
#============================#
forecast <- predict(m, future)
plot(m, forecast)

#============================#
# 6️⃣ Préparer les données pour calcul métriques
#============================#
# Transformer en format long pour aligner stations
data_long <- data1 %>%
  pivot_longer(cols = Industrial:SubUrban, names_to = "Station", values_to = "flag") %>%
  filter(flag == 1) %>%
  select(ds, Station, y)

forecast_long <- forecast %>%
  pivot_longer(cols = Industrial:SubUrban, names_to = "Station", values_to = "flag") %>%
  filter(flag == 1) %>%
  select(ds, Station, yhat)

# Merge réel vs prédit
eval_df <- data_long %>%
  left_join(forecast_long, by = c("ds", "Station"))

#============================#
# 7️⃣ Calculer RMSE, MAE, MAPE par station
#============================#
# Assurez-vous que 'forecast' a exactement le même nombre de lignes que data1
eval_df <- data1 %>%
  mutate(yhat = forecast$yhat[1:nrow(data1)])

# Calcul des métriques par station
metrics_by_station <- eval_df %>%
  pivot_longer(cols = Industrial:Urban, names_to = "Station", values_to = "flag") %>%
  filter(flag == 1) %>%
  group_by(Station) %>%
  summarise(
    RMSE = rmse(y, yhat),
    MAE  = mae(y, yhat),
    MAPE = mape(y, yhat) * 100,
  )

print(metrics_by_station)


# Alignement des prédictions sur les données réelles
eval_df1 <- data1 %>%
  mutate(yhat = forecast$yhat[1:nrow(data1)])  # ne prendre que les lignes historiques

# Calcul des métriques globales
rmse_global <- rmse(eval_df$y, eval_df$yhat)
mae_global  <- mae(eval_df$y, eval_df$yhat)
mape_global <- mape(eval_df$y, eval_df$yhat) * 100  # en pourcentage

# Affichage
cat("Global RMSE  =", round(rmse_global,2), "\n")
cat("Global MAE   =", round(mae_global,2), "\n")
cat("Global MAPE  =", round(mape_global,2), "%\n")

install.packages("writexl")
library(writexl)

write_xlsx(forecast, path = "C:/Users/Lenovo/Documents/Trip_UGANDA_APHRC/Air_Pollution/Final_Data_Air_Pollution/forecast_PM10prophet_cov.xlsx")
write_xlsx(metrics_by_station, path = "C:/Users/Lenovo/Desktop/Data_Air_Pollution/(metricsbystation_PM2.5prophet_cov.xlsx")

### Extraction des betas et leur IDC

# Extraire les coefficients des régresseurs (stations)
betas <- m$params$beta  # Matrice des coefficients des régresseurs

# Créer un dataframe avec les noms des stations et leurs coefficients
stations_coef <- data.frame(
  Station = names(m$extra_regressors),
  Beta = colMeans(betas),  # Moyenne des coefficients sur toutes les itérations MCMC
  Beta_SD = apply(betas, 2, sd)  # Écart-type pour voir la variabilité
)

# Afficher les coefficients
print(stations_coef)

# Visualisation des coefficients
library(ggplot2)
ggplot(stations_coef, aes(x = Station, y = Beta, fill = Station)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Beta - Beta_SD, ymax = Beta + Beta_SD), width = 0.2) +
  labs(title = "Coefficients des stations dans le modèle Prophet",
       y = "Valeur du coefficient",
       x = "Type de station") +
  theme_minimal()

# Alternative : extraire les informations complètes des régresseurs
print(m$extra_regressors)

# Pour avoir les intervalles de crédibilité (si besoin)
beta_samples <- as.data.frame(betas)
colnames(beta_samples) <- names(m$extra_regressors)

# Résumé statistique des échantillons MCMC
summary(beta_samples)

# Visualisation des distributions
library(tidyr)
beta_samples_long <- pivot_longer(beta_samples, everything(), names_to = "Station", values_to = "Beta")

ggplot(beta_samples_long, aes(x = Beta, fill = Station)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Station, scales = "free") +
  labs(title = "Distributions des coefficients par station",
       x = "Valeur du coefficient",
       y = "Densité") +
  theme_minimal()

regressor_names <- names(m$extra_regressors)

beta_intervals <- data.frame(
  Station = regressor_names,
  Mean = colMeans(beta_samples),
  Median = apply(beta_samples, 2, median),
  SD = apply(beta_samples, 2, sd),
  CI_2.5 = apply(beta_samples, 2, quantile, probs = 0.025),
  CI_97.5 = apply(beta_samples, 2, quantile, probs = 0.975),
  CI_5 = apply(beta_samples, 2, quantile, probs = 0.05),
  CI_95 = apply(beta_samples, 2, quantile, probs = 0.95),
  P_value = apply(beta_samples, 2, function(x) 2 * min(mean(x > 0), mean(x < 0)))  # p-value bayésienne
)
print(beta_intervals)

# Obtenir les intervalles de confiance des coefficients beta
beta_samples <- m$params$beta
beta_means <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, probs = c(0.025, 0.975))

# Afficher les résultats pour la covariable
cat("\nCoefficient beta pour Type_of_station:\n")
cat("Estimation:", beta_means[1], "\n")
cat("Intervalle de confiance à 95%: [", beta_ci[1, 1], ",", beta_ci[2, 1], "]\n")