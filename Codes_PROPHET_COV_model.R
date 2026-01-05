############################################
#     PM2.5
############################################
library(readxl)
library(dplyr)
library(tidyr)
library(prophet)
library(Metrics)


data <- PM2_5_long

data$Type_of_station <- factor(data$Type_of_station, 
                               levels = c(1,2), 
                               labels = c("Industrial", "Urban"))

data <- data %>%
  mutate(
    ds = as.Date(ID_Date),
    y  = `PM2.5`,
    Type_of_station = as.factor(Type_of_station)
  )

data_dummy <- data %>%
  pivot_wider(
    names_from = Type_of_station,
    values_from = Type_of_station,
    values_fn = length,
    values_fill = 0
  )

# supprimer tirets éventuels
names(data_dummy) <- gsub("-", "", names(data_dummy))


covars <- setdiff(names(data_dummy), c("ds", "y", "ID_Date","PM2.5"))

data_prophet <- data_dummy %>%
  select(ds, y, all_of(covars))

m <- prophet(
  mcmc.samples = 300,      # génère intervalles crédibles pour les betas
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE
)

# Ajouter automatiquement toutes les covariables
for (cv in covars) {
  m <- add_regressor(m, cv)
}

m <- fit.prophet(m, data_prophet)


future <- make_future_dataframe(m, periods = 3652)

# joindre covariables
future <- left_join(future, data_prophet, by = "ds")

# remplir valeurs futures avec dernières observées
last_row <- data_prophet %>% tail(1)

for (cv in covars) {
  future[[cv]][is.na(future[[cv]])] <- last_row[[cv]]
}

forecast <- predict(m, future)


results <- forecast %>%
  select(ds, yhat) %>%
  left_join(data_prophet %>% select(ds, y), by = "ds")

# Historique seulement
results_hist <- results %>%
  filter(ds <= max(data_prophet$ds))

RMSE <- rmse(results_hist$y, results_hist$yhat)
MAE  <- mae(results_hist$y, results_hist$yhat)

cat("RMSE =", RMSE, "\n")
cat("MAE  =", MAE, "\n")


betas_matrix <- m$params$beta
colnames(betas_matrix) <- names(m$extra_regressors)

beta_mean  <- apply(betas_matrix, 2, mean)
beta_lower <- apply(betas_matrix, 2, quantile, 0.025)
beta_upper <- apply(betas_matrix, 2, quantile, 0.975)

betas_table <- data.frame(
  Covariate = covars,
  Beta_Estimate = beta_mean,
  Lower_95 = beta_lower,
  Upper_95 = beta_upper
)

print(betas_table)


############################################
#     9. EXPORT RESULTATS (OPTION)
############################################
write.csv(betas_table, "betas_PM25.csv", row.names = FALSE)
write.csv(results_hist, "forecast_PM25_history.csv", row.names = FALSE)

############################################
#     10. GRAPHIQUES
############################################
plot(m, forecast)
prophet_plot_components(m, forecast)
