library(tidyverse)
library(scales)
library(lubridate)

file_path <- "PM2.5_PREDICTS.xlsx - Feuil1.csv"
df <- file_path

df <- df %>%
  mutate(Date = ymd(Date))

# Définir les colonnes des modèles
prediction_cols <- c("PM2.5_PROPHET", "PM2.5_PROPHET_COV", "PM2.5_SARIMA", "PM2.5_XGBOOST")

# Créer un dataframe "long" pour ggplot2
df_long <- df %>%
  pivot_longer(
    cols = all_of(prediction_cols),
    names_to = "Model",
    values_to = "PM2.5_Concentration"
  )

# --- 2. DÉFINITION DE L'ÉCHELLE COMMUNE ---

# Trouver les limites maximales et minimales de l'axe Y sur l'ensemble des données
# On ajoute une petite marge (5%) pour la lisibilité
y_max_global <- max(df_long$PM2.5_Concentration, na.rm = TRUE) * 1.05
y_min_global <- 0 # On s'assure que le minimum est 0 pour la concentration

# --- 3. GENERATION DES GRAPHIQUES SÉPARÉS AVEC ÉCHELLE COMMUNE ---

# Définir les étiquettes en anglais
english_labels <- c(
  "PM2.5_PROPHET" = "PROPHET Model",
  "PM2.5_PROPHET_COV" = "PROPHET with Covariates Model",
  "PM2.5_SARIMA" = "SARIMA Model",
  "PM2.5_XGBOOST" = "XGBOOST Model"
)

# Fonction pour générer et enregistrer chaque graphique
generate_plot <- function(data, model_name) {
  
  # Filtrer les données pour le modèle actuel
  df_model <- data %>%
    filter(Model == model_name)
  
  # Titre du graphique en anglais
  plot_title <- paste0("PM2.5 Predictions: ", english_labels[model_name], " (2010-2030)")
  
  # Génération du graphique
  p <- ggplot(df_model, aes(x = Date, y = PM2.5_Concentration)) +
    geom_line(aes(color = Model), linewidth = 0.8) +
    
    # Définition de l'échelle Y commune et des étiquettes d'axes en anglais
    scale_y_continuous(limits = c(y_min_global, y_max_global), name = "PM2.5 Concentration") +
    scale_x_date(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
    
    # Titre
    labs(title = plot_title) +
    
    # Personnalisation (Suppression du quadrillage et thème clair)
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      panel.grid.major = element_blank(), # Supprime le quadrillage majeur
      panel.grid.minor = element_blank(), # Supprime le quadrillage mineur
      legend.position = "none" # La légende n'est pas nécessaire pour les graphiques séparés
    )
  
  # Enregistrement du graphique
  file_name <- paste0("PM25_Predictions_", gsub(" ", "_", english_labels[model_name]), ".png")
  ggsave(file_name, plot = p, width = 12, height = 6, units = "in")
  
  # Affichage du graphique (en environnement RStudio ou notebook)
  print(p)
  
  return(file_name)
}

# Générer les graphiques pour chaque modèle
generated_files <- sapply(prediction_cols, function(col) generate_plot(df_long, col))

print("Les graphiques suivants ont été générés et sauvegardés dans votre répertoire de travail R :")
print(generated_files)

# Affichage des 4 graphiques (si vous êtes dans un environnement qui supporte cela)
# Note : Dans cet environnement, je ne peux afficher que les images générées précédemment, 
# mais le code R ci-dessus vous permet de les reproduire.