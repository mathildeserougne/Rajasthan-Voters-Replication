## REPLICATION TA2 - TURNOUT ###################################################
################################################################################


# Charger les bibliothèques nécessaires
library(dplyr)
library(haven)
library(broom)
library(lmtest)
library(sandwich)
library(writexl)

# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")

colnames(data)
head(data)



# Vérifiez les valeurs uniques dans les colonnes utilisées pour le filtrage
unique_res10_gender <- unique(data$RES10_gender)
unique_gp_tag <- unique(data$GP_tag)

# Afficher les valeurs uniques
cat("Valeurs uniques dans RES10_gender :", unique_res10_gender, "\n")
cat("Valeurs uniques dans GP_tag :", unique_gp_tag, "\n")

# Vérifiez le nombre d'observations avant le filtrage
cat("Nombre d'observations avant filtrage :", nrow(data), "\n")

# Filtrer les données sans supprimer les valeurs manquantes
filtered_data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1)

# Vérifiez le nombre d'observations après le filtrage
cat("Nombre d'observations après filtrage :", nrow(filtered_data), "\n")

# Afficher un résumé des données filtrées
summary(filtered_data)

# Si vous avez des observations, vous pouvez continuer avec les régressions
if (nrow(filtered_data) > 0) {
  # Définir les variables globales
  gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
  outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
  
  # Définir les variables dépendantes
  dep_vars <- c("ELEC10_turnoutrate_total", "ELEC10_turnoutrate_male", "ELEC10_turnoutrate_female")
  
  # Créer une liste pour stocker les résultats
  results <- list()
  
  # Effectuer les régressions pour chaque variable dépendante
  for (dep_var in dep_vars) {
    # Calculer les moyennes des contrôles
    control_mean1 <- mean(filtered_data$`dep_var`[filtered_data$INT_treatment == 0 & filtered_data$RES05_gender == 0], na.rm = TRUE)
    control_mean2 <- mean(filtered_data$`dep_var`[filtered_data$INT_treatment == 0 & filtered_data$RES05_gender == 1], na.rm = TRUE)
    
    # Effectuer la régression
    formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols), collapse = " + ")))
    model <- lm(formula, data = filtered_data)
    
    # Stocker les résultats
    results[[dep_var]] <- list(
      model_summary = summary(model),
      control_mean1 = control_mean1,
      control_mean2 = control_mean2
    )
  }
  
  # Afficher les résultats
  print(results)
} else {
  cat("Aucune observation valide après le filtrage.")
}





