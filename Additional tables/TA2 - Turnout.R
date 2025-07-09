## REPLICATION TA2 - TURNOUT ###################################################
################################################################################


# nul nul nul



# Required libraries
# Uncomment packages installation if necessary
# install.packages(c("dplyr","haven","stargazer"))
library(haven)
library(dplyr)
library(stargazer)

# Définir les chemins et les variables globales
path <- "~/work"
data_path <- file.path(path, "Electoral data cleaned.dta")
output_file_path <- file.path("~/work/Rajasthan-Voters-Replication", "TA2 - Turnout.txt")



# Créer le répertoire de résultats s'il n'existe pas
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Charger les données
data <- read_dta(data_path) %>%
  filter(RES10_gender == 0 & GP_tag == 1)

# Variables dépendantes
dep_vars <- c("ELEC10_turnoutrate_total", "ELEC10_turnoutrate_male", "ELEC10_turnoutrate_female")

# Fonction pour effectuer les régressions et générer les tableaux
perform_regressions <- function(data, dep_vars, output_file_path) {
  models <- list()
  i <- 1
  
  for (dep_var in dep_vars) {
    # Filtrer les données pour exclure les NA
    filtered_data <- na.omit(data[, c(dep_var, outregvar2, gpcontrols, "district")])
    
    # Effectuer la régression
    formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols), collapse = " + "), "+ factor(district)"))
    model <- lm(formula, data = filtered_data)
    
    # Stocker le modèle
    models[[i]] <- model
    i <- i + 1
  }
  
  # Écrire le fichier de sortie avec stargazer
  stargazer(models, type = "text", out = output_file_path,
            title = "TA2 - Turnout",
            dep.var.labels = c("Total Turnout Rate", "Male Turnout Rate", "Female Turnout Rate"),
            covariate.labels = c(gpcontrols, "District Fixed Effects"),
            omit = c(gpcontrols, "\\(Intercept\\)"))
}

# Exécuter les régressions et générer les tableaux
perform_regressions(data, dep_vars, output_file_path)




############ ça ne marche pas donc tentative de résolution avant abandon


colnames(data)
head(data)

is.na(data)



### tentative sans stargazer, pour au moins vérifier que la rég marche normalement


# Charger les bibliothèques nécessaires
library(haven)
library(dplyr)
library(lmtest)

# Définir les chemins et les variables globales
path <- "~/work"
data_path <- file.path(path, "Electoral data cleaned.dta")

# Charger les données
data <- read_dta(data_path) %>%
  filter(RES10_gender == 0 & GP_tag == 1)

# Variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Variables dépendantes
dep_vars <- c("ELEC10_turnoutrate_total", "ELEC10_turnoutrate_male", "ELEC10_turnoutrate_female")

# Fonction pour effectuer les régressions
perform_regressions <- function(data, dep_vars) {
  results <- list()
  
  for (dep_var in dep_vars) {
    # Calculer les moyennes de contrôle
    control_mean1 <- mean(data[[dep_var]][data$INT_treatment == 0 & data$RES05_gender == 0], na.rm = TRUE)
    control_mean2 <- mean(data[[dep_var]][data$INT_treatment == 0 & data$RES05_gender == 1], na.rm = TRUE)
    
    # Effectuer la régression
    formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols), collapse = " + "), "+ factor(district)"))
    model <- lm(formula, data = data)
    
    # Effectuer le test d'hypothèse
    p_val <- round(coeftest(model, vcov. = sandwich, df = Inf, test = "RES05_gender + X_anytr_genderres05 = 0")[2, 4], 2)
    
    # Stocker les résultats
    results[[dep_var]] <- list(model = model, control_mean1 = control_mean1, p_value = p_val)
    
    # Afficher les résultats
    cat("\nResults for", dep_var, ":\n")
    print(summary(model))
    cat("Mean in Control not WR in 2015:", control_mean1, "\n")
    cat("Test Treat Effect in WR=0 p-value:", p_val, "\n")
  }
  
  return(results)
}

# Exécuter les régressions
regression_results <- perform_regressions(data, dep_vars)








