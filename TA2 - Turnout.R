## REPLICATION TA2 - TURNOUT

# Charger les bibliothèques nécessaires

install.packages("lmtest")
install.packages("sandwich")
library(foreign)
library(dplyr)
library(stargazer)
library(haven)
library(lmtest)
library(sandwich)

# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données selon les conditions spécifiées
filtered_data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1)

# Variables globales
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
dep_vars <- c("ELEC10_turnoutrate_total", "ELEC10_turnoutrate_male", "ELEC10_turnoutrate_female")

# Initialiser une liste pour stocker les modèles
models <- list()

# Exécuter les régressions pour chaque variable dépendante
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Calculer la moyenne pour le groupe de contrôle
  control_mean1 <- filtered_data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    pull(.data[[dep_var]]) %>%
    mean(na.rm = TRUE)
  
  # Exécuter la régression
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols), collapse = " + "), "+ factor(district)"))
  models[[i]] <- lm(formula, data = filtered_data)
  
  # Effectuer un test d'hypothèse sur les coefficients de régression
  test_result <- coeftest(models[[i]], vcov. = vcovHC, df = Inf)
  pval <- round(coeftest(models[[i]], vcov. = vcovHC, df = Inf)[3, 4], 2)
  
  # Afficher les résultats
  cat("Mean in Control not WR in 2015 for", dep_var, ":", control_mean1, "\n")
  cat("Test Treat Effect in WR=0 p-value:", pval, "\n")
}

# Générer le tableau de résultats
stargazer(models, type = "text", title = "TA2 - Turnout",
          notes = c("District FE: Yes", "GP controls: Yes"),
          out = "~/work/Rajasthan-Voters-Replication/TA2-replication.txt",
          omit = gpcontrols, omit.labels = gpcontrols,
          dep.var.labels = c("Total Turnout", "Male Turnout", "Female Turnout"),
          column.labels = c("Total Turnout", "Male Turnout", "Female Turnout"))
