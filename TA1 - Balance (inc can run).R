## REPLICATION 
## BALANCE SURVEY SAMPLE WHERE INC CAN RUN

# Charger les bibliothèques nécessaires
library(foreign)
library(dplyr)
library(stargazer)
library(haven)

# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données selon les conditions spécifiées
filtered_data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1 & INC05_can_run == 1 & SAMPLE_hhsurvey == 1)

# Afficher la table de fréquence pour RES05_gender et INT_treatment
table_data <- table(filtered_data$RES05_gender, filtered_data$INT_treatment)
print(table_data)

# Variables globales
outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")

# Liste des variables dépendantes
dep_vars <- c("GP_nbvillages", "GP_population", "GP_sc", "GP_st", "GP_lit", "GP_cult", "GP_aglb",
              "CENSUS_VD2011_drnk_wat_f", "CENSUS_VD2011_app_pr", "CENSUS_VD2011_power_dom",
              "ELEC10_electorate_total", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
              "RES05_obc", "RES05_sc", "RES05_st", "RES10_obc", "RES10_sc", "RES10_st", "INC05_can_run")

# Initialiser une liste pour stocker les modèles
models <- list()

# Exécuter les régressions pour chaque variable dépendante
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Calculer la moyenne et l'écart-type pour le groupe de contrôle
  control_data <- filtered_data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    pull(.data[[dep_var]])
  
  control_mean <- mean(control_data, na.rm = TRUE)
  control_sd <- sd(control_data, na.rm = TRUE)
  
  # Exécuter la régression
  formula <- as.formula(paste(dep_var, "~", paste(outregvar0, collapse = " + ")))
  models[[i]] <- lm(formula, data = filtered_data)
}

# Générer le tableau de résultats
stargazer(models, type = "text", title = "TA1c - Balance test (survey Sample incumbent can run)",
          notes = c("Mean in Control in Women Unreserved 2005", control_mean,
                    "S.D. in Control in Women Unreserved 2005", control_sd),
          out = "~/work/Rajasthan-Voters-Replication/TA1c-replication-inc-can-run.txt")
