# replication tableA0



# Charger les bibliothèques nécessaires
library(foreign)
library(dplyr)
library(stargazer)
library(haven)

# Définir les chemins et les variables globales
path <- "~/work"
Data <- file.path(path, "Electoral data cleaned.dta")

# Chemin de sortie explicite
output_file_path <- file.path("~/work/Rajasthan-Voters-Replication", "TA0-replication.txt")

# Créer le répertoire de résultats s'il n'existe pas
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")
outregvar1 <- c("RES05_gender", "INT_treatment")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar3 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender")
outregvar4 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender", "X_generaltr_genderres05", "X_gendertr_genderres05")
outregvar5 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05", "INC05_can_run", "X_anytr_inc_can", "X_inc_can_genderres05", "X_anytr_inc_can_genderres05")

# Fonction pour effectuer les régressions et générer les tableaux
perform_regressions <- function(data, table_name, output_file_path) {
  data <- data %>%
    filter(RES10_gender == 0 & GP_tag == 1)
  
  summary_stats <- data %>%
    group_by(INT_treatment) %>%
    summarise(mean = mean(RES10_obc, na.rm = TRUE),
              sd = sd(RES10_obc, na.rm = TRUE))
  
  control_mean <- summary_stats$mean[summary_stats$INT_treatment == 0]
  control_sd <- summary_stats$sd[summary_stats$INT_treatment == 0]
  
  models <- list()
  i <- 1
  dep_vars <- c("GP_nbvillages", "GP_population", "GP_sc", "GP_st", "GP_lit", "GP_cult", "GP_aglb",
                "CENSUS_VD2011_drnk_wat_f", "CENSUS_VD2011_app_pr", "CENSUS_VD2011_power_dom",
                "ELEC10_electorate_total", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES05_obc", "RES05_sc", "RES05_st", "RES10_obc", "RES10_sc", "RES10_st", "INC05_can_run")
  
  for (dep_var in dep_vars) {
    formula <- as.formula(paste(dep_var, "~", paste(outregvar0, collapse = " + ")))
    models[[i]] <- lm(formula, data = data)
    i <- i + 1
  }
  
  # Écrire le fichier de sortie
  stargazer(models, type = "text", out = output_file_path,
            title = table_name, notes = c("Mean in Control in Women Unreserved 2005", control_mean,
                                          "S.D. in Control in Women Unreserved 2005", control_sd))
}

# Charger les données et exécuter les régressions pour chaque tableau
data <- read_dta(Data)
perform_regressions(data, "TA0a - Balance test (whole sample)", output_file_path)
