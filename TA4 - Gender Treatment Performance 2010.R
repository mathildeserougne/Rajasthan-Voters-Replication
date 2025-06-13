## REPLICATION: TA4 - Gender Treatment - Performance - 2010 ###################

# Libraries
# Install packages if necessary
# install.packages(c("dplyr","readr","lmtest","broom","writexl","stargazer"))
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(writexl)
library(stargazer)

# Change path if necessary
path <- "~/work"


# Macros pour les contrôles et les variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

outregvar <- c("INT_treatment_gender", "INT_treatment_general",
               "TEMP_X_gender_index", "TEMP_X_general_index", "TEMP_index")

# Charger les données
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == TRUE & INC05_can_run == 1)

# Créer de nouvelles variables
indices <- "index_empl_pre_svysample"
for (index in indices) {
  data <- data %>%
    mutate(
      TEMP_index = .data[[index]],
      TEMP_X_res_index = RES05_gender * .data[[index]],
      TEMP_X_gender_index = INT_treatment_gender * .data[[index]],
      TEMP_X_general_index = INT_treatment_general * .data[[index]],
      TEMP_X_gender_res_index = INT_treatment_gender * RES05_gender * .data[[index]],
      TEMP_X_general_res_index = INT_treatment_general * RES05_gender * .data[[index]]
    )
}

# Variables dépendantes
incum_dep_vars <- c("INC05_running", "INC05_voteshare", "INC05_won",
                    "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                    "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won")

# Régressions
models <- list()
i <- 0

for (x in c(0, 1)) {
  for (dep_var in incum_dep_vars) {
    i <- i + 1
    
    # Calculer la moyenne de contrôle
    control_mean1 <- data %>%
      filter(INT_treatment == 0 & RES05_gender == x) %>%
      summarise(mean = mean(.data[[dep_var]], na.rm = TRUE)) %>%
      pull(mean) %>%
      round(0.01)
    
    # Régression
    formula <- as.formula(paste(dep_var, "~", paste(c(outregvar, gpcontrols, "district"), collapse = " + ")))
    models[[paste("Model", i)]] <- lm(formula, data = filter(data, RES05_gender == x))
  }
}

# Exportation des résultats en Excel
results <- lapply(models, tidy)
write_xlsx(results, file.path(Results, "TA4 - Gender Treatment - Performance - 2010.xlsx"))

# Affichage des résultats avec stargazer
stargazer(models, type = "text", out = file.path("~/work/Rajasthan-Voters-Replication/TA4- Gender Treatment - Performance - 2010 - COMPLETE.txt"))




