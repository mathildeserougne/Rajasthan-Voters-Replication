### TA3 REPLICATION - summary statistics where 2010 inc could run ###########

# libraries
# uncomment the packages before if necessary
#install.packages(c("dplyr","readr","lmtest","broom","writexl","stargazer"))
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(writexl)
library(stargazer)

# paths
path <- "~/work"
Data <- file.path(path, "Data")
Results <- file.path(path,"Rajasthan-Voters-Replication", "Results")


# Définir les variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Charger les données
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == TRUE & INC05_can_run == 1)

# Créer de nouvelles variables
data <- data %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    INC05_voteshare_cond = ifelse(INC05_running == 1, INC05_voteshare, NA),
    INC05_won_cond = ifelse(INC05_running == 1, INC05_won, NA)
  )

# Définir les variables dépendantes
incum_dep_vars1 <- c("INC05_won", "INCSPOUSE05_won", "INCOTHER05_won", "INC05_voteshare_cond", "INC05_won_cond")

# Effectuer les régressions
results <- list()
for (dep_var in incum_dep_vars1) {
  # Construire la formule correctement
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols, "district"), collapse = " + ")))
  model <- lm(formula, data = data)
  summary_model <- tidy(model)
  results[[dep_var]] <- summary_model
}

# Exporter les résultats
write_xlsx(results, file.path(path, "TA3a - Incumbent - Conditional.xlsx"))



## with stargazer

# Charger les bibliothèques nécessaires
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(writexl)
library(stargazer)


# Définir les variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Charger les données
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == TRUE & INC05_can_run == 1)

# Créer de nouvelles variables
data <- data %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    INC05_voteshare_cond = ifelse(INC05_running == 1, INC05_voteshare, NA),
    INC05_won_cond = ifelse(INC05_running == 1, INC05_won, NA)
  )

# Définir les variables dépendantes
incum_dep_vars1 <- c("INC05_won", "INCSPOUSE05_won", "INCOTHER05_won", "INC05_voteshare_cond", "INC05_won_cond")

# Effectuer les régressions et stocker les modèles
models <- list()
for (dep_var in incum_dep_vars1) {
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols, "district"), collapse = " + ")))
  models[[dep_var]] <- lm(formula, data = data)
}

# Exporter les résultats en Excel
results <- lapply(models, tidy)
write_xlsx(results, file.path(Results, "TA3a - Incumbent - Conditional.xlsx"))

# Exporter les résultats en format Stargazer
stargazer(models, type = "text", out = file.path(path, "TA3a - Incumbent - Conditional.txt"))
