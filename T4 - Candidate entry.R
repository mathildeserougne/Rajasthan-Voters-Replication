## T4 - candidate ENTRY

# Charger les bibliothèques nécessaires
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(stargazer)
library(writexl)

# Définir les chemins
path <- "~/work"

# Définir les variables globales
gpcontrols15 <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st", "RES15_obc", "RES15_sc", "RES15_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Charger les données
data <- read_dta(file.path(path, "Electoral data 2015 cleaned.dta"))

# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == TRUE & RES15_gender == 0)

# Créer de nouvelles variables
data <- data %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run),
    X15_INT_treatment = INT_treatment * (RES15_gender == 1),
    X15_RES05_gender = RES05_gender * (RES15_gender == 1),
    X15_X_anytr_genderres05 = X_anytr_genderres05 * (RES15_gender == 1)
  )

# Définir les variables dépendantes
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")



# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == TRUE & RES15_gender == 0)

# Créer de nouvelles variables
data <- data %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run),
    X15_INT_treatment = INT_treatment * (RES15_gender == 1),
    X15_RES05_gender = RES05_gender * (RES15_gender == 1),
    X15_X_anytr_genderres05 = X_anytr_genderres05 * (RES15_gender == 1)
  )

# Définir les variables dépendantes
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")

# Effectuer les régressions
models <- list()
results <- list()

for (dep_var in dep_vars) {
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols15, "district"), collapse = " + ")))
  model <- lm(formula, data = data)
  models[[dep_var]] <- model
  summary_model <- tidy(model)
  results[[dep_var]] <- summary_model
  
  # Calculer la moyenne de contrôle et le test
  control_mean <- round(mean(data[[dep_var]][data$INT_treatment == 0 & data$RES05_gender == 0], na.rm = TRUE), 2)
  test_result <- sum(coeftest(model, vcov. = vcovHC(model, type = "HC1"))["RES05_gender", "Pr(>|t|)"],
                     coeftest(model, vcov. = vcovHC(model, type = "HC1"))["X_anytr_genderres05", "Pr(>|t|)"])
  
  # Ajouter les résultats au tableau
  stargazer_result <- stargazer(model, type = "text", title = dep_var,
                                notes = c(paste("Mean in Control not WR in 2015:", control_mean),
                                          paste("Test Treat Effect in WR=Treat Effect in NWR p-value:", round(test_result, 2))))
  print(stargazer_result)
}



## stargazer




# Effectuer les régressions
models <- list()
results <- list()

for (dep_var in dep_vars) {
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols15, "district"), collapse = " + ")))
  model <- lm(formula, data = data)
  models[[dep_var]] <- model
  summary_model <- tidy(model)
  results[[dep_var]] <- summary_model
}

# Exporter les résultats en Excel
write_xlsx(results, file.path(Results, "T4 - Candidates - 2015.xlsx"))

# Exporter les résultats en format texte avec stargazer
stargazer(models, type = "text", out = file.path(path, "T4 - Candidates - 2015.txt"))
