# FWER for Table 1, this time with new panels.

# We keep the same Panel A (whole sample).
# We add a Panel B: without gender quota in 2005.
# We add a Panel C: with gender quota in 2005.

library(tidyverse)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(car)
library(multcomp)

# Variables de contrôle et dépendantes
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Chargement et filtrage des données
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Fonction pour créer les formules de régression
create_formula <- function(dep_var, model_type) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  if (model_type == "any_treatment") {
    formula_str <- paste(dep_var, "~ INT_treatment + RES05_gender + INT_treatment:RES05_gender +",
                         base_controls, "+ factor(district)")
  } else if (model_type == "gender_general") {
    formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender +",
                         base_controls, "+ factor(district)")
  }
  return(as.formula(formula_str))
}

# Fonction pour extraire les p-values et appliquer la correction FWER
get_adjusted_pvalues <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    coef_table <- summary(model)$coefficients
    pvals <- coef_table[var_names, "Pr(>|t|)", drop = FALSE]
    all_pvalues <- c(all_pvalues, as.numeric(pvals))
  }
  adjusted_pvalues <- p.adjust(all_pvalues, method = "holm")
  return(data.frame(raw = all_pvalues, adjusted = adjusted_pvalues))
}

# Estimation des modèles pour chaque panel
models_list <- list()
control_means <- list()

# Panel A : effets moyens (toutes les observations)
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means[[i]] <- control_mean
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_filtered)
  models_list[[paste0("A_", i)]] <- model
}

# Panel B : sans quota en 2005 (RES05_gender == 0)
data_panel_B <- data_filtered %>% filter(RES05_gender == 0)
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  control_mean <- data_panel_B %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means[[i + length(incum_dep_vars1)]] <- control_mean
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_panel_B)
  models_list[[paste0("B_", i)]] <- model
}

# Panel C : avec quota en 2005 (RES05_gender == 1)
data_panel_C <- data_filtered %>% filter(RES05_gender == 1)
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  control_mean <- data_panel_C %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means[[i + 2 * length(incum_dep_vars1)]] <- control_mean
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_panel_C)
  models_list[[paste0("C_", i)]] <- model
}

# Variables pour chaque panel 
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
panel_B_vars <- c("INT_treatment")  # RES05_gender est constante = 0, donc pas estimée
panel_C_vars <- c("INT_treatment")  # RES05_gender est constante = 1, donc pas estimée



# Extraction des modèles par panel
panel_A_models <- models_list[1:6]
panel_B_models <- models_list[7:12]
panel_C_models <- models_list[13:18]

# Application des corrections FWER
pvalues_panel_A <- get_adjusted_pvalues(panel_A_models, panel_A_vars)
pvalues_panel_B <- get_adjusted_pvalues(panel_B_models, panel_B_vars)
pvalues_panel_C <- get_adjusted_pvalues(panel_C_models, panel_C_vars)

# Fonction pour extraire les p-values ajustées par variable
get_pvals_by_var <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

pvals_A_by_var <- get_pvals_by_var(pvalues_panel_A, panel_A_vars, length(panel_A_models))
pvals_B_by_var <- get_pvals_by_var(pvalues_panel_B, panel_B_vars, length(panel_B_models))
pvals_C_by_var <- get_pvals_by_var(pvalues_panel_C, panel_C_vars, length(panel_C_models))

# Fonction pour afficher les résultats
print_panel_results <- function(models, var_names, pvalues_by_var, panel_name) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    dep_var_name <- col_names[i]
    cat("--- ", dep_var_name, " ---\n")
    cat("Variable          | Coeff (Std. Error) | p-value | FWER-adj p\n")
    cat("-------------------|--------------------|---------|------------\n")
    for (var in var_names) {
      if (var %in% rownames(coef_table)) {
        coef_val <- round(coef_table[var, "Estimate"], 4)
        se_val <- round(coef_table[var, "Std. Error"], 4)
        pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
        pval_adj <- round(pvalues_by_var[[var]][i], 4)
        cat(sprintf("%-18s | %4.3f (%4.3f)       | %5.4f   | %6.4f\n",
                    var, coef_val, se_val, pval_raw, pval_adj))
      }
    }
    cat("\n")
  }
}

# Noms des colonnes pour l'affichage
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# Affichage des résultats
print_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, "PANEL A: Average Effects")
print_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, "PANEL B: No Quota in 2005")
print_panel_results(panel_C_models, panel_C_vars, pvals_C_by_var, "PANEL C: Quota in 2005")
