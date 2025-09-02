# fwer for table 4
# family-wise error rate method, following table 4's original sample splitting

# FWER pour Table 4 - Correction uniquement sur les modèles sélectionnés
# Script for Table 4: Candidate Entry in the Next Election with FWER correction

# Charger les bibliothèques nécessaires
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(car)
library(multcomp)

# Définir les variables de contrôle
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# Charger les données
data <- read_dta("~/work/Electoral data 2015 cleaned.dta")

# Filtrer les données
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1, RES15_gender == 0) %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run)
  )

# Générer de nouvelles variables
for (var in c("INT_treatment", "X_anytr_genderres05", "RES05_gender")) {
  data_filtered <- data_filtered %>%
    mutate(!!paste0("X15_", var) := get(var) * (RES15_gender == 1))
}

# Variables de régression d'intérêt
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Variables dépendantes
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")

# Estimation des modèles pour le panel "Whole Sample"
models_list <- list()
control_means <- numeric(length(dep_vars))

for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Moyenne de contrôle
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # Estimation du modèle
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols15), collapse = " + "), "+ factor(district)"))
  model <- tryCatch({
    lm(formula, data = data_filtered)
  }, error = function(e) {
    message("Error in model fitting: ", e$message)
    NULL
  })
  
  models_list[[i]] <- model
}

# Fonction pour extraire les p-values et appliquer la correction FWER
get_adjusted_pvalues <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      pvals <- coef_table[var_names, "Pr(>|t|)", drop = FALSE]
      all_pvalues <- c(all_pvalues, as.numeric(pvals))
    }
  }
  adjusted_pvalues <- p.adjust(all_pvalues, method = "holm")
  return(data.frame(raw = all_pvalues, adjusted = adjusted_pvalues))
}

# Appliquer la correction FWER uniquement sur le panel "Whole Sample"
pvalues_adjusted <- get_adjusted_pvalues(models_list, outregvar2)

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

# Extraire les p-values ajustées pour chaque variable d'intérêt
pvals_by_var <- get_pvals_by_var(pvalues_adjusted, outregvar2, length(dep_vars))

# Fonction pour imprimer les résultats avec les p-values ajustées par FWER
print_results <- function(models, pvals_by_var, control_means, dep_vars) {
  cat("\n\n======================================================================\n")
  cat("Whole Sample (FWER-adjusted)\n")
  cat("======================================================================\n\n")
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[i]
      
      cat("--- Model ", i, ": ", dep_var_name, " ---\n", sep = "")
      cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
      cat("-----------------------------|--------------------|---------|------------\n")
      
      for (var in outregvar2) {
        if (var %in% rownames(coef_table)) {
          coef_val <- round(coef_table[var, "Estimate"], 4)
          se_val <- round(coef_table[var, "Std. Error"], 4)
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
          pval_adj <- round(pvals_by_var[[var]][i], 4)
          
          # Ajouter des étoiles de significativité
          stars <- ifelse(pval_adj < 0.01, "***",
                          ifelse(pval_adj < 0.05, "**",
                                 ifelse(pval_adj < 0.1, "*", "")))
          
          cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f   | %6.4f%s\n",
                      var, coef_val, se_val, pval_raw, pval_adj, stars))
        }
      }
      cat("Mean in Control not WR in 2015: ", control_means[i], "\n")
      cat("Observations: ", nobs(model), "\n\n")
    }
  }
}

# Imprimer les résultats pour le panel "Whole Sample"
print_results(models_list, pvals_by_var, control_means, dep_vars)




