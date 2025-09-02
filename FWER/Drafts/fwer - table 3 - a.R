# fwer for table 3 - following the original sample splitting

# FWER pour Table 3 - Correction uniquement sur les modèles sélectionnés
# Script for Table 3: Challenger Entry with FWER correction

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

# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1)

# Variables dépendantes
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# Estimation des modèles et stockage
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample
for (dep_var in dep_vars) {
  i <- i + 1
  
  # Moyenne de contrôle
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # Estimation du modèle
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered)
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Panel B: Incumbent Can Run
for (dep_var in dep_vars) {
  i <- i + 1
  
  # Moyenne de contrôle
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, INC05_can_run == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # Estimation du modèle
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(INC05_can_run == 1))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Panel C: Incumbent Cannot Run
for (dep_var in dep_vars) {
  i <- i + 1
  
  # Moyenne de contrôle
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, INC05_can_run == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # Estimation du modèle
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(INC05_can_run == 0))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Sélection des modèles spécifiques pour chaque panel
selected_models_A <- c(1, 2, 3, 4, 5, 6)  # Full Sample
selected_models_B <- c(7, 8, 9, 10, 11, 12)  # Incumbent Can Run
selected_models_C <- c(13, 14, 15, 16, 17, 18)  # Incumbent Cannot Run

panel_A_models_selected <- models_list[selected_models_A]
panel_B_models_selected <- models_list[selected_models_B]
panel_C_models_selected <- models_list[selected_models_C]

# Fonction pour extraire les p-values et appliquer la correction FWER
get_adjusted_pvalues_selected <- function(models, var_names) {
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

# Appliquer la correction FWER
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(panel_A_models_selected, outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(panel_B_models_selected, outregvar2)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(panel_C_models_selected, outregvar2)

# Fonction pour extraire les p-values ajustées par variable pour les modèles sélectionnés
get_pvals_by_var_selected <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extraire les p-values ajustées pour les modèles sélectionnés
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(panel_A_models_selected))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2, length(panel_B_models_selected))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2, length(panel_C_models_selected))

# Fonction pour imprimer les résultats avec les p-values ajustées par FWER pour les modèles sélectionnés
print_selected_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, dep_vars, selected_models) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[(model_index-1) %% length(dep_vars) + 1]
      
      cat("--- Model ", selected_models[i], ": ", dep_var_name, " ---\n", sep = "")
      cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
      cat("-----------------------------|--------------------|---------|------------\n")
      
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          coef_val <- round(coef_table[var, "Estimate"], 4)
          se_val <- round(coef_table[var, "Std. Error"], 4)
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
          pval_adj <- round(pvalues_by_var[[var]][i], 4)
          
          # Ajouter des étoiles de significativité
          stars <- ifelse(pval_adj < 0.01, "***",
                          ifelse(pval_adj < 0.05, "**",
                                 ifelse(pval_adj < 0.1, "*", "")))
          
          cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f   | %6.4f%s\n",
                      var, coef_val, se_val, pval_raw, pval_adj, stars))
        }
      }
      cat("Mean in Control not WR in 2005: ", control_means[model_index], "\n")
      cat("Observations: ", nobs(model), "\n")
    }
  }
}

# Imprimer les résultats pour les modèles sélectionnés de chaque panel
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: Full Sample",
                       control_means, dep_vars, selected_models_A)

print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Incumbent Can Run",
                       control_means, dep_vars, selected_models_B)

print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: Incumbent Cannot Run",
                       control_means, dep_vars, selected_models_C)



