## TABLE 1 - FINAL 
## pure effect, separate outcome, fwer adjusted p values included.

library(tidyverse)
library(fixest)
library(modelsummary)
library(broom)


# DATA
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    RES05_gender = as.factor(RES05_gender)
  )

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Variables dépendantes
running_dep_vars <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
voteshare_dep_vars <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")

# Sous-échantillons
data_whole <- data_filtered
data_RES05_gender_0 <- data_filtered %>% filter(RES05_gender == 0)
data_RES05_gender_1 <- data_filtered %>% filter(RES05_gender == 1)

# Formule simplifiée (toujours sans interaction)
create_formula <- function(dep_var) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  as.formula(paste(dep_var, "~ INT_treatment +", base_controls, "+ factor(district)"))
}

# Extraction des p-values
get_pvalues <- function(model, var_names) {
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients
  pvals <- coef_table[var_names, "Pr(>|t|)"]
  return(as.numeric(pvals))
}

# Ajustement FWER
calculate_fwer <- function(models, var_names) {
  all_pvalues <- unlist(lapply(models, function(model) {
    if (!is.null(model)) get_pvalues(model, var_names) else numeric(0)
  }))
  p.adjust(all_pvalues, method = "holm")
}

# Extraction des résultats
get_model_results <- function(model, var_names, var_labels, p_adjusted, offset = 0) {
  if (is.null(model)) return(NULL)
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients[var_names, , drop = FALSE]
  if (nrow(coef_table) == 0) return(NULL)
  data.frame(
    Variable = var_labels[rownames(coef_table)],
    Estimate = coef_table[, "Estimate"],
    P.value = coef_table[, "Pr(>|t|)"],
    FWER = p_adjusted[(1:nrow(coef_table)) + offset]
  )
}

# Estimation des modèles et extraction des résultats
var_names <- c("INT_treatment")  # Seule variable d'intérêt
var_labels <- c("INT_treatment" = "Treatment")

# Running - Tous échantillons
running_models_whole <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_whole))
running_models_gender0 <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_0))
running_models_gender1 <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_1))

# Vote Share - Tous échantillons
voteshare_models_whole <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_whole))
voteshare_models_gender0 <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_0))
voteshare_models_gender1 <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_1))

# Calcul des p-values ajustées
pvalues_running_whole <- get_pvalues(running_models_whole[[1]], var_names)
adjusted_pvalues_running_whole <- calculate_fwer(running_models_whole, var_names)
pvalues_running_gender0 <- get_pvalues(running_models_gender0[[1]], var_names)
adjusted_pvalues_running_gender0 <- calculate_fwer(running_models_gender0, var_names)
pvalues_running_gender1 <- get_pvalues(running_models_gender1[[1]], var_names)
adjusted_pvalues_running_gender1 <- calculate_fwer(running_models_gender1, var_names)

pvalues_voteshare_whole <- get_pvalues(voteshare_models_whole[[1]], var_names)
adjusted_pvalues_voteshare_whole <- calculate_fwer(voteshare_models_whole, var_names)
pvalues_voteshare_gender0 <- get_pvalues(voteshare_models_gender0[[1]], var_names)
adjusted_pvalues_voteshare_gender0 <- calculate_fwer(voteshare_models_gender0, var_names)
pvalues_voteshare_gender1 <- get_pvalues(voteshare_models_gender1[[1]], var_names)
adjusted_pvalues_voteshare_gender1 <- calculate_fwer(voteshare_models_gender1, var_names)

# Résultats
running_results_whole <- lapply(1:3, function(i) get_model_results(running_models_whole[[i]], var_names, var_labels, adjusted_pvalues_running_whole, (i-1)))
running_results_gender0 <- lapply(1:3, function(i) get_model_results(running_models_gender0[[i]], var_names, var_labels, adjusted_pvalues_running_gender0, (i-1)))
running_results_gender1 <- lapply(1:3, function(i) get_model_results(running_models_gender1[[i]], var_names, var_labels, adjusted_pvalues_running_gender1, (i-1)))

voteshare_results_whole <- lapply(1:3, function(i) get_model_results(voteshare_models_whole[[i]], var_names, var_labels, adjusted_pvalues_voteshare_whole, (i-1)))
voteshare_results_gender0 <- lapply(1:3, function(i) get_model_results(voteshare_models_gender0[[i]], var_names, var_labels, adjusted_pvalues_voteshare_gender0, (i-1)))
voteshare_results_gender1 <- lapply(1:3, function(i) get_model_results(voteshare_models_gender1[[i]], var_names, var_labels, adjusted_pvalues_voteshare_gender1, (i-1)))

# Labels pour les variables dépendantes
running_dep_var_labels <- c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs")
voteshare_dep_var_labels <- c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share")

# Affichage des résultats
print_results <- function(results_list, dep_var_labels, sample_name, panel_name) {
  cat("\n\n======================================================================\n")
  cat(panel_name, " (", sample_name, ")\n")
  cat("======================================================================\n")
  for (i in 1:3) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      cat("\n--- ", dep_var_labels[i], " ---\n")
      cat(sprintf("%-25s %-15s %-15s %-15s\n", "Variable", "Estimate", "p-value", "FWER-adj p"))
      cat("----------------------------------------------------------------------\n")
      cat(sprintf("%-25s %-15.4f %-15s %-15s\n",
                  result$Variable, result$Estimate,
                  format.pval(result$P.value, digits = 3),
                  format.pval(result$FWER, digits = 3)))
    }
  }
}

# Affichage
print_results(running_results_whole, running_dep_var_labels, "Whole Sample", "Treatment Effect on Running")
print_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Running")
print_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Running")

print_results(voteshare_results_whole, voteshare_dep_var_labels, "Whole Sample", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Vote Share")




# OUTPUT #


