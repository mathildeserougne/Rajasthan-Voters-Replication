# tentative de nettoyage du fwer pour table 1, en enlevant le code pas utilisé

# Family-wise error rate method on Table 1
# Install and load libraries
#install.packages(c("tidyverse", "stargazer", "knitr", "broom", "haven", "fixest",
#                   "modelsummary", "gt", "webshot2", "car", "multcomp"))

library(tidyverse)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(car)        # For FWER corrections
library(multcomp)

# Define control and dependent variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Load and filter data
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Function to create regression formulas
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

# Function to extract p-values and apply FWER correction
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

# Estimate models
models_list <- list()
control_means <- list()
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
  models_list[[i]] <- model
}
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  j <- i + length(incum_dep_vars1)
  control_means[[j]] <- control_means[[i]]
  formula <- create_formula(dep_var, "gender_general")
  model <- lm(formula, data = data_filtered)
  models_list[[j]] <- model
}

# Define variables and panels for output
outregvar2 <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                "INT_treatment:RES05_gender", "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
panel_B_vars <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                  "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")
panel_A_models <- models_list[1:6]
panel_B_models <- models_list[7:12]

# Apply FWER correction for each panel
pvalues_panel_A <- get_adjusted_pvalues(panel_A_models, panel_A_vars)
pvalues_panel_B <- get_adjusted_pvalues(panel_B_models, panel_B_vars)

# Function to extract adjusted p-values by variable
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

# Print results to console
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

# Print panels
print_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, "PANEL A: Average Effects")
print_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, "PANEL B: Effects by Type of Campaign")

