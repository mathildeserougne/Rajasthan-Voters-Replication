# Attempting the family-wise error rate method on Table 1


# nettoyages à faire sur EST-CE QUE LE CALCULATE-tests intervient dans la sortie finale??

# Attempting the family-wise error rate method on Table 1

install.packages(c("tidyverse", "stargazer", "knitr", "broom", "haven", "fixest",
                   "modelsummary", "gt", "webshot2", "car", "multcomp"))

# Libraries
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
library(multcomp)  # Pour les corrections FWER

# Defining the control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Defining the dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Loading the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtering the data
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Function for the regression formulas
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

# Function for the statistical tests
# mais est-ce qu'elle intervient même dans la sortie finale???
#calculate_tests <- function(model, model_type) {
#  if (model_type == "any_treatment") {
#    test1 <- tryCatch({ car::linearHypothesis(model, "RES05_gender = 0") }, error = function(e) list(PrF = NA))
#    pval1 <- if (!is.null(test1$PrF)) round(test1$`Pr(>F)`[2], 2) else NA
#    test2 <- tryCatch({ car::linearHypothesis(model, "INT_treatment:RES05_gender = 0") }, error = function(e) list(PrF = NA))
#    pval2 <- if (!is.null(test2$PrF)) round(test2$`Pr(>F)`[2], 2) else NA
#    test3 <- tryCatch({ car::linearHypothesis(model, "INT_treatment = INT_treatment:RES05_gender") }, error = function(e) list(PrF = NA))
#    pval3 <- if (!is.null(test3$PrF)) round(test3$`Pr(>F)`[2], 2) else NA
#    return(list(pval1 = pval1, pval2 = pval2, pval3 = pval3))
#  } else if (model_type == "gender_general") {
#    test1 <- tryCatch({ car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = 0") }, error = function(e) list(PrF = NA))
#    pval1 <- if (!is.null(test1$PrF)) round(test1$`Pr(>F)`[2], 2) else NA
#    test2 <- tryCatch({ car::linearHypothesis(model, "INT_treatment_general:RES05_gender = 0") }, error = function(e) list(PrF = NA))
#    pval2 <- if (!is.null(test2$PrF)) round(test2$`Pr(>F)`[2], 2) else NA
#    test3 <- tryCatch({ car::linearHypothesis(model, "INT_treatment_gender = INT_treatment_general") }, error = function(e) list(PrF = NA))
#    pval3 <- if (!is.null(test3$PrF)) round(test3$`Pr(>F)`[2], 2) else NA
#    test4 <- tryCatch({ car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = INT_treatment_general:RES05_gender") }, error = function(e) list(PrF = NA))
#    pval4 <- if (!is.null(test4$PrF)) round(test4$`Pr(>F)`[2], 2) else NA
#    return(list(pval1 = pval1, pval2 = pval2, pval3 = pval3, pval4 = pval4))
#  }
#}


# Function to extract p-values and apply FWER correction
get_adjusted_pvalues <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    coef_table <- summary(model)$coefficients
    pvals <- coef_table[var_names, "Pr(>|t|)", drop = FALSE]
    all_pvalues <- c(all_pvalues, as.numeric(pvals))
  }
  # Apply Holm correction (step-down)
  adjusted_pvalues <- p.adjust(all_pvalues, method = "holm")
  return(data.frame(raw = all_pvalues, adjusted = adjusted_pvalues))
}

# Estimating the models
models_list <- list()
control_means <- list()
test_results <- list()

### Models with "any treatment"
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
  test_results[[i]] <- calculate_tests(model, "any_treatment")
}

### Models with "gender and general treatment"
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  j <- i + length(incum_dep_vars1)
  control_means[[j]] <- control_means[[i]]
  formula <- create_formula(dep_var, "gender_general")
  model <- lm(formula, data = data_filtered)
  models_list[[j]] <- model
  test_results[[j]] <- calculate_tests(model, "gender_general")
}

# Variables to display
outregvar2 <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                "INT_treatment:RES05_gender", "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

# Column names
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# Define variables for each panel
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
panel_B_vars <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                  "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

# Extract models for each panel
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
    # Positions: i, i+n_vars, i+2*n_vars, etc.
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extract adjusted p-values for each variable
pvals_A_by_var <- get_pvals_by_var(pvalues_panel_A, panel_A_vars, length(panel_A_models))
pvals_B_by_var <- get_pvals_by_var(pvalues_panel_B, panel_B_vars, length(panel_B_models))


# =============================================================================
# AFFICHAGE DES RÉSULTATS DANS LA CONSOLE 
# =============================================================================

# résultats d'un panel dans la console
print_panel_results <- function(models, var_names, pvalues_by_var, panel_name) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  # chaque colonne: 
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    dep_var_name <- col_names[i]
    
    cat("--- ", dep_var_name, " ---\n")
    cat("Variable          | Coeff (Std. Error) | p-value | FWER-adj p\n")
    cat("-------------------|--------------------|---------|------------\n")
    
    # chaque ligne (v d'intérêt)
    for (var in var_names) {
      if (var %in% rownames(coef_table)) {
        coef_val <- round(coef_table[var, "Estimate"], 4)
        se_val <- round(coef_table[var, "Std. Error"], 4)
        pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
        pval_adj <- round(pvalues_by_var[[var]][i], 4)  # p-value corrigée 
        
        cat(sprintf("%-18s | %4.3f (%4.3f)       | %5.4f   | %6.4f\n",
                    var, coef_val, se_val, pval_raw, pval_adj))
      }
    }
    cat("\n")
  }
}

# Panel A
print_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, "PANEL A: Average Effects")

# Panel B
print_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, "PANEL B: Effects by Type of Campaign")





## STARGAZER MARCHE PAS ENCORE ##
# Build lines for Stargazer
additional_lines_A <- list()
for (var in names(pvals_A_by_var)) {
  line <- c(paste0("FWER-adj p (", var, ")"), round(pvals_A_by_var[[var]], 4))
  additional_lines_A <- c(additional_lines_A, list(line))
}

additional_lines_B <- list()
for (var in names(pvals_B_by_var)) {
  line <- c(paste0("FWER-adj p (", var, ")"), round(pvals_B_by_var[[var]], 4))
  additional_lines_B <- c(additional_lines_B, list(line))
}

# Generate Stargazer tables with adjusted p-values
stargazer(panel_A_models,
          type = "text",
          column.labels = col_names,
          keep = panel_A_vars,
          digits = 3,
          title = "Panel A: Average Effects (FWER-adjusted p-values below)",
          covariate.labels = c("Treatment", "Reserved for Women 2005", "Treatment × Reserved 2005"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq"),
          add.lines = additional_lines_A,
          notes = "FWER-adjusted p-values control for multiple testing within the panel.")

stargazer(panel_B_models,
          type = "text",
          column.labels = col_names,
          keep = panel_B_vars,
          digits = 3,
          title = "Panel B: Effects by Type of Campaign (FWER-adjusted p-values below)",
          covariate.labels = c("Gender Treatment", "General Treatment", "Reserved for Women 2005",
                               "Gender Treatment × Reserved 2005", "General Treatment × Reserved 2005"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq"),
          add.lines = additional_lines_B,
          notes = "FWER-adjusted p-values control for multiple testing within the panel.")

# Compact version (optional)
col_names_short <- c("IncR", "IncV", "SpR", "SpV", "OthR", "OthV")
stargazer(panel_A_models,
          type = "text",
          column.labels = col_names_short,
          keep = panel_A_vars,
          digits = 2,
          title = "Panel A: Average Effects (Compact)",
          covariate.labels = c("T", "F", "T×F"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq", "n"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "0pt",
          notes = "FWER-adjusted p-values available in full table.")

