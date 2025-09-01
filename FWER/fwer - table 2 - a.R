# FWER pour table 2

# Script for Table 2: Performance 2010 with FWER correction


# Required libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(car)
library(multcomp)

# Defining the macros: control variables and variables related to the regression
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Data processing: uploading, filtering
data <- read_dta("~/work/Electoral data cleaned.dta")

# verification de presence des variables
# Vérifier les colonnes disponibles liées à NREGA
nrega_columns <- grep("NREGA", colnames(data), value = TRUE)
print(nrega_columns)

# Filtering the data
data_filtered <- data %>%
  filter(SAMPLE_hhsurvey == 1, GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCFAM05_won - INC05_won
  )

# Generation of the performance indices of the program
data_filtered <- data_filtered %>%
  mutate(
    index_empl_svy_0 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE),
    index_empl_svy_1 = rowMeans(select(., std_HH_NREGA_unmet_demand, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f,
                                       std_HH_NREGA_waiting_time_m, std_HH_NREGA_waiting_time_f, std_HH_NREGA,
                                       std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_2 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_3 = rowMeans(select(., std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE)
  )

# Dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won")
indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")

# Function to extract p-values and apply FWER correction
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

# Doing the regressions and storing models
models_list <- list()
control_means <- numeric(length(incum_dep_vars1) * length(indices) * 2)
i <- 0

for (x in 0:1) {  # Loop over gender (0 and 1)
  for (dep_var in incum_dep_vars1) {
    for (index in indices) {
      i <- i + 1
      
      # Control mean
      control_mean <- data_filtered %>%
        filter(INT_treatment == 0 & RES05_gender == x) %>%
        summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean) %>%
        round(2)
      
      control_means[i] <- control_mean
      
      # Interaction variables
      data_filtered <- data_filtered %>%
        mutate(
          TEMP_index = get(index),
          TEMP_X_res_index = RES05_gender * get(index),
          TEMP_X_anytr_index = INT_treatment * get(index),
          TEMP_X_anytr_res_index = INT_treatment * RES05_gender * get(index)
        )
      
      # Model estimation
      all_vars <- c(dep_var, "INT_treatment", "TEMP_index", "TEMP_X_anytr_index", gpcontrols, "district")
      if (all(all_vars %in% names(data_filtered))) {
        formula <- as.formula(paste(dep_var, "~ INT_treatment + TEMP_index + TEMP_X_anytr_index +",
                                    paste(gpcontrols, collapse = " + "), "+ factor(district)"))
        model <- tryCatch({
          lm(formula, data = data_filtered %>% filter(RES05_gender == x))
        }, error = function(e) {
          message("Error in model fitting: ", e$message)
          NULL
        })
        
        models_list[[i]] <- model
      }
    }
  }
  
  # Define variables for FWER correction
  outregvar2 <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")
  
  # Split models into Panel A (RES05_gender == 0) and Panel B (RES05_gender == 1)
  panel_A_models <- models_list[1:(length(models_list)/2)]
  panel_B_models <- models_list[(length(models_list)/2 + 1):length(models_list)]
  
  # Apply FWER correction for each panel
  pvalues_panel_A <- get_adjusted_pvalues(panel_A_models, outregvar2)
  pvalues_panel_B <- get_adjusted_pvalues(panel_B_models, outregvar2)
  
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
  
  pvals_A_by_var <- get_pvals_by_var(pvalues_panel_A, outregvar2, length(panel_A_models))
  pvals_B_by_var <- get_pvals_by_var(pvalues_panel_B, outregvar2, length(panel_B_models))
  
  # Function to print results with FWER-adjusted p-values
  print_panel_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, indices, incum_dep_vars1) {
    cat("\n\n======================================================================\n")
    cat(panel_name, "\n")
    cat("======================================================================\n\n")
    
    for (i in 1:length(models)) {
      model <- models[[i]]
      if (!is.null(model)) {
        coef_table <- summary(model)$coefficients
        dep_var_name <- incum_dep_vars1[(i-1) %% length(incum_dep_vars1) + 1]
        index_name <- indices[(i-1) %/% length(incum_dep_vars1) + 1]
        
        cat("--- ", dep_var_name, " (Index: ", index_name, ") ---\n", sep = "")
        cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
        cat("-----------------------------|--------------------|---------|------------\n")
        
        for (var in var_names) {
          if (var %in% rownames(coef_table)) {
            coef_val <- round(coef_table[var, "Estimate"], 4)
            se_val <- round(coef_table[var, "Std. Error"], 4)
            pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
            pval_adj <- round(pvalues_by_var[[var]][i], 4)
            cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f   | %6.4f\n",
                        var, coef_val, se_val, pval_raw, pval_adj))
          }
        }
        cat("Mean in Control not WR in 2005: ", control_means[i], "\n\n")
      }
    }
  }
  
  # Print results for Panel A and Panel B
  print_panel_results(panel_A_models, outregvar2, pvals_A_by_var,
                      "PANEL A: GP without Gender Quota in 2005",
                      control_means[1:(length(control_means)/2)],
                      indices, incum_dep_vars1)
  
  print_panel_results(panel_B_models, outregvar2, pvals_B_by_var,
                      "PANEL B: GP with Gender Quota in 2005",
                      control_means[(length(control_means)/2 + 1):length(control_means)],
                      indices, incum_dep_vars1)
  
  # Optionally, export to text file
  sink("output/Table2_Performance_2010_FWER.txt")
  print_panel_results(panel_A_models, outregvar2, pvals_A_by_var,
                      "PANEL A: GP without Gender Quota in 2005",
                      control_means[1:(length(control_means)/2)],
                      indices, incum_dep_vars1)
  
  print_panel_results(panel_B_models, outregvar2, pvals_B_by_var,
                      "PANEL B: GP with Gender Quota in 2005",
                      control_means[(length(control_means)/2 + 1):length(control_means)],
                      indices, incum_dep_vars1)
  sink()
  