# table 2 final fwer, with tex output


# Loading required libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(car)
library(multcomp)

# Define control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Load data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filter data
data_filtered <- data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1, GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCFAM05_won - INC05_won,
    index_empl_svy_0 = rowMeans(cbind(`std_HH_NREGA`, `std_HH_NREGA_unmet_demand_m`, `std_HH_NREGA_unmet_demand_f`), na.rm = TRUE),
    index_empl_svy_1 = rowMeans(cbind(`std_HH_NREGA_unmet_demand`, `std_HH_NREGA_unmet_demand_m`, `std_HH_NREGA_unmet_demand_f`,
                                      `std_HH_NREGA_waiting_time_m`, `std_HH_NREGA_waiting_time_f`, `std_HH_NREGA`,
                                      `std_HH_NREGA_work_m`, `std_HH_NREGA_work_f`), na.rm = TRUE),
    index_empl_svy_2 = rowMeans(cbind(`std_HH_NREGA`, `std_HH_NREGA_work_m`, `std_HH_NREGA_work_f`), na.rm = TRUE),
    index_empl_svy_3 = rowMeans(cbind(`std_HH_NREGA_unmet_demand_m`, `std_HH_NREGA_unmet_demand_f`), na.rm = TRUE)
  )





## RUNS ##

# Dpdt variables (runs)
incum_dep_vars_runs <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")

# Estimation of the models
models_list <- list()
control_means <- numeric(length(incum_dep_vars_runs) * length(indices) * 3)  
i <- 0

# Panel A and B: Loop over gender (0 and 1)
for (x in 0:1) {
  for (dep_var in incum_dep_vars_runs) {  
    for (index in indices) {
      i <- i + 1
      
      # control mean
      control_mean <- data_filtered %>%
        filter(INT_treatment == 0 & RES05_gender == x) %>%
        summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean) %>%
        round(2)
      
      control_means[i] <- control_mean
      
      # interaction variables
      data_filtered <- data_filtered %>%
        mutate(
          TEMP_index = get(index),
          TEMP_X_res_index = RES05_gender * get(index),
          TEMP_X_anytr_index = INT_treatment * get(index)
        )
      
      # model estimation
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
}

# Panel C: All cases (no filter on RES05_gender)
for (dep_var in incum_dep_vars_runs) {  # Seulement les "runs"
  for (index in indices) {
    i <- i + 1
    
    # control mean
    control_mean <- data_filtered %>%
      filter(INT_treatment == 0) %>%
      summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean) %>%
      round(2)
    
    control_means[i] <- control_mean
    
    # interaction variables
    data_filtered <- data_filtered %>%
      mutate(
        TEMP_index = get(index),
        TEMP_X_anytr_index = INT_treatment * get(index)
      )
    
    # model estimation
    all_vars <- c(dep_var, "INT_treatment", "TEMP_index", "TEMP_X_anytr_index", gpcontrols, "district")
    if (all(all_vars %in% names(data_filtered))) {
      formula <- as.formula(paste(dep_var, "~ INT_treatment + TEMP_index + TEMP_X_anytr_index +",
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
}

outregvar2 <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")

# Select models for each panel
selected_models_A <- c(1, 5, 9)
selected_models_B <- c(13, 17, 21)
selected_models_C <- c(25, 29, 33)

# Function to extract p-values and apply FWER correction
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

# Implement FWER correction
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(models_list[selected_models_A], outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(models_list[selected_models_B], outregvar2)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(models_list[selected_models_C], outregvar2)

# Function to extract adjusted p-values for each variable, for each model
get_pvals_by_var_selected <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extract adjusted p-values for the relevant models
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(selected_models_A))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2, length(selected_models_B))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2, length(selected_models_C))

# Function to print results (with both p-values)
print_selected_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, indices, incum_dep_vars, selected_models) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- incum_dep_vars[(model_index-1) %% length(incum_dep_vars) + 1]
      index_name <- indices[(model_index-1) %/% length(incum_dep_vars) + 1]
      
      cat("--- Model ", selected_models[i], ": ", dep_var_name, " (Index: ", index_name, ") ---\n", sep = "")
      cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
      cat("-----------------------------|--------------------|---------|------------\n")
      
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          coef_val <- round(coef_table[var, "Estimate"], 4)
          se_val <- round(coef_table[var, "Std. Error"], 4)
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
          pval_adj <- round(pvalues_by_var[[var]][i], 4)
          
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

# Print results for each panel
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: GP without Gender Quota in 2005 (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: GP with Gender Quota in 2005 (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: All cases (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_C)






## VOTESHARE ##

incum_dep_vars_voteshare <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")
indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")

# Estimation of the models
models_list <- list()
control_means <- numeric(length(incum_dep_vars_voteshare) * length(indices) * 3)  
i <- 0

# Panel A and B: Loop over gender (0 and 1)
for (x in 0:1) {
  for (dep_var in incum_dep_vars_voteshare) { 
    for (index in indices) {
      i <- i + 1
      
      # control mean
      control_mean <- data_filtered %>%
        filter(INT_treatment == 0 & RES05_gender == x) %>%
        summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean) %>%
        round(2)
      
      control_means[i] <- control_mean
      
      # interaction variables
      data_filtered <- data_filtered %>%
        mutate(
          TEMP_index = get(index),
          TEMP_X_res_index = RES05_gender * get(index),
          TEMP_X_anytr_index = INT_treatment * get(index)
        )
      
      # model estimation
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
}

# Panel C: All cases (no filter on RES05_gender)
for (dep_var in incum_dep_vars_voteshare) {  # Seulement les "voteshare"
  for (index in indices) {
    i <- i + 1
    
    # control mean
    control_mean <- data_filtered %>%
      filter(INT_treatment == 0) %>%
      summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean) %>%
      round(2)
    
    control_means[i] <- control_mean
    
    # interaction variables
    data_filtered <- data_filtered %>%
      mutate(
        TEMP_index = get(index),
        TEMP_X_anytr_index = INT_treatment * get(index)
      )
    
    # model estimation
    all_vars <- c(dep_var, "INT_treatment", "TEMP_index", "TEMP_X_anytr_index", gpcontrols, "district")
    if (all(all_vars %in% names(data_filtered))) {
      formula <- as.formula(paste(dep_var, "~ INT_treatment + TEMP_index + TEMP_X_anytr_index +",
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
}

outregvar2 <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")

# Model selection for each voteshare panel
selected_models_A <- c(1, 5, 9)
selected_models_B <- c(13, 17, 21)
selected_models_C <- c(25, 29, 33)

# Function to extract p-values and apply FWER correction
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

# Implement FWER correction
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(models_list[selected_models_A], outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(models_list[selected_models_B], outregvar2)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(models_list[selected_models_C], outregvar2)

# Function to extract adjusted p-values for each variable, for each model
get_pvals_by_var_selected <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extract adjusted p-values for the relevant models
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(selected_models_A))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2, length(selected_models_B))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2, length(selected_models_C))

# Function to print results (with both p-values)
print_selected_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, indices, incum_dep_vars, selected_models) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- incum_dep_vars[(model_index-1) %% length(incum_dep_vars) + 1]
      index_name <- indices[(model_index-1) %/% length(incum_dep_vars) + 1]
      
      cat("--- Model ", selected_models[i], ": ", dep_var_name, " (Index: ", index_name, ") ---\n", sep = "")
      cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
      cat("-----------------------------|--------------------|---------|------------\n")
      
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          coef_val <- round(coef_table[var, "Estimate"], 4)
          se_val <- round(coef_table[var, "Std. Error"], 4)
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
          pval_adj <- round(pvalues_by_var[[var]][i], 4)
          
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

# Print results for each panel
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: GP without Gender Quota in 2005 (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: GP with Gender Quota in 2005 (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: All cases (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_C)




## OUTPUT TXT ##


sink("~/work/Table_2_final_fwer.txt", append = FALSE, split = TRUE)

# runs
cat("======================================================================\n")
cat("FWER - Table 2: Performance (Runs)\n")
cat("======================================================================\n\n")

print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: GP without Gender Quota in 2005 (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_A)

print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: GP with Gender Quota in 2005 (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_B)

print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: All cases (Runs only)",
                       control_means, indices, incum_dep_vars_runs, selected_models_C)

# voteshare
cat("\n\n======================================================================\n")
cat("FWER - Table 2: Performance (Voteshare)\n")
cat("======================================================================\n\n")

print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: GP without Gender Quota in 2005 (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_A)

print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: GP with Gender Quota in 2005 (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_B)

print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: All cases (Voteshare only)",
                       control_means, indices, incum_dep_vars_voteshare, selected_models_C)


sink()





### OUTPUT IN TEX ###

# new labels (no underscores)
clean_var_name <- function(name) {
  gsub("_", " ", name)
}

# generate script for a sub table
generate_latex_table <- function(models, var_names, pvalues_by_var, pvalues_raw_by_var, control_means, indices, incum_dep_vars, selected_models, table_title) {
  latex_lines <- character()
  
  # heading
  latex_lines <- c(latex_lines,
                   "\\begin{center}",
                   "\\begin{adjustbox}{max width=\\textwidth}",
                   "\\begin{tabular}{lcccc}",
                   "\\toprule",
                   " & \\multicolumn{4}{c}{Dependent Variable} \\\\ \\cmidrule(lr){2-5}",
                   " & (1) & (2) & (3) & (4) \\\\ \\midrule"
  )
  
  # for each variable
  for (var in var_names) {
    clean_var <- clean_var_name(var)
    line_coef <- paste0(" & \\multicolumn{1}{c}{", clean_var, "}")
    line_pval <- paste0(" & \\multicolumn{1}{c}{\\textit{p-value}}")
    line_pval_adj <- paste0(" & \\multicolumn{1}{c}{\\textit{FWER-adj p}}")
    
    for (i in 1:length(selected_models)) {
      model_index <- selected_models[i]
      model <- models[[model_index]]
      
      if (!is.null(model)) {
        coef_table <- summary(model)$coefficients
        if (var %in% rownames(coef_table)) {
          # coeff and error
          coef_val <- round(coef_table[var, "Estimate"], 3)
          se_val <- round(coef_table[var, "Std. Error"], 3)
          line_coef <- paste0(line_coef, " & $", coef_val, "$, (", se_val, ")")
          
          # p-values 
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 3)
          line_pval <- paste0(line_pval, " & ", pval_raw)
          
          # adj p-values 
          pval_adj <- round(pvalues_by_var[[var]][i], 3)
          stars <- ifelse(pval_adj < 0.01, "$^{***}$",
                          ifelse(pval_adj < 0.05, "$^{**}$",
                                 ifelse(pval_adj < 0.1, "$^{*}$", "")))
          line_pval_adj <- paste0(line_pval_adj, " & ", pval_adj, stars)
        } else {
          line_coef <- paste0(line_coef, " & ")
          line_pval <- paste0(line_pval, " & ")
          line_pval_adj <- paste0(line_pval_adj, " & ")
        }
      }
    }
    
    latex_lines <- c(latex_lines, paste0(line_coef, " \\\\"))
    latex_lines <- c(latex_lines, paste0(line_pval, " \\\\"))
    latex_lines <- c(latex_lines, paste0(line_pval_adj, " \\\\"))
  }
  
  # mean and obs
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    dep_var_name <- clean_var_name(incum_dep_vars[(model_index-1) %% length(incum_dep_vars) + 1])
    index_name <- clean_var_name(indices[(model_index-1) %/% length(incum_dep_vars) + 1])
    latex_lines <- c(latex_lines,
                     "\\midrule",
                     paste0("\\multicolumn{5}{l}{\\textit{Mean in Control not WR in 2005 (", dep_var_name, ", ", index_name, "): } ",
                            control_means[model_index], "} \\\\"))
    if (!is.null(models[[model_index]])) {
      nobs_val <- nobs(models[[model_index]])
      latex_lines <- c(latex_lines, paste0("\\multicolumn{5}{l}{\\textit{Observations: } ", nobs_val, "} \\\\"))
    }
  }
  
  # end doc
  latex_lines <- c(latex_lines,
                   "\\bottomrule",
                   "\\end{tabular}",
                   "\\end{adjustbox}",
                   "\\end{center}",
                   "")
  
  # Title
  latex_lines <- c(paste0("\\subsection*{", table_title, "}"), "", latex_lines)
  
  return(latex_lines)
}

# extract raw p values
get_raw_pvalues_selected <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      pvals <- coef_table[var_names, "Pr(>|t|)", drop = FALSE]
      all_pvalues <- c(all_pvalues, as.numeric(pvals))
    }
  }
  return(data.frame(raw = all_pvalues))
}

# Extracting p-values for each panel
raw_pvalues_panel_A_selected <- get_raw_pvalues_selected(models_list[selected_models_A], outregvar2)
raw_pvalues_panel_B_selected <- get_raw_pvalues_selected(models_list[selected_models_B], outregvar2)
raw_pvalues_panel_C_selected <- get_raw_pvalues_selected(models_list[selected_models_C], outregvar2)

# Extract p-values for each variable
get_raw_pvals_by_var_selected <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$raw[idx]
  }
  return(pvals_list)
}

# Extracting p value for each var
raw_pvals_A_by_var_selected <- get_raw_pvals_by_var_selected(raw_pvalues_panel_A_selected, outregvar2, length(selected_models_A))
raw_pvals_B_by_var_selected <- get_raw_pvals_by_var_selected(raw_pvalues_panel_B_selected, outregvar2, length(selected_models_B))
raw_pvals_C_by_var_selected <- get_raw_pvals_by_var_selected(raw_pvalues_panel_C_selected, outregvar2, length(selected_models_C))


# tex for runs
latex_runs <- generate_latex_table(
  models_list, outregvar2, pvals_A_by_var_selected, raw_pvals_A_by_var_selected,
  control_means, indices, incum_dep_vars_runs, selected_models_A,
  "GP without Gender Quota in 2005 (Runs only)"
)

latex_runs <- c(latex_runs,
                generate_latex_table(
                  models_list, outregvar2, pvals_B_by_var_selected, raw_pvals_B_by_var_selected,
                  control_means, indices, incum_dep_vars_runs, selected_models_B,
                  "GP with Gender Quota in 2005 (Runs only)"
                ),
                generate_latex_table(
                  models_list, outregvar2, pvals_C_by_var_selected, raw_pvals_C_by_var_selected,
                  control_means, indices, incum_dep_vars_runs, selected_models_C,
                  "All cases (Runs only)"
                )
)

# tex for voteshare
latex_voteshare <- generate_latex_table(
  models_list, outregvar2, pvals_A_by_var_selected, raw_pvals_A_by_var_selected,
  control_means, indices, incum_dep_vars_voteshare, selected_models_A,
  "GP without Gender Quota in 2005 (Voteshare only)"
)

latex_voteshare <- c(latex_voteshare,
                     generate_latex_table(
                       models_list, outregvar2, pvals_B_by_var_selected, raw_pvals_B_by_var_selected,
                       control_means, indices, incum_dep_vars_voteshare, selected_models_B,
                       "GP with Gender Quota in 2005 (Voteshare only)"
                     ),
                     generate_latex_table(
                       models_list, outregvar2, pvals_C_by_var_selected, raw_pvals_C_by_var_selected,
                       control_means, indices, incum_dep_vars_voteshare, selected_models_C,
                       "All cases (Voteshare only)"
                     )
)



# Write in .tex
writeLines(c(
  "\\documentclass{article}",
  "\\usepackage{adjustbox}",
  "\\usepackage{booktabs}",
  "\\usepackage{amsmath}",
  "\\begin{document}",
  "",
  latex_runs,
  latex_voteshare,
  "",
  "\\end{document}"
), "~/work/Table_2_final.tex")
