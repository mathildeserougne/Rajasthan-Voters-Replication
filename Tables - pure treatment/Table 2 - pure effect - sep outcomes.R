# Table 2 - isolated regression - separated outcomes
# we keep interaction between treatment and performance index
# but we split the families of outcomes
# and we split into two subsamples


# Libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(car)

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# DATA
data <- read_dta("~/work/Electoral data cleaned.dta")

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

# Dependent variables
incum_dep_vars_runs <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
incum_dep_vars_voteshare <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")
indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")



# Model estimation for variable family and subsample
estimate_models <- function(dep_vars) {
  models_list <- list()
  control_means <- numeric(length(dep_vars) * length(indices) * 2)  # 2 panels (A and B)
  i <- 0
  
  for (x in 0:1) {
    for (dep_var in dep_vars) {
      for (index in indices) {
        i <- i + 1
        
        # control mean
        control_means[i] <- data_filtered %>%
          filter(INT_treatment == 0 & RES05_gender == x) %>%
          summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
          pull(mean) %>%
          round(2)
        
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
  
  return(list(models = models_list, control_means = control_means))
}

# model estimation for both of variables families
results_runs <- estimate_models(incum_dep_vars_runs)
results_voteshare <- estimate_models(incum_dep_vars_voteshare)



### OUTPUT ##

# Printing results to a file
print_results_to_file <- function(models_runs, control_means_runs, models_voteshare, control_means_voteshare, filename) {
  sink(filename, append = FALSE, split = TRUE)
  
  # Panel names
  panel_names <- c(
    "MODELS: GP without Gender Quota in 2005 (Runs only)",
    "MODELS: GP with Gender Quota in 2005 (Runs only)",
    "MODELS: GP without Gender Quota in 2005 (Voteshare only)",
    "MODELS: GP with Gender Quota in 2005 (Voteshare only)"
  )
  
  # Selected models for each panel (A: 1,5,9; B: 13,17,21)
  selected_models_A <- c(1, 5, 9)
  selected_models_B <- c(13, 17, 21)
  
  # Print runs results
  for (panel in 1:2) {
    cat("\n\n======================================================================\n")
    cat(panel_names[panel], "\n")
    cat("======================================================================\n\n")
    
    selected_models <- if (panel == 1) selected_models_A else selected_models_B
    dep_vars <- incum_dep_vars_runs
    
    for (i in 1:length(selected_models)) {
      model_index <- selected_models[i]
      model <- models_runs[[model_index]]
      if (!is.null(model)) {
        coef_table <- summary(model)$coefficients
        dep_var_name <- dep_vars[(model_index-1) %% length(dep_vars) + 1]
        index_name <- indices[(model_index-1) %/% length(dep_vars) + 1]
        
        cat("--- Model ", selected_models[i], ": ", dep_var_name, " (Index: ", index_name, ") ---\n", sep = "")
        cat("Variable                     | Coeff (Std. Error) | p-value\n")
        cat("-----------------------------|--------------------|---------\n")
        
        for (var in c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")) {
          if (var %in% rownames(coef_table)) {
            coef_val <- round(coef_table[var, "Estimate"], 4)
            se_val <- round(coef_table[var, "Std. Error"], 4)
            pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
            
            stars <- ifelse(pval_raw < 0.01, "***",
                            ifelse(pval_raw < 0.05, "**",
                                   ifelse(pval_raw < 0.1, "*", "")))
            
            cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f%s\n",
                        var, coef_val, se_val, pval_raw, stars))
          }
        }
        cat("Mean in Control not WR in 2005: ", control_means_runs[model_index], "\n")
        cat("Observations: ", nobs(model), "\n")
      }
    }
  }
  
  # Print voteshare results
  for (panel in 1:2) {
    cat("\n\n======================================================================\n")
    cat(panel_names[panel + 2], "\n")
    cat("======================================================================\n\n")
    
    selected_models <- if (panel == 1) selected_models_A else selected_models_B
    dep_vars <- incum_dep_vars_voteshare
    
    for (i in 1:length(selected_models)) {
      model_index <- selected_models[i]
      model <- models_voteshare[[model_index]]
      if (!is.null(model)) {
        coef_table <- summary(model)$coefficients
        dep_var_name <- dep_vars[(model_index-1) %% length(dep_vars) + 1]
        index_name <- indices[(model_index-1) %/% length(dep_vars) + 1]
        
        cat("--- Model ", selected_models[i], ": ", dep_var_name, " (Index: ", index_name, ") ---\n", sep = "")
        cat("Variable                     | Coeff (Std. Error) | p-value\n")
        cat("-----------------------------|--------------------|---------\n")
        
        for (var in c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")) {
          if (var %in% rownames(coef_table)) {
            coef_val <- round(coef_table[var, "Estimate"], 4)
            se_val <- round(coef_table[var, "Std. Error"], 4)
            pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
            
            stars <- ifelse(pval_raw < 0.01, "***",
                            ifelse(pval_raw < 0.05, "**",
                                   ifelse(pval_raw < 0.1, "*", "")))
            
            cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f%s\n",
                        var, coef_val, se_val, pval_raw, stars))
          }
        }
        cat("Mean in Control not WR in 2005: ", control_means_voteshare[model_index], "\n")
        cat("Observations: ", nobs(model), "\n")
      }
    }
  }
  
  sink()
}


# Print all results to the same file
#print_results_to_file(
#  results_runs$models, results_runs$control_means,
#  results_voteshare$models, results_voteshare$control_means,
#  "~/work/Table2_four_panels_stacked.txt"
#)



# section gathering all columns and panels in a cleaner presentation: 

# display results in three columns
print_results_three_columns <- function(models_runs, control_means_runs, models_voteshare, control_means_voteshare, filename) {
  sink(filename, append = FALSE, split = TRUE)
  
  # panels: res05_gender==0 or 1
  panel_names <- c(
    "GP without Gender Quota in 2005",
    "GP with Gender Quota in 2005"
  )
  
  # selected models (A: 1,5,9 ; B: 13,17,21)
  selected_models_A <- c(1, 5, 9)
  selected_models_B <- c(13, 17, 21)
  
  # runs
  cat("\n\n")
  cat("================================================================================================================\n")
  cat("DEPENDENT VARIABLE: RUNNING\n")
  cat("================================================================================================================\n\n")
  
  for (panel in 1:2) {
    cat(panel_names[panel], "\n")
    selected_models <- if (panel == 1) selected_models_A else selected_models_B
    dep_vars <- incum_dep_vars_runs
    
    # colnames
    cat(sprintf("%-30s", ""))
    for (i in 1:length(selected_models)) {
      dep_var_name <- dep_vars[(selected_models[i]-1) %% length(dep_vars) + 1]
      index_name <- indices[(selected_models[i]-1) %/% length(dep_vars) + 1]
      cat(sprintf("%-30s", paste0(dep_var_name, " (", index_name, ")")))
    }
    cat("\n")
    
    # variables
    for (var in c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")) {
      cat(sprintf("%-30s", var))
      for (i in 1:length(selected_models)) {
        model <- models_runs[[selected_models[i]]]
        if (!is.null(model)) {
          coef_val <- coef(summary(model))[var, "Estimate"]
          se_val <- coef(summary(model))[var, "Std. Error"]
          pval_raw <- coef(summary(model))[var, "Pr(>|t|)"]
          stars <- ifelse(pval_raw < 0.01, "***",
                          ifelse(pval_raw < 0.05, "**",
                                 ifelse(pval_raw < 0.1, "*", "")))
          cat(sprintf("%-15s %5.3f (%5.3f)%s",
                      "", coef_val, se_val, stars))
        }
      }
      cat("\n")
    }
    
    # mean
    cat(sprintf("%-30s", "Mean (Control):"))
    for (i in 1:length(selected_models)) {
      cat(sprintf("%-15s %5.2f", "", control_means_runs[selected_models[i]]))
    }
    cat("\n")
    
    # observations nb
    cat(sprintf("%-30s", "Observations:"))
    for (i in 1:length(selected_models)) {
      model <- models_runs[[selected_models[i]]]
      if (!is.null(model)) cat(sprintf("%-15s %5d", "", nobs(model)))
    }
    cat("\n\n")
  }
  
  # voteshare
  cat("\n\n")
  cat("================================================================================================================\n")
  cat("DEPENDENT VARIABLE: VOTESHARE\n")
  cat("================================================================================================================\n\n")
  
  for (panel in 1:2) {
    cat(panel_names[panel], "\n")
    selected_models <- if (panel == 1) selected_models_A else selected_models_B
    dep_vars <- incum_dep_vars_voteshare
    
    # colnames
    cat(sprintf("%-30s", ""))
    for (i in 1:length(selected_models)) {
      dep_var_name <- dep_vars[(selected_models[i]-1) %% length(dep_vars) + 1]
      index_name <- indices[(selected_models[i]-1) %/% length(dep_vars) + 1]
      cat(sprintf("%-30s", paste0(dep_var_name, " (", index_name, ")")))
    }
    cat("\n")
    
    # variables
    for (var in c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")) {
      cat(sprintf("%-30s", var))
      for (i in 1:length(selected_models)) {
        model <- models_voteshare[[selected_models[i]]]
        if (!is.null(model)) {
          coef_val <- coef(summary(model))[var, "Estimate"]
          se_val <- coef(summary(model))[var, "Std. Error"]
          pval_raw <- coef(summary(model))[var, "Pr(>|t|)"]
          stars <- ifelse(pval_raw < 0.01, "***",
                          ifelse(pval_raw < 0.05, "**",
                                 ifelse(pval_raw < 0.1, "*", "")))
          cat(sprintf("%-15s %5.3f (%5.3f)%s",
                      "", coef_val, se_val, stars))
        }
      }
      cat("\n")
    }
    
    # mean
    cat(sprintf("%-30s", "Mean (Control):"))
    for (i in 1:length(selected_models)) {
      cat(sprintf("%-15s %5.2f", "", control_means_voteshare[selected_models[i]]))
    }
    cat("\n")
    
    # observation nb
    cat(sprintf("%-30s", "Observations:"))
    for (i in 1:length(selected_models)) {
      model <- models_voteshare[[selected_models[i]]]
      if (!is.null(model)) cat(sprintf("%-15s %5d", "", nobs(model)))
    }
    cat("\n\n")
  }
  
  sink()
}



# printing the results
print_results_three_columns(
  results_runs$models, results_runs$control_means,
  results_voteshare$models, results_voteshare$control_means,
  "~/work/Table2_three_columns_per_panel.txt"
)



## OUTPUT IN .TEX ##



print_results_to_tex <- function(models_runs, control_means_runs,
                                 models_voteshare, control_means_voteshare,
                                 filename) {
  # open file
  sink(paste0(filename, ".tex"), append = FALSE, split = TRUE)
  
  # heading
  cat("\\documentclass{article}
\\usepackage{booktabs}
\\usepackage{caption}
\\usepackage{adjustbox}
\\usepackage{pdflscape}
\\begin{document}
\\begin{landscape}
\\begin{table}[htbp]
\\centering
\\caption{Résultats des régressions pour les variables dépendantes (Running et Voteshare)}
\\label{tab:regression_results}", sep = "\n")
  
  # Panel A: GP without Gender Quota in 2005 (Running)
  cat("\\begin{tabular}{lccc}
\\toprule
& \\multicolumn{3}{c}{GP without Gender Quota in 2005 (Running)} \\\\
\\cmidrule(lr){2-4}
Variable & INC05 running & INCSPOUSE05 running & INCOTHER05 running \\\\
& (index empl svy 0) & (index empl svy 1) & (index empl svy 2) \\\\
\\midrule", sep = "\n")
  
  for (var in c("INT treatment", "TEMP index", "TEMP X anytr index")) {
    var_real <- gsub(" ", "_", var)
    cat(gsub("_", " ", var_real))
    for (i in c(1, 5, 9)) {
      model <- models_runs[[i]]
      if (!is.null(model) && var_real %in% rownames(coef(summary(model)))) {
        coef_val <- round(coef(summary(model))[var_real, "Estimate"], 4)
        se_val <- round(coef(summary(model))[var_real, "Std. Error"], 4)
        cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
      } else {
        cat(" & ", sep = "")
      }
    }
    cat(" \\\\", sep = "")
  }
  
  cat("\\\\
\\midrule
Mean (Control) ")
  for (i in c(1, 5, 9)) {
    cat(" & ", control_means_runs[i], sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("Observations ")
  for (i in c(1, 5, 9)) {
    model <- models_runs[[i]]
    if (!is.null(model)) cat(" & ", nobs(model), sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("\\bottomrule
\\end{tabular}", sep = "\n")
  
  # Panel B: GP with Gender Quota in 2005 (Running)
  cat("\\quad
\\begin{tabular}{lccc}
\\toprule
& \\multicolumn{3}{c}{GP with Gender Quota in 2005 (Running)} \\\\
\\cmidrule(lr){2-4}
Variable & INC05 running & INCSPOUSE05 running & INCOTHER05 running \\\\
& (index empl svy 0) & (index empl svy 1) & (index empl svy 2) \\\\
\\midrule", sep = "\n")
  
  for (var in c("INT treatment", "TEMP index", "TEMP X anytr index")) {
    var_real <- gsub(" ", "_", var)
    cat(gsub("_", " ", var_real))
    for (i in c(13, 17, 21)) {
      model <- models_runs[[i]]
      if (!is.null(model) && var_real %in% rownames(coef(summary(model)))) {
        coef_val <- round(coef(summary(model))[var_real, "Estimate"], 4)
        se_val <- round(coef(summary(model))[var_real, "Std. Error"], 4)
        cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
      } else {
        cat(" & ", sep = "")
      }
    }
    cat(" \\\\", sep = "")
  }
  
  cat("\\\\
\\midrule
Mean (Control) ")
  for (i in c(13, 17, 21)) {
    cat(" & ", control_means_runs[i], sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("Observations ")
  for (i in c(13, 17, 21)) {
    model <- models_runs[[i]]
    if (!is.null(model)) cat(" & ", nobs(model), sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("\\bottomrule
\\end{tabular}", sep = "\n")
  
  # clearpage to be able to read
  cat("\\end{table}
\\clearpage
\\begin{table}[htbp]
\\centering", sep = "\n")
  
  # Panel C: GP without Gender Quota in 2005 (Voteshare)
  cat("\\quad
\\begin{tabular}{lccc}
\\toprule
& \\multicolumn{3}{c}{GP without Gender Quota in 2005 (Voteshare)} \\\\
\\cmidrule(lr){2-4}
Variable & INC05 voteshare & INCSPOUSE05 voteshare & INCOTHER05 voteshare \\\\
& (index empl svy 0) & (index empl svy 1) & (index empl svy 2) \\\\
\\midrule", sep = "\n")
  
  for (var in c("INT treatment", "TEMP index", "TEMP X anytr index")) {
    var_real <- gsub(" ", "_", var)
    cat(gsub("_", " ", var_real))
    for (i in c(1, 5, 9)) {
      model <- models_voteshare[[i]]
      if (!is.null(model) && var_real %in% rownames(coef(summary(model)))) {
        coef_val <- round(coef(summary(model))[var_real, "Estimate"], 4)
        se_val <- round(coef(summary(model))[var_real, "Std. Error"], 4)
        cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
      } else {
        cat(" & ", sep = "")
      }
    }
    cat(" \\\\", sep = "")
  }
  
  cat("\\\\
\\midrule
Mean (Control) ")
  for (i in c(1, 5, 9)) {
    cat(" & ", control_means_voteshare[i], sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("Observations ")
  for (i in c(1, 5, 9)) {
    model <- models_voteshare[[i]]
    if (!is.null(model)) cat(" & ", nobs(model), sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("\\bottomrule
\\end{tabular}", sep = "\n")
  
  # Panel D: GP with Gender Quota in 2005 (Voteshare)
  cat("\\quad
\\begin{tabular}{lccc}
\\toprule
& \\multicolumn{3}{c}{GP with Gender Quota in 2005 (Voteshare)} \\\\
\\cmidrule(lr){2-4}
Variable & INC05 voteshare & INCSPOUSE05 voteshare & INCOTHER05 voteshare \\\\
& (index empl svy 0) & (index empl svy 1) & (index empl svy 2) \\\\
\\midrule", sep = "\n")
  
  for (var in c("INT treatment", "TEMP index", "TEMP X anytr index")) {
    var_real <- gsub(" ", "_", var)
    cat(gsub("_", " ", var_real))
    for (i in c(13, 17, 21)) {
      model <- models_voteshare[[i]]
      if (!is.null(model) && var_real %in% rownames(coef(summary(model)))) {
        coef_val <- round(coef(summary(model))[var_real, "Estimate"], 4)
        se_val <- round(coef(summary(model))[var_real, "Std. Error"], 4)
        cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
      } else {
        cat(" & ", sep = "")
      }
    }
    cat(" \\\\", sep = "")
  }
  
  cat("\\\\
\\midrule
Mean (Control) ")
  for (i in c(13, 17, 21)) {
    cat(" & ", control_means_voteshare[i], sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("Observations ")
  for (i in c(13, 17, 21)) {
    model <- models_voteshare[[i]]
    if (!is.null(model)) cat(" & ", nobs(model), sep = "")
  }
  cat(" \\\\", sep = "")
  
  cat("\\bottomrule
\\end{tabular}
\\end{table}
\\end{landscape}
\\end{document}", sep = "\n")
  
  # close file
  sink()
}



print_results_to_tex(
  results_runs$models, results_runs$control_means,
  results_voteshare$models, results_voteshare$control_means,
  "~/work/Table2_four_panels_tex"
)
