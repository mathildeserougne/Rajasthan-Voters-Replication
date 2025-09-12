# table 3 pure fwer b (correction)


# FWER on selected outcomes: number candidates, number challengers, % female challengers, % low caste challengers
# Required libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(car)
library(multcomp)

# Define controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Load data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filter data
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1)

# Selected dependent variables (Part One)
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", "CHAL_prop_nongen")

# Estimation of the models (Part One)
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample (only INT_treatment)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
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

# Panel B: Without Previous Gender Reservation (RES05_gender == 0)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(RES05_gender == 0))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Panel C: With Previous Gender Reservation (RES05_gender == 1)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(RES05_gender == 1))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Selecting the models corresponding to the panels
selected_models_A <- c(1, 2, 3, 4)  # Full Sample
selected_models_B <- c(5, 6, 7, 8)  # Without Previous Gender Reservation
selected_models_C <- c(9, 10, 11, 12)  # With Previous Gender Reservation
panel_A_models_selected <- models_list[selected_models_A]
panel_B_models_selected <- models_list[selected_models_B]
panel_C_models_selected <- models_list[selected_models_C]

# Interest variable (only INT_treatment)
outregvar2 <- c("INT_treatment")

# Function to extract p-values and apply the FWER correction
get_adjusted_pvalues_selected <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          all_pvalues <- c(all_pvalues, coef_table[var, "Pr(>|t|)"])
        } else {
          all_pvalues <- c(all_pvalues, NA)
        }
      }
    }
  }
  valid_pvalues <- all_pvalues[!is.na(all_pvalues)]
  adjusted_pvalues <- p.adjust(valid_pvalues, method = "holm")
  result <- all_pvalues
  result[!is.na(all_pvalues)] <- adjusted_pvalues
  return(data.frame(raw = all_pvalues, adjusted = result))
}

# Implement FWER correction
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(panel_A_models_selected, outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(panel_B_models_selected, outregvar2)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(panel_C_models_selected, outregvar2)

# Function to extract adjusted p-values for each variable, each model
get_pvals_by_var_selected <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extracting the adjusted p-values for our models
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(panel_A_models_selected))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2, length(panel_B_models_selected))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2, length(panel_C_models_selected))

# Function to print the results with both types of p-values
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

# Print results for each panel (Part One)
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: Full Sample (INT_treatment only)",
                       control_means, dep_vars, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Without Previous Gender Reservation (RES05_gender == 0, INT_treatment only)",
                       control_means, dep_vars, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: With Previous Gender Reservation (RES05_gender == 1, INT_treatment only)",
                       control_means, dep_vars, selected_models_C)

# Saving the results of Part One
sink("~/work/fwer_table3_part1_results.txt")
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: Full Sample (INT_treatment only)",
                       control_means, dep_vars, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Without Previous Gender Reservation (RES05_gender == 0, INT_treatment only)",
                       control_means, dep_vars, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: With Previous Gender Reservation (RES05_gender == 1, INT_treatment only)",
                       control_means, dep_vars, selected_models_C)
sink()




# PREPARING THE TEX: extracting results from part 1 

extract_panel_results <- function(models, pvalues_by_var, dep_vars, selected_models, panel_name) {
  results <- list()
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[(model_index - 1) %% length(dep_vars) + 1]
      var <- "INT_treatment"
      if (var %in% rownames(coef_table)) {
        estimate <- round(coef_table[var, "Estimate"], 3)
        p_value <- round(coef_table[var, "Pr(>|t|)"], 3)
        fwer <- round(pvalues_by_var[[var]][i], 3)
        results[[i]] <- data.frame(
          Family = "Treatment Effect on Running",
          Panel = panel_name,
          Outcome = dep_var_name,
          Estimate = estimate,
          P.value = p_value,
          FWER = fwer,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(data.frame())
  }
}

# Extracting:
panel_names_part1 <- c("WHOLE SAMPLE", "RES05_gender = 0", "RES05_gender = 1")
selected_models_part1 <- list(selected_models_A, selected_models_B, selected_models_C)
pvals_by_var_part1 <- list(pvals_A_by_var_selected, pvals_B_by_var_selected, pvals_C_by_var_selected)
dep_vars_part1 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", "CHAL_prop_nongen")

results_part1 <- list()
for (i in 1:3) {
  results_part1[[i]] <- extract_panel_results(
    models_list, pvals_by_var_part1[[i]], dep_vars_part1, selected_models_part1[[i]], panel_names_part1[i]
  )
}
results_part1 <- bind_rows(results_part1)





### PART TWO ################################################
# FWER on selected outcomes: number candidates, number challengers, vote share female challengers, vote share low caste challengers
# Selected dependent variables (Part Two)
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_voteshare_female", "CHAL_voteshare_nongen")

# Estimation of the models (Part Two)
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample (only INT_treatment)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
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

# Panel B: Without Previous Gender Reservation (RES05_gender == 0)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(RES05_gender == 0))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Panel C: With Previous Gender Reservation (RES05_gender == 1)
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation: only INT_treatment + controls
  all_vars <- c(dep_var, "INT_treatment", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols, collapse = " + "), "+ factor(district)"))
    model <- tryCatch({
      lm(formula, data = data_filtered %>% filter(RES05_gender == 1))
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      NULL
    })
    
    models_list[[i]] <- model
  }
}

# Selecting the models corresponding to the panels
selected_models_A <- c(1, 2, 3, 4)  # Full Sample
selected_models_B <- c(5, 6, 7, 8)  # Without Previous Gender Reservation
selected_models_C <- c(9, 10, 11, 12)  # With Previous Gender Reservation
panel_A_models_selected <- models_list[selected_models_A]
panel_B_models_selected <- models_list[selected_models_B]
panel_C_models_selected <- models_list[selected_models_C]

# Implement FWER correction (Part Two)
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(panel_A_models_selected, outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(panel_B_models_selected, outregvar2)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(panel_C_models_selected, outregvar2)

# Extracting the adjusted p-values for our models (Part Two)
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(panel_A_models_selected))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2, length(panel_B_models_selected))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2, length(panel_C_models_selected))

# Print results for each panel (Part Two)
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: Full Sample (INT_treatment only)",
                       control_means, dep_vars, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Without Previous Gender Reservation (RES05_gender == 0, INT_treatment only)",
                       control_means, dep_vars, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: With Previous Gender Reservation (RES05_gender == 1, INT_treatment only)",
                       control_means, dep_vars, selected_models_C)

# Saving the results of Part Two
sink("~/work/fwer_table3_part2_results.txt")
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "SELECTED MODELS: Full Sample (INT_treatment only)",
                       control_means, dep_vars, selected_models_A)
print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Without Previous Gender Reservation (RES05_gender == 0, INT_treatment only)",
                       control_means, dep_vars, selected_models_B)
print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: With Previous Gender Reservation (RES05_gender == 1, INT_treatment only)",
                       control_means, dep_vars, selected_models_C)
sink()




# PREPARING THE TEX FILE: extracting results from part 2
panel_names_part2 <- c("WHOLE SAMPLE", "RES05_gender = 0", "RES05_gender = 1")
selected_models_part2 <- list(selected_models_A, selected_models_B, selected_models_C)
pvals_by_var_part2 <- list(pvals_A_by_var_selected, pvals_B_by_var_selected, pvals_C_by_var_selected)
dep_vars_part2 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_voteshare_female", "CHAL_voteshare_nongen")

results_part2 <- list()
for (i in 1:3) {
  results_part2[[i]] <- extract_panel_results(
    models_list, pvals_by_var_part2[[i]], dep_vars_part2, selected_models_part2[[i]], panel_names_part2[i]
  )
}
results_part2 <- bind_rows(results_part2)
results_part2$Family <- "Treatment Effect on Vote Share"


############### OUTPUTS ############

## FINAL OUTPUT (TXT)

# Merge the temporary files we saved
final_file <- "~/work/Table_3_final_fwer.txt"
writeLines(readLines("~/work/fwer_table3_part1_results.txt"), final_file)
con <- file(final_file, open = "a")
writeLines(readLines("~/work/fwer_table3_part2_results.txt"), con)
close(con)

# Delete temporary files
file.remove("~/work/fwer_table3_part1_results.txt", "~/work/fwer_table3_part2_results.txt")




### TEX OUTPUT ###

library(dplyr)

# Function to extract results from a panel:
extract_panel_results <- function(models, pvalues_by_var, dep_vars, selected_models, panel_name, family_name) {
  results <- list()
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[(model_index - 1) %% length(dep_vars) + 1]
      var <- "INT_treatment"
      if (var %in% rownames(coef_table)) {
        estimate <- round(coef_table[var, "Estimate"], 3)
        p_value <- round(coef_table[var, "Pr(>|t|)"], 3)
        fwer <- round(pvalues_by_var[[var]][i], 3)
        results[[i]] <- data.frame(
          Family = family_name,
          Panel = panel_name,
          Outcome = dep_var_name,
          Estimate = estimate,
          P.value = p_value,
          FWER = fwer,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(data.frame())
  }
}

# extract part 1 results (prop)
panel_names_part1 <- c("WHOLE SAMPLE", "RES05_gender = 0", "RES05_gender = 1")
selected_models_part1 <- list(selected_models_A, selected_models_B, selected_models_C)
pvals_by_var_part1 <- list(pvals_A_by_var_selected, pvals_B_by_var_selected, pvals_C_by_var_selected)
dep_vars_part1 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", "CHAL_prop_nongen")

results_part1 <- list()
for (i in 1:3) {
  results_part1[[i]] <- extract_panel_results(
    models_list, pvals_by_var_part1[[i]], dep_vars_part1, selected_models_part1[[i]], panel_names_part1[i], "Treatment Effect on Running"
  )
}
results_part1 <- bind_rows(results_part1)

# extract part2 results (vote share)
panel_names_part2 <- c("WHOLE SAMPLE", "RES05_gender = 0", "RES05_gender = 1")
selected_models_part2 <- list(selected_models_A, selected_models_B, selected_models_C)
pvals_by_var_part2 <- list(pvals_A_by_var_selected, pvals_B_by_var_selected, pvals_C_by_var_selected)
dep_vars_part2 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_voteshare_female", "CHAL_voteshare_nongen")

results_part2 <- list()
for (i in 1:3) {
  results_part2[[i]] <- extract_panel_results(
    models_list, pvals_by_var_part2[[i]], dep_vars_part2, selected_models_part2[[i]], panel_names_part2[i], "Treatment Effect on Vote Share"
  )
}
results_part2 <- bind_rows(results_part2)


# Combine the sections
final_results <- bind_rows(results_part1, results_part2)

str(final_results)

# structure of the table
final_table <- final_results %>%
  dplyr::select(Family, Panel, Outcome, Estimate, P.value, FWER) %>%
  dplyr::arrange(Family, Panel, Outcome)

# display
print(final_table, row.names = FALSE)




# GENERATE tex script

sink("~/work/Table_3_final.tex")

cat("\\documentclass{article}
\\begin{document}

\\begin{table}[htbp]
\\centering
\\caption{Treatment Effects}
\\label{tab:treatment_effects}
\\begin{tabular}{lccc}
\\hline\\hline
& \\multicolumn{3}{c}{Treatment Effect on Running} \\\\ \\cline{2-4}
Outcome & Estimate & P.value & FWER \\\\ \\hline
")

# "Treatment Effect on Running"
running_data <- final_table[final_table$Family == "Treatment Effect on Running", ]
for (panel in unique(running_data$Panel)) {
  # label (no underscores)
  panel_label <- gsub("_", " ", panel)
  cat("\\multicolumn{4}{l}{\\textbf{" , panel_label, "}} \\\\ \\hline\n", sep = "")
  panel_data <- running_data[running_data$Panel == panel, ]
  for (i in 1:nrow(panel_data)) {
    row <- panel_data[i, ]
    
    outcome_label <- gsub("_", " ", row$Outcome)
    cat("\\texttt{" , outcome_label, "}", "& ", row$Estimate, " & ", row$P.value, " & ", row$FWER, row$Significance, " \\\\\n", sep = "")
  }
}

cat("\\hline
& \\multicolumn{3}{c}{Treatment Effect on Vote Share} \\\\ \\cline{2-4}
Outcome & Estimate & P.value & FWER \\\\ \\hline
")

# "Treatment Effect on Vote Share"
vote_data <- final_table[final_table$Family == "Treatment Effect on Vote Share", ]
for (panel in unique(vote_data$Panel)) {
  # label
  panel_label <- gsub("_", " ", panel)
  cat("\\multicolumn{4}{l}{\\textbf{" , panel_label, "}} \\\\ \\hline\n", sep = "")
  panel_data <- vote_data[vote_data$Panel == panel, ]
  for (i in 1:nrow(panel_data)) {
    row <- panel_data[i, ]
    
    outcome_label <- gsub("_", " ", row$Outcome)
    cat("\\texttt{" , outcome_label, "}", "& ", row$Estimate, " & ", row$P.value, " & ", row$FWER, row$Significance, " \\\\\n", sep = "")
  }
}

cat("\\hline\\hline
\\end{tabular}
\\end{table}

\\end{document}")
sink()

