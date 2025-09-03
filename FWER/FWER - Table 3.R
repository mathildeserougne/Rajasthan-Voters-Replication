# FWER on Table 3, challenger entry
# with new panels, based on previous gender reservation.
# Baseline panels: whole sample, inc can run, inc cannot run
# New panels: A - whole sample, B - RES05_gender==0, C - RES05_gender==1


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
  filter(RES10_gender == 0, GP_tag == 1) %>%
  mutate(X_anytr_genderres05 = INT_treatment * RES05_gender)  # Ajout de l'interaction

# Dependent variables
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# Estimation of the models
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample
for (dep_var in dep_vars) {
  i <- i + 1
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation
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
  
  # model estimation
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
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
  
  # model estimation
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
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
selected_models_A <- c(1, 2, 3, 4, 5, 6)  # Full Sample
selected_models_B <- c(7, 8, 9, 10, 11, 12)  # Without Previous Gender Reservation
selected_models_C <- c(13, 14, 15, 16, 17, 18)  # With Previous Gender Reservation

panel_A_models_selected <- models_list[selected_models_A]
panel_B_models_selected <- models_list[selected_models_B]
panel_C_models_selected <- models_list[selected_models_C]


# Variables d'intérêt
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

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
          pval_adj_idx <- which(var_names == var) + (i - 1) * length(var_names)
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
                       "SELECTED MODELS: Full Sample",
                       control_means, dep_vars, selected_models_A)

print_selected_results(models_list, outregvar2, pvals_B_by_var_selected,
                       "SELECTED MODELS: Without Previous Gender Reservation (RES05_gender == 0)",
                       control_means, dep_vars, selected_models_B)

print_selected_results(models_list, outregvar2, pvals_C_by_var_selected,
                       "SELECTED MODELS: With Previous Gender Reservation (RES05_gender == 1)",
                       control_means, dep_vars, selected_models_C)





#### .tex output ####

# output path
output_path <- "~/work/FWER_table3.tex"

# change labels of variables (easier to read + to compile)
var_labels <- c(
  "INT_treatment" = "Treatment",
  "RES05_gender" = "Reserved for Women (2005)",
  "X_anytr_genderres05" = "Treatment$\\times$Reserved for Women (2005)",
  "ELEC10_nbcands" = "Number of Candidates 2010",
  "CHAL_nbchal" = "Number of Challengers",
  "CHAL_prop_female" = "Proportion of Female Challengers",
  "CHAL_voteshare_female" = "Vote Share of Female Challengers",
  "CHAL_prop_nongen" = "Proportion of NonGeneral Challengers",
  "CHAL_voteshare_nongen" = "Vote Share of NonGeneral Challengers"
)

# extract previous results from the models
extract_panel_results <- function(models, var_names, pvals_by_var, dep_vars, selected_models, control_means) {
  panel_results <- list()
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[(model_index-1) %% length(dep_vars) + 1]
      
      coefs <- data.frame(
        Variable = rownames(coef_table),
        Coeff = coef_table[, "Estimate"],
        SE = coef_table[, "Std. Error"],
        P = coef_table[, "Pr(>|t|)"]
      )
      
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          idx <- which(var_names == var)
          coefs$FWER[coefs$Variable == var] <- pvals_by_var[[var]][i]
        }
      }
      
      coefs <- coefs[coefs$Variable %in% var_names, ]
      coefs$Variable <- var_labels[match(coefs$Variable, names(var_labels))]
      
      # no underscores (to compile)
      dep_var_label <- gsub("_", " ", dep_var_name)
      
      panel_results[[i]] <- list(
        dep_var = dep_var_label,
        coefs = coefs,
        control_mean = control_means[model_index],
        observations = nobs(model)
      )
    }
  }
  return(panel_results)
}

# extracting for each panel
panel_A_results <- extract_panel_results(
  models_list, outregvar2, pvals_A_by_var_selected,
  dep_vars, selected_models_A, control_means
)

panel_B_results <- extract_panel_results(
  models_list, c("INT_treatment"), pvals_B_by_var_selected,
  dep_vars, selected_models_B, control_means
)

panel_C_results <- extract_panel_results(
  models_list, c("INT_treatment"), pvals_C_by_var_selected,
  dep_vars, selected_models_C, control_means
)


file <- file(output_path, open = "wt")
cat(
  c(
    "\\documentclass[a4paper, 12pt]{article}",
    "\\usepackage[margin=2cm]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{amsmath}",
    "\\usepackage{pdflscape}",
    "\\usepackage{graphicx}",
    "\\usepackage{longtable}",
    "\\begin{document}"
  ),
  file = file, sep = "\n"
)
close(file)

# .tex writing
write_panel_to_tex <- function(file_path, panel_name, panel_results) {
  file <- file(file_path, open = "at")
  
  cat("\\begin{landscape}\n", file = file)
  cat("\\begin{longtable}{lccc}\n", file = file)
  cat("\\caption{" , panel_name, "} \\\\\n", file = file)
  cat("\\label{tab:", gsub("[^a-zA-Z0-9]", "", tolower(panel_name)), "} \\\\\n", file = file)
  cat("\\toprule\n", file = file)
  cat("\\multicolumn{4}{c}{", panel_name, "} \\\\\n", file = file)
  cat("\\midrule\n", file = file)
  cat("\\endfirsthead\n", file = file)
  cat("\\toprule\n", file = file)
  cat("\\multicolumn{4}{c}{", panel_name, "} \\\\\n", file = file)
  cat("\\midrule\n", file = file)
  cat("\\endhead\n", file = file)
  
  for (result in panel_results) {
    cat("\\multicolumn{4}{l}{--- ", result$dep_var, " ---} \\\\\n", file = file)
    cat("\\midrule\n", file = file)
    cat("Variable & Coeff (Std. Error) & p-value & FWER-adj p \\\\\n", file = file)
    
    for (i in 1:nrow(result$coefs)) {
      var_name <- result$coefs$Variable[i]
      coef_val <- round(result$coefs$Coeff[i], 4)
      se_val <- round(result$coefs$SE[i], 4)
      p_val <- round(result$coefs$P[i], 4)
      fwer_p <- round(result$coefs$FWER[i], 4)
      
      cat(var_name, " & ", coef_val, " (", se_val, ") & ", p_val, " & ", fwer_p, " \\\\\n", file = file)
    }
    cat("\\midrule\n", file = file)
    cat("Control Mean: ", result$control_mean, " & & & \\\\\n", file = file)
    cat("Observations: ", result$observations, " & & & \\\\\n", file = file)
  }
  cat("\\bottomrule\n", file = file)
  cat("\\end{longtable}\n", file = file)
  cat("\\end{landscape}\n\n", file = file)
  
  close(file)
}

# write each panel
write_panel_to_tex(output_path, "Panel A Full Sample", panel_A_results)
write_panel_to_tex(output_path, "Panel B Without Previous Gender Reservation", panel_B_results)
write_panel_to_tex(output_path, "Panel C With Previous Gender Reservation", panel_C_results)

# close .tex file
file <- file(output_path, open = "at")
cat("\\end{document}", file = file)
close(file)

