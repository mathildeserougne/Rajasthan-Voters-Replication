# FWER for Table 1, this time with new panels.

# We keep the same Panel A (whole sample).
# We add a Panel B: without gender quota in 2005.
# We add a Panel C: with gender quota in 2005.

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
library(multcomp)


# Controls and dependent variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Loading and filtering the data
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Function for regression formulas
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


# Function to extract p-values and apply fwer correction
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


# Model estimation for each panel
models_list <- list()
control_means <- list()

# Panel A : mean effect, whole sample of observations
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
  models_list[[paste0("A_", i)]] <- model
}

# Panel B : RES05_gender == 0
data_panel_B <- data_filtered %>% filter(RES05_gender == 0)
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  control_mean <- data_panel_B %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means[[i + length(incum_dep_vars1)]] <- control_mean
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_panel_B)
  models_list[[paste0("B_", i)]] <- model
}

# Panel C : RES05_gender == 1
data_panel_C <- data_filtered %>% filter(RES05_gender == 1)
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  control_mean <- data_panel_C %>%
    filter(INT_treatment == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means[[i + 2 * length(incum_dep_vars1)]] <- control_mean
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_panel_C)
  models_list[[paste0("C_", i)]] <- model
}


# Variables for each panel
# Knowing that by construction, no variation of RES05_gender in B, C
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
panel_B_vars <- c("INT_treatment")  # RES05_gender constant = 0
panel_C_vars <- c("INT_treatment")  # RES05_gender constant = 1



# Extracting the models for each panel
panel_A_models <- models_list[1:6]
panel_B_models <- models_list[7:12]
panel_C_models <- models_list[13:18]

# Implementation of FWER correction
pvalues_panel_A <- get_adjusted_pvalues(panel_A_models, panel_A_vars)
pvalues_panel_B <- get_adjusted_pvalues(panel_B_models, panel_B_vars)
pvalues_panel_C <- get_adjusted_pvalues(panel_C_models, panel_C_vars)


# Function to extract the adjusted p-values
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
pvals_C_by_var <- get_pvals_by_var(pvalues_panel_C, panel_C_vars, length(panel_C_models))



# Function to display the results
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

# names to display
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# Displaying the results (print)
print_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, "PANEL A: Average Effects")
print_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, "PANEL B: No Quota in 2005")
print_panel_results(panel_C_models, panel_C_vars, pvals_C_by_var, "PANEL C: Quota in 2005")





#################################################################################
#### Output in .tex file ####



## version corrigée avec les noms des variables

# output path
output_path <- "~/work/FWER_table1.tex"

# reference of the variable names
var_labels <- c(
  "INT_treatment" = "Treatment",
  "RES05_gender" = "Reserved for Women (2005)",
  "INT_treatment:RES05_gender" = "Treatment × Reserved for Women (2005)",
  "INC05_running" = "Incumbent Runs",
  "INC05_voteshare" = "Incumbent Vote Share",
  "INCSPOUSE05_running" = "Incumbent Spouse Runs",
  "INCSPOUSE05_voteshare" = "Incumbent Spouse Vote Share",
  "INCOTHER05_running" = "Other Family Member Runs",
  "INCOTHER05_voteshare" = "Other Family Member Vote Share"
)

# Fonction pour extraire les résultats des modèles avec les noms de variables modifiés
extract_panel_results <- function(models, var_names, pvals_by_var, var_labels) {
  panel_results <- list()
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    dep_var_name <- names(models)[i]
    
    # Extraire les coefficients, erreurs standards, p-values
    coefs <- data.frame(
      Variable = rownames(coef_table),
      Coeff = coef_table[, "Estimate"],
      SE = coef_table[, "Std. Error"],
      P = coef_table[, "Pr(>|t|)"]
    )
    
    # Ajouter les p-values ajustées (FWER)
    for (var in var_names) {
      if (var %in% rownames(coef_table)) {
        idx <- which(var_names == var)
        coefs$FWER[coefs$Variable == var] <- pvals_by_var[[var]][i]
      }
    }
    
    # Filtrer uniquement les variables d'intérêt
    coefs <- coefs[coefs$Variable %in% var_names, ]
    
    # Remplacer les noms des variables par les étiquettes
    coefs$Variable <- var_labels[coefs$Variable]
    
    # Nom de la variable dépendante (pour l'affichage)
    dep_var_label <- var_labels[dep_var_name]
    
    panel_results[[i]] <- list(
      dep_var = dep_var_label,
      coefs = coefs
    )
  }
  return(panel_results)
}

# Fonction pour écrire les résultats dans un fichier .tex
write_panel_to_tex <- function(file_path, panel_name, panel_results) {
  file <- file(file_path, open = "at")
  
  cat("\\begin{table}[htbp]\n", file = file)
  cat("\\centering\n", file = file)
  cat("\\caption{", panel_name, "}\n", file = file)
  cat("\\label{tab:", gsub(" ", "_", tolower(panel_name)), "}\n", file = file)
  cat("\\begin{tabular}{lccc}\n", file = file)
  cat("\\toprule\n", file = file)
  cat("\\multicolumn{4}{c}{", panel_name, "} \\\\\n", file = file)
  cat("\\midrule\n", file = file)
  
  for (result in panel_results) {
    cat("\\multicolumn{4}{l}{--- ", result$dep_var, " ---} \\\\\n", file = file)
    cat("\\cmidrule(lr){1-4}\n", file = file)
    cat("Variable & Coefficient (Std. Error) & p-value & FWER-adj p \\\\\n", file = file)
    cat("\\midrule\n", file = file)
    
    for (i in 1:nrow(result$coefs)) {
      var_name <- result$coefs$Variable[i]
      coef_val <- round(result$coefs$Coeff[i], 4)
      se_val <- round(result$coefs$SE[i], 4)
      p_val <- round(result$coefs$P[i], 4)
      fwer_p <- round(result$coefs$FWER[i], 4)
      
      cat(var_name, " & ", coef_val, " (", se_val, ") & ", p_val, " & ", fwer_p, " \\\\\n", file = file)
    }
  }
  
  cat("\\bottomrule\n", file = file)
  cat("\\end{tabular}\n", file = file)
  cat("\\end{table}\n\n", file = file)
  
  close(file)
}

# Écrire chaque panel dans le fichier .tex
file <- file(output_path, open = "wt")
cat(
  c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{amsmath}",
    "\\begin{document}\n"
  ),
  file = file, sep = "\n"
)
close(file)

# Extraire les résultats pour chaque panel avec les noms modifiés
panel_A_results <- extract_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, var_labels)
panel_B_results <- extract_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, var_labels)
panel_C_results <- extract_panel_results(panel_C_models, panel_C_vars, pvals_C_by_var, var_labels)

# Écrire chaque panel dans le fichier .tex
write_panel_to_tex(output_path, "PANEL A: Average Effects", panel_A_results)
write_panel_to_tex(output_path, "PANEL B: No Quota in 2005", panel_B_results)
write_panel_to_tex(output_path, "PANEL C: Quota in 2005", panel_C_results)

file <- file(output_path, open = "at")
cat("\\end{document}", file = file)
close(file)




#################################################################################

#### Output in .tex file ####

# output path
output_path <- "~/work/FWER_table1.tex"

# Referencing the variable's names
var_labels <- c(
  "INT_treatment" = "Treatment",
  "RES05_gender" = "Reserved for Women (2005)",
  "INT_treatment:RES05_gender" = "Treatment × Reserved for Women (2005)"
)

# List of dependent variables, in the order of the models
dep_var_labels <- c(
  "Incumbent Runs",
  "Incumbent Vote Share",
  "Incumbent Spouse Runs",
  "Incumbent Spouse Vote Share",
  "Other Family Member Runs",
  "Other Family Member Vote Share"
)

# Extract models' results, with the right labels
extract_panel_results <- function(models, var_names, pvals_by_var, var_labels) {
  panel_results <- list()
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    
    # extract coeffs, std errors, p-values
    coefs <- data.frame(
      Variable = rownames(coef_table),
      Coeff = coef_table[, "Estimate"],
      SE = coef_table[, "Std. Error"],
      P = coef_table[, "Pr(>|t|)"]
    )
    
    # add adjusted p-values
    for (var in var_names) {
      if (var %in% rownames(coef_table)) {
        idx <- which(var_names == var)
        coefs$FWER[coefs$Variable == var] <- pvals_by_var[[var]][i]
      }
    }
    
    # filter
    coefs <- coefs[coefs$Variable %in% var_names, ]
    
    # replace variables' names by labels (for compilation purposes)
    coefs$Variable <- var_labels[coefs$Variable]
    
    panel_results[[i]] <- coefs
  }
  return(panel_results)
}


# Function to write the results in a .tex
write_panel_to_tex <- function(file_path, panel_name, panel_results) {
  file <- file(file_path, open = "at")
  
  cat("\\begin{table}[htbp]\n", file = file)
  cat("\\centering\n", file = file)
  cat("\\caption{", panel_name, "}\n", file = file)
  cat("\\label{tab:", gsub(" ", "_", tolower(panel_name)), "}\n", file = file)
  cat("\\begin{tabular}{lccc}\n", file = file)
  cat("\\toprule\n", file = file)
  cat("\\multicolumn{4}{c}{", panel_name, "} \\\\\n", file = file)
  cat("\\midrule\n", file = file)
  
  for (i in 1:length(panel_results)) {
    result <- panel_results[[i]]
    dep_var_label <- dep_var_labels[i] 
    
    # add a line with the name of the dpdt variable
    cat("\\multicolumn{4}{l}{\\textbf{", dep_var_label, "}} \\\\\n", file = file)
    cat("\\cmidrule(lr){1-4}\n", file = file)
    cat("Variable & Coefficient (Std. Error) & p-value & FWER-adj p \\\\\n", file = file)
    cat("\\midrule\n", file = file)
    
    for (j in 1:nrow(result)) {
      var_name <- result$Variable[j]
      coef_val <- round(result$Coeff[j], 4)
      se_val <- round(result$SE[j], 4)
      p_val <- round(result$P[j], 4)
      fwer_p <- round(result$FWER[j], 4)
      
      cat(var_name, " & ", coef_val, " (", se_val, ") & ", p_val, " & ", fwer_p, " \\\\\n", file = file)
    }
  }
  
  cat("\\bottomrule\n", file = file)
  cat("\\end{tabular}\n", file = file)
  cat("\\end{table}\n\n", file = file)
  
  close(file)
}

# Initiation of the .tex script
file <- file(output_path, open = "wt")
cat(
  c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{amsmath}",
    "\\begin{document}\n"
  ),
  file = file,
  sep = "\n"
)
close(file)

# Extract results for each panel
panel_A_results <- extract_panel_results(panel_A_models, panel_A_vars, pvals_A_by_var, var_labels)
panel_B_results <- extract_panel_results(panel_B_models, panel_B_vars, pvals_B_by_var, var_labels)
panel_C_results <- extract_panel_results(panel_C_models, panel_C_vars, pvals_C_by_var, var_labels)

# Write each panel in the .tex
write_panel_to_tex(output_path, "PANEL A: Average Effects", panel_A_results)
write_panel_to_tex(output_path, "PANEL B: No Quota in 2005", panel_B_results)
write_panel_to_tex(output_path, "PANEL C: Quota in 2005", panel_C_results)

file <- file(output_path, open = "at")
cat("\\end{document}", file = file)
close(file)
