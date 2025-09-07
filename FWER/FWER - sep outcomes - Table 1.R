# FWER - new panels - table 1
# one family per outcome (running/voteshare)
# all subsamples within the same family.


# we will obtain two families (two tables with different types of outcomes)
# table 1 - runs.
# one panel only. treatment, treatmentxRES==0, treatmentxRES==1
# columns: incumbent runs, spouse runs, other runs.

# table 2 - voteshare.
# one panel only. treatment, treatmentxRES==0, treatmentxRES==1
# columns: incumbent voteshare, spouse voteshare, other voteshare.


install.packages(c("haven","dplyr"))
library(haven)

# LIBRARIES 

library(tidyverse)
library(broom)
library(fixest)


# DATA

# Loading and filtering
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

data_filtered <- data_filtered %>%
  mutate(RES05_gender = as.factor(RES05_gender))


# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Dpdt variables for "running" 
running_dep_vars <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
# Dpdt variables "voteshare"
voteshare_dep_vars <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")

# Function for the regressions
create_formula <- function(dep_var) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  formula_str <- paste(dep_var, "~ INT_treatment * RES05_gender +", base_controls, "+ factor(district)")
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

# Extracting adjusted p-value
get_pvals_by_var <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Function to display the results
print_results <- function(models, var_names, pvalues_by_var, dep_var_names) {
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    dep_var_name <- dep_var_names[i]
    cat("\n---", dep_var_name, "---\n")
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
  }
}




## RUNNING

# List to stock models
running_models <- list()
# Variables
running_vars <- c("INT_treatment", "RES05_gender1", "INT_treatment:RES05_gender1")

# Model estimation "running"
for (i in 1:length(running_dep_vars)) {
  dep_var <- running_dep_vars[i]
  formula <- create_formula(dep_var)
  model <- lm(formula, data = data_filtered)
  running_models[[i]] <- model
}

# Extracting adjusted p-values
pvalues_running <- get_adjusted_pvalues(running_models, running_vars)
pvals_running_by_var <- get_pvals_by_var(pvalues_running, running_vars, length(running_models))


# Printing running results
cat("\n\n======================================================================\n")
cat("TABLE 1: TREATMENT EFFECT ON RUNNING \n")
cat("======================================================================\n")
print_results(running_models, running_vars, pvals_running_by_var,
              c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs"))





## FOR VOTESHARE 

# List to stock the models
voteshare_models <- list()
# Variables
voteshare_vars <- c("INT_treatment", "RES05_gender1", "INT_treatment:RES05_gender1")

# Model estimation for "voteshare"
for (i in 1:length(voteshare_dep_vars)) {
  dep_var <- voteshare_dep_vars[i]
  formula <- create_formula(dep_var)
  model <- lm(formula, data = data_filtered)
  voteshare_models[[i]] <- model
}

# Extracting adjusted p-values
pvalues_voteshare <- get_adjusted_pvalues(voteshare_models, voteshare_vars)
pvals_voteshare_by_var <- get_pvals_by_var(pvalues_voteshare, voteshare_vars, length(voteshare_models))

# Displaying results "voteshare"
cat("\n\n======================================================================\n")
cat("TABLE 2: TREATMENT EFFECT ON VOTESHARE \n")
cat("======================================================================\n")
print_results(voteshare_models, voteshare_vars, pvals_voteshare_by_var,
              c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share"))




### .TEX OUTPUT ###############################################################



# Chemin de sortie
output_path <- "~/work/FWER_sep_o_table1.tex"

# Étiquettes des variables
var_labels <- c(
  "INT_treatment" = "Treatment",
  "RES05_gender1" = "Reserved for Women (2005)",
  "INT_treatment:RES05_gender1" = "Treatment × Reserved for Women (2005)"
)

# Étiquettes des variables dépendantes (dans l'ordre des modèles)
running_dep_var_labels <- c(
  "Incumbent Runs",
  "Spouse Runs",
  "Other Family Member Runs"
)

voteshare_dep_var_labels <- c(
  "Incumbent Vote Share",
  "Spouse Vote Share",
  "Other Family Member Vote Share"
)

# Fonction pour extraire les résultats des modèles
extract_panel_results <- function(models, var_names, pvals_by_var, var_labels) {
  panel_results <- list()
  for (i in 1:length(models)) {
    model <- models[[i]]
    coef_table <- summary(model)$coefficients
    
    # Extraire coefficients, erreurs standards, p-values
    coefs <- data.frame(
      Variable = rownames(coef_table),
      Coeff = coef_table[, "Estimate"],
      SE = coef_table[, "Std. Error"],
      P = coef_table[, "Pr(>|t|)"]
    )
    
    # Ajouter les p-values FWER ajustées
    for (var in var_names) {
      if (var %in% rownames(coef_table)) {
        coefs$FWER[coefs$Variable == var] <- pvals_by_var[[var]][i]
      }
    }
    
    # Filtrer pour ne garder que les variables d'intérêt
    coefs <- coefs[coefs$Variable %in% var_names, ]
    
    # Remplacer les noms des variables par les étiquettes
    coefs$Variable <- var_labels[coefs$Variable]
    
    panel_results[[i]] <- coefs
  }
  return(panel_results)
}

# Fonction pour écrire les résultats dans un fichier .tex
write_panel_to_tex <- function(file_path, panel_name, panel_results, dep_var_labels) {
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
    
    # Ajouter une ligne avec le nom de la variable dépendante
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

# Initialisation du fichier .tex
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

# Extraire les résultats pour "running"
running_results <- extract_panel_results(
  running_models, running_vars, pvals_running_by_var, var_labels
)

# Extraire les résultats pour "voteshare"
voteshare_results <- extract_panel_results(
  voteshare_models, voteshare_vars, pvals_voteshare_by_var, var_labels
)

# Écrire les résultats dans le fichier .tex
write_panel_to_tex(
  output_path,
  "Treatment Effect on Running",
  running_results,
  running_dep_var_labels
)

write_panel_to_tex(
  output_path,
  "Treatment Effect on Vote Share",
  voteshare_results,
  voteshare_dep_var_labels
)

# Fermer le document LaTeX
file <- file(output_path, open = "at")
cat("\\end{document}", file = file)
close(file)




##### DOING IT WITH A SPLIT ALONG PREVIOUS GENDER RESERVATION ####
# six subtables




# subsamples
data_whole <- data_filtered
data_RES05_gender_0 <- data_filtered %>% filter(RES05_gender == 0)
data_RES05_gender_1 <- data_filtered %>% filter(RES05_gender == 1)

# formulas
create_formula <- function(dep_var, include_interaction) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  if (include_interaction) {
    as.formula(paste(dep_var, "~ INT_treatment * RES05_gender +", base_controls, "+ factor(district)"))
  } else {
    as.formula(paste(dep_var, "~ INT_treatment +", base_controls, "+ factor(district)"))
  }
}

# extract p values
get_pvalues <- function(model, var_names) {
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients
  pvals <- coef_table[var_names, "Pr(>|t|)"]
  return(as.numeric(pvals))
}

# adjust p values (fwer)
calculate_fwer <- function(models, var_names) {
  all_pvalues <- c()
  for (model in models) {
    if (!is.null(model)) {
      pvals <- get_pvalues(model, var_names)
      all_pvalues <- c(all_pvalues, pvals)
    }
  }
  adjusted_pvalues <- p.adjust(all_pvalues, method = "holm")
  return(adjusted_pvalues)
}

# extract model results
get_model_results <- function(model, var_names, var_labels, p_adjusted, offset = 0) {
  if (is.null(model)) return(NULL)
  
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients
  coef_table <- coef_table[var_names, , drop = FALSE]
  
  if (nrow(coef_table) == 0) return(NULL)
  
  result_df <- data.frame(
    Variable = rownames(coef_table),
    Estimate = coef_table[, "Estimate"],
    P.value = coef_table[, "Pr(>|t|)"]
  )
  
  # add adjusted p values
  result_df$FWER <- p_adjusted[(1:nrow(result_df)) + offset]
  
  # label variables
  result_df$Variable <- var_labels[result_df$Variable]
  
  return(result_df)
}

# model estimation and extraction of results

## Whole Sample 
var_names_whole <- c("INT_treatment", "RES05_gender1", "INT_treatment:RES05_gender1")
var_names_sub <- c("INT_treatment")

# Running - Whole Sample
running_models_whole <- lapply(running_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, TRUE), data = data_whole)
})
pvalues_running_whole <- get_pvalues(running_models_whole[[1]], var_names_whole)
adjusted_pvalues_running_whole <- calculate_fwer(running_models_whole, var_names_whole)
running_results_whole <- lapply(1:length(running_dep_vars), function(i) {
  get_model_results(running_models_whole[[i]], var_names_whole, var_labels,
                    adjusted_pvalues_running_whole, (i-1)*length(var_names_whole))
})

# Vote Share - Whole Sample
voteshare_models_whole <- lapply(voteshare_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, TRUE), data = data_whole)
})
pvalues_voteshare_whole <- get_pvalues(voteshare_models_whole[[1]], var_names_whole)
adjusted_pvalues_voteshare_whole <- calculate_fwer(voteshare_models_whole, var_names_whole)
voteshare_results_whole <- lapply(1:length(voteshare_dep_vars), function(i) {
  get_model_results(voteshare_models_whole[[i]], var_names_whole, var_labels,
                    adjusted_pvalues_voteshare_whole, (i-1)*length(var_names_whole))
})

# Running - RES05_gender = 0
running_models_gender0 <- lapply(running_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, FALSE), data = data_RES05_gender_0)
})
pvalues_running_gender0 <- get_pvalues(running_models_gender0[[1]], var_names_sub)
adjusted_pvalues_running_gender0 <- calculate_fwer(running_models_gender0, var_names_sub)
running_results_gender0 <- lapply(1:length(running_dep_vars), function(i) {
  get_model_results(running_models_gender0[[i]], var_names_sub, var_labels,
                    adjusted_pvalues_running_gender0, (i-1)*length(var_names_sub))
})

# Vote Share - RES05_gender = 0
voteshare_models_gender0 <- lapply(voteshare_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, FALSE), data = data_RES05_gender_0)
})
pvalues_voteshare_gender0 <- get_pvalues(voteshare_models_gender0[[1]], var_names_sub)
adjusted_pvalues_voteshare_gender0 <- calculate_fwer(voteshare_models_gender0, var_names_sub)
voteshare_results_gender0 <- lapply(1:length(voteshare_dep_vars), function(i) {
  get_model_results(voteshare_models_gender0[[i]], var_names_sub, var_labels,
                    adjusted_pvalues_voteshare_gender0, (i-1)*length(var_names_sub))
})

# Running - RES05_gender = 1
running_models_gender1 <- lapply(running_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, FALSE), data = data_RES05_gender_1)
})
pvalues_running_gender1 <- get_pvalues(running_models_gender1[[1]], var_names_sub)
adjusted_pvalues_running_gender1 <- calculate_fwer(running_models_gender1, var_names_sub)
running_results_gender1 <- lapply(1:length(running_dep_vars), function(i) {
  get_model_results(running_models_gender1[[i]], var_names_sub, var_labels,
                    adjusted_pvalues_running_gender1, (i-1)*length(var_names_sub))
})

# Vote Share - RES05_gender = 1
voteshare_models_gender1 <- lapply(voteshare_dep_vars, function(dep_var) {
  lm(create_formula(dep_var, FALSE), data = data_RES05_gender_1)
})
pvalues_voteshare_gender1 <- get_pvalues(voteshare_models_gender1[[1]], var_names_sub)
adjusted_pvalues_voteshare_gender1 <- calculate_fwer(voteshare_models_gender1, var_names_sub)
voteshare_results_gender1 <- lapply(1:length(voteshare_dep_vars), function(i) {
  get_model_results(voteshare_models_gender1[[i]], var_names_sub, var_labels,
                    adjusted_pvalues_voteshare_gender1, (i-1)*length(var_names_sub))
})

# display results in the console
print_results <- function(results_list, dep_var_labels, sample_name, panel_name) {
  cat("\n\n")
  cat("======================================================================\n")
  cat(panel_name, " (", sample_name, ")\n")
  cat("======================================================================\n")
  
  for (i in seq_along(results_list)) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      dep_var_label <- dep_var_labels[i]
      cat("\n--- ", dep_var_label, " ---\n")
      cat(sprintf("%-35s %-15s %-15s %-15s\n", "Variable", "Estimate", "p-value", "FWER-adj p"))
      cat("----------------------------------------------------------------------\n")
      
      for (j in 1:nrow(result)) {
        var_name <- as.character(result$Variable[j])
        estimate <- round(result$Estimate[j], 4)
        p_val <- format.pval(result$P.value[j], digits = 3)
        fwer_val <- format.pval(result$FWER[j], digits = 3)
        
        cat(sprintf("%-35s %-15s %-15s %-15s\n",
                    var_name,
                    estimate,
                    p_val,
                    fwer_val))
      }
    }
  }
}

# display:

# Running
print_results(running_results_whole, running_dep_var_labels, "Whole Sample", "Treatment Effect on Running")
print_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Running")
print_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Running")

# Vote Share
print_results(voteshare_results_whole, voteshare_dep_var_labels, "Whole Sample", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Vote Share")


