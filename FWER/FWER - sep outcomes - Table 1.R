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
