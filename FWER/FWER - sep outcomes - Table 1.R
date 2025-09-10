# FWER - new panels - table 1
# one family per outcome (running/voteshare)


# we will obtain two families (two tables with different types of outcomes)
# table 1 - runs.
# panels: treatment, treatmentxRES==0, treatmentxRES==1 (cannot mix subsamples)
# columns: incumbent runs, spouse runs, other runs.

# table 2 - voteshare.
# treatment, treatmentxRES==0, treatmentxRES==1
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



# Define labels for your dependent variables
running_dep_var_labels <- c("INC05 running", "INCSPOUSE05 running", "INCOTHER 05 running")
voteshare_dep_var_labels <- c("INC05 vote share", "INCSPOUSE05 vote share", "INCOTHER 05 vote share")

# Define labels for your variables of interest
var_labels <- c(
  "INT_treatment" = "Treatment",
  "RES05_gender1" = "Previously gender reserved",
  "INT_treatment:RES05_gender1" = "Interaction Treatment × GQ"
)


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




## .TEX OUTPUT ##

export_all_to_latex <- function(file_path = "~/work/FWER_sep_outcomes_Table1.tex") {
  # results listed
  all_results <- list(
    list(results = running_results_whole, labels = c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs"), sample = "WholeSample", panel = "TreatmentEffectOnRunning"),
    list(results = running_results_gender0, labels = c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs"), sample = "RES05gender0", panel = "TreatmentEffectOnRunning"),
    list(results = running_results_gender1, labels = c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs"), sample = "RES05gender1", panel = "TreatmentEffectOnRunning"),
    list(results = voteshare_results_whole, labels = c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share"), sample = "WholeSample", panel = "TreatmentEffectOnVoteShare"),
    list(results = voteshare_results_gender0, labels = c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share"), sample = "RES05gender0", panel = "TreatmentEffectOnVoteShare"),
    list(results = voteshare_results_gender1, labels = c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share"), sample = "RES05gender1", panel = "TreatmentEffectOnVoteShare")
  )

  # open .tex file
  sink(file_path, append = FALSE, split = TRUE)

  # initiate script
  cat("%% LaTeX document with all regression tables\n")
  cat("\\documentclass{article}\n")
  cat("\\usepackage{booktabs}\n")
  cat("\\usepackage{siunitx} % Pour le formatage des nombres\n")
  cat("\\usepackage{array} % Pour les colonnes ajustables\n")
  cat("\\usepackage{caption}\n")
  cat("\\usepackage{threeparttable} % Pour les notes\n")
  cat("\\sisetup{detect-all, output-decimal-marker = {,}} % Formatage des nombres\n")
  cat("\\begin{document}\n\n")

  # loop on each panel
  for (result_set in all_results) {
    results_list <- result_set$results
    dep_var_labels <- result_set$labels
    sample_name <- result_set$sample
    panel_name <- result_set$panel

    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    cat("\\caption{", gsub("([a-z])([A-Z])", "\\1 \\2", panel_name), " (", gsub("([a-z])([A-Z])", "\\1 \\2", sample_name), ")}\n")
    cat("\\label{tab:", tolower(gsub("([a-z])([A-Z])", "\\1_\\2", panel_name)), "_", tolower(sample_name), "}\n")
    cat("\\begin{threeparttable}\n")
    cat("\\begin{tabular}{@{}lSSS@{}}\n")
    cat("\\toprule\n")
    cat("& {Estimate} & {p-value} & {FWER-adj p} \\\\ \\midrule\n")

    for (i in seq_along(results_list)) {
      result <- results_list[[i]]
      if (!is.null(result) && nrow(result) > 0) {
        dep_var_label <- dep_var_labels[i]
        cat("\\multicolumn{4}{l}{\\textit{", dep_var_label, "}} \\\\ \\midrule\n")

        for (j in 1:nrow(result)) {
          var_name <- as.character(result$Variable[j])
          estimate <- result$Estimate[j]
          p_val <- result$P.value[j]
          fwer_val <- result$FWER[j]

          # format number
          cat(var_name, " & ", sprintf("%.4f", estimate), " & ", format.pval(p_val, digits = 3), " & ", format.pval(fwer_val, digits = 3), " \\\\\n")
        }
      }
    }

    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{threeparttable}\n")
    cat("\\end{table}\n")
    cat("\\clearpage \n\n")
  }

  cat("\\end{document}\n")
  sink()
}

# export
export_all_to_latex(file_path = "~/work/FWER_sep_outcomes_Table1.tex")
