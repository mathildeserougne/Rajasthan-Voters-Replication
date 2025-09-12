# table 4 final presentation: split outcomes, split samples, fwer, pure effect


# Required libraries
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
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# Load the data
data <- read_dta("~/work/Electoral data 2015 cleaned.dta")

# Function to write the results from a panel
extract_panel_results <- function(models, pvalues_by_var, var_names, dep_vars, selected_models, panel_name, family_name) {
  results <- list()
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[i]
      
      for (var in var_names) {
        if (var %in% rownames(coef_table)) {
          estimate <- round(coef_table[var, "Estimate"], 3)
          p_value <- round(coef_table[var, "Pr(>|t|)"], 3)
          fwer <- round(pvalues_by_var[[var]][i], 3)
          
          results[[length(results) + 1]] <- data.frame(
            Family = family_name,
            Panel = panel_name,
            Outcome = dep_var_name,
            Variable = var,
            Estimate = estimate,
            P.value = p_value,
            FWER = fwer,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(data.frame())
  }
}

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



#################### PART 1: CANDIDATE ENTRY 

# Filter the data
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1, RES15_gender == 0) %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run),
    X_anytr_genderres05 = INT_treatment * RES05_gender
  )

# Dependent variables for Candidate Entry
dep_vars_part1 <- c("ELEC15_nbcands", "ELEC15_incum10_running",
                    "ELEC15_prop_cand2010", "ELEC15_prop_female", "ELEC15_prop_nongen")

# Estimation of the models for Candidate Entry
models_list_part1 <- list()
control_means_part1 <- numeric(length(dep_vars_part1) * 3)
i <- 0

# Panel A: Full Sample
for (dep_var in dep_vars_part1) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part1[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered), error = function(e) NULL)
    models_list_part1[[i]] <- model
  }
}

# Panel B: RES05_gender == 0
for (dep_var in dep_vars_part1) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part1[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered %>% filter(RES05_gender == 0)), error = function(e) NULL)
    models_list_part1[[i]] <- model
  }
}

# Panel C: RES05_gender == 1
for (dep_var in dep_vars_part1) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part1[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered %>% filter(RES05_gender == 1)), error = function(e) NULL)
    models_list_part1[[i]] <- model
  }
}

# Selecting the models corresponding to the panels for Candidate Entry
selected_models_A_part1 <- 1:length(dep_vars_part1)
selected_models_B_part1 <- (length(dep_vars_part1) + 1):(2 * length(dep_vars_part1))
selected_models_C_part1 <- ((2 * length(dep_vars_part1) + 1)):(3 * length(dep_vars_part1))

# Variables for Candidate Entry
outregvar2_part1 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar2_B_part1 <- c("INT_treatment")
outregvar2_C_part1 <- c("INT_treatment")

# Implement FWER correction for Candidate Entry
pvalues_panel_A_part1 <- get_adjusted_pvalues_selected(models_list_part1[selected_models_A_part1], outregvar2_part1)
pvalues_panel_B_part1 <- get_adjusted_pvalues_selected(models_list_part1[selected_models_B_part1], outregvar2_B_part1)
pvalues_panel_C_part1 <- get_adjusted_pvalues_selected(models_list_part1[selected_models_C_part1], outregvar2_C_part1)

# Extracting the adjusted p-values for Candidate Entry
pvals_A_by_var_part1 <- get_pvals_by_var_selected(pvalues_panel_A_part1, outregvar2_part1, length(selected_models_A_part1))
pvals_B_by_var_part1 <- get_pvals_by_var_selected(pvalues_panel_B_part1, outregvar2_B_part1, length(selected_models_B_part1))
pvals_C_by_var_part1 <- get_pvals_by_var_selected(pvalues_panel_C_part1, outregvar2_C_part1, length(selected_models_C_part1))

# Extract results for Candidate Entry
results_part1_A <- extract_panel_results(models_list_part1, pvals_A_by_var_part1, outregvar2_part1, dep_vars_part1, selected_models_A_part1, "WHOLE SAMPLE", "Candidate Entry")
results_part1_B <- extract_panel_results(models_list_part1, pvals_B_by_var_part1, outregvar2_B_part1, dep_vars_part1, selected_models_B_part1, "RES05 gender = 0", "Candidate Entry")
results_part1_C <- extract_panel_results(models_list_part1, pvals_C_by_var_part1, outregvar2_C_part1, dep_vars_part1, selected_models_C_part1, "RES05 gender = 1", "Candidate Entry")
results_part1 <- bind_rows(results_part1_A, results_part1_B, results_part1_C)





########################### PART 2: VOTE SHARE ###########################


# Dependent variables for Vote Share
dep_vars_part2 <- c("ELEC15_nbcands", "ELEC15_voteshare_incum10",
                    "ELEC15_voteshare_cand2010", "ELEC15_voteshare_female",
                    "ELEC15_voteshare_nongen")

# Estimation of the models for Vote Share
models_list_part2 <- list()
control_means_part2 <- numeric(length(dep_vars_part2) * 3)
i <- 0

# Panel A: Full Sample
for (dep_var in dep_vars_part2) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part2[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered), error = function(e) NULL)
    models_list_part2[[i]] <- model
  }
}

# Panel B: RES05_gender == 0
for (dep_var in dep_vars_part2) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part2[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered %>% filter(RES05_gender == 0)), error = function(e) NULL)
    models_list_part2[[i]] <- model
  }
}

# Panel C: RES05_gender == 1
for (dep_var in dep_vars_part2) {
  i <- i + 1
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  control_means_part2[i] <- control_mean
  
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    model <- tryCatch(lm(formula, data = data_filtered %>% filter(RES05_gender == 1)), error = function(e) NULL)
    models_list_part2[[i]] <- model
  }
}

# Selecting the models corresponding to the panels for Vote Share
selected_models_A_part2 <- 1:length(dep_vars_part2)
selected_models_B_part2 <- (length(dep_vars_part2) + 1):(2 * length(dep_vars_part2))
selected_models_C_part2 <- ((2 * length(dep_vars_part2) + 1)):(3 * length(dep_vars_part2))

# Variables for Vote Share
outregvar2_part2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar2_B_part2 <- c("INT_treatment")
outregvar2_C_part2 <- c("INT_treatment")

# Implement FWER correction for Vote Share
pvalues_panel_A_part2 <- get_adjusted_pvalues_selected(models_list_part2[selected_models_A_part2], outregvar2_part2)
pvalues_panel_B_part2 <- get_adjusted_pvalues_selected(models_list_part2[selected_models_B_part2], outregvar2_B_part2)
pvalues_panel_C_part2 <- get_adjusted_pvalues_selected(models_list_part2[selected_models_C_part2], outregvar2_C_part2)

# Extracting the adjusted p-values for Vote Share
pvals_A_by_var_part2 <- get_pvals_by_var_selected(pvalues_panel_A_part2, outregvar2_part2, length(selected_models_A_part2))
pvals_B_by_var_part2 <- get_pvals_by_var_selected(pvalues_panel_B_part2, outregvar2_B_part2, length(selected_models_B_part2))
pvals_C_by_var_part2 <- get_pvals_by_var_selected(pvalues_panel_C_part2, outregvar2_C_part2, length(selected_models_C_part2))

# Extract results for Vote Share
results_part2_A <- extract_panel_results(models_list_part2, pvals_A_by_var_part2, outregvar2_part2, dep_vars_part2, selected_models_A_part2, "WHOLE SAMPLE", "Vote Share")
results_part2_B <- extract_panel_results(models_list_part2, pvals_B_by_var_part2, outregvar2_B_part2, dep_vars_part2, selected_models_B_part2, "RES05 gender = 0", "Vote Share")
results_part2_C <- extract_panel_results(models_list_part2, pvals_C_by_var_part2, outregvar2_C_part2, dep_vars_part2, selected_models_C_part2, "RES05 gender = 1", "Vote Share")
results_part2 <- bind_rows(results_part2_A, results_part2_B, results_part2_C)




########################## COMBINE RESULTS AND GENERATE LATEX ######


# Combine results from both parts
final_results <- bind_rows(results_part1, results_part2)

# stars
add_significance_stars <- function(fwer) {
  if (fwer < 0.01) {
    return("***")
  } else if (fwer < 0.05) {
    return("**")
  } else if (fwer < 0.1) {
    return("*")
  } else {
    return("")
  }
}

final_results$Significance <- sapply(final_results$FWER, add_significance_stars)

# Generate tex script

sink("~/work/Table_4_final.tex")

cat("\\documentclass{article}
\\usepackage{booktabs}
\\usepackage{threeparttable}
\\usepackage{adjustbox}

\\begin{document}

\\begin{table}[htbp]
\\centering
\\caption{Treatment Effects on Candidate Entry and Vote Share}
\\label{tab:treatment_effects_table4}
\\resizebox{\\textwidth}{!}{
\\begin{tabular}{lcccc}
\\toprule
& & \\multicolumn{3}{c}{Candidate Entry} \\\\ \\cmidrule(lr){3-5}
Panel & Outcome & Variable & Estimate & FWER \\\\ \\midrule
")

# family "Candidate Entry"
candidate_entry_data <- final_results[final_results$Family == "Candidate Entry", ]
for (panel in c("WHOLE SAMPLE", "RES05 gender = 0", "RES05 gender = 1")) {
  panel_data <- candidate_entry_data[candidate_entry_data$Panel == panel, ]
  if (nrow(panel_data) > 0) {
    cat("\\multicolumn{5}{l}{\\textbf{" , panel, "}} \\\\ \\midrule\n", sep = "")
    for (outcome in unique(panel_data$Outcome)) {
      outcome_label <- gsub("_", " ", outcome)
      outcome_data <- panel_data[panel_data$Outcome == outcome, ]
      for (i in 1:nrow(outcome_data)) {
        row <- outcome_data[i, ]
        variable_label <- gsub("_", " ", row$Variable)
        cat("\\multicolumn{1}{l}{\\textit{" , outcome_label, "}} & ",
            "\\multicolumn{1}{l}{\\texttt{" , variable_label, "}} & ",
            row$Estimate, " & ", row$FWER, ifelse(row$Significance != "", paste0(" ", row$Significance), ""), " \\\\\n", sep = "")
      }
    }
  }
}

cat("\\midrule
& & \\multicolumn{3}{c}{Vote Share} \\\\ \\cmidrule(lr){3-5}
Panel & Outcome & Variable & Estimate & FWER \\\\ \\midrule
")

# family "Vote Share"
vote_share_data <- final_results[final_results$Family == "Vote Share", ]
for (panel in c("WHOLE SAMPLE", "RES05 gender = 0", "RES05 gender = 1")) {
  panel_data <- vote_share_data[vote_share_data$Panel == panel, ]
  if (nrow(panel_data) > 0) {
    cat("\\multicolumn{5}{l}{\\textbf{" , panel, "}} \\\\ \\midrule\n", sep = "")
    for (outcome in unique(panel_data$Outcome)) {
      outcome_label <- gsub("_", " ", outcome)
      outcome_data <- panel_data[panel_data$Outcome == outcome, ]
      for (i in 1:nrow(outcome_data)) {
        row <- outcome_data[i, ]
        variable_label <- gsub("_", " ", row$Variable)
        cat("\\multicolumn{1}{l}{\\textit{" , outcome_label, "}} & ",
            "\\multicolumn{1}{l}{\\texttt{" , variable_label, "}} & ",
            row$Estimate, " & ", row$FWER, ifelse(row$Significance != "", paste0(" ", row$Significance), ""), " \\\\\n", sep = "")
      }
    }
  }
}

cat("\\bottomrule
\\end{tabular}
}
\\begin{tablenotes}
\\small
\\item Note: Stars indicate significance levels: *$p<0.1$, **$p<0.05$, ***$p<0.01$.
\\end{tablenotes}
\\end{table}

\\end{document}")
sink()

