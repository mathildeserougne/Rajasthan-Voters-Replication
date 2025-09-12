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

# Dependent variables 
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running",
              "ELEC15_prop_cand2010", "ELEC15_prop_female", "ELEC15_prop_nongen")

# Estimation of the models
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample
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
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    
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
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
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
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
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
selected_models_A <- 1:length(dep_vars)  # Full Sample
selected_models_B <- (length(dep_vars) + 1):(2 * length(dep_vars))  # Without Previous Gender Reservation
selected_models_C <- ((2 * length(dep_vars) + 1)):(3 * length(dep_vars))  # With Previous Gender Reservation

# Variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar2_B <- c("INT_treatment")
outregvar2_C <- c("INT_treatment")

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
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(models_list[selected_models_A], outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(models_list[selected_models_B], outregvar2_B)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(models_list[selected_models_C], outregvar2_C)

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
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(selected_models_A))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2_B, length(selected_models_B))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2_C, length(selected_models_C))

# Function to print the results with both types of p-values
print_selected_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, selected_models, dep_vars) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[i]
      
      cat("--- Model ", model_index, ": ", dep_var_name, " ---\n", sep = "")
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
                       "Panel A: Full Sample",
                       control_means, selected_models_A, dep_vars)
print_selected_results(models_list, outregvar2_B, pvals_B_by_var_selected,
                       "Panel B: Without Previous Gender Reservation (RES05_gender == 0)",
                       control_means, selected_models_B, dep_vars)
print_selected_results(models_list, outregvar2_C, pvals_C_by_var_selected,
                       "Panel C: With Previous Gender Reservation (RES05_gender == 1)",
                       control_means, selected_models_C, dep_vars)



# saving the results from the first family
sink("~/work/fwer_table4_part1_results.txt")
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "Panel A: Full Sample",
                       control_means, selected_models_A, dep_vars)
print_selected_results(models_list, outregvar2_B, pvals_B_by_var_selected,
                       "Panel B: Without Previous Gender Reservation (RES05_gender == 0)",
                       control_means, selected_models_B, dep_vars)
print_selected_results(models_list, outregvar2_C, pvals_C_by_var_selected,
                       "Panel C: With Previous Gender Reservation (RES05_gender == 1)",
                       control_means, selected_models_C, dep_vars)
sink()




## VOTESHARE

# FWER correction on Table 4, candidate entry in next election.
# New panels added.
# A = baseline table. B = without GQ. C = with GQ.
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

# Dependent variables (only those for FWER and output)
dep_vars <- c("ELEC15_nbcands", "ELEC15_voteshare_incum10",
              "ELEC15_voteshare_cand2010", "ELEC15_voteshare_female",
              "ELEC15_voteshare_nongen")

# Estimation of the models
models_list <- list()
control_means <- numeric(length(dep_vars) * 3)
i <- 0

# Panel A: Full Sample
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
  all_vars <- c(dep_var, "INT_treatment", "RES05_gender", "X_anytr_genderres05", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
    
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
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
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
  all_vars <- c(dep_var, "INT_treatment", gpcontrols15, "district")
  if (all(all_vars %in% names(data_filtered))) {
    formula <- as.formula(paste(dep_var, "~ INT_treatment +",
                                paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
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
selected_models_A <- 1:length(dep_vars)  # Full Sample
selected_models_B <- (length(dep_vars) + 1):(2 * length(dep_vars))  # Without Previous Gender Reservation
selected_models_C <- ((2 * length(dep_vars) + 1)):(3 * length(dep_vars))  # With Previous Gender Reservation

# Variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar2_B <- c("INT_treatment")
outregvar2_C <- c("INT_treatment")

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
pvalues_panel_A_selected <- get_adjusted_pvalues_selected(models_list[selected_models_A], outregvar2)
pvalues_panel_B_selected <- get_adjusted_pvalues_selected(models_list[selected_models_B], outregvar2_B)
pvalues_panel_C_selected <- get_adjusted_pvalues_selected(models_list[selected_models_C], outregvar2_C)

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
pvals_A_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_A_selected, outregvar2, length(selected_models_A))
pvals_B_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_B_selected, outregvar2_B, length(selected_models_B))
pvals_C_by_var_selected <- get_pvals_by_var_selected(pvalues_panel_C_selected, outregvar2_C, length(selected_models_C))

# Function to print the results with both types of p-values
print_selected_results <- function(models, var_names, pvalues_by_var, panel_name, control_means, selected_models, dep_vars) {
  cat("\n\n======================================================================\n")
  cat(panel_name, "\n")
  cat("======================================================================\n\n")
  
  for (i in 1:length(selected_models)) {
    model_index <- selected_models[i]
    model <- models[[model_index]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[i]
      
      cat("--- Model ", model_index, ": ", dep_var_name, " ---\n", sep = "")
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
                       "Panel A: Full Sample",
                       control_means, selected_models_A, dep_vars)
print_selected_results(models_list, outregvar2_B, pvals_B_by_var_selected,
                       "Panel B: Without Previous Gender Reservation (RES05_gender == 0)",
                       control_means, selected_models_B, dep_vars)
print_selected_results(models_list, outregvar2_C, pvals_C_by_var_selected,
                       "Panel C: With Previous Gender Reservation (RES05_gender == 1)",
                       control_means, selected_models_C, dep_vars)


# saving results from the family 2

sink("~/work/fwer_table4_part2_results.txt")
print_selected_results(models_list, outregvar2, pvals_A_by_var_selected,
                       "Panel A: Full Sample",
                       control_means, selected_models_A, dep_vars)
print_selected_results(models_list, outregvar2_B, pvals_B_by_var_selected,
                       "Panel B: Without Previous Gender Reservation (RES05_gender == 0)",
                       control_means, selected_models_B, dep_vars)
print_selected_results(models_list, outregvar2_C, pvals_C_by_var_selected,
                       "Panel C: With Previous Gender Reservation (RES05_gender == 1)",
                       control_means, selected_models_C, dep_vars)
sink()



## OUTPUT ##

# merge the two previously saved series of results
final_file <- "~/work/Table_4_final_fwer.txt"


writeLines(readLines("~/work/fwer_table4_part1_results.txt"), final_file)

con <- file(final_file, open = "a")
writeLines(readLines("~/work/fwer_table4_part2_results.txt"), con)
close(con)

# delete temporary files
file.remove("~/work/fwer_table4_part1_results.txt", "~/work/fwer_table4_part2_results.txt")

