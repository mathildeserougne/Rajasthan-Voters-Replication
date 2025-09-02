# FWER correction method for the Table 4, following original panel
# Candidate entry in the next election



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
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run)
  )

# Generate new variables
for (var in c("INT_treatment", "X_anytr_genderres05", "RES05_gender")) {
  data_filtered <- data_filtered %>%
    mutate(!!paste0("X15_", var) := get(var) * (RES15_gender == 1))
}

# Interest variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Dependent variables
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")


# Estimating the models (whole sample)
models_list <- list()
control_means <- numeric(length(dep_vars))

for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[i] <- control_mean
  
  # model estimation
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols15), collapse = " + "), "+ factor(district)"))
  model <- tryCatch({
    lm(formula, data = data_filtered)
  }, error = function(e) {
    message("Error in model fitting: ", e$message)
    NULL
  })
  
  models_list[[i]] <- model
}


# Function to extract the p-values and apply the FWER correction
get_adjusted_pvalues <- function(models, var_names) {
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
pvalues_adjusted <- get_adjusted_pvalues(models_list, outregvar2)


# Function to extract each adjusted p-value
get_pvals_by_var <- function(pvalues_df, var_names, n_models) {
  pvals_list <- list()
  for (i in 1:length(var_names)) {
    var <- var_names[i]
    idx <- seq(i, nrow(pvalues_df), by = length(var_names))
    pvals_list[[var]] <- pvalues_df$adjusted[idx]
  }
  return(pvals_list)
}

# Extracting adjusted p-values
pvals_by_var <- get_pvals_by_var(pvalues_adjusted, outregvar2, length(dep_vars))


# Function to print results, with both types of p-values
print_results <- function(models, pvals_by_var, control_means, dep_vars) {
  cat("\n\n======================================================================\n")
  cat("Whole Sample (FWER-adjusted)\n")
  cat("======================================================================\n\n")
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    if (!is.null(model)) {
      coef_table <- summary(model)$coefficients
      dep_var_name <- dep_vars[i]
      
      cat("--- Model ", i, ": ", dep_var_name, " ---\n", sep = "")
      cat("Variable                     | Coeff (Std. Error) | p-value | FWER-adj p\n")
      cat("-----------------------------|--------------------|---------|------------\n")
      
      for (var in outregvar2) {
        if (var %in% rownames(coef_table)) {
          coef_val <- round(coef_table[var, "Estimate"], 4)
          se_val <- round(coef_table[var, "Std. Error"], 4)
          pval_raw <- round(coef_table[var, "Pr(>|t|)"], 4)
          pval_adj <- round(pvals_by_var[[var]][i], 4)
          
          # if wanted, keep significance stars
          stars <- ifelse(pval_adj < 0.01, "***",
                          ifelse(pval_adj < 0.05, "**",
                                 ifelse(pval_adj < 0.1, "*", "")))
          
          cat(sprintf("%-28s | %4.3f (%4.3f)       | %5.4f   | %6.4f%s\n",
                      var, coef_val, se_val, pval_raw, pval_adj, stars))
        }
      }
      cat("Mean in Control not WR in 2015: ", control_means[i], "\n")
      cat("Observations: ", nobs(model), "\n\n")
    }
  }
}

# Print results
print_results(models_list, pvals_by_var, control_means, dep_vars)






