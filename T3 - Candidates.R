### REPLICATION - TABLE 3 - CHALLENGER ENTRY ###############


# Libraries (install packages if necessary)
library(dplyr)
library(fixest)  # Pour les régressions avec effets fixes
library(stargazer)  # Pour les tableaux de sortie
library(haven)    # Pour lire les fichiers Stata (.dta)
library(broom)    # Pour extraire les résultats de régression
library(aod)      # Pour les tests de Wald


## DEFINING THE MACROS 

# Controls on GP
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", 
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", 
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", 
                "RES05_sc", "RES05_st")

# Main regression variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Dependent variables
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", 
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")


## DATA PROCESSING

# Change path to load your data
data_path <- "~/work/Electoral data cleaned.dta"

# Reading
cat("Reading the data\n")
data <- read_dta(data_path)

# Check the structure of the data (i think it is useless now that it works)
cat("Data dimensions:", dim(data), "\n")
cat("Main present variables:\n")
key_vars <- c("RES10_gender", "GP_tag", "INT_treatment", "RES05_gender", 
              "X_anytr_genderres05", "INC05_can_run", "district")
print(key_vars[key_vars %in% names(data)])

# Filtering the data (exclude gender-reserved GPs, keep one observation per GP)
cat("Filtering. \n")
data_filtered <- data %>%
  filter(RES10_gender == 0, 
         GP_tag == 1)       

cat("Data after filtering:", nrow(data_filtered), "observations\n")


## FUNCTION FOR THE REGRESSIONS

run_regression_analysis <- function(data_subset, subset_name) {
  
  cat(paste("\n=== ANALYZE FOR:", subset_name, "===\n"))
  cat("Number of observations:", nrow(data_subset), "\n")
  
  results_list <- list()
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    cat(paste(" Dependent variable:", dep_var, "\n"))
    
    # check existence of variable (there again, could be taken out now that it works)
    if (!dep_var %in% names(data_subset)) {
      cat(paste("    ATTENTION: Variable", dep_var, "not found!\n"))
      next
    }
    
    # compute control mean
    control_subset <- data_subset %>%
      filter(INT_treatment == 0, RES05_gender == 0)
    
    if (nrow(control_subset) == 0) {
      cat(paste("    ATTENTION: no control observation for ", dep_var, "\n"))
      control_mean <- NA
    } else {
      control_mean <- control_subset %>%
        summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean_val) %>%
        round(2)
    }
    
    cat(paste("    Control mean (non-previously gender-reserved):", control_mean, "\n"))
    
    # check presence of control variables (could be taken out now that it works)
    available_controls <- gpcontrols[gpcontrols %in% names(data_subset)]
    missing_controls <- gpcontrols[!gpcontrols %in% names(data_subset)]
    
    if (length(missing_controls) > 0) {
      cat(paste("    Missing control variables:", paste(missing_controls, collapse = ", "), "\n"))
    }
    
    # Create regression formula with available variables
    reg_vars <- c(outregvar2, available_controls)
    reg_vars <- reg_vars[reg_vars %in% names(data_subset)]
    
    formula_str <- paste(dep_var, "~", 
                         paste(reg_vars, collapse = " + "), "+ factor(district)")
    
    cat(paste("    Formule:", formula_str, "\n"))
    
    # Do the regression
    reg_formula <- as.formula(formula_str)
    
    tryCatch({
      model <- lm(reg_formula, data = data_subset)
      
      # joint test: RES05_gender + X_anytr_genderres05 = 0
      coef_names <- names(coef(model))
      
      # look for coefficients
      res05_coef <- coef_names[grepl("RES05_gender", coef_names)]
      anytr_coef <- coef_names[grepl("X_anytr_genderres05", coef_names)]
      
      if (length(res05_coef) > 0 && length(anytr_coef) > 0) {
        # wald test
        vcov_matrix <- vcov(model)
        coef_vector <- coef(model)
        
        # contrast matrix
        L <- matrix(0, nrow = 1, ncol = length(coef_vector))
        names_L <- names(coef_vector)
        L[1, which(names_L == res05_coef[1])] <- 1
        L[1, which(names_L == anytr_coef[1])] <- 1
        
        # wald statistics
        restriction <- L %*% coef_vector
        var_restriction <- L %*% vcov_matrix %*% t(L)
        wald_stat <- as.numeric(restriction^2 / var_restriction)
        pval <- round(1 - pchisq(wald_stat, df = 1), 3)
        
        cat(paste("    Joint test p-value:", pval, "\n"))
      } else {
        pval <- NA
        cat("    Joint test: variables not found\n")
      }
      
      # stock the results
      results_list[[i]] <- list(
        model = model,
        dep_var = dep_var,
        control_mean = control_mean,
        joint_test_pval = pval,
        subset = subset_name,
        formula = formula_str,
        n_obs = nrow(model$model)
      )
      
      cat(paste("    Successful regression, N =", nrow(model$model), "\n"))
      
    }, error = function(e) {
      cat(paste("    ERROR in the regression:", e$message, "\n"))
      results_list[[i]] <- NULL
    })
  }
  
  # clean NULL results
  results_list <- results_list[!sapply(results_list, is.null)]
  
  return(results_list)
}


## ANALYSIS FOR THE THREE SUB-SAMPLES

# 1. Panel A sample: All GPs
cat("All GPs \n")
results_full <- run_regression_analysis(data_filtered, "Full Sample")

# 2. Panel B sample: Incumbent can run
cat("INC05_can_run == 1...\n")
data_inc_can_run <- data_filtered %>% filter(INC05_can_run == 1)
results_inc_can <- run_regression_analysis(data_inc_can_run, "Incumbent Can Run")

# 3. Panel C sample: Incumbent cannot run
cat("INC05_can_run == 0...\n")
data_inc_cannot_run <- data_filtered %>% filter(INC05_can_run == 0)
results_inc_cannot <- run_regression_analysis(data_inc_cannot_run, "Incumbent Cannot Run")



### FULL RESULTS TABLE

create_outreg_table <- function(results_list_full, results_list_inc, results_list_no_inc) {
  
  cat("\n=== Generating the output table ===\n")
  
  # extracting coefficients from the model
  extract_model_results <- function(model_result) {
    if (is.null(model_result) || is.null(model_result$model)) {
      return(NULL)
    }
    
    model <- model_result$model
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    # extract coefficients of interest variables
    results <- list()
    
    for (var in outregvar2) {
      # look for the variable
      matching_vars <- rownames(coef_table)[grepl(var, rownames(coef_table))]
      
      if (length(matching_vars) > 0) {
        var_name <- matching_vars[1]  # take first match
        
        coef_val <- coef_table[var_name, "Estimate"]
        se_val <- coef_table[var_name, "Std. Error"]
        pval <- coef_table[var_name, "Pr(>|t|)"]
        
        # stars
        stars <- ""
        if (pval < 0.01) stars <- "***"
        else if (pval < 0.05) stars <- "**"
        else if (pval < 0.1) stars <- "*"
        
        results[[var]] <- list(
          coef = round(coef_val, 3),
          se = round(se_val, 3),
          pval = pval,
          stars = stars,
          coef_formatted = paste0(round(coef_val, 3), stars),
          se_formatted = paste0("(", round(se_val, 3), ")")
        )
      } else {
        results[[var]] <- list(
          coef = NA,
          se = NA,
          pval = NA,
          stars = "",
          coef_formatted = "NA",
          se_formatted = "(NA)"
        )
      }
    }
    
    return(results)
  }
  
  # final table
  final_table <- data.frame(
    Variable = character(),
    Full_Sample = character(),
    Inc_Can_Run = character(),
    Inc_Cannot_Run = character(),
    stringsAsFactors = FALSE
  )
  
  # for each dependent variable
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    cat(paste("\nTreatment of", dep_var, "...\n"))
    
    # extract results for each subsample
    if (i <= length(results_list_full)) {
      full_results <- extract_model_results(results_list_full[[i]])
    } else {
      full_results <- NULL
    }
    
    if (i <= length(results_list_inc)) {
      inc_results <- extract_model_results(results_list_inc[[i]])
    } else {
      inc_results <- NULL
    }
    
    if (i <= length(results_list_no_inc)) {
      no_inc_results <- extract_model_results(results_list_no_inc[[i]])
    } else {
      no_inc_results <- NULL
    }
    
    # rows for each explanatory variable
    for (var in outregvar2) {
      
      # coefficients row
      coef_row <- data.frame(
        Variable = var,
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$coef_formatted else "NA",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$coef_formatted else "NA",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$coef_formatted else "NA",
        stringsAsFactors = FALSE
      )
      
      # std errors
      se_row <- data.frame(
        Variable = paste0("  ", var, "_se"),
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$se_formatted else "(NA)",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$se_formatted else "(NA)",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$se_formatted else "(NA)",
        stringsAsFactors = FALSE
      )
      
      final_table <- rbind(final_table, coef_row, se_row)
    }
    
    # visual separators
    if (i < length(dep_vars)) {
      sep_row <- data.frame(
        Variable = paste("--- End of", dep_var, "---"),
        Full_Sample = "",
        Inc_Can_Run = "",
        Inc_Cannot_Run = "",
        stringsAsFactors = FALSE
      )
      final_table <- rbind(final_table, sep_row)
    }
  }
  
  return(final_table)
}

# summary stats (not used in the paper, might delete that as well)
create_summary_stats <- function(results_list_full, results_list_inc, results_list_no_inc) {
  
  summary_table <- data.frame(
    Dep_Var = character(),
    Full_Control_Mean = numeric(),
    Full_Joint_Test = numeric(),
    Inc_Control_Mean = numeric(),
    Inc_Joint_Test = numeric(),
    NoInc_Control_Mean = numeric(),
    NoInc_Joint_Test = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    row_data <- data.frame(
      Dep_Var = dep_var,
      Full_Control_Mean = if (i <= length(results_list_full) && !is.null(results_list_full[[i]])) 
        results_list_full[[i]]$control_mean else NA,
      Full_Joint_Test = if (i <= length(results_list_full) && !is.null(results_list_full[[i]])) 
        results_list_full[[i]]$joint_test_pval else NA,
      Inc_Control_Mean = if (i <= length(results_list_inc) && !is.null(results_list_inc[[i]])) 
        results_list_inc[[i]]$control_mean else NA,
      Inc_Joint_Test = if (i <= length(results_list_inc) && !is.null(results_list_inc[[i]])) 
        results_list_inc[[i]]$joint_test_pval else NA,
      NoInc_Control_Mean = if (i <= length(results_list_no_inc) && !is.null(results_list_no_inc[[i]])) 
        results_list_no_inc[[i]]$control_mean else NA,
      NoInc_Joint_Test = if (i <= length(results_list_no_inc) && !is.null(results_list_no_inc[[i]])) 
        results_list_no_inc[[i]]$joint_test_pval else NA,
      stringsAsFactors = FALSE
    )
    
    summary_table <- rbind(summary_table, row_data)
  }
  
  return(summary_table)
}

# Create the main table
main_table <- create_outreg_table(results_full, results_inc_can, results_inc_cannot)

# Create summary table
summary_stats <- create_summary_stats(results_full, results_inc_can, results_inc_cannot)






## DISPLAYING AND SAVING THE RESULTS

# Outlook...
cat("\n=== Regression results ===\n")
print(main_table)

# Formatted clean table with stargazer

# Combine all models
all_models <- c(
  lapply(results_full, function(x) x$model),
  lapply(results_inc_can, function(x) x$model),
  lapply(results_inc_cannot, function(x) x$model)
)

# Columns names
col_names <- c(
  paste("Full Sample", 1:6),
  paste("Inc Can Run", 1:6),
  paste("Inc Cannot Run", 1:6)
)

# Generate table
stargazer(all_models,
          type = "text",  
          column.labels = col_names,
          keep = outregvar2,
          add.lines = list(
            c("District FE", rep("Yes", length(all_models))),
            c("GP Controls", rep("Yes", length(all_models)))
          ),
          digits = 2,
          title = "Table 3: Challenger entry",
          out = "Table3_Challengers_2010.txt")





# ALTERNATIVE OUTPUT, MORE SUMMARY STATS (like in the article)
## VERSION KEPT !!


create_outreg_table <- function(results_list_full, results_list_inc, results_list_no_inc) {
  
  cat("\n=== Generating the output table ===\n")
  
  # Extracting coefficients from the model
  extract_model_results <- function(model_result) {
    if (is.null(model_result) || is.null(model_result$model)) {
      return(NULL)
    }
    
    model <- model_result$model
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    # Extract coefficients of interest variables
    results <- list()
    
    for (var in outregvar2) {
      # Look for the variable
      matching_vars <- rownames(coef_table)[grepl(var, rownames(coef_table))]
      
      if (length(matching_vars) > 0) {
        var_name <- matching_vars[1]  # Take first match
        
        coef_val <- coef_table[var_name, "Estimate"]
        se_val <- coef_table[var_name, "Std. Error"]
        pval <- coef_table[var_name, "Pr(>|t|)"]
        
        # Stars
        stars <- ""
        if (pval < 0.01) stars <- "***"
        else if (pval < 0.05) stars <- "**"
        else if (pval < 0.1) stars <- "*"
        
        results[[var]] <- list(
          coef = round(coef_val, 3),
          se = round(se_val, 3),
          pval = pval,
          stars = stars,
          coef_formatted = paste0(round(coef_val, 3), stars),
          se_formatted = paste0("(", round(se_val, 3), ")")
        )
      } else {
        results[[var]] <- list(
          coef = NA,
          se = NA,
          pval = NA,
          stars = "",
          coef_formatted = "NA",
          se_formatted = "(NA)"
        )
      }
    }
    
    return(results)
  }
  
  # Final table
  final_table <- data.frame(
    Variable = character(),
    Full_Sample = character(),
    Inc_Can_Run = character(),
    Inc_Cannot_Run = character(),
    stringsAsFactors = FALSE
  )
  
  # For each dependent variable
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    cat(paste("\nTreatment of", dep_var, "...\n"))
    
    # Extract results for each subsample
    if (i <= length(results_list_full)) {
      full_results <- extract_model_results(results_list_full[[i]])
    } else {
      full_results <- NULL
    }
    
    if (i <= length(results_list_inc)) {
      inc_results <- extract_model_results(results_list_inc[[i]])
    } else {
      inc_results <- NULL
    }
    
    if (i <= length(results_list_no_inc)) {
      no_inc_results <- extract_model_results(results_list_no_inc[[i]])
    } else {
      no_inc_results <- NULL
    }
    
    # Rows for each explanatory variable
    for (var in outregvar2) {
      
      # Coefficients row
      coef_row <- data.frame(
        Variable = var,
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$coef_formatted else "NA",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$coef_formatted else "NA",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$coef_formatted else "NA",
        stringsAsFactors = FALSE
      )
      
      # Std errors
      se_row <- data.frame(
        Variable = paste0("  ", var, "_se"),
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$se_formatted else "(NA)",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$se_formatted else "(NA)",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$se_formatted else "(NA)",
        stringsAsFactors = FALSE
      )
      
      final_table <- rbind(final_table, coef_row, se_row)
    }
    
    # Visual separators
    if (i < length(dep_vars)) {
      sep_row <- data.frame(
        Variable = paste("--- End of", dep_var, "---"),
        Full_Sample = "",
        Inc_Can_Run = "",
        Inc_Cannot_Run = "",
        stringsAsFactors = FALSE
      )
      final_table <- rbind(final_table, sep_row)
    }
  }
  
  # Add new rows for "Mean in Control not WR in 2005" and "Test Treat Effect in WR=0"
  mean_control_row <- data.frame(
    Variable = "Mean in Control not WR in 2005",
    Full_Sample = summary_stats$Full_Control_Mean[1],
    Inc_Can_Run = summary_stats$Inc_Control_Mean[1],
    Inc_Cannot_Run = summary_stats$NoInc_Control_Mean[1],
    stringsAsFactors = FALSE
  )
  
  test_effect_row <- data.frame(
    Variable = "Test Treat Effect in WR=0",
    Full_Sample = summary_stats$Full_Joint_Test[1],
    Inc_Can_Run = summary_stats$Inc_Joint_Test[1],
    Inc_Cannot_Run = summary_stats$NoInc_Joint_Test[1],
    stringsAsFactors = FALSE
  )
  
  final_table <- rbind(final_table, mean_control_row, test_effect_row)
  
  return(final_table)
}

# Create the main table
main_table <- create_outreg_table(results_full, results_inc_can, results_inc_cannot)

# Create summary table
summary_stats <- create_summary_stats(results_full, results_inc_can, results_inc_cannot)

# Displaying and saving the results
cat("\n=== Regression results ===\n")
print(main_table)

# Formatted clean table with stargazer
# Combine all models
all_models <- c(
  lapply(results_full, function(x) x$model),
  lapply(results_inc_can, function(x) x$model),
  lapply(results_inc_cannot, function(x) x$model)
)

# Columns names
col_names <- c(
  paste("Full Sample", 1:6),
  paste("Inc Can Run", 1:6),
  paste("Inc Cannot Run", 1:6)
)

# Generate table
stargazer(all_models,
          type = "text",
          column.labels = col_names,
          keep = outregvar2,
          add.lines = list(
            c("District FE", rep("Yes", length(all_models))),
            c("GP Controls", rep("Yes", length(all_models))),
            c("Mean in Control not WR in 2005", summary_stats$Full_Control_Mean[1], summary_stats$Inc_Control_Mean[1], summary_stats$NoInc_Control_Mean[1], rep("", length(all_models) - 3)),
            c("Test Treat Effect in WR=0", summary_stats$Full_Joint_Test[1], summary_stats$Inc_Joint_Test[1], summary_stats$NoInc_Joint_Test[1], rep("", length(all_models) - 3))
          ),
          digits = 2,
          title = "Table 3: Challenger entry",
          out = "Table3_Challengers_2010.txt")













###### Clean output in html (like in Table 1)

## IN PROGRESS

