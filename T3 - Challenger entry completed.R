### REPLICATION - TABLE 3 - CHALLENGER ENTRY ###############
# Libraries
library(dplyr)
library(fixest)
library(stargazer)
library(haven)
library(broom)
library(aod)

## DEFINING THE MACROS
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

## DATA PROCESSING
data_path <- "~/work/Electoral data cleaned.dta"
data <- read_dta(data_path)
data_filtered <- data %>% filter(RES10_gender == 0, GP_tag == 1)

## FUNCTION FOR THE REGRESSIONS
run_regression_analysis <- function(data_subset, subset_name) {
  cat(paste("\n=== ANALYZE FOR:", subset_name, "===\n"))
  cat("Number of observations:", nrow(data_subset), "\n")
  
  results_list <- list()
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    cat(paste(" Dependent variable:", dep_var, "\n"))
    
    if (!dep_var %in% names(data_subset)) {
      cat(paste("    ATTENTION: Variable", dep_var, "not found!\n"))
      next
    }
    
    control_subset <- data_subset %>% filter(INT_treatment == 0, RES05_gender == 0)
    control_mean <- if (nrow(control_subset) == 0) NA else control_subset %>%
      summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean_val) %>%
      round(2)
    
    cat(paste("    Control mean (non-previously gender-reserved):", control_mean, "\n"))
    
    available_controls <- gpcontrols[gpcontrols %in% names(data_subset)]
    reg_vars <- c(outregvar2, available_controls)
    reg_vars <- reg_vars[reg_vars %in% names(data_subset)]
    
    formula_str <- paste(dep_var, "~", paste(reg_vars, collapse = " + "), "+ factor(district)")
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = data_subset)
      coef_names <- names(coef(model))
      
      res05_coef <- coef_names[grepl("RES05_gender", coef_names)]
      anytr_coef <- coef_names[grepl("X_anytr_genderres05", coef_names)]
      
      if (length(res05_coef) > 0 && length(anytr_coef) > 0) {
        L <- matrix(0, nrow = 1, ncol = length(coef(model)))
        names_L <- names(coef(model))
        L[1, which(names_L == res05_coef[1])] <- 1
        L[1, which(names_L == anytr_coef[1])] <- 1
        
        restriction <- L %*% coef(model)
        var_restriction <- L %*% vcov(model) %*% t(L)
        wald_stat <- as.numeric(restriction^2 / var_restriction)
        pval <- round(1 - pchisq(wald_stat, df = 1), 3)
      } else {
        pval <- NA
      }
      
      results_list[[i]] <- list(
        model = model,
        dep_var = dep_var,
        control_mean = control_mean,
        joint_test_pval = pval,
        subset = subset_name,
        formula = formula_str,
        n_obs = nrow(model$model)
      )
      
    }, error = function(e) {
      cat(paste("    ERROR in the regression:", e$message, "\n"))
      results_list[[i]] <- NULL
    })
  }
  
  results_list <- results_list[!sapply(results_list, is.null)]
  return(results_list)
}

## ANALYSIS FOR THE THREE SUB-SAMPLES
results_full <- run_regression_analysis(data_filtered, "Full Sample")
data_inc_can_run <- data_filtered %>% filter(INC05_can_run == 1)
results_inc_can <- run_regression_analysis(data_inc_can_run, "Incumbent Can Run")
data_inc_cannot_run <- data_filtered %>% filter(INC05_can_run == 0)
results_inc_cannot <- run_regression_analysis(data_inc_cannot_run, "Incumbent Cannot Run")

## CREATE OUTPUT TABLE
create_outreg_table <- function(results_list_full, results_list_inc, results_list_no_inc) {
  extract_model_results <- function(model_result) {
    if (is.null(model_result) || is.null(model_result$model)) return(NULL)
    
    model <- model_result$model
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    results <- list()
    for (var in outregvar2) {
      matching_vars <- rownames(coef_table)[grepl(var, rownames(coef_table))]
      if (length(matching_vars) > 0) {
        var_name <- matching_vars[1]
        coef_val <- coef_table[var_name, "Estimate"]
        se_val <- coef_table[var_name, "Std. Error"]
        pval <- coef_table[var_name, "Pr(>|t|)"]
        
        stars <- if (pval < 0.01) "***" else if (pval < 0.05) "**" else if (pval < 0.1) "*" else ""
        
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
  
  final_table <- data.frame(
    Variable = character(),
    Full_Sample = character(),
    Inc_Can_Run = character(),
    Inc_Cannot_Run = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    full_results <- if (i <= length(results_list_full)) extract_model_results(results_list_full[[i]]) else NULL
    inc_results <- if (i <= length(results_list_inc)) extract_model_results(results_list_inc[[i]]) else NULL
    no_inc_results <- if (i <= length(results_list_no_inc)) extract_model_results(results_list_no_inc[[i]]) else NULL
    
    for (var in outregvar2) {
      coef_row <- data.frame(
        Variable = var,
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$coef_formatted else "NA",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$coef_formatted else "NA",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$coef_formatted else "NA",
        stringsAsFactors = FALSE
      )
      
      se_row <- data.frame(
        Variable = paste0("  ", var, "_se"),
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$se_formatted else "(NA)",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$se_formatted else "(NA)",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$se_formatted else "(NA)",
        stringsAsFactors = FALSE
      )
      
      final_table <- rbind(final_table, coef_row, se_row)
    }
  }
  
  return(final_table)
}

main_table <- create_outreg_table(results_full, results_inc_can, results_inc_cannot)
print(main_table)



## table in txt stargazer


# Regression for each sample (each panel)
results_full <- run_regression_analysis(data_filtered, "Full Sample")
results_inc_can <- run_regression_analysis(data_inc_can_run, "Incumbent Can Run")
results_inc_cannot <- run_regression_analysis(data_inc_cannot_run, "Incumbent Cannot Run")

# Extract the results
panel_A_models <- lapply(results_full, function(x) x$model)
panel_B_models <- lapply(results_inc_can, function(x) x$model)
panel_C_models <- lapply(results_inc_cannot, function(x) x$model)

# Extract control means and p values for additional rows
control_means <- sapply(results_full, function(x) x$control_mean)
pvals <- sapply(results_full, function(x) x$joint_test_pval)

# Table for each panel, then combined into one txt file
panel_A <- stargazer(
  panel_A_models,
  type = "text",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  keep = c("INT_treatment", "RES05_gender", "X_anytr_genderres05"),
  add.lines = list(
    c("District FE", rep("Yes", length(panel_A_models))),
    c("GP Controls", rep("Yes", length(panel_A_models))),
    c("Mean in Control not WR in 2005", control_means),
    c("Test Treat Effect", pvals)
  ),
  digits = 2,
  title = "Panel A: All GPs",
  single.row = TRUE
)

panel_B <- stargazer(
  panel_B_models,
  type = "text",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  keep = c("INT_treatment", "RES05_gender", "X_anytr_genderres05"),
  add.lines = list(
    c("District FE", rep("Yes", length(panel_B_models))),
    c("GP Controls", rep("Yes", length(panel_B_models))),
    c("Mean in Control not WR in 2005", control_means),
    c("Test Treat Effect", pvals)
  ),
  digits = 2,
  title = "Panel B: Incumbent Can Run",
  single.row = TRUE
)

panel_C <- stargazer(
  panel_C_models,
  type = "text",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  keep = c("INT_treatment", "RES05_gender", "X_anytr_genderres05"),
  add.lines = list(
    c("District FE", rep("Yes", length(panel_C_models))),
    c("GP Controls", rep("Yes", length(panel_C_models))),
    c("Mean in Control not WR in 2005", control_means),
    c("Test Treat Effect", pvals)
  ),
  digits = 2,
  title = "Panel C: Incumbent Cannot Run",
  single.row = TRUE
)

# Combine three panels
combined_output <- c(panel_A, "\n\n", panel_B, "\n\n", panel_C)
writeLines(combined_output, "Table3_Challengers_2010_completed.txt")







