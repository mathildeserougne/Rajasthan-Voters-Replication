### REPLICATION - TABLE 2 - INCUMBENT PERFORMANCE AND ENTRY ###########

# IN PROGRESS

# OK SO 
# it kind of works for the treatment
# on the other hand, it still has some issues concerning the performance index. we lose in significance on the perf index row.




# PACKAGES
install.packages(c("tidyverse","stargazer","knitr","broom","haven","fixest","modelsummary","gt","webshot2","car"))
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

# Defining the control variables (EXACT same as Stata $gpcontrols)
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", 
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", 
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# EXACT dependent variables from Stata code
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won", 
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won")

# Loading the data
# Change the path if necessary.
data <- read_dta("~/work/Electoral data cleaned.dta")

# EXACT STATA FILTERING: keep if RES10_gender==0 & SAMPLE_hhsurvey==1 & GP_tag & INC05_can_run==1
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1)

# EXACT FAMnotINC05 creation from Stata
data_filtered <- data_filtered %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# EXACT STATA INDEX CREATION
# First, standardize if needed (assuming they are already standardized in the dataset)
# If not standardized, uncomment and adapt these lines:
# data_filtered <- data_filtered %>%
#   mutate(
#     std_HH_NREGA = scale(HH_NREGA)[,1],
#     std_HH_NREGA_unmet_demand = scale(HH_NREGA_unmet_demand)[,1],
#     std_HH_NREGA_unmet_demand_m = scale(HH_NREGA_unmet_demand_m)[,1],
#     std_HH_NREGA_unmet_demand_f = scale(HH_NREGA_unmet_demand_f)[,1],
#     std_HH_NREGA_waiting_time_m = scale(HH_NREGA_waiting_time_m)[,1],
#     std_HH_NREGA_waiting_time_f = scale(HH_NREGA_waiting_time_f)[,1],
#     std_HH_NREGA_work_m = scale(HH_NREGA_work_m)[,1],
#     std_HH_NREGA_work_f = scale(HH_NREGA_work_f)[,1]
#   )

# EXACT index creation as in Stata: 
# egen index_empl_svy_1=rowmean(std_HH_NREGA_unmet_demand std_HH_NREGA_unmet_demand_m std_HH_NREGA_unmet_demand_f std_HH_NREGA_waiting_time_m std_HH_NREGA_waiting_time_f  std_HH_NREGA std_HH_NREGA_work_m std_HH_NREGA_work_f)

# Check which variables exist in the dataset
available_vars <- names(data_filtered)
required_vars <- c("std_HH_NREGA", "std_HH_NREGA_unmet_demand", "std_HH_NREGA_unmet_demand_m", 
                   "std_HH_NREGA_unmet_demand_f", "std_HH_NREGA_waiting_time_m", 
                   "std_HH_NREGA_waiting_time_f", "std_HH_NREGA_work_m", "std_HH_NREGA_work_f")

cat("Available variables for index creation:\n")
print(intersect(required_vars, available_vars))

# Create index with available variables (adapt based on what's actually in your dataset)
if(all(required_vars %in% available_vars)) {
  data_filtered <- data_filtered %>%
    rowwise() %>%
    mutate(
      index_empl_svy_1 = mean(c(std_HH_NREGA_unmet_demand, std_HH_NREGA_unmet_demand_m, 
                                std_HH_NREGA_unmet_demand_f, std_HH_NREGA_waiting_time_m, 
                                std_HH_NREGA_waiting_time_f, std_HH_NREGA, 
                                std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE)
    ) %>%
    ungroup()
} else {
  # If standardized variables don't exist, create them first
  cat("Creating standardized variables and index...\n")
  
  # Check what base variables exist
  base_vars <- c("HH_NREGA", "HH_NREGA_unmet_demand", "HH_NREGA_unmet_demand_m", 
                 "HH_NREGA_unmet_demand_f", "HH_NREGA_waiting_time_m", 
                 "HH_NREGA_waiting_time_f", "HH_NREGA_work_m", "HH_NREGA_work_f")
  
  existing_base_vars <- intersect(base_vars, available_vars)
  cat("Found base variables:", paste(existing_base_vars, collapse = ", "), "\n")
  
  # Create standardized versions
  for(var in existing_base_vars) {
    std_var_name <- paste0("std_", var)
    data_filtered[[std_var_name]] <- scale(data_filtered[[var]])[,1]
  }
  
  # Create index
  data_filtered <- data_filtered %>%
    rowwise() %>%
    mutate(
      index_empl_svy_1 = mean(c_across(starts_with("std_HH_NREGA")), na.rm = TRUE)
    ) %>%
    ungroup()
}

# Initialize storage
models_list <- list()
control_means <- list()
test_results <- list()
treatment_effects <- list()
model_counter <- 1

# EXACT STATA LOOP: forvalues x=0/1
for(x in 0:1) {
  
  cat(paste("\n=== Processing RES05_gender =", x, "===\n"))
  
  # Filter by RES05_gender value (this is implicit in Stata regression: if RES05_gender==`x')
  data_subset <- data_filtered %>% filter(RES05_gender == x)
  
  cat("Sample size for RES05_gender =", x, ":", nrow(data_subset), "\n")
  
  # EXACT STATA LOOP: foreach dep_var in `incum_dep_vars1'
  for(dep_var in incum_dep_vars1) {
    
    cat(paste("Processing", dep_var, "for RES05_gender =", x, "\n"))
    
    # EXACT STATA: sum `dep_var' if INT_treatment==0 & RES05_gender==`x'
    control_mean <- data_subset %>%
      filter(INT_treatment == 0) %>%
      summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean)
    
    if(is.na(control_mean)) control_mean <- 0
    control_mean <- round(control_mean, 2)
    control_means[[model_counter]] <- control_mean
    
    # EXACT STATA: sum `index' if RES05_gender==`x' (for mean calculation)
    index_stats <- data_subset %>%
      summarise(
        mean_index = mean(index_empl_svy_1, na.rm = TRUE),
        sd_index = sd(index_empl_svy_1, na.rm = TRUE)
      )
    
    mean_index <- index_stats$mean_index
    if(is.na(mean_index)) mean_index <- 0
    
    # EXACT STATA TEMP VARIABLE CREATION:
    # gen TEMP_index=`index'
    # gen TEMP_X_res_index=RES05_gender*`index'  [Not used in final regression]
    # gen TEMP_X_anytr_index=INT_treatment*`index'
    # gen TEMP_X_anytr_res_index=INT_treatment*RES05_gender*`index' [Not used in final regression]
    
    data_temp <- data_subset %>%
      mutate(
        TEMP_index = index_empl_svy_1,
        TEMP_X_anytr_index = INT_treatment * index_empl_svy_1
      )
    
    # EXACT STATA REGRESSION:
    # xi: reg `dep_var' INT_treatment TEMP_index TEMP_X_anytr_index $gpcontrols i.district if RES05_gender==`x'
    
    formula_str <- paste(dep_var, "~ INT_treatment + TEMP_index + TEMP_X_anytr_index +", 
                         paste(gpcontrols, collapse = " + "), "+ factor(district)")
    formula <- as.formula(formula_str)
    
    model <- lm(formula, data = data_temp)
    models_list[[model_counter]] <- model
    
    # Get coefficients for tests
    coef_model <- coef(model)
    vcov_model <- vcov(model)
    
    # EXACT STATA TESTS:
    # test INT_treatment+TEMP_X_anytr_index*`mean_index'+TEMP_index*`mean_index'=0
    constraint1 <- paste0("INT_treatment + TEMP_X_anytr_index * ", mean_index, " + TEMP_index * ", mean_index, " = 0")
    
    # test INT_treatment+TEMP_X_anytr_index*`mean_index'=0  
    constraint2 <- paste0("INT_treatment + TEMP_X_anytr_index * ", mean_index, " = 0")
    
    # Calculate tests manually if car::linearHypothesis fails
    tryCatch({
      test1 <- car::linearHypothesis(model, constraint1)
      pval_1 <- round(test1$`Pr(>F)`[2], 2)
    }, error = function(e) {
      # Manual calculation
      est1 <- coef_model["INT_treatment"] + coef_model["TEMP_X_anytr_index"] * mean_index + coef_model["TEMP_index"] * mean_index
      se1 <- sqrt(vcov_model["INT_treatment", "INT_treatment"] + 
                    (mean_index^2) * vcov_model["TEMP_X_anytr_index", "TEMP_X_anytr_index"] +
                    (mean_index^2) * vcov_model["TEMP_index", "TEMP_index"] +
                    2 * mean_index * vcov_model["INT_treatment", "TEMP_X_anytr_index"] +
                    2 * mean_index * vcov_model["INT_treatment", "TEMP_index"] +
                    2 * (mean_index^2) * vcov_model["TEMP_X_anytr_index", "TEMP_index"])
      t_stat1 <- est1 / se1
      pval_1 <- round(2 * (1 - pt(abs(t_stat1), df = model$df.residual)), 2)
    })
    
    tryCatch({
      test2 <- car::linearHypothesis(model, constraint2)
      pval_2 <- round(test2$`Pr(>F)`[2], 2)
    }, error = function(e) {
      # Manual calculation  
      est2 <- coef_model["INT_treatment"] + coef_model["TEMP_X_anytr_index"] * mean_index
      se2 <- sqrt(vcov_model["INT_treatment", "INT_treatment"] + 
                    (mean_index^2) * vcov_model["TEMP_X_anytr_index", "TEMP_X_anytr_index"] +
                    2 * mean_index * vcov_model["INT_treatment", "TEMP_X_anytr_index"])
      t_stat2 <- est2 / se2
      pval_2 <- round(2 * (1 - pt(abs(t_stat2), df = model$df.residual)), 2)
    })
    
    if(!exists("pval_1")) pval_1 <- NA
    if(!exists("pval_2")) pval_2 <- NA
    
    test_results[[model_counter]] <- list(pval1 = pval_1, pval2 = pval_2)
    
    # EXACT STATA EFFECT CALCULATIONS (noisily display):
    effect_average <- coef_model["INT_treatment"] + coef_model["TEMP_X_anytr_index"] * mean_index
    effect_good <- coef_model["INT_treatment"] + coef_model["TEMP_X_anytr_index"] * (mean_index + index_stats$sd_index)
    effect_bad <- coef_model["INT_treatment"] + coef_model["TEMP_X_anytr_index"] * (mean_index - index_stats$sd_index)
    
    treatment_effects[[model_counter]] <- list(
      effect_average = round(effect_average, 4),
      effect_good = round(effect_good, 4), 
      effect_bad = round(effect_bad, 4)
    )
    
    # EXACT STATA DISPLAY OUTPUT:
    cat(paste("Effects on outcome", dep_var, "\n"))
    cat(paste("Effect of treatment for average performing incumbent is", round(effect_average, 4), "\n"))
    cat(paste("Effect of treatment for +1 sd performing incumbent is", round(effect_good, 4), "\n"))
    cat(paste("Effect of treatment for -1 sd performing incumbent is", round(effect_bad, 4), "\n"))
    cat(paste("Control mean:", control_mean, "\n"))
    cat(paste("Test 1 p-val:", pval_1, "\n"))
    cat(paste("Test 2 p-val:", pval_2, "\n\n"))
    
    model_counter <- model_counter + 1
  }
}

# Create summary table that matches Stata output format
create_stata_table <- function(models_list, control_means, test_results, treatment_effects) {
  
  # Only keep the key coefficients as in Stata outreg2
  coef_map <- c(
    "INT_treatment" = "Treatment",
    "TEMP_index" = "Performance Index",
    "TEMP_X_anytr_index" = "Treatment × Performance"
  )
  
  table_data <- modelsummary(
    models_list,
    output = "gt",
    statistic = "std.error", 
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
    coef_map = coef_map,
    gof_omit = ".*"
  ) %>%
    tab_header(
      title = "Table 2 - Performance Effects (Exact Stata Replication)",
      subtitle = "Models 1-9: RES05_gender=0; Models 10-18: RES05_gender=1"
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = "#667eea")
      ),
      locations = cells_column_labels()
    )
  
  return(table_data)
}

# Create and display table
stata_table <- create_stata_table(models_list, control_means, test_results, treatment_effects)
stata_table

# Save
stata_table %>%
  gtsave(filename = file.path("~/work/T2_Performance_EXACT_STATA.html"))

# Print summary matching Stata format
cat("\n=== FINAL SUMMARY (Stata format) ===\n")
for(i in 1:length(models_list)) {
  cat(paste("Model", i, "- Control Mean:", control_means[[i]], 
            "- Test 1:", test_results[[i]]$pval1,
            "- Test 2:", test_results[[i]]$pval2, "\n"))
}

# Check if we have the expected number of models (should be 18: 9 outcomes × 2 gender restrictions)
cat(paste("\nTotal models estimated:", length(models_list), "\n"))
cat("Expected: 18 models (9 outcomes × 2 gender values)\n")

# Display coefficient comparison for first few models
cat("\n=== COEFFICIENT CHECK (First 3 models) ===\n")
for(i in 1:min(3, length(models_list))) {
  cat(paste("Model", i, ":\n"))
  print(round(coef(models_list[[i]])[c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")], 4))
  cat("\n")
}