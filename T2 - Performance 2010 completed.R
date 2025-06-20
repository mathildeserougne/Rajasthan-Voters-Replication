## TABLE 2 PERFORMANCE COMPLETED ###############

### REPLICATION - TABLE 2 - INCUMBENT PERFORMANCE AND ENTRY ###########
# Install packages if necessary
install.packages(c("tidyverse", "fixest", "stargazer", "haven", "lmtest"))

# Libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)

# ## DEFINITION OF THE MACROS ## #
# Control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Regression variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# ## DATA PROCESSING ## #
# Loading the data. Change path accordingly to your workspace.
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtering the data
data_filtered <- data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1, GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCFAM05_won - INC05_won
  )

# Generate the PERFORMANCE INDICES of the program
data_filtered <- data_filtered %>%
  mutate(
    index_empl_svy_0 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE),
    index_empl_svy_1 = rowMeans(select(., std_HH_NREGA_unmet_demand, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f, std_HH_NREGA_waiting_time_m, std_HH_NREGA_waiting_time_f, std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_2 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_3 = rowMeans(select(., std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE)
  )

# Dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won")

indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")

# Starting lists to stock the upcoming results
models_list <- list()
control_means <- numeric(length(incum_dep_vars1) * length(indices))
pvals_1 <- numeric(length(incum_dep_vars1) * length(indices))
pvals_2 <- numeric(length(incum_dep_vars1) * length(indices))
effect_average <- numeric(length(incum_dep_vars1) * length(indices))
effect_good <- numeric(length(incum_dep_vars1) * length(indices))
effect_bad <- numeric(length(incum_dep_vars1) * length(indices))

# ## DOING THE REGRESSIONS ## #
i <- 0
for (x in 0:1) {
  for (dep_var in incum_dep_vars1) {
    for (index in indices) {
      i <- i + 1
      
      # control mean
      control_mean <- data_filtered %>%
        filter(INT_treatment == 0 & RES05_gender == x) %>%
        summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean) %>%
        round(2)
      
      control_means[i] <- control_mean
      
      # mean and standard error of the index
      index_stats <- data_filtered %>%
        filter(RES05_gender == x) %>%
        summarise(mean = mean(!!sym(index), na.rm = TRUE),
                  sd = sd(!!sym(index), na.rm = TRUE))
      
      index_mean <- round(index_stats$mean, 2)
      index_sd <- round(index_stats$sd, 2)
      
      # interaction variables
      data_filtered <- data_filtered %>%
        mutate(
          TEMP_index = get(index),
          TEMP_X_res_index = RES05_gender * get(index),
          TEMP_X_anytr_index = INT_treatment * get(index),
          TEMP_X_anytr_res_index = INT_treatment * RES05_gender * get(index)
        )
      
      # checking that all the variables exist in the set
      all_vars <- c(dep_var, "INT_treatment", "TEMP_index", "TEMP_X_anytr_index", gpcontrols, "district")
      if (all(all_vars %in% names(data_filtered))) {
        # model estimation
        formula <- as.formula(paste(dep_var, "~ INT_treatment + TEMP_index + TEMP_X_anytr_index +", paste(gpcontrols, collapse = " + "), "+ factor(district)"))
        model <- tryCatch({
          lm(formula, data = data_filtered %>% filter(RES05_gender == x))
        }, error = function(e) {
          message("Error in model fitting: ", e$message)
          NULL
        })
        
        if (!is.null(model)) {
          models_list[[i]] <- model
          
          # doing the tests
          test_1 <- tryCatch({
            waldtest(model, c("INT_treatment + TEMP_X_anytr_index = 0", paste("TEMP_index =", index_mean)))
          }, error = function(e) {
            message("Error in test 1: ", e$message)
            NULL
          })
          
          if (!is.null(test_1)) {
            pvals_1[i] <- round(test_1$p.value, 2)
          } else {
            pvals_1[i] <- NA
          }
          
          test_2 <- tryCatch({
            waldtest(model, c("INT_treatment + TEMP_X_anytr_index = 0"))
          }, error = function(e) {
            message("Error in test 2: ", e$message)
            NULL
          })
          
          if (!is.null(test_2)) {
            pvals_2[i] <- round(test_2$p.value, 2)
          } else {
            pvals_2[i] <- NA
          }
          
          # effects
          effect_average[i] <- coef(model)["INT_treatment"] + coef(model)["TEMP_X_anytr_index"] * index_mean
          effect_good[i] <- coef(model)["INT_treatment"] + coef(model)["TEMP_X_anytr_index"] * (index_mean + index_sd)
          effect_bad[i] <- coef(model)["INT_treatment"] + coef(model)["TEMP_X_anytr_index"] * (index_mean - index_sd)
          
          # displaying said effects
          cat("Effects on outcome", dep_var, "\n")
          cat("Effect of treatment for average performing incumbent is", effect_average[i], "\n")
          cat("Effect of treatment for +1 sd performing incumbent is", effect_good[i], "\n")
          cat("Effect of treatment for -1 sd performing incumbent is", effect_bad[i], "\n")
        } else {
          message("Model fitting failed for ", dep_var)
        }
      } else {
        message("Some variables are missing in the dataset for ", dep_var)
      }
    }
  }
}

# ## GENERATING THE OUTPUT TABLE ## #
stargazer(models_list,
          type = "text",
          column.labels = paste("Model", 1:length(models_list)),
          keep = c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index"),
          add.lines = list(
            c("District FE", rep("Yes", length(models_list))),
            c("GP Controls", rep("Yes", length(models_list))),
            c("Mean in Control not WR in 2005", control_means),
            c("Test Treat Effect", pvals_1),
            c("Test Perf Effect in Treat", pvals_2)
          ),
          digits = 2,
          title = "Table 2: Performance - 2010",
          out = file.path("~/work/Rajasthan-Voters-Replication/Table2_Performance_2010.txt"))


## reduced table: 

# Selection of the right models to display
panel_A_models <- models_list[c(1, 5, 13, 17, 25, 29)]
panel_B_models <- models_list[c(37, 41, 49, 53, 61, 65)]

# Panel A
panel_A <- stargazer(
  panel_A_models,
  type = "text",
  column.labels = c("Model 1", "Model 5", "Model 13", "Model 17", "Model 25", "Model 29"),
  keep = c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index"),
  add.lines = list(
    c("District FE", rep("Yes", length(panel_A_models))),
    c("GP Controls", rep("Yes", length(panel_A_models))),
    c("Mean in Control not WR in 2005", control_means[c(1, 5, 13, 17, 25, 29)]),
    c("Test Treat Effect", pvals_1[c(1, 5, 13, 17, 25, 29)]),
    c("Test Perf Effect in Treat", pvals_2[c(1, 5, 13, 17, 25, 29)])
  ),
  digits = 2,
  title = "Panel A: GP without Gender Quota in 2005",
  single.row = TRUE
)

# Panel B
panel_B <- stargazer(
  panel_B_models,
  type = "text",
  column.labels = c("Model 37", "Model 41", "Model 49", "Model 53", "Model 61", "Model 65"),
  keep = c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index"),
  add.lines = list(
    c("District FE", rep("Yes", length(panel_B_models))),
    c("GP Controls", rep("Yes", length(panel_B_models))),
    c("Mean in Control not WR in 2005", control_means[c(37, 41, 49, 53, 61, 65)]),
    c("Test Treat Effect", pvals_1[c(37, 41, 49, 53, 61, 65)]),
    c("Test Perf Effect in Treat", pvals_2[c(37, 41, 49, 53, 61, 65)])
  ),
  digits = 2,
  title = "Panel B: GP with Gender Quota in 2005",
  single.row = TRUE
)

# Combine the two in one txt doc
combined_output <- c(panel_A, "\n\n", panel_B)
writeLines(combined_output, file.path("~/work/Rajasthan-Voters-Replication/Table2_Performance_2010_completed.txt"))




### non formatted table to fit into the pdf!!

## CREATE REDUCED TABLE WITH SELECTED MODELS
create_reduced_performance_table <- function(models_list, control_means, pvals_1, pvals_2) {
  
  # Function to extract model results
  extract_model_results <- function(model) {
    if (is.null(model)) return(NULL)
    
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    target_vars <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")
    
    results <- list()
    for (var in target_vars) {
      if (var %in% rownames(coef_table)) {
        coef_val <- coef_table[var, "Estimate"]
        se_val <- coef_table[var, "Std. Error"]
        pval <- coef_table[var, "Pr(>|t|)"]
        
        stars <- if (pval < 0.01) "***" else if (pval < 0.05) "**" else if (pval < 0.1) "*" else ""
        
        results[[var]] <- list(
          coef_formatted = paste0(round(coef_val, 3), stars),
          se_formatted = paste0("(", round(se_val, 3), ")")
        )
      } else {
        results[[var]] <- list(
          coef_formatted = "NA",
          se_formatted = "(NA)"
        )
      }
    }
    return(results)
  }
  
  # Selection of models to display
  panel_A_indices <- c(1, 5, 13, 17, 25, 29)
  panel_B_indices <- c(37, 41, 49, 53, 61, 65)
  
  # Column names for the selected models
  panel_A_cols <- c("Model_1", "Model_5", "Model_13", "Model_17", "Model_25", "Model_29")
  panel_B_cols <- c("Model_37", "Model_41", "Model_49", "Model_53", "Model_61", "Model_65")
  
  # Create Panel A table
  panel_A_table <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  for (col in panel_A_cols) {
    panel_A_table[[col]] <- character()
  }
  
  # Create Panel B table
  panel_B_table <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  for (col in panel_B_cols) {
    panel_B_table[[col]] <- character()
  }
  
  # Variables to extract
  vars_to_extract <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")
  
  # Fill Panel A
  for (var in vars_to_extract) {
    coef_row <- data.frame(Variable = var, stringsAsFactors = FALSE)
    se_row <- data.frame(Variable = paste0("  ", var, "_se"), stringsAsFactors = FALSE)
    
    for (i in 1:length(panel_A_indices)) {
      model_idx <- panel_A_indices[i]
      col_name <- panel_A_cols[i]
      
      if (model_idx <= length(models_list) && !is.null(models_list[[model_idx]])) {
        results <- extract_model_results(models_list[[model_idx]])
        coef_row[[col_name]] <- if (!is.null(results[[var]])) results[[var]]$coef_formatted else "NA"
        se_row[[col_name]] <- if (!is.null(results[[var]])) results[[var]]$se_formatted else "(NA)"
      } else {
        coef_row[[col_name]] <- "NA"
        se_row[[col_name]] <- "(NA)"
      }
    }
    
    panel_A_table <- rbind(panel_A_table, coef_row, se_row)
  }
  
  # Fill Panel B
  for (var in vars_to_extract) {
    coef_row <- data.frame(Variable = var, stringsAsFactors = FALSE)
    se_row <- data.frame(Variable = paste0("  ", var, "_se"), stringsAsFactors = FALSE)
    
    for (i in 1:length(panel_B_indices)) {
      model_idx <- panel_B_indices[i]
      col_name <- panel_B_cols[i]
      
      if (model_idx <= length(models_list) && !is.null(models_list[[model_idx]])) {
        results <- extract_model_results(models_list[[model_idx]])
        coef_row[[col_name]] <- if (!is.null(results[[var]])) results[[var]]$coef_formatted else "NA"
        se_row[[col_name]] <- if (!is.null(results[[var]])) results[[var]]$se_formatted else "(NA)"
      } else {
        coef_row[[col_name]] <- "NA"
        se_row[[col_name]] <- "(NA)"
      }
    }
    
    panel_B_table <- rbind(panel_B_table, coef_row, se_row)
  }
  
  # Add additional statistics to Panel A
  obs_row_A <- data.frame(Variable = "Observations", stringsAsFactors = FALSE)
  for (i in 1:length(panel_A_indices)) {
    model_idx <- panel_A_indices[i]
    col_name <- panel_A_cols[i]
    
    if (model_idx <= length(models_list) && !is.null(models_list[[model_idx]])) {
      obs_row_A[[col_name]] <- nobs(models_list[[model_idx]])
    } else {
      obs_row_A[[col_name]] <- "NA"
    }
  }
  panel_A_table <- rbind(panel_A_table, obs_row_A)
  
  mean_row_A <- data.frame(Variable = "Mean in Control not WR in 2005", stringsAsFactors = FALSE)
  for (i in 1:length(panel_A_indices)) {
    model_idx <- panel_A_indices[i]
    col_name <- panel_A_cols[i]
    mean_row_A[[col_name]] <- control_means[model_idx]
  }
  panel_A_table <- rbind(panel_A_table, mean_row_A)
  
  test1_row_A <- data.frame(Variable = "Test Treat Effect", stringsAsFactors = FALSE)
  test2_row_A <- data.frame(Variable = "Test Perf Effect in Treat", stringsAsFactors = FALSE)
  for (i in 1:length(panel_A_indices)) {
    model_idx <- panel_A_indices[i]
    col_name <- panel_A_cols[i]
    test1_row_A[[col_name]] <- pvals_1[model_idx]
    test2_row_A[[col_name]] <- pvals_2[model_idx]
  }
  panel_A_table <- rbind(panel_A_table, test1_row_A, test2_row_A)
  
  # Add additional statistics to Panel B
  obs_row_B <- data.frame(Variable = "Observations", stringsAsFactors = FALSE)
  for (i in 1:length(panel_B_indices)) {
    model_idx <- panel_B_indices[i]
    col_name <- panel_B_cols[i]
    
    if (model_idx <= length(models_list) && !is.null(models_list[[model_idx]])) {
      obs_row_B[[col_name]] <- nobs(models_list[[model_idx]])
    } else {
      obs_row_B[[col_name]] <- "NA"
    }
  }
  panel_B_table <- rbind(panel_B_table, obs_row_B)
  
  mean_row_B <- data.frame(Variable = "Mean in Control not WR in 2005", stringsAsFactors = FALSE)
  for (i in 1:length(panel_B_indices)) {
    model_idx <- panel_B_indices[i]
    col_name <- panel_B_cols[i]
    mean_row_B[[col_name]] <- control_means[model_idx]
  }
  panel_B_table <- rbind(panel_B_table, mean_row_B)
  
  test1_row_B <- data.frame(Variable = "Test Treat Effect", stringsAsFactors = FALSE)
  test2_row_B <- data.frame(Variable = "Test Perf Effect in Treat", stringsAsFactors = FALSE)
  for (i in 1:length(panel_B_indices)) {
    model_idx <- panel_B_indices[i]
    col_name <- panel_B_cols[i]
    test1_row_B[[col_name]] <- pvals_1[model_idx]
    test2_row_B[[col_name]] <- pvals_2[model_idx]
  }
  panel_B_table <- rbind(panel_B_table, test1_row_B, test2_row_B)
  
  return(list(panel_A = panel_A_table, panel_B = panel_B_table))
}

# Create the reduced tables
reduced_tables <- create_reduced_performance_table(models_list, control_means, pvals_1, pvals_2)

# Display the tables
print("=== PANEL A: GP without Gender Quota in 2005 ===")
print(reduced_tables$panel_A)

print("\n=== PANEL B: GP with Gender Quota in 2005 ===")
print(reduced_tables$panel_B)




