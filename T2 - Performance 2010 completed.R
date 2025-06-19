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

