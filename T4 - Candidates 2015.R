### REPLICATION - TABLE 4: CANDIDATE ENTRY IN THE NEXT ELECTION ################
################################################################################

# Packages to install if necessary
install.packages(c("tidyverse", "haven", "fixest","stargazer"))
#Libraries
library(tidyverse)
library(fixest)
library(stargazer)
library(haven)



## MACROS

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# Regression variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")


## DATA PROCESSING 

# Loading the data. Change path depending on your workspace.
data <- read_dta("~/work/Electoral data 2015 cleaned.dta")

# Filtering the data
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

outregvar15 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05", "RES15_gender",
                 "X15_INT_treatment", "X15_RES05_gender", "X15_X_anytr_genderres05")

# Dependent variables
dep_vars <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")

# List to stock the results:
models_list <- list()
control_means <- numeric(length(dep_vars))
pvals <- numeric(length(dep_vars))


## DOING THE REGRESSIONS


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
  model <- lm(formula, data = data_filtered)
  models_list[[i]] <- model
  
  # do the test
  test_result <- summary(lm(test = RES05_gender + X_anytr_genderres05, data = model$model))$coefficients[2, 4]
  pvals[i] <- round(test_result, 2)
}


## GENERATING THE OUTPUT TABLE

# listing the models
# adding the list of summary statistics
# saving the table

stargazer(models_list,
          type = "text",
          column.labels = paste("Model", 1:length(dep_vars)),
          keep = outregvar2,
          add.lines = list(
            c("District FE", rep("Yes", length(dep_vars))),
            c("GP Controls", rep("Yes", length(dep_vars))),
            c("Mean in Control not WR in 2015", control_means),
            c("Test Treat Effect in WR=Treat Effect in NWR", pvals)
          ),
          digits = 2,
          title = "Table 4: Effects on Candidates - 2015",
          out = file.path("~/work/Rajasthan-Voters-Replication/Table4_Candidates_2015.txt"))


# it's weird because it corresponds to the output of the Sata code.
# but not to the table 4 in the paper ? 








## non formatted table to fit into the pdf!!

create_regression_table <- function(models_list, dep_vars, outregvar2) {
  extract_model_results <- function(model) {
    if (is.null(model)) return(NULL)
    
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    results <- list()
    for (var in outregvar2) {
      if (var %in% rownames(coef_table)) {
        coef_val <- coef_table[var, "Estimate"]
        se_val <- coef_table[var, "Std. Error"]
        pval <- coef_table[var, "Pr(>|t|)"]
        
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
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    model_results <- extract_model_results(models_list[[i]])
    
    for (var in outregvar2) {
      coef_row <- data.frame(
        Variable = var,
        stringsAsFactors = FALSE
      )
      
      se_row <- data.frame(
        Variable = paste0("  ", var, "_se"),
        stringsAsFactors = FALSE
      )
      
      for (j in seq_along(models_list)) {
        col_name <- paste0("Model_", j)
        coef_row[[col_name]] <- if (!is.null(model_results[[var]])) model_results[[var]]$coef_formatted else "NA"
        se_row[[col_name]] <- if (!is.null(model_results[[var]])) model_results[[var]]$se_formatted else "(NA)"
      }
      
      final_table <- rbind(final_table, coef_row, se_row)
    }
  }
  
  return(final_table)
}

regression_table <- create_regression_table(models_list, dep_vars, outregvar2)
print(regression_table)



