### REPLICATION - TABLE 1 - INCUMBENT AND INCUMBENT'S FAMILY ENTRY ###########

# Libraries
# Uncomment packages if necessary
#install.packages(c("tidyverse","stargazer","knitr","broom","haven","fixest","modelsummary","gt","webshot2"))
library(tidyverse)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)


# Defining the control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", 
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", 
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Defining the dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won", 
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won",
                     "INCorFAM05_running", "INCorFAM05_voteshare", "INCorFAM05_won")

# Loading the data
# Change the path if necessary.
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtering the data (keeping non-reserved GPs, and only 1 observation)
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Function for the regression formulas
create_formula <- function(dep_var, model_type) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  
  if (model_type == "any_treatment") {
    formula_str <- paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +", 
                         base_controls, "+ factor(district)")
  } else if (model_type == "gender_general") {
    formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender +",
                         "X_generaltr_genderres05 + X_gendertr_genderres05 +", 
                         base_controls, "+ factor(district)")
  }
  
  return(as.formula(formula_str))
}

# Function for the statistical tests
calculate_tests <- function(model, model_type) {
  if (model_type == "any_treatment") {
    # Test: RES05_gender + X_anytr_genderres05 = 0
    test1 <- car::linearHypothesis(model, "RES05_gender + X_anytr_genderres05 = 0")
    pval1 <- test1$`Pr(>F)`[2]
    
    # Test: INT_treatment = RES05_gender
    test2 <- car::linearHypothesis(model, "INT_treatment - RES05_gender = 0")
    pval2 <- test2$`Pr(>F)`[2]
    
    return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2)))
    
  } else if (model_type == "gender_general") {
    # Test: INT_treatment_gender = INT_treatment_general
    test1 <- car::linearHypothesis(model, "INT_treatment_gender - INT_treatment_general = 0")
    pval1 <- test1$`Pr(>F)`[2]
    
    # Test: INT_treatment_gender + X_gendertr_genderres05 = INT_treatment_general + X_generaltr_genderres05
    test2 <- car::linearHypothesis(model, 
                                   "INT_treatment_gender + X_gendertr_genderres05 - INT_treatment_general - X_generaltr_genderres05 = 0")
    pval2 <- test2$`Pr(>F)`[2]
    
    return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2)))
  }
}

# Estimating the models
models_list <- list()
control_means <- list()
test_results <- list()


### Models with "any treatment" 
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[[i]] <- control_mean
  
  # model estimate
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_filtered)
  models_list[[i]] <- model
  
  # statistical tests
  test_results[[i]] <- calculate_tests(model, "any_treatment")
}

### Models with "gender and general treatment" 
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  j <- i + length(incum_dep_vars1)
  
  # control mean
  control_means[[j]] <- control_means[[i]]
  
  # model estimate
  formula <- create_formula(dep_var, "gender_general")
  model <- lm(formula, data = data_filtered)
  models_list[[j]] <- model
  
  # statistical tests
  test_results[[j]] <- calculate_tests(model, "gender_general")
}


### TABLE v1
# This one is exhaustive compared to the one in the article.
# For each group (incumbent, incumbent spouse, or other family member), it displays running, voteshare, and victory outcome.


create_formatted_table <- function(models_list, control_means, test_results) {
  
  # modelsummary to extract coefficients
  table_data <- modelsummary(models_list,
                             output = "data.frame",
                             statistic = "std.error",
                             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                             coef_omit = paste(gpcontrols, collapse = "|"),
                             gof_omit = ".*")
  
  # table
  final_table <- table_data %>%
    gt() %>%
    tab_header(
      title = "Table 1 - Incumbent and incumbent's family entry",
      subtitle = "1-12 = Average treatment ; 13-24 = Gender vs General treatments"
    ) %>%
    tab_spanner(
      label = "Models 1-12 (Any Treatment)",
      columns = 2:13
    ) %>%
    tab_spanner(
      label = "Models 13-24 (Gender vs General)",
      columns = 14:25
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = "#667eea")
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(color = "#e74c3c", weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows = str_detect(part, "\\*\\*\\*")
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#f39c12", weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows = str_detect(part, "\\*\\*") & !str_detect(part, "\\*\\*\\*")
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#3498db", weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows = str_detect(part, "\\*") & !str_detect(part, "\\*\\*")
      )
    ) %>%
    tab_footnote(
      footnote = "Usual significance thresholds. *** p<0.01, ** p<0.05, * p<0.1",
      locations = cells_title("subtitle")
    )
  
  return(final_table)
}

# Main table
regression_table <- create_formatted_table(models_list, control_means, test_results)

# Saving the table
# Change path if necessary.
regression_table %>%
  gtsave(filename = file.path("~/work/T1_Incumbent_2010_formatted.html"))


### TABLE v2

# This table is less exhaustive.
# It has the same columns as on page 21 of the article.

# only six columns
incum_dep_vars_reduced <- c("INC05_running", "INC05_voteshare", 
                            "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                            "INCOTHER05_running", "INCOTHER05_voteshare")

# estimation
models_list_reduced <- list()
control_means_reduced <- list()
test_results_reduced <- list()

### PANEL A: average effects
for (i in 1:length(incum_dep_vars_reduced)) {
  dep_var <- incum_dep_vars_reduced[i]
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means_reduced[[i]] <- control_mean
  
  # model estimate
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_filtered)
  models_list_reduced[[i]] <- model
  
  # statistical tests
  test_results_reduced[[i]] <- calculate_tests(model, "any_treatment")
}

### PANEL B: effects by type of campaign
for (i in 1:length(incum_dep_vars_reduced)) {
  dep_var <- incum_dep_vars_reduced[i]
  j <- i + length(incum_dep_vars_reduced)
  
  # control mean
  control_means_reduced[[j]] <- control_means_reduced[[i]]
  
  # model estimate
  formula <- create_formula(dep_var, "gender_general")
  model <- lm(formula, data = data_filtered)
  models_list_reduced[[j]] <- model
  
  # statistical tests
  test_results_reduced[[j]] <- calculate_tests(model, "gender_general")
}

# creating the table
regression_table_reduced <- modelsummary(models_list_reduced,
                                         output = "gt",
                                         statistic = "std.error",
                                         stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                                         coef_omit = paste(gpcontrols, collapse = "|"),
                                         gof_omit = ".*") %>%
  tab_header(
    title = "Table 1 - Incumbent and family entry",
    subtitle = "Columns 1-6 = Average treatment ; Columns 7-12 = Gender vs General treatments"
  ) %>%
  tab_spanner(
    label = "Incumbent (Runs, Voteshare)",
    columns = 2:3
  ) %>%
  tab_spanner(
    label = "Spouse (Runs, Voteshare)", 
    columns = 4:5
  ) %>%
  tab_spanner(
    label = "Other Family (Runs, Voteshare)",
    columns = 6:7
  ) %>%
  tab_spanner(
    label = "Panel B: Gender vs General Treatment",
    columns = 8:13
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#667eea")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_footnote(
    footnote = "Usual significance levels. *** p<0.01, ** p<0.05, * p<0.1",
    locations = cells_title("subtitle")
  )

# display
regression_table_reduced

# saving
# change path if necessary.
regression_table_reduced %>%
  gtsave(filename = file.path("~/work/T1_Incumbent_reduced.html"))




########### Table reduced but with stargazer: 


# Variables to display
outregvar2 <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general")

# col names
col_names <- c(
  paste("Any Treat", 1:11),
  paste("Gender/General", 1:11)
)

# generate table
stargazer(models_list,
          type = "text",  
          column.labels = col_names,
          keep = outregvar2,
          add.lines = list(
            c("District FE", rep("Yes", length(models_list))),
            c("GP Controls", rep("Yes", length(models_list)))
          ),
          digits = 2,
          title = "Table 1: Effects on Incumbent and Family Candidate Entry (2005)",
          out = "Table1_Incumbent_2010.txt")




########### Table reduced but EXTENDED with the summary stats on control means 

# variables to display
outregvar2 <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general")

# colnames
col_names <- c(
  paste("Any Treat", 1:12),
  paste("Gender/General", 1:12)
)

# additional lines for means and test results!
additional_lines <- list(
  c("District FE", rep("Yes", length(models_list))),
  c("GP Controls", rep("Yes", length(models_list))),
  c("Mean in Control not WR in 2005", unlist(control_means)),
  c("Test Treat Effect in WR=0", sapply(test_results, function(x) x$pval1))
)

# generate table
stargazer(models_list,
          type = "text",  
          column.labels = col_names,
          keep = outregvar2,
          add.lines = additional_lines,
          digits = 2,
          title = "Table 1: Effects on Incumbent and Family Candidate Entry (2005)",
          out = "Table1_Incumbent_2010.txt")













# Model summary display
cat("\nSummary of estimated models:\n")
cat("Total amount of models:", length(models_list), "\n")
cat("Dependent variables:", paste(incum_dep_vars1, collapse = ", "), "\n")
cat("Amount of observations per model:", sapply(models_list, nobs), "\n")


