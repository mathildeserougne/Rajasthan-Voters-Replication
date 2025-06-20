## TABLE 1  - Incumbent and Incumbent's family entry - completed ###########

# Libraries
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

# Defining the control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Defining the dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Loading the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtering the data
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
    formula_str <- paste(dep_var, "~ INT_treatment + RES05_gender + INT_treatment:RES05_gender +",
                         base_controls, "+ factor(district)")
  } else if (model_type == "gender_general") {
    formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender +",
                         base_controls, "+ factor(district)")
  }
  
  return(as.formula(formula_str))
}

# Function for the statistical tests
calculate_tests <- function(model, model_type) {
  if (model_type == "any_treatment") {
    test1 <- tryCatch({
      car::linearHypothesis(model, "RES05_gender = 0")
    }, error = function(e) list(PrF = NA))
    pval1 <- if (!is.null(test1$PrF)) round(test1$`Pr(>F)`[2], 2) else NA
    
    test2 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment:RES05_gender = 0")
    }, error = function(e) list(PrF = NA))
    pval2 <- if (!is.null(test2$PrF)) round(test2$`Pr(>F)`[2], 2) else NA
    
    test3 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment = INT_treatment:RES05_gender")
    }, error = function(e) list(PrF = NA))
    pval3 <- if (!is.null(test3$PrF)) round(test3$`Pr(>F)`[2], 2) else NA
    
    return(list(pval1 = pval1, pval2 = pval2, pval3 = pval3))
  } else if (model_type == "gender_general") {
    test1 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = 0")
    }, error = function(e) list(PrF = NA))
    pval1 <- if (!is.null(test1$PrF)) round(test1$`Pr(>F)`[2], 2) else NA
    
    test2 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment_general:RES05_gender = 0")
    }, error = function(e) list(PrF = NA))
    pval2 <- if (!is.null(test2$PrF)) round(test2$`Pr(>F)`[2], 2) else NA
    
    test3 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment_gender = INT_treatment_general")
    }, error = function(e) list(PrF = NA))
    pval3 <- if (!is.null(test3$PrF)) round(test3$`Pr(>F)`[2], 2) else NA
    
    test4 <- tryCatch({
      car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = INT_treatment_general:RES05_gender")
    }, error = function(e) list(PrF = NA))
    pval4 <- if (!is.null(test4$PrF)) round(test4$`Pr(>F)`[2], 2) else NA
    
    return(list(pval1 = pval1, pval2 = pval2, pval3 = pval3, pval4 = pval4))
  }
}

# Estimating the models
models_list <- list()
control_means <- list()
test_results <- list()

### Models with "any treatment"
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means[[i]] <- control_mean
  
  formula <- create_formula(dep_var, "any_treatment")
  model <- lm(formula, data = data_filtered)
  models_list[[i]] <- model
  
  test_results[[i]] <- calculate_tests(model, "any_treatment")
}

### Models with "gender and general treatment"
for (i in 1:length(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  j <- i + length(incum_dep_vars1)
  
  control_means[[j]] <- control_means[[i]]
  
  formula <- create_formula(dep_var, "gender_general")
  model <- lm(formula, data = data_filtered)
  models_list[[j]] <- model
  
  test_results[[j]] <- calculate_tests(model, "gender_general")
}

# Variables to display
outregvar2 <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general", "RES05_gender", "INT_treatment:RES05_gender", "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

# Column names
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# Additional lines for means and test results
additional_lines <- list(
  c("Observations", sapply(models_list, function(x) nobs(x))),
  c("Mean in Control without GQ", unlist(control_means)),
  c("Treatment with GQ = Treat without GQ", sapply(test_results[1:length(incum_dep_vars1)], function(x) x$pval3)),
  c("Gender Treat = General Treat without GQ", sapply(test_results[(length(incum_dep_vars1)+1):length(test_results)], function(x) x$pval3)),
  c("Gender Treat = General Treat with GQ", sapply(test_results[(length(incum_dep_vars1)+1):length(test_results)], function(x) x$pval4))
)

# Generate table
stargazer(models_list,
          type = "text",
          column.labels = col_names,
          keep = outregvar2,
          add.lines = additional_lines,
          digits = 2,
          title = "Table 1: Effects on Incumbent and Family Candidate Entry",
          out = "Table1_Incumbent_2010_completed.txt")



## ATTEMPTS AT SOMETHING NARROWER


## 1) still too wide

# Table 1: Effects on Incumbent and Family Candidate Entry
# Narrower version of the table.

# Reminder of column names
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# Panel A: Average Effects
panel_A_models <- models_list[1:6]
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")

stargazer(panel_A_models,
          type = "text",  
          column.labels = col_names,
          keep = panel_A_vars,
          digits = 2,
          title = "Panel A: Average Effects on Incumbent and Family Candidate Entry",
          covariate.labels = c("Treatment", "Reserved for Women 2005", "Treatment × Reserved 2005"),
          omit.stat = c("ser", "adj.rsq", "f"),
          notes.append = FALSE)

# Panel B: Effects by Type of Campaign
panel_B_models <- models_list[7:12]
panel_B_vars <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender", 
                  "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

stargazer(panel_B_models,
          type = "text",  
          column.labels = col_names,
          keep = panel_B_vars,
          digits = 2,
          title = "Panel B: Effects by Type of Campaign on Incumbent and Family Candidate Entry",
          covariate.labels = c("Gender Treatment", "General Treatment", "Reserved for Women 2005", 
                               "Gender Treatment × Reserved 2005", "General Treatment × Reserved 2005"),
          omit.stat = c("ser", "adj.rsq", "f"),
          notes.append = FALSE)




## 2) NEW ATTEMPT

# Table 1: Effects - Version Ultra-Compacte

# Noms de colonnes minimaux
col_names_short <- c("Inc Run", "Inc Vote", "Spouse Run", "Spouse Vote", "Other Run", "Other Vote")

# Panel A: Average Effects
panel_A_models <- models_list[1:6]
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")

stargazer(panel_A_models,
          type = "text",
          column.labels = col_names_short,
          keep = panel_A_vars,
          digits = 2,
          title = "Panel A: Average Effects",
          covariate.labels = c("Treatment", "Reserved 05", "Treatment × Reserved"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "1pt",
          notes.append = FALSE,
          notes = "Controls + district FE included")

# Panel B: Effects by Campaign Type
panel_B_models <- models_list[7:12]
panel_B_vars <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender", 
                  "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

stargazer(panel_B_models,
          type = "text",
          column.labels = col_names_short,
          keep = panel_B_vars,
          digits = 2,
          title = "Panel B: By Campaign Type",
          covariate.labels = c("Gender T", "General T", "Reserved 05", "Gender T × Res", "General T × Res"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "1pt",
          notes.append = FALSE,
          notes = "Controls + district FE included")

# Version ULTRA-MINIMALISTE si encore trop large
# Panel A - Version extrême
stargazer(panel_A_models,
          type = "text",
          column.labels = c("IncR", "IncV", "SpR", "SpV", "OthR", "OthV"),
          keep = panel_A_vars,
          digits = 2,
          title = "A: Average",
          covariate.labels = c("T", "R05", "T×R"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq", "n"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "0pt",
          notes.append = FALSE,
          notes = "")

# Panel B - Version extrême
stargazer(panel_B_models,
          type = "text",
          column.labels = c("IncR", "IncV", "SpR", "SpV", "OthR", "OthV"),
          keep = panel_B_vars,
          digits = 2,
          title = "B: By Type",
          covariate.labels = c("GT", "GenT", "R05", "GT×R", "GenT×R"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq", "n"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "0pt",
          notes.append = FALSE,
          notes = "")




### always narrower

# Table 1: Effects - Version Ultra-Compacte
# Noms de colonnes ultra-courts (2-3 caractères max)
col_names_ultra_short <- c("IR", "IV", "SR", "SV", "OR", "OV")

# Panel A: Average Effects
panel_A_models <- models_list[1:6]
panel_A_vars <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")

# Panel A - Version ultra-compacte
stargazer(panel_A_models,
          type = "text",
          column.labels = col_names_ultra_short,  # Noms ultra-courts
          keep = panel_A_vars,
          digits = 2,
          title = "Panel A: Average Effects",
          covariate.labels = c("T", "F", "T×F"),  # T=Treatment, F=Female, T×F=Interaction
          omit.stat = c("ser", "adj.rsq", "f", "rsq", "n"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "0pt",
          notes.append = FALSE,
          notes = "IR=Incumbent Run, IV=Incumbent Vote, SR=Spouse Run, SV=Spouse Vote, OR=Other Run, OV=Other Vote",
          star.cutoffs = c(0.05, 0.01, 0.001),  # Optionnel: ajuster les seuils de significativité
          header = FALSE)  # Supprime l'en-tête automatique de stargazer

# Alternative encore plus compacte si nécessaire:
# Vous pouvez aussi utiliser des numéros:
col_names_numeric <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

# Version avec numéros de colonnes
stargazer(panel_A_models,
          type = "text",
          column.labels = col_names_numeric,
          keep = panel_A_vars,
          digits = 2,
          title = "Panel A: Average Effects",
          covariate.labels = c("Treatment", "Female", "Treatment × Female"),
          omit.stat = c("ser", "adj.rsq", "f", "rsq", "n"),
          font.size = "tiny",
          no.space = TRUE,
          column.sep.width = "0pt",
          notes.append = FALSE,
          notes = "(1)-(2) Incumbent, (3)-(4) Spouse, (5)-(6) Other; Odd cols=Run, Even cols=Vote",
          header = FALSE)




### non formatted version to fit the PDF!

## CREATE OUTPUT TABLE FOR INCUMBENT ANALYSIS
create_outreg_table <- function(models_list, incum_dep_vars1, outregvar2, control_means, test_results) {
  
  # Function to extract model results
  extract_model_results <- function(model) {
    if (is.null(model)) return(NULL)
    
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    results <- list()
    for (var in outregvar2) {
      matching_vars <- rownames(coef_table)[grepl(paste0("^", gsub(":", ":", var), "$"), rownames(coef_table))]
      if (length(matching_vars) > 0) {
        var_name <- matching_vars[1]
        coef_val <- coef_table[var_name, "Estimate"]
        se_val <- coef_table[var_name, "Std. Error"]
        pval <- coef_table[var_name, "Pr(>|t|)"]
        
        # Adding significance stars
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
  
  # Create column names
  col_names <- c("Incumbent Runs", "Incumbent Vote Share",
                 "Incumbent Spouse Runs", "Incumbent Spouse Vote Share", 
                 "Other Family Member Runs", "Other Family Member Vote Share")
  
  # Initialize the final table
  final_table <- data.frame(
    Variable = character(),
    stringsAsFactors = FALSE
  )
  
  # Add columns for each dependent variable (both any_treatment and gender_general models)
  for (i in 1:length(incum_dep_vars1)) {
    final_table[[paste0(col_names[i], "_Any")]] <- character()
    final_table[[paste0(col_names[i], "_Detailed")]] <- character()
  }
  
  # Extract results for each variable
  for (var in outregvar2) {
    # Coefficient row
    coef_row <- data.frame(Variable = var, stringsAsFactors = FALSE)
    se_row <- data.frame(Variable = paste0("  ", var, "_se"), stringsAsFactors = FALSE)
    
    # For each dependent variable
    for (i in 1:length(incum_dep_vars1)) {
      # Any treatment model (first 6 models)
      any_results <- extract_model_results(models_list[[i]])
      coef_row[[paste0(col_names[i], "_Any")]] <- if (!is.null(any_results[[var]])) any_results[[var]]$coef_formatted else "NA"
      se_row[[paste0(col_names[i], "_Any")]] <- if (!is.null(any_results[[var]])) any_results[[var]]$se_formatted else "(NA)"
      
      # Gender/General treatment model (models 7-12)
      detailed_results <- extract_model_results(models_list[[i + length(incum_dep_vars1)]])
      coef_row[[paste0(col_names[i], "_Detailed")]] <- if (!is.null(detailed_results[[var]])) detailed_results[[var]]$coef_formatted else "NA"
      se_row[[paste0(col_names[i], "_Detailed")]] <- if (!is.null(detailed_results[[var]])) detailed_results[[var]]$se_formatted else "(NA)"
    }
    
    final_table <- rbind(final_table, coef_row, se_row)
  }
  
  # Add additional statistics rows
  # Observations
  obs_row <- data.frame(Variable = "Observations", stringsAsFactors = FALSE)
  for (i in 1:length(incum_dep_vars1)) {
    obs_row[[paste0(col_names[i], "_Any")]] <- nobs(models_list[[i]])
    obs_row[[paste0(col_names[i], "_Detailed")]] <- nobs(models_list[[i + length(incum_dep_vars1)]])
  }
  final_table <- rbind(final_table, obs_row)
  
  # Control means
  mean_row <- data.frame(Variable = "Mean in Control without GQ", stringsAsFactors = FALSE)
  for (i in 1:length(incum_dep_vars1)) {
    mean_row[[paste0(col_names[i], "_Any")]] <- control_means[[i]]
    mean_row[[paste0(col_names[i], "_Detailed")]] <- control_means[[i]]
  }
  final_table <- rbind(final_table, mean_row)
  
  # Test results
  test1_row <- data.frame(Variable = "Treatment with GQ = Treat without GQ", stringsAsFactors = FALSE)
  for (i in 1:length(incum_dep_vars1)) {
    test1_row[[paste0(col_names[i], "_Any")]] <- test_results[[i]]$pval3
    test1_row[[paste0(col_names[i], "_Detailed")]] <- ""
  }
  final_table <- rbind(final_table, test1_row)
  
  test2_row <- data.frame(Variable = "Gender Treat = General Treat without GQ", stringsAsFactors = FALSE)
  for (i in 1:length(incum_dep_vars1)) {
    test2_row[[paste0(col_names[i], "_Any")]] <- ""
    test2_row[[paste0(col_names[i], "_Detailed")]] <- test_results[[i + length(incum_dep_vars1)]]$pval3
  }
  final_table <- rbind(final_table, test2_row)
  
  test3_row <- data.frame(Variable = "Gender Treat = General Treat with GQ", stringsAsFactors = FALSE)
  for (i in 1:length(incum_dep_vars1)) {
    test3_row[[paste0(col_names[i], "_Any")]] <- ""
    test3_row[[paste0(col_names[i], "_Detailed")]] <- test_results[[i + length(incum_dep_vars1)]]$pval4
  }
  final_table <- rbind(final_table, test3_row)
  
  return(final_table)
}

# Create the main table
main_table <- create_outreg_table(models_list, incum_dep_vars1, outregvar2, control_means, test_results)

# Display the table
print(main_table)

# Optional: Save as CSV
# write.csv(main_table, "incumbent_regression_results.csv", row.names = FALSE)

# Optional: Create a more formatted table using knitr
library(knitr)
kable(main_table, caption = "Table 1: Incumbent and Incumbent's Family Entry")