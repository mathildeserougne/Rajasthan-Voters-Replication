## T1 - Incumbents 2010 - Summary statistics ##################################

# Packages and libraries
install.packages(c("haven","dplyr", "broom", "stargazer", "carData"))
library(haven)        # .dta files
library(dplyr)        
library(broom)        # regression results
library(stargazer)    # regression tables
library(car)          # tests (linearHypothesis)

# Importing the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Restricting it to the relevant sample
# We exclude GPs that were gender reserved, and keep one observation only per GP
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1) %>%  
  filter(GP_tag == 1) %>%                                
  filter(INC05_can_run == 1)

# Creating variables to decompose the effect
# We look at effect on incumbent, effet on incumbent's family, global effect of someone from the dynasty 
outcomes <- c("running", "won", "voteshare")
for(outcome in outcomes) {
  col_fam <- paste0("INCorFAM05_", outcome)
  col_inc <- paste0("INC05_", outcome)
  col_new <- paste0("FAMnotINC05_", outcome)
  
  if(col_fam %in% names(data) & col_inc %in% names(data)) {
    data[[col_new]] <- data[[col_fam]] - data[[col_inc]]
  }
}

# Defining the dependent variables

# first list: just "running"
incum_dep_vars <- c("INC05_running", "FAMnotINC05_running", "INCSPOUSE05_running", "INCOTHER05_running")

# second list: extended one because intersection of the three outcomes + the four categories of people
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won", 
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won",
                     "INCorFAM05_running", "INCorFAM05_voteshare", "INCorFAM05_won")


# Location of the results table
table_path <- "~work/T1_Incumbent_2010"


# Extract the regression statistics
extract_reg_stats <- function(model, dep_var, data) {
  # mean of the control group
  control_mean <- data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean_val) %>%
    round(3)
  
  # hypothesis tests - adapted to the variables
  # test 1: RES05_gender + X_anytr_genderres05 = 0
  test1_pval <- tryCatch({
    if("RES05_gender" %in% names(coef(model)) & "X_anytr_genderres05" %in% names(coef(model))) {
      test_result <- linearHypothesis(model, "RES05_gender + X_anytr_genderres05 = 0")
      round(test_result$`Pr(>F)`[2], 3)
    } else { NA }
  }, error = function(e) NA)
  
  # test 2: INT_treatment = RES05_gender
  test2_pval <- tryCatch({
    if("INT_treatment" %in% names(coef(model)) & "RES05_gender" %in% names(coef(model))) {
      test_result <- linearHypothesis(model, "INT_treatment - RES05_gender = 0")
      round(test_result$`Pr(>F)`[2], 3)
    } else { NA }
  }, error = function(e) NA)
  
  return(list(control_mean = control_mean, test1_pval = test1_pval, test2_pval = test2_pval))
}



### FIRST REGRESSIONS: general treatment 

results_list1 <- list()

for(i in seq_along(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  # existing variable check
  if(!dep_var %in% names(data)) {
    warning(paste("Variable", dep_var, "not found in the data"))
    next
  }
  
  # regression formula
  formula_str <- paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 + factor(district)")
  
  # regression
  model <- lm(as.formula(formula_str), data = data)
  
  # extracting the statistics
  stats <- extract_reg_stats(model, dep_var, data)
  
  # stocking the results
  results_list1[[i]] <- list(
    model = model,
    dep_var = dep_var,
    stats = stats
  )
}



### SECOND REGRESSION : gender or general treatment

## THIS ONE GIVES WARNINGS

results_list2 <- list()

for(i in seq_along(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  if(!dep_var %in% names(data)) {
    warning(paste("Variable", dep_var, "not found in the data"))
    next
  }
  
  # formula for treatment by type (gender vs general)
  formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + 
                       X_gendertr_genderres05 + X_generaltr_genderres05 + factor(district)")
  
  model <- lm(as.formula(formula_str), data = data)
  
  # specific tests
  control_mean <- data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean_val) %>%
    round(3)
  
  # Test: INT_treatment_gender = INT_treatment_general
  test1_pval <- tryCatch({
    if("INT_treatment_gender" %in% names(coef(model)) & "INT_treatment_general" %in% names(coef(model))) {
      test_result <- linearHypothesis(model, "INT_treatment_gender - INT_treatment_general = 0")
      round(test_result$`Pr(>F)`[2], 3)
    } else { NA }
  }, error = function(e) NA)
  
  # complex test for WR=1
  test2_pval <- tryCatch({
    req_vars <- c("INT_treatment_gender", "X_gendertr_genderres05", 
                  "INT_treatment_general", "X_generaltr_genderres05")
    if(all(req_vars %in% names(coef(model)))) {
      test_result <- linearHypothesis(model, 
                                      "(INT_treatment_gender + X_gendertr_genderres05) - (INT_treatment_general + X_generaltr_genderres05) = 0")
      round(test_result$`Pr(>F)`[2], 3)
    } else { NA }
  }, error = function(e) NA)
  
  results_list2[[i]] <- list(
    model = model,
    dep_var = dep_var,
    stats = list(control_mean = control_mean, test1_pval = test1_pval, test2_pval = test2_pval)
  )
}


### WARNINGS I AM TRYING TO SOLVE ##################################################

# Vérifiez si ces variables existent
treatment_vars <- c("INT_treatment_gender", "INT_treatment_general", 
                    "X_gendertr_genderres05", "X_generaltr_genderres05")
cat("Variables existantes:\n")
print(treatment_vars[treatment_vars %in% names(data)])
cat("Variables manquantes:\n")
print(treatment_vars[!treatment_vars %in% names(data)])

# Vérifiez si les variables ont de la variation
sapply(data[treatment_vars], function(x) length(unique(x, na.rm = TRUE)))




# Testez un modèle individuellement pour débugger
dep_var <- incum_dep_vars1[1]  # Premier modèle
formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + 
                     X_gendertr_genderres05 + X_generaltr_genderres05 + factor(district)")

model <- lm(as.formula(formula_str), data = data)

# Vérifiez les coefficients estimés
cat("=== Coefficients du modèle ===\n")
print(names(coef(model)))

cat("\n=== Résumé du modèle ===\n")
print(summary(model)$coefficients[1:10, ])  # Premiers coefficients seulement

# Test simple d'abord
cat("\n=== Test simple ===\n")
tryCatch({
  test_result <- linearHypothesis(model, "INT_treatment_gender = INT_treatment_general")
  print(test_result)
}, error = function(e) {
  cat("Erreur:", e$message, "\n")
})

# Test complexe
cat("\n=== Test complexe ===\n")
tryCatch({
  test_result <- linearHypothesis(model, 
                                  c("INT_treatment_gender + X_gendertr_genderres05 = INT_treatment_general + X_generaltr_genderres05"))
  print(test_result)
}, error = function(e) {
  cat("Erreur:", e$message, "\n")
})




# Affichage correct du résumé
cat("\n=== Résumé du modèle ===\n")
coef_summary <- summary(model)$coefficients
print(coef_summary)

cat("\n=== Nombre de coefficients ===\n")
cat("Total coefficients:", nrow(coef_summary), "\n")

# Test simple d'abord
cat("\n=== Test simple ===\n")
tryCatch({
  test_result <- linearHypothesis(model, "INT_treatment_gender = INT_treatment_general")
  print(test_result)
}, error = function(e) {
  cat("Erreur:", e$message, "\n")
})

# Test complexe avec syntaxe corrigée
cat("\n=== Test complexe ===\n")
tryCatch({
  test_result <- linearHypothesis(model, 
                                  "INT_treatment_gender + X_gendertr_genderres05 = INT_treatment_general + X_generaltr_genderres05")
  print(test_result)
}, error = function(e) {
  cat("Erreur:", e$message, "\n")
})

#########################################################################

# DEUXIÈME SÉRIE DE RÉGRESSIONS: Traitement par genre vs général
results_list2 <- list()

for(i in seq_along(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  if(!dep_var %in% names(data)) {
    warning(paste("Variable", dep_var, "not found in the data"))
    next
  }
  
  # Formule pour traitement par type (gender vs general)
  formula_str <- paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + 
                       X_gendertr_genderres05 + X_generaltr_genderres05 + factor(district)")
  
  model <- lm(as.formula(formula_str), data = data)
  
  # Tests spécifiques
  control_mean <- data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean_val) %>%
    round(3)
  
  # Test 1: INT_treatment_gender = INT_treatment_general (syntaxe simplifiée)
  test1_pval <- tryCatch({
    test_result <- linearHypothesis(model, "INT_treatment_gender = INT_treatment_general")
    round(test_result$`Pr(>F)`[2], 3)
  }, error = function(e) {
    cat("Erreur test1 pour", dep_var, ":", e$message, "\n")
    NA
  })
  
  # Test 2: Effets combinés (syntaxe corrigée)
  test2_pval <- tryCatch({
    test_result <- linearHypothesis(model, 
                                    "INT_treatment_gender + X_gendertr_genderres05 = INT_treatment_general + X_generaltr_genderres05")
    round(test_result$`Pr(>F)`[2], 3)
  }, error = function(e) {
    cat("Erreur test2 pour", dep_var, ":", e$message, "\n")
    NA
  })
  
  results_list2[[i]] <- list(
    model = model,
    dep_var = dep_var,
    stats = list(control_mean = control_mean, test1_pval = test1_pval, test2_pval = test2_pval)
  )
}



### Output table

# extracting the models
models1 <- lapply(results_list1, function(x) x$model)
models2 <- lapply(results_list2, function(x) x$model)

# columns names
column_names1 <- paste("(", 1:length(models1), ")", sep="")
column_names2 <- paste("(", (length(models1)+1):(length(models1)+length(models2)), ")", sep="")

# generating the table (html/latex)
stargazer(models1, 
          type = "html",
          out = paste0(table_path, "_panel_A.html"),
          title = "Panel A: Average Effects",
          column.labels = column_names1,
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          digits = 2,
          add.lines = list(
            c("District FE", rep("Yes", length(models1))),
            c("GP controls", rep("Yes", length(models1)))
          ))

stargazer(models2, 
          type = "html",
          out = paste0(table_path, "_panel_B.html"),
          title = "Panel B: Effects by Type of Campaign",
          column.labels = column_names2,
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          digits = 2,
          add.lines = list(
            c("District FE", rep("Yes", length(models2))),
            c("GP controls", rep("Yes", length(models2)))
          ))


# ok it gives nonsense


# Overview table VERSION 1 


create_summary_table <- function(results_list, panel_name) {
  summary_df <- data.frame()
  
  for(i in seq_along(results_list)) {
    if(is.null(results_list[[i]])) next
    
    model <- results_list[[i]]$model
    dep_var <- results_list[[i]]$dep_var
    stats <- results_list[[i]]$stats
    
    # extract main coeffs
    coef_summary <- summary(model)$coefficients
    
    # variables
    main_vars <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
    
    for(var in main_vars) {
      if(var %in% rownames(coef_summary)) {
        coef_val <- round(coef_summary[var, "Estimate"], 3)
        se_val <- round(coef_summary[var, "Std. Error"], 3)
        stars <- ifelse(coef_summary[var, "Pr(>|t|)"] < 0.001, "***",
                        ifelse(coef_summary[var, "Pr(>|t|)"] < 0.01, "**",
                               ifelse(coef_summary[var, "Pr(>|t|)"] < 0.05, "*", "")))
        
        # add to summary dataframe
        row_data <- data.frame(
          Panel = panel_name,
          Variable = var,
          Column = i,
          Coefficient = paste0(coef_val, stars),
          SE = paste0("(", se_val, ")"),
          stringsAsFactors = FALSE
        )
        summary_df <- rbind(summary_df, row_data)
      }
    }
  }
  
  return(summary_df)
}

# Création des tableaux récapitulatifs
panel_A_summary <- create_summary_table(results_list1, "Panel A: Average Effects")
panel_B_summary <- create_summary_table(results_list2, "Panel B: Effects by Type of Campaign")

# Affichage des résultats
print("=== PANEL A: Average Effects ===")
print(panel_A_summary)

print("=== PANEL B: Effects by Type of Campaign ===")
print(panel_B_summary)

# Sauvegarde en CSV
write.csv(panel_A_summary, paste0(table_path, "_panel_A_summary.csv"), row.names = FALSE)
write.csv(panel_B_summary, paste0(table_path, "_panel_B_summary.csv"), row.names = FALSE)


# issue with this one is that it has too many columns
# it also displays the "won" outcome (not just runs, voteshare) and the outcomes for the group 'any member of the family'



## SUMMARY TABLES VERSION 2
# corrected selection of columns
# Colonne 1 Incumbent Runs
# Colonne 2 Incumbent Voteshare  
# Colonne 3 Incumbent Spouse Runs
# Colonne 4 Incumbent Spouse Voteshare
# Colonne 5 Other Family Member Runs
# Colonne 6 Other Family Member Voteshare

create_summary_table_2 <- function(results_list, panel_name) {
  
  column_mapping <- c(1, 2, 4, 5, 7, 8)  # keeping tables from the previous one
  new_column_numbers <- 1:6  # new numbers
  
  summary_df <- data.frame()
  
  for(i in seq_along(column_mapping)) {
    original_col <- column_mapping[i]
    new_col <- new_column_numbers[i]
    
    if(original_col <= length(results_list) && !is.null(results_list[[original_col]])) {
      model <- results_list[[original_col]]$model
      dep_var <- results_list[[original_col]]$dep_var
      stats <- results_list[[original_col]]$stats
      
      # extract main coeffs
      coef_summary <- summary(model)$coefficients
      
      # Panel A
      if(grepl("Panel A", panel_name)) {
        main_vars <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
      } else {
        # Panel B
        main_vars <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender", 
                       "X_gendertr_genderres05", "X_generaltr_genderres05")
      }
      
      for(var in main_vars) {
        if(var %in% rownames(coef_summary)) {
          coef_val <- round(coef_summary[var, "Estimate"], 3)
          se_val <- round(coef_summary[var, "Std. Error"], 3)
          stars <- ifelse(coef_summary[var, "Pr(>|t|)"] < 0.001, "***",
                          ifelse(coef_summary[var, "Pr(>|t|)"] < 0.01, "**",
                                 ifelse(coef_summary[var, "Pr(>|t|)"] < 0.05, "*", "")))
          
          # add to summary dataframe
          row_data <- data.frame(
            Panel = panel_name,
            Variable = var,
            Column = new_col,  # utilise la nouvelle numérotation
            Coefficient = paste0(coef_val, stars),
            SE = paste0("(", se_val, ")"),
            stringsAsFactors = FALSE
          )
          summary_df <- rbind(summary_df, row_data)
        }
      }
    }
  }
  
  return(summary_df)
}


panel_A_summary <- create_summary_table_2(results_list1, "Panel A: Average Effects")
panel_B_summary <- create_summary_table_2(results_list2, "Panel B: Effects by Type of Campaign")

