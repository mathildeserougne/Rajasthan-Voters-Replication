### REPLICATION - TABLE 3 - CANDIDATE CHARACTERISTICS ###############

# ok that was so bad it's actually embarrassing
# let's try this again.


# Packages to install if necessary and libraries
install.packages(c("haven", "dplyr", "fixest", "stargazer", "broom", "aod"))
library(dplyr)
library(fixest)  # helps with fixed effects
library(stargazer)  # generates output tables
library(haven)    # reads .dta files
library(broom)    # extracts regression results
library(aod)      # wald tests


## Defining the macros

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", 
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", 
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", 
                "RES05_sc", "RES05_st")

# Main regression variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Dependent variables
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", 
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")


## Data processing

# Replace path with yours for upcoming extraction and filtering: 
data_path <- "~/work/Electoral data cleaned.dta"

# Reading
cat("Loading the data: \n")
data <- read_dta(data_path)

# Structure of the data:
cat("Dimension of the data:", dim(data), "\n")
cat("Main present variables:\n")
key_vars <- c("RES10_gender", "GP_tag", "INT_treatment", "RES05_gender", 
              "X_anytr_genderres05", "INC05_can_run", "district")
print(key_vars[key_vars %in% names(data)])

# Filtering the data
cat("Filters on non-gender reservation and one observation per GP. \n")
data_filtered <- data %>%
  filter(RES10_gender == 0,  
         GP_tag == 1)        


## Function for the regressions

run_regression_analysis <- function(data_subset, subset_name) {
  
  cat(paste("\n=== ANALYSE POUR:", subset_name, "===\n"))
  cat("Nombre d'observations:", nrow(data_subset), "\n")
  
  results_list <- list()
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    #cat(paste("  Variable dépendante:", dep_var, "\n"))
    
    # check if variable exists
    if (!dep_var %in% names(data_subset)) {
      cat(paste("    ATTENTION: Variable", dep_var, "non trouvée!\n"))
      next
    }
    
    # control mean
    control_subset <- data_subset %>%
      filter(INT_treatment == 0, RES05_gender == 0)
    
    if (nrow(control_subset) == 0) {
      cat(paste("    ATTENTION: Aucune observation de contrôle pour", dep_var, "\n"))
      control_mean <- NA
    } else {
      control_mean <- control_subset %>%
        summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean_val) %>%
        round(2)
    }
    
    cat(paste("    Moyenne contrôle (non-WR 2005):", control_mean, "\n"))
    
    # check presence of controls
    available_controls <- gpcontrols[gpcontrols %in% names(data_subset)]
    missing_controls <- gpcontrols[!gpcontrols %in% names(data_subset)]
    
    if (length(missing_controls) > 0) {
      cat(paste("    Variables de contrôle manquantes:", paste(missing_controls, collapse = ", "), "\n"))
    }
    
    # regression formula with available variables
    reg_vars <- c(outregvar2, available_controls)
    reg_vars <- reg_vars[reg_vars %in% names(data_subset)]
    
    formula_str <- paste(dep_var, "~", 
                         paste(reg_vars, collapse = " + "), "+ factor(district)")
    
    cat(paste("    Formule:", formula_str, "\n"))
    
    # do the regression
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
        
        cat(paste("    Test joint p-value:", pval, "\n"))
      } else {
        pval <- NA
        cat("    Test joint: variables non trouvées\n")
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
      
      cat(paste("    Régression réussie, N =", nrow(model$model), "\n"))
      
    }, error = function(e) {
      cat(paste("    ERREUR dans la régression:", e$message, "\n"))
      results_list[[i]] <- NULL
    })
  }
  
  # Clean NULL results
  results_list <- results_list[!sapply(results_list, is.null)]
  
  return(results_list)
}


## Analysis for the THREE sub-samples 

# 1. All GPs
cat("Analyse pour l'échantillon complet...\n")
results_full <- run_regression_analysis(data_filtered, "Full Sample")

# 2. Incumbent can run
cat("Analyse pour INC05_can_run == 1...\n")
data_inc_can_run <- data_filtered %>% filter(INC05_can_run == 1)
results_inc_can <- run_regression_analysis(data_inc_can_run, "Incumbent Can Run")

# 3. Incumbent cannot run
cat("Analyse pour INC05_can_run == 0...\n")
data_inc_cannot_run <- data_filtered %>% filter(INC05_can_run == 0)
results_inc_cannot <- run_regression_analysis(data_inc_cannot_run, "Incumbent Cannot Run")


## Create the output table

create_outreg_table <- function(results_list_full, results_list_inc, results_list_no_inc) {
  
  cat("\n=== CREATING THE TABLE ===\n")
  
  # function to extract the results from the model
  extract_model_results <- function(model_result) {
    if (is.null(model_result) || is.null(model_result$model)) {
      return(NULL)
    }
    
    model <- model_result$model
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    # Extraire les coefficients des variables d'intérêt
    results <- list()
    
    for (var in outregvar2) {
      # Chercher la variable (peut avoir un nom légèrement différent)
      matching_vars <- rownames(coef_table)[grepl(var, rownames(coef_table))]
      
      if (length(matching_vars) > 0) {
        var_name <- matching_vars[1]  # Prendre le premier match
        
        coef_val <- coef_table[var_name, "Estimate"]
        se_val <- coef_table[var_name, "Std. Error"]
        pval <- coef_table[var_name, "Pr(>|t|)"]
        
        # Étoiles de significativité
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
  
  # Créer le tableau final
  final_table <- data.frame(
    Variable = character(),
    Full_Sample = character(),
    Inc_Can_Run = character(),
    Inc_Cannot_Run = character(),
    stringsAsFactors = FALSE
  )
  
  # Pour chaque variable dépendante
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    cat(paste("\nTraitement de", dep_var, "...\n"))
    
    # Extraire les résultats pour chaque sous-échantillon
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
    
    # Ajouter les lignes pour chaque variable explicative
    for (var in outregvar2) {
      
      # Ligne des coefficients
      coef_row <- data.frame(
        Variable = var,
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$coef_formatted else "NA",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$coef_formatted else "NA",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$coef_formatted else "NA",
        stringsAsFactors = FALSE
      )
      
      # Ligne des erreurs standard
      se_row <- data.frame(
        Variable = paste0("  ", var, "_se"),
        Full_Sample = if (!is.null(full_results[[var]])) full_results[[var]]$se_formatted else "(NA)",
        Inc_Can_Run = if (!is.null(inc_results[[var]])) inc_results[[var]]$se_formatted else "(NA)",
        Inc_Cannot_Run = if (!is.null(no_inc_results[[var]])) no_inc_results[[var]]$se_formatted else "(NA)",
        stringsAsFactors = FALSE
      )
      
      final_table <- rbind(final_table, coef_row, se_row)
    }
    
    # Ajouter une ligne de séparation entre les variables dépendantes
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

# Function pour créer des statistiques récapitulatives
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

# Extraire les résultats pour tous les sous-échantillons
all_results <- c(results_full, results_inc_can, results_inc_cannot)
final_table <- extract_coef_stats(all_results)

# ===============================================================================
# AFFICHER ET SAUVEGARDER LES RÉSULTATS
# ===============================================================================

# Afficher un aperçu des résultats
cat("\n=== RÉSULTATS DES RÉGRESSIONS ===\n")
print(final_table)

# Créer un tableau plus formaté avec stargazer
# Combiner tous les modèles pour stargazer
all_models <- c(
  lapply(results_full, function(x) x$model),
  lapply(results_inc_can, function(x) x$model),
  lapply(results_inc_cannot, function(x) x$model)
)

# Noms des colonnes pour le tableau
col_names <- c(
  paste("Full Sample", 1:6),
  paste("Inc Can Run", 1:6),
  paste("Inc Cannot Run", 1:6)
)

# Créer le tableau avec stargazer
stargazer(all_models,
          type = "text",  # Changer en "latex" ou "html" selon le besoin
          column.labels = col_names,
          keep = outregvar2,
          add.lines = list(
            c("District FE", rep("Yes", length(all_models))),
            c("GP Controls", rep("Yes", length(all_models)))
          ),
          digits = 2,
          title = "Table 3: Candidate Entry - 2010 Elections",
          out = "Table3_Candidates_2010.txt")

# ===============================================================================
# FONCTION ALTERNATIVE AVEC FIXEST (plus rapide pour les effets fixes)
# ===============================================================================

run_regression_fixest <- function(data_subset, subset_name) {
  
  results_fixest <- list()
  
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    # Calculer la moyenne de contrôle
    control_mean <- data_subset %>%
      filter(INT_treatment == 0, RES05_gender == 0) %>%
      summarise(mean_val = mean(get(dep_var), na.rm = TRUE)) %>%
      pull(mean_val) %>%
      round(2)
    
    # Régression avec fixest (plus efficace pour les effets fixes)
    controls_str <- paste(c(outregvar2, gpcontrols), collapse = " + ")
    formula_str <- paste(dep_var, "~", controls_str, "| district")
    
    model_fixest <- feols(as.formula(formula_str), data = data_subset)
    
    # Test joint avec fixest
    joint_test <- wald(model_fixest, c("RES05_gender", "X_anytr_genderres05"))
    pval <- round(joint_test$p, 2)
    
    results_fixest[[i]] <- list(
      model = model_fixest,
      dep_var = dep_var,
      control_mean = control_mean,
      joint_test_pval = pval,
      subset = subset_name
    )
  }
  
  return(results_fixest)
}

# Alternative avec fixest (décommentez si vous préférez cette méthode)
# results_full_fixest <- run_regression_fixest(data_filtered, "Full Sample")
# results_inc_can_fixest <- run_regression_fixest(data_inc_can_run, "Incumbent Can Run")
# results_inc_cannot_fixest <- run_regression_fixest(data_inc_cannot_run, "Incumbent Cannot Run")

cat("\n=== CONVERSION TERMINÉE ===\n")
cat("Les résultats sont stockés dans les objets 'results_full', 'results_inc_can', et 'results_inc_cannot'\n")
cat("Le tableau final est sauvegardé dans 'Table3_Candidates_2010.txt'\n")