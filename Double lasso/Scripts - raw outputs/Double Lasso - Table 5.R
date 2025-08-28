# clean script for table 5 double lasso

## LIBRARIES AND PACKAGES 
# packages
install.packages(c("fixest", "glmnet", "sandwich", "lmtest", "broom"))
# libraries
library(tidyverse)
library(glmnet)
library(sandwich)
library(lmtest)
library(broom)
library(fixest)
library(haven)
library(car)  

## PREPARING THE DATA ##

# Loading and merging the data
path <- file.path("~/work")
electoral <- read_dta(file.path(path, "Electoral data cleaned.dta")) %>%
  select(district, ps, gp, starts_with("GP_"), starts_with("RES"), starts_with("std_HH_NREGA"), index_empl_pre_svysample) %>%
  distinct()
household <- read_dta(file.path(path, "Household survey data cleaned.dta"))

df <- household %>%
  left_join(electoral, by = c("district", "ps", "gp")) %>%
  filter(!is.na(index_empl_pre_svysample))

# Deleting the duplicates after the join
dups <- names(df) %>% str_subset("\\.x$")
for (vx in dups) {
  base <- str_remove(vx, "\\.x$")
  vy <- paste0(base, ".y")
  df <- df %>%
    mutate(!!base := coalesce(
      if (vx %in% names(df)) .data[[vx]] else NULL,
      if (vy %in% names(df)) .data[[vy]] else NULL
    ))
}
df <- df %>% select(-matches("\\.(x|y)$"))

# Transformation of the variables at individual level
long_vars <- c("A_age", "A_educ", "A_literacy", "D_NREGA_work",
               "E_know_minimumwage", "E_know_maximumdays", "E_know_sarpanchrole_projects",
               "E_know_sarpanchrole_jobcard", "E_know_sarpanchrole_work", "E_know_jobcardapplication",
               "E_know_waitingdays", "E_know_unemploymentallowance", "E_know_postofficepay",
               "E_rate_NREGAimplementation", "E_rate_NREGAimplementation_g", "E_rate_NREGAimplementation_vg",
               "E_rate_sarpanchperformance", "E_rate_sarpanchperformance_g", "E_rate_sarpanchperformance_vg",
               "F_rank_publicgoods_road", "F_rank_publicgoods_pump", "F_rank_publicgoods_school",
               "F_rate_publicgoods_road", "F_rate_publicgoods_pump", "F_rate_publicgoods_school",
               "F_optimistic_sarpanch", "F_optimistic_govprograms")

base_dups <- long_vars[long_vars %in% names(df)]
df <- df %>% select(-all_of(base_dups))

# Dealing with NA
df_long <- df %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols = matches(paste0("^(", paste(long_vars, collapse = "|"), ")_(m|f)$")),
    names_to = c(".value", "gender"),
    names_pattern = "^(.*)_(m|f)$"
  ) %>%
  mutate(
    # just create an indicator (not making it become a 0 when NA)
    C_I_AgeMissing = is.na(A_age),
    A_age = ifelse(is.na(A_age), median(A_age, na.rm = TRUE), A_age)
  )

# Index variables
df_long <- df_long %>%
  mutate(
    C_I_AgeBelow25 = A_age < 25,
    C_I_Age2535 = between(A_age, 25, 34),
    C_I_Age3545 = between(A_age, 35, 44),
    C_I_AgeAbove45 = A_age >= 45,
    C_I_Female = gender == "f",
    C_I_Literate = A_literacy == 4,
    C_I_EducNone = A_educ == 0,
    C_I_EducPrimary = A_educ > 0 & A_educ <= 5,
    C_I_EducLowerSec = A_educ > 5 & A_educ <= 9,
    C_I_EducUpperSec = A_educ > 9 & A_educ <= 12,
    C_I_EducTertiary = A_educ > 12 & !is.na(A_educ),
    C_I_Missing = if_any(c(A_educ, A_literacy), is.na),
    C_H_bpl = H_bpl == 1,
    C_H_ownland = H_ownland == 1,
    C_H_hindu = H_religion == 1,
    C_H_CasteGen = H_caste %in% c(1, 5),
    C_H_CasteOBC = H_caste %in% c(2, 6),
    C_H_CasteSC = H_caste == 3,
    C_H_CasteST = H_caste == 4,
    C_H_Missing = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na)
  )


# Converting dummies to numerals
indcontrols <- grep("^C_I_", names(df_long), value = TRUE)
hhcontrols <- grep("^C_H_", names(df_long), value = TRUE)
dummy_vars <- c(indcontrols, hhcontrols)
df_long <- df_long %>%
  mutate(across(all_of(dummy_vars), ~as.numeric(replace_na(., 0))))

# Variables' normalisation
to_z <- grep("^(E_know_|F_rate_|E_rate)", names(df_long), value = TRUE)
ref_mu <- df_long %>%
  filter(INT_treatment == 0, RES05_gender == 0) %>%
  summarise(across(all_of(to_z), mean, na.rm = TRUE))
ref_sd <- df_long %>%
  filter(INT_treatment == 0, RES05_gender == 0) %>%
  summarise(across(all_of(to_z), sd, na.rm = TRUE))
df_long[to_z] <- map2_dfc(df_long[to_z], names(df_long[to_z]), \(x, v) (x - ref_mu[[v]]) / ref_sd[[v]])

# Composite variables
df_long <- df_long %>%
  mutate(
    E_know_nregarules = rowMeans(cbind(E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(pick(starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(cbind(E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(pick(starts_with("F_rate_publicgoods_")), na.rm = TRUE),
    TEMP_index = index_empl_pre_svysample,
    TEMP_X_res_index = RES05_gender * TEMP_index,
    TEMP_X_anytr_index = INT_treatment * TEMP_index,
    TEMP_X_anytr_res_index = INT_treatment * RES05_gender * TEMP_index
  )

# Complete list of candidates
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
#census_controls <- c("CENSUS_PCA2001_tot_pop", "CENSUS_PCA2001_tot_lit", "CENSUS_PCA2001_tot_sc",
#"CENSUS_PCA2001_tot_st", "CENSUS_PCA2001_tot_aglb", "CENSUS_PCA2001_tot_nm_hh",
#"CENSUS_PCA2001_tot_cult", "CENSUS_VD2001_power_dom", "CENSUS_VD2001_drnk_wat_f",
#"CENSUS_VD2001_edu_fac", "CENSUS_VD2001_medi_fac")
full_controls_candidates <- c(gpcontrols,
                              census_controls, 
                              indcontrols, hhcontrols)

# Dependent variables
dep_set1 <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")
dep_set2 <- c("index_empl_pre_svysample", dep_set1)

# Deleting NA
df_long_clean <- df_long %>%
  filter(!is.na(INT_treatment), !is.na(TEMP_index), !is.na(TEMP_X_anytr_index)) %>%
  filter_at(vars(dep_set2), all_vars(!is.na(.)))

# Checking amount of observations
table(df_long_clean$RES05_gender)


## DOUBLE LASSO ##

double_lasso_fit_forced <- function(data_df, y_name, d_names, x_candidates, baseline_names = NULL) {
  # checks
  if (!y_name %in% names(data_df)) stop(paste("Missing dependent variable :", y_name))
  missing_d <- setdiff(d_names, names(data_df))
  if (length(missing_d) > 0) stop(paste("Missing variables :", paste(missing_d, collapse = ", ")))
  x_candidates <- x_candidates[x_candidates %in% names(data_df)]
  if (length(x_candidates) == 0) stop("No valid candidate.")
  
  # filter NA
  keep_rows <- complete.cases(data_df[, c(y_name, d_names, x_candidates)])
  if (sum(keep_rows) < 10) {
    warning(paste("Not enough obs after filter for", y_name, ". Remaining obs:", sum(keep_rows)))
    return(NULL)
  }
  
  # check if constant
  if (sd(data_df[[y_name]][keep_rows], na.rm = TRUE) < 0.01) {
    warning(paste("Constant or quasi-constant dependent variable", y_name))
    return(NULL)
  }
  
  # X and D matrices
  X_all <- model.matrix(~ . - 1, data = data_df[keep_rows, x_candidates, drop = FALSE])
  D_mat <- model.matrix(~ . - 1, data = data_df[keep_rows, d_names, drop = FALSE])
  y_vec <- data_df[[y_name]][keep_rows]
  
  # D_mat not constant
  if (any(apply(D_mat, 2, function(x) sd(x, na.rm = TRUE) < 0.01))) {  
    warning(paste("Insterest variables constant/quasi-constant for ", y_name))
    return(NULL)
  }
  
  # Double lasso for Y ~ D + X
  X_y <- cbind(D_mat, X_all)
  pf_y <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_all))) 
  
  set.seed(123)
  fit1 <- tryCatch({
    cv.glmnet(X_y, y_vec, alpha = 1, penalty.factor = pf_y)
  }, error = function(e) {
    warning(paste("Error in cv.glmnet pour Y ~ D + X :", e$message))
    NULL
  })
  
  sel_y <- character(0)
  if (!is.null(fit1)) {
    coef1 <- coef(fit1, s = "lambda.min")
    sel_y <- setdiff(rownames(coef1)[which(coef1 != 0)], "(Intercept)")
  } else {
    warning("Fail on first double lasso stage Y ~ D + X")
    return(NULL)
  }
  
  # Double lasso for D_k ~ X
  sel_d <- character(0)
  for (k in 1:ncol(D_mat)) {
    pf_d <- rep(1, ncol(X_all))  
    
    fitk <- tryCatch({
      cv.glmnet(X_all, D_mat[, k], alpha = 1, penalty.factor = pf_d)
    }, error = function(e) {
      warning(paste("Error in cv.glmnet pour D_k ~ X :", e$message))
      NULL
    })
    
    if (!is.null(fitk)) {
      coefk <- coef(fitk, s = "lambda.min")
      sel_k <- setdiff(rownames(coefk)[which(coefk != 0)], "(Intercept)")
      sel_d <- union(sel_d, sel_k)
    } else {
      warning(paste("Fail of the lasso for D_k ~ X, variable :", colnames(D_mat)[k]))
      return(NULL)
    }
  }
  
  # Final selection of controls
  selected_controls <- union(sel_y, sel_d)
  if (length(selected_controls) == 0) selected_controls <- colnames(X_all)
  
  # Final model: force inclusion of interest variables
  final_data <- data.frame(Y = y_vec, D_mat, X_all[, selected_controls, drop = FALSE])
  
  # Build formula with interest variables and selected controls
  rhs_terms <- c(colnames(D_mat), colnames(X_all[, selected_controls, drop = FALSE]))
  
  # Add interactions
  if ("INT_treatment" %in% colnames(D_mat) && "TEMP_index" %in% colnames(D_mat)) {
    rhs_terms <- c(rhs_terms, "INT_treatment:TEMP_index")
  }
  if ("INT_treatment" %in% colnames(D_mat) && "RES05_gender" %in% colnames(D_mat)) {
    rhs_terms <- c(rhs_terms, "INT_treatment:RES05_gender")
  }
  if ("TEMP_index" %in% colnames(D_mat) && "RES05_gender" %in% colnames(D_mat)) {
    rhs_terms <- c(rhs_terms, "TEMP_index:RES05_gender")
  }
  if ("INT_treatment" %in% colnames(D_mat) && "TEMP_index" %in% colnames(D_mat) && "RES05_gender" %in% colnames(D_mat)) {
    rhs_terms <- c(rhs_terms, "INT_treatment:TEMP_index:RES05_gender")
  }
  rhs_terms <- unique(rhs_terms)
  
  if (length(rhs_terms) == 0) {
    warning("No valid variable.")
    return(NULL)
  }
  
  f_final <- as.formula(paste("Y ~", paste(rhs_terms, collapse = " + ")))
  
  # Model estimation
  model <- tryCatch({
    lm(f_final, data = final_data)
  }, error = function(e) {
    warning(paste("Error in final model estimation:", e$message))
    NULL
  })
  
  if (is.null(model)) return(NULL)
  
  list(
    model = model,
    selected_controls = selected_controls,
    coefficients = coef(model),
    se = sqrt(diag(vcovHC(model, type = "HC1")))
  )
}



## EXECUTION OF REGRESSIONS ##

# Initialisation
models_list_forced <- vector("list", length(dep_set2) * 2)
selected_controls_list_forced <- vector("list", length(dep_set2) * 2)
results_list_forced <- vector("list", length(dep_set2) * 2)

# Estimation loop
for (x in 0:1) {  # RES05_gender
  for (i in seq_along(dep_set2)) {
    y_name <- dep_set2[i]
    index <- x * length(dep_set2) + i
    
    cat("\n--- Estimation for", y_name, "| RES05_gender =", x, "---\n")
    
    filtered_data <- df_long_clean %>% filter(RES05_gender == x)
    if (nrow(filtered_data) < 10) {
      warning(paste("Not enough data for RES05_gender =", x, "et", y_name))
      next
    }
    
    # Interest variables
    d_names <- c("INT_treatment", "TEMP_index", "RES05_gender")
    
    # Estimation
    result <- tryCatch({
      double_lasso_fit_forced(
        data_df = filtered_data,
        y_name = y_name,
        d_names = d_names,
        x_candidates = full_controls_candidates
      )
    }, error = function(e) {
      warning(paste("Error :", e$message))
      NULL
    })
    
    if (!is.null(result) && !is.null(result$model)) {
      models_list_forced[[index + 1]] <- result$model
      selected_controls_list_forced[[index + 1]] <- result$selected_controls
      results_list_forced[[index + 1]] <- result
    } else {
      warning(paste("Model not estimated", y_name, "| RES05_gender =", x))
      models_list_forced[[index + 1]] <- NULL
      selected_controls_list_forced[[index + 1]] <- character(0)
    }
  }
}


# Numer of valid estimated models
valid_models <- sapply(models_list, function(x) !is.null(x))
cat("\nSuccessful estimations :", sum(valid_models), "\n")

# Displaying the results
if (sum(valid_models) > 0) {
  cat("\n==== MODELS ====\n")
  for (i in which(valid_models)) {
    cat("Model", i, ":", names(dep_set2)[((i-1) %% length(dep_set2)) + 1],
        "| RES05_gender =", (i-1) %/% length(dep_set2), "\n")
    print(summary(models_list[[i]]))
    if (!is.null(results_list[[i]]$vif)) {
      cat("VIF :\n")
      print(results_list[[i]]$vif)
    }
  }
} else {
  cat("\nNo model estimated.\n")
}

# Display selected controls.
cat("\n==== CONTROLS SELECTED BY DOUBLE LASSO ====\n")
for (i in seq_along(selected_controls_list)) {
  if (length(selected_controls_list[[i]]) > 0) {
    cat("Model", i, ":", names(dep_set2)[((i-1) %% length(dep_set2)) + 1],
        "| RES05_gender =", (i-1) %/% length(dep_set2), "\n")
    cat("Selected controls :", paste(selected_controls_list[[i]], collapse = ", "), "\n\n")
  } else {
    cat("Model", i, ":", names(dep_set2)[((i-1) %% length(dep_set2)) + 1],
        "| RES05_gender =", (i-1) %/% length(dep_set2), "\n")
    cat("No selected controls.\n\n")
  }
}




## Selecting what to display. (to compare with ols)

library(broom)

coefs_to_extract <- c(
  "TEMP_index",
  "INT_treatment",
  "RES05_gender",
  "TEMP_X_anytr_index",      
  "TEMP_X_res_index",        
  "TEMP_X_anytr_res_index",  
  "X_anytr_genderres05"      
)


extract_and_display_coefficients <- function(models_list, selected_controls_list, dep_set2) {
  for (i in seq_along(models_list)) {
    if (!is.null(models_list[[i]])) {
      model <- models_list[[i]]
      tidy_model <- tidy(model, conf.int = TRUE)
      model_name <- names(dep_set2)[((i-1) %% length(dep_set2)) + 1]
      gender <- (i-1) %/% length(dep_set2)
      
      cat("\n==== Modèle", i, ":", model_name, "| RES05_gender =", gender, "====\n")
      
      relevant_coefs <- tidy_model %>%
        filter(term %in% coefs_to_extract) %>%
        select(term, estimate, std.error, p.value)
      
      if (nrow(relevant_coefs) > 0) {
        print(relevant_coefs)
      } else {
        cat("No coefficient found in the model.\n")
      }
      
      # Display selected controls
      cat("\nSelected controls :\n")
      cat(paste(selected_controls_list[[i]], collapse = ", "), "\n")
    }
  }
}

extract_and_display_coefficients(models_list, selected_controls_list, dep_set2)




