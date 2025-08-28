## SCRIPT DOING THE DOUBLE LASSO REGRESSIONS REPLICATING THE TABLES ###########

####### LIBRARIES AND PACKAGES #################################################

install.packages(c("tidyverse", "glmnet", "sandwich", "lmtest", "car", "modelsummary", "broom", "haven"))
library(tidyverse)
library(glmnet)
library(sandwich)
library(lmtest)
library(car)
library(modelsummary)
library(broom)
library(haven)

############################# TABLE 1 ##########################################

## DATA AND MACROS ##

# Baseline list of controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")


# Full list of candidates
full_controls_candidates <- c(
  # baseline elements
  "GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st",
  
  # Added : from the census
  "CENSUS_PCA2001_tot_pop", "CENSUS_PCA2001_tot_lit", "CENSUS_PCA2001_tot_sc",
  "CENSUS_PCA2001_tot_st", "CENSUS_PCA2001_tot_aglb", "CENSUS_PCA2001_tot_nm_hh",
  "CENSUS_PCA2001_tot_cult", "CENSUS_VD2001_power_dom", "CENSUS_VD2001_drnk_wat_f",
  "CENSUS_VD2001_edu_fac", "CENSUS_VD2001_medi_fac"
)

# Dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Data loading and filtering
data <- haven::read_dta("~/work/Electoral data cleaned.dta")

data_filtered <- data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1, GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running   = INCorFAM05_running  - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won       = INCorFAM05_won - INC05_won
  )

data_filtered %>% select(all_of(full_controls_candidates)) %>% summary()


## FUNCTIONS ##

# Non zero coefficients, no intercept
get_nonzero_names <- function(cf, drop = "(Intercept)") {
  cf_mat <- as.matrix(cf)
  nz <- which(cf_mat != 0)
  vars <- rownames(cf_mat)[nz]
  setdiff(vars, drop)
}


# Double lasso function
double_lasso_fit <- function(data_df, y_name, d_names, x_candidates, baseline_names = NULL, fe_name = "district", se_cluster = NULL, family = "gaussian") {
  # FE
  if (!is.null(fe_name)) {
    fe_vec <- data_df[[fe_name]]
    fe_mat <- model.matrix(~ factor(fe_vec) - 1)
    colnames(fe_mat) <- paste0("fe_", levels(factor(fe_vec)))
  } else {
    fe_mat <- NULL
  }
  
  # X : candidates
  X_all <- data_df %>% dplyr::select(all_of(x_candidates))
  X_mat <- model.matrix(~ . - 1, data = X_all)
  
  # Filter NA rows
  keep_rows_X <- complete.cases(X_mat)
  if (!is.null(fe_mat)) {
    keep_rows_FE <- complete.cases(fe_mat)
    keep_rows <- keep_rows_X & keep_rows_FE
    X_mat <- X_mat[keep_rows, , drop = FALSE]
    fe_mat <- fe_mat[keep_rows, , drop = FALSE]
  } else {
    X_mat <- X_mat[keep_rows_X, , drop = FALSE]
    keep_rows <- keep_rows_X
  }
  
  baseline_idx <- if (!is.null(baseline_names)) {
    which(colnames(X_mat) %in% baseline_names)
  } else integer(0)
  
  if (!is.null(fe_mat)) {
    X_mat <- cbind(X_mat, fe_mat)
    fe_idx <- (ncol(X_mat) - ncol(fe_mat) + 1):ncol(X_mat)
  } else {
    fe_idx <- integer(0)
  }
  
  # Y and D
  missing_d <- setdiff(d_names, names(data_df))
  if (length(missing_d) > 0) {
    for (nm in missing_d) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      stopifnot(all(parts %in% names(data_df)))
      data_df[[nm]] <- data_df[[parts[1]]] * data_df[[parts[2]]]
    }
  }
  
  y_vec <- as.numeric(data_df[[y_name]])
  D_mat <- as.matrix(data_df %>% dplyr::select(all_of(d_names)))
  if (is.null(ncol(D_mat))) D_mat <- matrix(D_mat, ncol = 1)
  
  # Filter y_vec and D_mat
  y_vec <- y_vec[keep_rows]
  D_mat <- D_mat[keep_rows, , drop = FALSE]
  if (!is.null(se_cluster)) se_cluster <- se_cluster[keep_rows]
  
  # Checks
  y_constant <- length(unique(y_vec)) < 2
  d_constant <- apply(D_mat, 2, function(z) length(unique(z)) < 2)
  
  # Y ~ D + X
  sel_y <- character(0); lambda_y <- NA
  if (!y_constant) {
    X_y <- cbind(D_mat, X_mat)
    pf_y <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_mat)))
    if (length(fe_idx) > 0) pf_y[ncol(D_mat) + fe_idx] <- 0
    
    if (all(pf_y == 0)) {
      sel_y <- character(0)
      lambda_y <- NA
    } else {
      set.seed(123)
      fit1 <- tryCatch({
        cv.glmnet(X_y, y_vec, alpha = 1, family = family, penalty.factor = pf_y)
      }, error = function(e) NULL)
      
      if (!is.null(fit1)) {
        coef1 <- coef(fit1, s = "lambda.min")
        cf_mat1 <- as.matrix(coef1)
        nz1 <- which(cf_mat1 != 0)
        sel_y <- rownames(cf_mat1)[nz1]
        sel_y <- setdiff(sel_y, c("(Intercept)", colnames(D_mat)))
        lambda_y <- fit1$lambda.min
      } else {
        sel_y <- character(0)
        lambda_y <- NA
      }
    }
  }
  
  # D_k ~ X
  pf_d <- rep(1, ncol(X_mat))
  if (length(baseline_idx) > 0) pf_d[baseline_idx] <- 1
  if (length(fe_idx) > 0) pf_d[fe_idx] <- 0
  
  sel_d <- character(0)
  d_lambdas <- rep(NA, ncol(D_mat))
  
  if (!all(pf_d == 0)) {
    for (k in seq_len(ncol(D_mat))) {
      if (d_constant[k]) next
      set.seed(123)
      fitk <- tryCatch({
        cv.glmnet(X_mat, D_mat[, k], alpha = 1, family = "gaussian", penalty.factor = pf_d)
      }, error = function(e) NULL)
      
      if (!is.null(fitk)) {
        coefk <- coef(fitk, s = "lambda.min")
        cf_matk <- as.matrix(coefk)
        nz_k <- which(cf_matk != 0)
        sel_k <- rownames(cf_matk)[nz_k]
        sel_k <- setdiff(sel_k, "(Intercept)")
        sel_d <- union(sel_d, sel_k)
        d_lambdas[k] <- fitk$lambda.min
      }
    }
  }
  
  # Union and final regression
  selected_controls <- union(sel_y, sel_d)
  if (length(fe_idx) > 0) selected_controls <- union(selected_controls, colnames(X_mat)[fe_idx])
  
  X_final <- X_mat[, selected_controls, drop = FALSE]
  final_df <- data.frame(Y = y_vec, D_mat, X_final)
  colnames(final_df)[2:(1 + ncol(D_mat))] <- colnames(D_mat)
  
  rhs <- paste(c(colnames(D_mat), colnames(X_final)), collapse = " + ")
  f_final <- as.formula(paste("Y ~", rhs))
  lm_final <- lm(f_final, data = final_df)
  
  vc <- if (!is.null(se_cluster)) {
    sandwich::vcovCL(lm_final, cluster = se_cluster)
  } else {
    sandwich::vcovHC(lm_final, type = "HC1")
  }
  
  coefs_rob <- lmtest::coeftest(lm_final, vcov = vc)
  
  list(
    selected_controls = selected_controls,
    model = lm_final,
    robust_coefs = coefs_rob,
    lambda_y = lambda_y,
    d_lambdas = d_lambdas,
    y_constant = y_constant,
    d_constant = d_constant
  )
}

# Linear tests (same regressions as table 1)
calculate_tests <- function(model, model_type) {
  if (model_type == "any_treatment") {
    pval1 <- tryCatch(car::linearHypothesis(model, "RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
    pval2 <- tryCatch(car::linearHypothesis(model, "INT_treatment:RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
    pval3 <- tryCatch(car::linearHypothesis(model, "INT_treatment = INT_treatment:RES05_gender")$`Pr(>F)`[2], error = function(e) NA)
    return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2), pval3 = round(pval3, 2)))
  } else {
    pval1 <- tryCatch(car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
    pval2 <- tryCatch(car::linearHypothesis(model, "INT_treatment_general:RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
    pval3 <- tryCatch(car::linearHypothesis(model, "INT_treatment_gender = INT_treatment_general")$`Pr(>F)`[2], error = function(e) NA)
    pval4 <- tryCatch(car::linearHypothesis(model, "INT_treatment_gender:RES05_gender = INT_treatment_general:RES05_gender")$`Pr(>F)`[2], error = function(e) NA)
    return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2), pval3 = round(pval3, 2), pval4 = round(pval4, 2)))
  }
}


# Loop on dependent variables

models_list <- list()
test_results <- list()
control_means <- list()

# Any treatment models
for (i in seq_along(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  
  
  # control mean
  control_means[[i]] <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>% round(2)
  
  
  # treatment variables 
  d_names_any <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
  
  # fe forced 
  
  results <- double_lasso_fit(
    data_df = data_filtered,
    y_name = dep_var,
    d_names = d_names_any,             
    x_candidates = full_controls_candidates, 
    baseline_names = gpcontrols, 
    fe_name = "district",                  
    se_cluster = data_filtered$district
  )
  
  
  models_list[[i]] <- results$model
  test_results[[i]] <- calculate_tests(results$model, "any_treatment")
}


# Gender / general models
for (i in seq_along(incum_dep_vars1)) {
  dep_var <- incum_dep_vars1[i]
  j <- i + length(incum_dep_vars1)
  
  control_means[[j]] <- control_means[[i]]
  
  d_names_gg <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                  "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")
  
  results <- double_lasso_fit(
    data_df = data_filtered,
    y_name = dep_var,
    d_names = d_names_gg,
    x_candidates = full_controls_candidates, 
    baseline_names = gpcontrols, 
    fe_name = "district",
    se_cluster = data_filtered$district
  )
  
  models_list[[j]] <- results$model
  test_results[[j]] <- calculate_tests(results$model, "gender_general")
}


## OUTPUT ##

# Summary of treatment effects
treat_vars_any <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
treat_vars_gg  <- c("INT_treatment_gender", "INT_treatment_general",
                    "RES05_gender", "INT_treatment_gender:RES05_gender",
                    "INT_treatment_general:RES05_gender")
all_treat_vars <- union(treat_vars_any, treat_vars_gg)

tidy_list <- map(models_list, ~ broom::tidy(.x))  
tidy_list <- Map(function(df, id) { df$model_id <- id; df }, tidy_list, seq_along(tidy_list))

treat_table <- bind_rows(tidy_list) %>%
  filter(term %in% all_treat_vars) %>%
  mutate(
    dep_var = rep(incum_dep_vars1, 2)[model_id],
    spec    = ifelse(model_id <= length(incum_dep_vars1), "any_treatment", "gender_general"),
    est_se  = sprintf("%.3f (%.3f)", estimate, std.error)
  ) %>%
  select(dep_var, spec, term, est_se, p.value)

cat("==== TREATMENT: COEFS AND SE ====\n")
print(treat_table, row.names = FALSE, n=Inf)
cat("\n\n")

print(treat_table, n = Inf, width = Inf)

# comparison of the two selected lists
baseline_controls <- gpcontrols

selected_controls_list <- map(models_list, ~ {
  vars <- names(coef(.x))[-1]           
  setdiff(vars, all_treat_vars)         
})

controls_diff <- tibble::tibble(
  dep_var     = rep(incum_dep_vars1, 2),
  spec        = c(rep("any_treatment", length(incum_dep_vars1)),
                  rep("gender_general", length(incum_dep_vars1))),
  selected    = selected_controls_list
) %>%
  mutate(
    kept    = map(selected, ~ intersect(.x, baseline_controls)),
    dropped = map(selected, ~ setdiff(baseline_controls, .x)),
    added   = map(selected, ~ setdiff(.x, baseline_controls)),
    n_selected = map_int(selected, length)
  )

cat("==== COMPARISON OF CONTROLS (baseline vs double lasso) ====\n")
for (r in 1:nrow(controls_diff)) {
  cat("\n--- DV:", controls_diff$dep_var[r],
      " | Spec:", controls_diff$spec[r], " ---\n")
  cat("# Selected controls =", controls_diff$n_selected[r], "\n")
  cat("Kept (baseline)   :", paste(controls_diff$kept[[r]],    collapse = ", "), "\n")
  cat("Dropped (baseline):", paste(controls_diff$dropped[[r]], collapse = ", "), "\n")
  cat("Added (non-base)  :", paste(controls_diff$added[[r]],   collapse = ", "), "\n")
}
cat("\n==== END ====\n")




#################################### TABLE 2 ###################################

# Data and macros
## Baseline list (initially chosen controls)
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

## Full list of candidates
full_controls_candidates <- c(
  # Baseline
  "GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st",
  # Other variables - from the census
  "CENSUS_PCA2001_tot_pop", "CENSUS_PCA2001_tot_lit", "CENSUS_PCA2001_tot_sc",
  "CENSUS_PCA2001_tot_st", "CENSUS_PCA2001_tot_aglb", "CENSUS_PCA2001_tot_nm_hh",
  "CENSUS_PCA2001_tot_cult", "CENSUS_VD2001_power_dom", "CENSUS_VD2001_drnk_wat_f",
  "CENSUS_VD2001_edu_fac", "CENSUS_VD2001_medi_fac"
)

# Dependent variables
incum_dep_vars2 <- c(
  "INC05_running", "INC05_voteshare", "INC05_won",
  "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
  "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won"
)

# Performance indices
indices <- c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")



# Loading and filtering the data
data <- haven::read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1, GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running   = INCorFAM05_running  - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won       = INCorFAM05_won - INC05_won,
    # Generating the performance indices
    index_empl_svy_0 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE),
    index_empl_svy_1 = rowMeans(select(., std_HH_NREGA_unmet_demand, std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f, std_HH_NREGA_waiting_time_m, std_HH_NREGA_waiting_time_f, std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_2 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_3 = rowMeans(select(., std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE)
  )

# Checking candidate controls
data_filtered %>% select(all_of(full_controls_candidates)) %>% summary()


## FUNCTIONS ##

# Function to extract the names of non zero coefficients
get_nonzero_names <- function(cf, drop = "(Intercept)") {
  cf_mat <- as.matrix(cf)
  nz <- which(cf_mat != 0)
  vars <- rownames(cf_mat)[nz]
  setdiff(vars, drop)
}

# Double lasso function
double_lasso_fit <- function(data_df, y_name, d_names, x_candidates, baseline_names = NULL, fe_name = "district", se_cluster = NULL, family = "gaussian") {
  # fixed effects
  if (!is.null(fe_name)) {
    fe_vec <- data_df[[fe_name]]
    fe_mat <- model.matrix(~ factor(fe_vec) - 1)
    colnames(fe_mat) <- paste0("fe_", levels(factor(fe_vec)))
  } else {
    fe_mat <- NULL
  }
  
  # X : candidates
  X_all <- data_df %>% dplyr::select(all_of(x_candidates))
  X_mat <- model.matrix(~ . - 1, data = X_all)
  
  # Filter NA
  keep_rows_X <- complete.cases(X_mat)
  if (!is.null(fe_mat)) {
    keep_rows_FE <- complete.cases(fe_mat)
    keep_rows <- keep_rows_X & keep_rows_FE
    X_mat <- X_mat[keep_rows, , drop = FALSE]
    fe_mat <- fe_mat[keep_rows, , drop = FALSE]
  } else {
    X_mat <- X_mat[keep_rows_X, , drop = FALSE]
    keep_rows <- keep_rows_X
  }
  
  baseline_idx <- if (!is.null(baseline_names)) {
    which(colnames(X_mat) %in% baseline_names)
  } else integer(0)
  
  if (!is.null(fe_mat)) {
    X_mat <- cbind(X_mat, fe_mat)
    fe_idx <- (ncol(X_mat) - ncol(fe_mat) + 1):ncol(X_mat)
  } else {
    fe_idx <- integer(0)
  }
  
  # Y and D
  missing_d <- setdiff(d_names, names(data_df))
  if (length(missing_d) > 0) {
    for (nm in missing_d) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      stopifnot(all(parts %in% names(data_df)))
      data_df[[nm]] <- data_df[[parts[1]]] * data_df[[parts[2]]]
    }
  }
  
  y_vec <- as.numeric(data_df[[y_name]])
  D_mat <- as.matrix(data_df %>% dplyr::select(all_of(d_names)))
  if (is.null(ncol(D_mat))) D_mat <- matrix(D_mat, ncol = 1)
  
  # Filter y_vec and D_mat
  y_vec <- y_vec[keep_rows]
  D_mat <- D_mat[keep_rows, , drop = FALSE]
  if (!is.null(se_cluster)) se_cluster <- se_cluster[keep_rows]
  
  # checks
  y_constant <- length(unique(y_vec)) < 2
  d_constant <- apply(D_mat, 2, function(z) length(unique(z)) < 2)
  
  # Y ~ D + X
  sel_y <- character(0); lambda_y <- NA
  if (!y_constant) {
    X_y <- cbind(D_mat, X_mat)
    pf_y <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_mat)))
    if (length(fe_idx) > 0) pf_y[ncol(D_mat) + fe_idx] <- 0
    
    if (all(pf_y == 0)) {
      sel_y <- character(0)
      lambda_y <- NA
    } else {
      set.seed(123)
      fit1 <- tryCatch({
        cv.glmnet(X_y, y_vec, alpha = 1, family = family, penalty.factor = pf_y)
      }, error = function(e) NULL)
      
      if (!is.null(fit1)) {
        coef1 <- coef(fit1, s = "lambda.min")
        cf_mat1 <- as.matrix(coef1)
        nz1 <- which(cf_mat1 != 0)
        sel_y <- rownames(cf_mat1)[nz1]
        sel_y <- setdiff(sel_y, c("(Intercept)", colnames(D_mat)))
        lambda_y <- fit1$lambda.min
      } else {
        sel_y <- character(0)
        lambda_y <- NA
      }
    }
  }
  
  # D_k ~ X
  pf_d <- rep(1, ncol(X_mat))
  if (length(baseline_idx) > 0) pf_d[baseline_idx] <- 1
  if (length(fe_idx) > 0) pf_d[fe_idx] <- 0
  
  sel_d <- character(0)
  d_lambdas <- rep(NA, ncol(D_mat))
  
  if (!all(pf_d == 0)) {
    for (k in seq_len(ncol(D_mat))) {
      if (d_constant[k]) next
      set.seed(123)
      fitk <- tryCatch({
        cv.glmnet(X_mat, D_mat[, k], alpha = 1, family = "gaussian", penalty.factor = pf_d)
      }, error = function(e) NULL)
      
      if (!is.null(fitk)) {
        coefk <- coef(fitk, s = "lambda.min")
        cf_matk <- as.matrix(coefk)
        nz_k <- which(cf_matk != 0)
        sel_k <- rownames(cf_matk)[nz_k]
        sel_k <- setdiff(sel_k, "(Intercept)")
        sel_d <- union(sel_d, sel_k)
        d_lambdas[k] <- fitk$lambda.min
      }
    }
  }
  
  # Union and final regression
  selected_controls <- union(sel_y, sel_d)
  if (length(fe_idx) > 0) selected_controls <- union(selected_controls, colnames(X_mat)[fe_idx])
  
  X_final <- X_mat[, selected_controls, drop = FALSE]
  final_df <- data.frame(Y = y_vec, D_mat, X_final)
  colnames(final_df)[2:(1 + ncol(D_mat))] <- colnames(D_mat)
  
  rhs <- paste(c(colnames(D_mat), colnames(X_final)), collapse = " + ")
  f_final <- as.formula(paste("Y ~", rhs))
  lm_final <- lm(f_final, data = final_df)
  
  vc <- if (!is.null(se_cluster)) {
    sandwich::vcovCL(lm_final, cluster = se_cluster)
  } else {
    sandwich::vcovHC(lm_final, type = "HC1")
  }
  
  coefs_rob <- lmtest::coeftest(lm_final, vcov = vc)
  
  list(
    selected_controls = selected_controls,
    model = lm_final,
    robust_coefs = coefs_rob,
    lambda_y = lambda_y,
    d_lambdas = d_lambdas,
    y_constant = y_constant,
    d_constant = d_constant
  )
}

# Function to compute marginal effects and do the tests
calculate_effects <- function(model, index_name, index_mean, index_sd) {
  # marginal effects
  effect_average <- coef(model)["INT_treatment"] + coef(model)[paste0("INT_treatment:", index_name)] * index_mean
  effect_good <- coef(model)["INT_treatment"] + coef(model)[paste0("INT_treatment:", index_name)] * (index_mean + index_sd)
  effect_bad <- coef(model)["INT_treatment"] + coef(model)[paste0("INT_treatment:", index_name)] * (index_mean - index_sd)
  
  # Wald tests
  test_1 <- tryCatch({
    waldtest(model, c(paste0("INT_treatment + INT_treatment:", index_name, " = 0"),
                      paste0(index_name, "=", index_mean)))
  }, error = function(e) NULL)
  
  test_2 <- tryCatch({
    waldtest(model, c(paste0("INT_treatment + INT_treatment:", index_name, " = 0")))
  }, error = function(e) NULL)
  
  pval_1 <- if (!is.null(test_1)) round(test_1$p.value, 2) else NA
  pval_2 <- if (!is.null(test_2)) round(test_2$p.value, 2) else NA
  
  list(
    effect_average = effect_average,
    effect_good = effect_good,
    effect_bad = effect_bad,
    pval_1 = pval_1,
    pval_2 = pval_2
  )
}


## LOOP FOR THE REGRESSIONS ## 

models_list <- list()
control_means <- list()
effects_list <- list()
pvals_1 <- list()
pvals_2 <- list()


i <- 0
for (x in 0:1) {  # loop on 2005 gender reservation
  for (dep_var in incum_dep_vars2) {
    for (index in indices) {
      i <- i + 1
      
      # control mean
      control_mean <- data_filtered %>%
        filter(INT_treatment == 0 & RES05_gender == x) %>%
        summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
        pull(mean) %>%
        round(2)
      control_means[[i]] <- control_mean
      
      # index stats
      index_stats <- data_filtered %>%
        filter(RES05_gender == x) %>%
        summarise(mean = mean(!!sym(index), na.rm = TRUE), sd = sd(!!sym(index), na.rm = TRUE))
      index_mean <- round(index_stats$mean, 2)
      index_sd <- round(index_stats$sd, 2)
      
      # interaction variables
      data_filtered <- data_filtered %>%
        mutate(
          TEMP_index = get(index),
          TEMP_X_anytr_index = INT_treatment * get(index)
        )
      
      # checking varaibles
      all_vars <- c(dep_var, "INT_treatment", "TEMP_index", "TEMP_X_anytr_index", gpcontrols, "district")
      if (all(all_vars %in% names(data_filtered))) {
        # checking clusters
        filtered_data <- data_filtered %>% filter(RES05_gender == x)
        if (length(unique(filtered_data$district)) < 2) {
          message("Not enough clusters for RES05_gender = ", x, " and dep_var = ", dep_var)
          next
        }
        
        # treatment variables
        d_names <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")
        
        # Double lasso
        results <- double_lasso_fit(
          data_df = filtered_data,
          y_name = dep_var,
          d_names = d_names,
          x_candidates = full_controls_candidates,
          baseline_names = gpcontrols,
          fe_name = "district",
          se_cluster = filtered_data$district  
        )
        
        models_list[[i]] <- results$model
        
        # compute effects and tests
        effects <- calculate_effects(results$model, "TEMP_index", index_mean, index_sd)
        effects_list[[i]] <- effects
        pvals_1[[i]] <- effects$pval_1
        pvals_2[[i]] <- effects$pval_2
        
        # display the effects
        cat("Effects on outcome", dep_var, "with index", index, "\n")
        cat("Effect for average performance:", effects$effect_average, "\n")
        cat("Effect for +1 sd performance:", effects$effect_good, "\n")
        cat("Effect for -1 sd performance:", effects$effect_bad, "\n")
      } else {
        message("Missing variables for ", dep_var, "with index", index)
      }
    }
  }
}


## OUTPUT ##

# Table of treatment variables' coefficients
treat_vars <- c("INT_treatment", "TEMP_index", "TEMP_X_anytr_index")
tidy_list <- map(models_list, ~ broom::tidy(.x))
tidy_list <- Map(function(df, id) { df$model_id <- id; df }, tidy_list, seq_along(tidy_list))
treat_table <- bind_rows(tidy_list) %>%
  filter(term %in% treat_vars) %>%
  mutate(
    dep_var = rep(incum_dep_vars2, each = length(indices) * 2)[model_id],
    index = rep(rep(indices, each = 2), length(incum_dep_vars2))[model_id],
    RES05_gender = rep(rep(c(0, 1), each = length(indices)), length(incum_dep_vars2))[model_id],
    est_se = sprintf("%.3f (%.3f)", estimate, std.error)
  ) %>%
  select(dep_var, index, RES05_gender, term, est_se, p.value)

cat("==== TREATMENT COEFFICIENTS ====\n")
print(treat_table, row.names = FALSE, n=Inf)

# Comparing selected controls and baseline controls
baseline_controls <- gpcontrols
selected_controls_list <- map(models_list, ~ {
  vars <- names(coef(.x))[-1]
  setdiff(vars, treat_vars)
})

controls_diff <- tibble::tibble(
  dep_var = rep(incum_dep_vars2, each = length(indices) * 2),
  index = rep(rep(indices, each = 2), length(incum_dep_vars2)),
  RES05_gender = rep(rep(c(0, 1), each = length(indices)), length(incum_dep_vars2)),
  selected = selected_controls_list
) %>%
  mutate(
    kept = map(selected, ~ intersect(.x, baseline_controls)),
    dropped = map(selected, ~ setdiff(baseline_controls, .x)),
    added = map(selected, ~ setdiff(.x, baseline_controls)),
    n_selected = map_int(selected, length)
  )

cat("\n==== COMPARING CONTROLS (baseline vs double lasso) ====\n")
for (r in 1:nrow(controls_diff)) {
  cat("\n--- DV:", controls_diff$dep_var[r],
      " | Index:", controls_diff$index[r],
      " | RES05_gender:", controls_diff$RES05_gender[r], "--- \n")
  cat("# Selected controls =", controls_diff$n_selected[r], "\n")
  cat("Kept (baseline)   :", paste(controls_diff$kept[[r]], collapse = ", "), "\n")
  cat("Dropped (baseline):", paste(controls_diff$dropped[[r]], collapse = ", "), "\n")
  cat("Added (non-base)  :", paste(controls_diff$added[[r]], collapse = ", "), "\n")
}

cat("\n==== END ====\n")




################################### TABLE 3 ####################################

## DATA AND MACROS ##

# Baseline list of controls for table 3
gpcontrols_table3 <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                       "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                       "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Full list of candidates
full_controls_candidates <- c(
  # Baseline elements: 
  "GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st",
  
  # Added from the census: 
  "CENSUS_PCA2001_tot_pop", "CENSUS_PCA2001_tot_lit", "CENSUS_PCA2001_tot_sc",
  "CENSUS_PCA2001_tot_st", "CENSUS_PCA2001_tot_aglb", "CENSUS_PCA2001_tot_nm_hh",
  "CENSUS_PCA2001_tot_cult", "CENSUS_VD2001_power_dom", "CENSUS_VD2001_drnk_wat_f",
  "CENSUS_VD2001_edu_fac", "CENSUS_VD2001_medi_fac"
)

# Dependent variables
dep_vars_table3 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
                     "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# Treatment variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Loading and filtering the data
data <- haven::read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>% filter(RES10_gender == 0, GP_tag == 1)



## FUNCTIONS ##

# Non zero coefficients
get_nonzero_names <- function(cf, drop = "(Intercept)") {
  cf_mat <- as.matrix(cf)
  nz <- which(cf_mat != 0)
  vars <- rownames(cf_mat)[nz]
  setdiff(vars, drop)
}

# Double lasso function

double_lasso_fit <- function(data_df, y_name, d_names, x_candidates, baseline_names = NULL, fe_name = "district", se_cluster = NULL, family = "gaussian") {
  # FE
  if (!is.null(fe_name)) {
    fe_vec <- data_df[[fe_name]]
    fe_mat <- model.matrix(~ factor(fe_vec) - 1)
    colnames(fe_mat) <- paste0("fe_", levels(factor(fe_vec)))
  } else {
    fe_mat <- NULL
  }
  
  # X : candidates
  X_all <- data_df %>% dplyr::select(all_of(x_candidates))
  X_mat <- model.matrix(~ . - 1, data = X_all)
  
  # Filter NA rows
  keep_rows_X <- complete.cases(X_mat)
  if (!is.null(fe_mat)) {
    keep_rows_FE <- complete.cases(fe_mat)
    keep_rows <- keep_rows_X & keep_rows_FE
    X_mat <- X_mat[keep_rows, , drop = FALSE]
    fe_mat <- fe_mat[keep_rows, , drop = FALSE]
  } else {
    X_mat <- X_mat[keep_rows_X, , drop = FALSE]
    keep_rows <- keep_rows_X
  }
  
  baseline_idx <- if (!is.null(baseline_names)) {
    which(colnames(X_mat) %in% baseline_names)
  } else integer(0)
  
  if (!is.null(fe_mat)) {
    X_mat <- cbind(X_mat, fe_mat)
    fe_idx <- (ncol(X_mat) - ncol(fe_mat) + 1):ncol(X_mat)
  } else {
    fe_idx <- integer(0)
  }
  
  # Y and D
  missing_d <- setdiff(d_names, names(data_df))
  if (length(missing_d) > 0) {
    for (nm in missing_d) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      stopifnot(all(parts %in% names(data_df)))
      data_df[[nm]] <- data_df[[parts[1]]] * data_df[[parts[2]]]
    }
  }
  
  y_vec <- as.numeric(data_df[[y_name]])
  D_mat <- as.matrix(data_df %>% dplyr::select(all_of(d_names)))
  if (is.null(ncol(D_mat))) D_mat <- matrix(D_mat, ncol = 1)
  
  # Filter y_vec and D_mat
  y_vec <- y_vec[keep_rows]
  D_mat <- D_mat[keep_rows, , drop = FALSE]
  if (!is.null(se_cluster)) se_cluster <- se_cluster[keep_rows]
  
  # Checks
  y_constant <- length(unique(y_vec)) < 2
  d_constant <- apply(D_mat, 2, function(z) length(unique(z)) < 2)
  
  # Y ~ D + X
  sel_y <- character(0); lambda_y <- NA
  if (!y_constant) {
    X_y <- cbind(D_mat, X_mat)
    pf_y <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_mat)))
    if (length(fe_idx) > 0) pf_y[ncol(D_mat) + fe_idx] <- 0
    
    if (all(pf_y == 0)) {
      sel_y <- character(0)
      lambda_y <- NA
    } else {
      set.seed(123)
      fit1 <- tryCatch({
        cv.glmnet(X_y, y_vec, alpha = 1, family = family, penalty.factor = pf_y)
      }, error = function(e) NULL)
      
      if (!is.null(fit1)) {
        coef1 <- coef(fit1, s = "lambda.min")
        cf_mat1 <- as.matrix(coef1)
        nz1 <- which(cf_mat1 != 0)
        sel_y <- rownames(cf_mat1)[nz1]
        sel_y <- setdiff(sel_y, c("(Intercept)", colnames(D_mat)))
        lambda_y <- fit1$lambda.min
      } else {
        sel_y <- character(0)
        lambda_y <- NA
      }
    }
  }
  
  # D_k ~ X
  pf_d <- rep(1, ncol(X_mat))
  if (length(baseline_idx) > 0) pf_d[baseline_idx] <- 1
  if (length(fe_idx) > 0) pf_d[fe_idx] <- 0
  
  sel_d <- character(0)
  d_lambdas <- rep(NA, ncol(D_mat))
  
  if (!all(pf_d == 0)) {
    for (k in seq_len(ncol(D_mat))) {
      if (d_constant[k]) next
      set.seed(123)
      fitk <- tryCatch({
        cv.glmnet(X_mat, D_mat[, k], alpha = 1, family = "gaussian", penalty.factor = pf_d)
      }, error = function(e) NULL)
      
      if (!is.null(fitk)) {
        coefk <- coef(fitk, s = "lambda.min")
        cf_matk <- as.matrix(coefk)
        nz_k <- which(cf_matk != 0)
        sel_k <- rownames(cf_matk)[nz_k]
        sel_k <- setdiff(sel_k, "(Intercept)")
        sel_d <- union(sel_d, sel_k)
        d_lambdas[k] <- fitk$lambda.min
      }
    }
  }
  
  # Union and final regression
  selected_controls <- union(sel_y, sel_d)
  if (length(fe_idx) > 0) selected_controls <- union(selected_controls, colnames(X_mat)[fe_idx])
  
  X_final <- X_mat[, selected_controls, drop = FALSE]
  final_df <- data.frame(Y = y_vec, D_mat, X_final)
  colnames(final_df)[2:(1 + ncol(D_mat))] <- colnames(D_mat)
  
  rhs <- paste(c(colnames(D_mat), colnames(X_final)), collapse = " + ")
  f_final <- as.formula(paste("Y ~", rhs))
  lm_final <- lm(f_final, data = final_df)
  
  vc <- if (!is.null(se_cluster)) {
    sandwich::vcovCL(lm_final, cluster = se_cluster)
  } else {
    sandwich::vcovHC(lm_final, type = "HC1")
  }
  
  coefs_rob <- lmtest::coeftest(lm_final, vcov = vc)
  
  list(
    selected_controls = selected_controls,
    model = lm_final,
    robust_coefs = coefs_rob,
    lambda_y = lambda_y,
    d_lambdas = d_lambdas,
    y_constant = y_constant,
    d_constant = d_constant
  )
}

# Checking missing data among candidates
missing_data <- data_filtered %>% select(all_of(full_controls_candidates)) %>% summarise_all(~sum(is.na(.)))
print(missing_data, n = Inf, width = Inf)

# Function for Wald tests
calculate_tests <- function(model) {
  pval1 <- tryCatch(car::linearHypothesis(model, "RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
  pval2 <- tryCatch(car::linearHypothesis(model, "X_anytr_genderres05 = 0")$`Pr(>F)`[2], error = function(e) NA)
  pval3 <- tryCatch(car::linearHypothesis(model, "INT_treatment + X_anytr_genderres05 = 0")$`Pr(>F)`[2], error = function(e) NA)
  
  return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2), pval3 = round(pval3, 2)))
}


## LOOP FOR REGRESSIONS ##

models_list <- list()
control_means <- list()
test_results <- list()

# For each subsample
subsets <- list(
  list(data = data_filtered, name = "Full Sample"),
  list(data = data_filtered %>% filter(INC05_can_run == 1), name = "Incumbent Can Run"),
  list(data = data_filtered %>% filter(INC05_can_run == 0), name = "Incumbent Cannot Run")
)

for (subset in subsets) {
  data_subset <- subset$data
  subset_name <- subset$name
  cat(paste("\n=== ANALYZE FOR:", subset_name, "===\n"))
  cat("Number of observations:", nrow(data_subset), "\n")
  
  # Filter NA data
  required_vars <- c(full_controls_candidates, "district", dep_vars_table3, outregvar2)
  data_subset <- data_subset[, required_vars, drop = FALSE]
  data_subset <- na.omit(data_subset)
  
  for (i in seq_along(dep_vars_table3)) {
    dep_var <- dep_vars_table3[i]
    cat(paste("Dependent variable:", dep_var, "\n"))
    
    if (!dep_var %in% names(data_subset)) {
      cat(paste("    ATTENTION: Variable", dep_var, "not found!\n"))
      next
    }
    
    # Control mean
    control_mean <- data_subset %>%
      filter(INT_treatment == 0, RES05_gender == 0) %>%
      summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean_val) %>%
      round(2)
    control_means[[i]] <- control_mean
    cat(paste("    Control mean (non-previously gender-reserved):", control_mean, "\n"))
    
    #  Treatment variables
    d_names <- outregvar2
    
    # Double lasso
    results <- double_lasso_fit(
      data_df = data_subset,
      y_name = dep_var,
      d_names = d_names,
      x_candidates = full_controls_candidates,
      baseline_names = gpcontrols_table3,
      fe_name = "district",
      se_cluster = data_subset$district
    )
    
    models_list[[paste0(subset_name, "_", dep_var)]] <- results$model
    test_results[[paste0(subset_name, "_", dep_var)]] <- calculate_tests(results$model)
  }
}


## OUTPUT ##

# Table of treatment variables' coefficients
tidy_list <- map(models_list, ~ broom::tidy(.x))
tidy_list <- Map(function(df, id) { df$model_id <- id; df }, tidy_list, names(models_list))
treat_table <- bind_rows(tidy_list) %>%
  filter(term %in% outregvar2) %>%
  mutate(
    dep_var = gsub("_[^_]+$", "", model_id),
    subset = gsub("^.*_", "", model_id),
    est_se = sprintf("%.3f (%.3f)", estimate, std.error)
  ) %>%
  select(dep_var, subset, term, est_se, p.value)

cat("==== TREATMENT: COEFS AND SE ====\n")
print(treat_table, row.names = FALSE, n=Inf)

# Comparison of selected and baseline controls
baseline_controls <- gpcontrols_table3
selected_controls_list <- map(models_list, ~ {
  vars <- names(coef(.x))[-1]
  setdiff(vars, outregvar2)
})

controls_diff <- tibble::tibble(
  dep_var = rep(dep_vars_table3, length(subsets)),
  subset = rep(names(subsets), each = length(dep_vars_table3)),
  selected = selected_controls_list
) %>%
  mutate(
    kept = map(selected, ~ intersect(.x, baseline_controls)),
    dropped = map(selected, ~ setdiff(baseline_controls, .x)),
    added = map(selected, ~ setdiff(.x, baseline_controls)),
    n_selected = map_int(selected, length)
  )

cat("\n==== CONTROLS COMPARISON (baseline vs double lasso) ====\n")
for (r in 1:nrow(controls_diff)) {
  cat("\n--- DV:", controls_diff$dep_var[r],
      " | Subset:", controls_diff$subset[r], " ---\n")
  cat("# Selected controls =", controls_diff$n_selected[r], "\n")
  cat("Kept (baseline)   :", paste(controls_diff$kept[[r]], collapse = ", "), "\n")
  cat("Dropped (baseline):", paste(controls_diff$dropped[[r]], collapse = ", "), "\n")
  cat("Added (non-base)  :", paste(controls_diff$added[[r]], collapse = ", "), "\n")
}

cat("\n==== END ====\n")




################################ TABLE 4 ########################################

## MACROS AND DATA ##

gpcontrols15 <- c(
  "GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st", "RES15_obc", "RES15_sc", "RES15_st"
)

# Full list of candidates as controls
full_controls_candidates_table4 <- c(
  gpcontrols15
  #"CAND_vil_illit",
  #"CAND_vil_aglb",
  #"CAND_village_SCST"
  #"CAND_village_index"
)

# Dependent variables
dep_vars_table4 <- c(
  "ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
  "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
  "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen"
)

# Treatment variables
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Loading and filtering the data
data <- read_dta("~/work/Electoral data 2015 cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1, RES15_gender == 0) %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run),
    district = as.factor(district) 
  )

# Interaction variables
for (var in c("INT_treatment", "X_anytr_genderres05", "RES05_gender")) {
  data_filtered <- data_filtered %>%
    mutate(!!paste0("X15_", var) := get(var) * (RES15_gender == 1))
}

# Checking NA among candidates
cat("NA in candidates :\n")
print(colSums(is.na(data_filtered[, full_controls_candidates_table4])))

# Filter NA for candidates and key variables
data_filtered <- data_filtered %>%
  filter(complete.cases(!!!syms(full_controls_candidates_table4)),
         complete.cases(!!!syms(dep_vars_table4)),
         complete.cases(!!!syms(outregvar2)))


## FUNCTIONS ##

# Function to extract names of non zero coefficients
get_nonzero_names <- function(cf, drop = "(Intercept)") {
  cf_mat <- as.matrix(cf)
  nz <- which(cf_mat != 0)
  vars <- rownames(cf_mat)[nz]
  setdiff(vars, drop)
}

# Double lasso function
double_lasso_fit <- function(data_df, y_name, d_names, x_candidates, baseline_names = NULL, fe_name = "district", se_cluster = NULL, family = "gaussian") {
  # fixed effects
  if (!is.null(fe_name)) {
    fe_vec <- data_df[[fe_name]]
    fe_mat <- model.matrix(~ fe_vec - 1)
    colnames(fe_mat) <- paste0("fe_", levels(fe_vec))
  } else {
    fe_mat <- NULL
  }
  
  # candidate controls
  X_all <- data_df %>% dplyr::select(all_of(x_candidates))
  X_mat <- model.matrix(~ . - 1, data = X_all)
  
  # filter NA rows for X_mat
  keep_rows_X <- complete.cases(X_mat)
  
  # filter NA rows for fe_mat
  if (!is.null(fe_mat)) {
    keep_rows_FE <- complete.cases(fe_mat)
    if (length(keep_rows_X) != length(keep_rows_FE)) {
      stop("Error : X_mat and fe_mat dimensions do not match.")
    }
    keep_rows <- keep_rows_X & keep_rows_FE
  } else {
    keep_rows <- keep_rows_X
  }
  
  # filter
  X_mat <- X_mat[keep_rows, , drop = FALSE]
  if (!is.null(fe_mat)) {
    fe_mat <- fe_mat[keep_rows, , drop = FALSE]
  }
  
  # baseline controls
  baseline_idx <- if (!is.null(baseline_names)) {
    which(colnames(X_mat) %in% baseline_names)
  } else integer(0)
  
  # add fixed effects X_mat
  if (!is.null(fe_mat)) {
    X_mat <- cbind(X_mat, fe_mat)
    fe_idx <- (ncol(X_mat) - ncol(fe_mat) + 1):ncol(X_mat)
  } else {
    fe_idx <- integer(0)
  }
  
  # dependent and treatment variables
  missing_d <- setdiff(d_names, names(data_df))
  if (length(missing_d) > 0) {
    for (nm in missing_d) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      stopifnot(all(parts %in% names(data_df)))
      data_df[[nm]] <- data_df[[parts[1]]] * data_df[[parts[2]]]
    }
  }
  
  y_vec <- as.numeric(data_df[[y_name]])
  D_mat <- as.matrix(data_df %>% dplyr::select(all_of(d_names)))
  if (is.null(ncol(D_mat))) D_mat <- matrix(D_mat, ncol = 1)
  
  # filter y_vec, D_mat, and se_cluster
  y_vec <- y_vec[keep_rows]
  D_mat <- D_mat[keep_rows, , drop = FALSE]
  if (!is.null(se_cluster)) se_cluster <- se_cluster[keep_rows]
  
  # checks
  y_constant <- length(unique(y_vec)) < 2
  d_constant <- apply(D_mat, 2, function(z) length(unique(z)) < 2)
  
  # selecting the controls Y ~ D + X
  sel_y <- character(0); lambda_y <- NA
  if (!y_constant) {
    X_y <- cbind(D_mat, X_mat)
    pf_y <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_mat)))
    if (length(fe_idx) > 0) pf_y[ncol(D_mat) + fe_idx] <- 0
    
    if (all(pf_y == 0)) {
      sel_y <- character(0)
      lambda_y <- NA
    } else {
      set.seed(123)
      fit1 <- tryCatch({
        cv.glmnet(X_y, y_vec, alpha = 1, family = family, penalty.factor = pf_y)
      }, error = function(e) NULL)
      
      if (!is.null(fit1)) {
        coef1 <- coef(fit1, s = "lambda.min")
        cf_mat1 <- as.matrix(coef1)
        nz1 <- which(cf_mat1 != 0)
        sel_y <- rownames(cf_mat1)[nz1]
        sel_y <- setdiff(sel_y, c("(Intercept)", colnames(D_mat)))
        lambda_y <- fit1$lambda.min
      } else {
        sel_y <- character(0)
        lambda_y <- NA
      }
    }
  }
  
  # selecting controls for D_k ~ X
  pf_d <- rep(1, ncol(X_mat))
  if (length(baseline_idx) > 0) pf_d[baseline_idx] <- 1
  if (length(fe_idx) > 0) pf_d[fe_idx] <- 0
  
  sel_d <- character(0)
  d_lambdas <- rep(NA, ncol(D_mat))
  
  if (!all(pf_d == 0)) {
    for (k in seq_len(ncol(D_mat))) {
      if (d_constant[k]) next
      set.seed(123)
      fitk <- tryCatch({
        cv.glmnet(X_mat, D_mat[, k], alpha = 1, family = "gaussian", penalty.factor = pf_d)
      }, error = function(e) NULL)
      
      if (!is.null(fitk)) {
        coefk <- coef(fitk, s = "lambda.min")
        cf_matk <- as.matrix(coefk)
        nz_k <- which(cf_matk != 0)
        sel_k <- rownames(cf_matk)[nz_k]
        sel_k <- setdiff(sel_k, "(Intercept)")
        sel_d <- union(sel_d, sel_k)
        d_lambdas[k] <- fitk$lambda.min
      }
    }
  }
  
  # union of selected controls
  selected_controls <- union(sel_y, sel_d)
  if (length(fe_idx) > 0) selected_controls <- union(selected_controls, colnames(X_mat)[fe_idx])
  
  # final regression
  X_final <- X_mat[, selected_controls, drop = FALSE]
  final_df <- data.frame(Y = y_vec, D_mat, X_final)
  colnames(final_df)[2:(1 + ncol(D_mat))] <- colnames(D_mat)
  
  rhs <- paste(c(colnames(D_mat), colnames(X_final)), collapse = " + ")
  f_final <- as.formula(paste("Y ~", rhs))
  lm_final <- lm(f_final, data = final_df)
  
  # standard errors
  vc <- if (!is.null(se_cluster)) {
    sandwich::vcovCL(lm_final, cluster = se_cluster)
  } else {
    sandwich::vcovHC(lm_final, type = "HC1")
  }
  
  coefs_rob <- lmtest::coeftest(lm_final, vcov = vc)
  
  list(
    selected_controls = selected_controls,
    model = lm_final,
    robust_coefs = coefs_rob,
    lambda_y = lambda_y,
    d_lambdas = d_lambdas,
    y_constant = y_constant,
    d_constant = d_constant
  )
}

# Function for Wald tests
calculate_tests <- function(model) {
  pval1 <- tryCatch(car::linearHypothesis(model, "RES05_gender = 0")$`Pr(>F)`[2], error = function(e) NA)
  pval2 <- tryCatch(car::linearHypothesis(model, "X_anytr_genderres05 = 0")$`Pr(>F)`[2], error = function(e) NA)
  pval3 <- tryCatch(car::linearHypothesis(model, "INT_treatment + X_anytr_genderres05 = 0")$`Pr(>F)`[2], error = function(e) NA)
  return(list(pval1 = round(pval1, 2), pval2 = round(pval2, 2), pval3 = round(pval3, 2)))
}

# Lists to stock results
models_list_table4 <- list()
control_means_table4 <- numeric(length(dep_vars_table4))
test_results_table4 <- list()

## LOOP ##

# Loop on dependent variables
for (i in seq_along(dep_vars_table4)) {
  dep_var <- dep_vars_table4[i]
  cat(paste("\n=== Dependent variable :", dep_var, "===\n"))
  
  # control mean
  control_mean <- data_filtered %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean_val = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean_val) %>%
    round(2)
  control_means_table4[i] <- control_mean
  cat(paste("    Control mean (previously unreserved) :", control_mean, "\n"))
  
  # double lasso
  results <- double_lasso_fit(
    data_df = data_filtered,
    y_name = dep_var,
    d_names = outregvar2,
    x_candidates = full_controls_candidates_table4,
    baseline_names = gpcontrols15,
    fe_name = "district",
    se_cluster = data_filtered$district
  )
  
  models_list_table4[[dep_var]] <- results$model
  test_results_table4[[dep_var]] <- calculate_tests(results$model)
}

# Table of coefficients for treatment variables
tidy_list_table4 <- map(models_list_table4, ~ broom::tidy(.x))
tidy_list_table4 <- Map(function(df, id) { df$model_id <- id; df }, tidy_list_table4, names(models_list_table4))
treat_table_table4 <- bind_rows(tidy_list_table4) %>%
  filter(term %in% outregvar2) %>%
  mutate(
    dep_var = model_id,
    est_se = sprintf("%.3f (%.3f)", estimate, std.error)
  ) %>%
  select(dep_var, term, est_se, p.value)
cat("\n==== TREATMENT VARIABLES COEFFICIENTS ====\n")
print(treat_table_table4, row.names = FALSE, n=Inf)

# Comparing selected controls vs baseline
baseline_controls <- gpcontrols15
selected_controls_list <- map(models_list_table4, ~ {
  vars <- names(coef(.x))[-1]
  setdiff(vars, outregvar2)
})
controls_diff <- tibble::tibble(
  dep_var = dep_vars_table4,
  selected = selected_controls_list
) %>%
  mutate(
    kept = map(selected, ~ intersect(.x, baseline_controls)),
    dropped = map(selected, ~ setdiff(baseline_controls, .x)),
    added = map(selected, ~ setdiff(.x, baseline_controls)),
    n_selected = map_int(selected, length)
  )
cat("\n==== COMPARING CONTROLS (baseline vs double lasso) ====\n")
for (r in 1:nrow(controls_diff)) {
  cat("\n--- Dependent variable :", controls_diff$dep_var[r], "---\n")
  cat("# Selected controls =", controls_diff$n_selected[r], "\n")
  cat("Kept (baseline)   :", paste(controls_diff$kept[[r]], collapse = ", "), "\n")
  cat("Dropped (baseline)   :", paste(controls_diff$dropped[[r]], collapse = ", "), "\n")
  cat("Added (non-baseline) :", paste(controls_diff$added[[r]], collapse = ", "), "\n")
}

cat("\n==== END ====\n")



##################################### TABLE 5 #################################

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
