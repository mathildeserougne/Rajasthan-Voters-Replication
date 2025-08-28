# Clean double lasso concerning table 2.

# Packages
install.packages(c("tidyverse", "glmnet", "sandwich", "lmtest", "car", "modelsummary", "broom", "haven"))
library(tidyverse)
library(glmnet)
library(sandwich)
library(lmtest)
library(car)
library(modelsummary)
library(broom)
library(haven)


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

