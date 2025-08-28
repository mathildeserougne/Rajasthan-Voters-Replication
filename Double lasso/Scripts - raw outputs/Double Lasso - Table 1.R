# Clean double lasso concerning table 1.

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



