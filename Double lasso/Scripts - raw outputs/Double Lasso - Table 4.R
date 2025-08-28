# Clean double lasso concerning table 4.

# Libraries and packages
install.packages(c("tidyverse", "glmnet", "sandwich", "lmtest", "car", "modelsummary", "broom", "haven", "fixest"))
library(tidyverse)
library(glmnet)
library(sandwich)
library(lmtest)
library(car)
library(modelsummary)
library(broom)
library(haven)
library(fixest)

## MACROS AND DATA ##

gpcontrols15 <- c(
  "GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st", "RES15_obc", "RES15_sc", "RES15_st"
)

# Full list of candidates as controls
full_controls_candidates_table4 <- c(
  gpcontrols15,
  # Added : from the census
  "CENSUS_PCA2001_tot_pop", "CENSUS_PCA2001_tot_lit", "CENSUS_PCA2001_tot_sc",
  "CENSUS_PCA2001_tot_st", "CENSUS_PCA2001_tot_aglb", "CENSUS_PCA2001_tot_nm_hh",
  "CENSUS_PCA2001_tot_cult", "CENSUS_VD2001_power_dom", "CENSUS_VD2001_drnk_wat_f",
  "CENSUS_VD2001_edu_fac", "CENSUS_VD2001_medi_fac"
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