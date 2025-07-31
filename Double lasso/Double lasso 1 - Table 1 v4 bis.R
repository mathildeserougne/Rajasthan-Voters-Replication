## double lasso using the broader selection of candidates ##

############################################################################
# DOUBLE-LASSO – TABLE 1  (sélection libre des contrôles)
############################################################################

## Packages ----------------------------------------------------------
library(tidyverse); library(glmnet); library(sandwich); library(lmtest)
library(car);        library(broom); library(purrr);   library(haven)

## 1.  Données et listes --------------------------------------------
data <- read_dta("~/work/Electoral data cleaned.dta")

gpcontrols <- c("GP_population","GP_lit","GP_sc","GP_st","GP_nbvillages",
                "RES00_gender","RES00_obc","RES00_sc","RES00_st",
                "RES10_obc","RES10_sc","RES10_st",
                "RES05_obc","RES05_sc","RES05_st")

incum_dep_vars1 <- c("INC05_running","INC05_voteshare",
                     "INCSPOUSE05_running","INCSPOUSE05_voteshare",
                     "INCOTHER05_running","INCOTHER05_voteshare")

data_filtered <- data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running   = INCorFAM05_running   - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won       = INCorFAM05_won       - INC05_won
  )

##  Candidats ---------------------------------------------------------
non_num <- c("CAND_caste","CAND_jati","CAND_caste_sec",
             "CAND_village","RES10_caste_sec")
# controls_candidates est supposé déjà défini ; sinon charge-le ici
all_candidates <- union(gpcontrols,
                        setdiff(controls_candidates, non_num))

## util ---------------------------------------------------------------
get_nz <- function(cf)
  rownames(as.matrix(cf))[which(as.matrix(cf) != 0)]

############################################################################
## double_lasso_fit  -------------------------------------------------------
############################################################################
double_lasso_fit <- function(data_df, y_name, d_names,
                             x_candidates, fe_name = "district",
                             se_cluster = NULL, family = "gaussian") {
  
  ## Effet fixe : retire 1 dummy
  if (!is.null(fe_name)) {
    fe_vec <- factor(data_df[[fe_name]], exclude = NULL)
    fe_mat <- model.matrix(~ fe_vec - 1, na.action = na.pass)[ , -1]
    colnames(fe_mat) <- paste0("fe_", levels(fe_vec)[-1])
  } else fe_mat <- NULL
  
  ## X ----------------------------------------------------------------
  X_mat <- as.matrix(data_df %>% dplyr::select(all_of(x_candidates)))
  storage.mode(X_mat) <- "double"
  if (!is.null(fe_mat)) {
    X_mat <- cbind(X_mat, fe_mat)
    fe_idx <- (ncol(X_mat) - ncol(fe_mat) + 1):ncol(X_mat)
  } else fe_idx <- integer(0)
  
  ## Y / D (+ interactions éventuelles) -------------------------------
  for (nm in setdiff(d_names, names(data_df))) {
    p <- strsplit(nm, ":", fixed = TRUE)[[1]]
    data_df[[nm]] <- data_df[[p[1]]] * data_df[[p[2]]]
  }
  y_vec <- as.numeric(data_df[[y_name]])
  D_mat <- as.matrix(data_df %>% dplyr::select(all_of(d_names)))
  if (is.null(ncol(D_mat))) D_mat <- matrix(D_mat, ncol = 1)
  
  keep <- complete.cases(y_vec, D_mat,
                         if (!is.null(se_cluster)) se_cluster else NULL)
  y_vec <- y_vec[keep]
  D_mat <- D_mat[keep, , drop = FALSE]
  X_mat <- X_mat[keep, , drop = FALSE]
  if (!is.null(se_cluster)) se_cluster <- se_cluster[keep]
  
  ## Impute NA de X (par zéros ; change en moyenne si tu préfères)
  X_mat[is.na(X_mat)] <- 0
  
  y_const <- length(unique(y_vec)) < 2
  d_const <- apply(D_mat, 2, \(z) length(unique(z)) < 2)
  
  ## Sélection 1 : Y ~ D + X -----------------------------------------
  sel_y <- character(0)
  if (!y_const) {
    pf <- c(rep(0, ncol(D_mat)), rep(1, ncol(X_mat)))
    pf[ncol(D_mat) + fe_idx] <- 0
    if (!all(pf == 0)) {
      fit <- cv.glmnet(cbind(D_mat, X_mat), y_vec,
                       alpha = 1, family = family,
                       penalty.factor = pf)
      sel_y <- setdiff(get_nz(coef(fit, "lambda.min")),
                       c("(Intercept)", colnames(D_mat)))
    }
  }
  
  ## Sélection 2 : D_k ~ X -------------------------------------------
  sel_d <- character(0)
  pf2 <- rep(1, ncol(X_mat)); pf2[fe_idx] <- 0
  if (!all(pf2 == 0))
    for (k in seq_len(ncol(D_mat))) if (!d_const[k]) {
      fitk <- cv.glmnet(X_mat, D_mat[, k],
                        alpha = 1, family = "gaussian",
                        penalty.factor = pf2)
      sel_d <- union(sel_d, setdiff(get_nz(coef(fitk, "lambda.min")),
                                    "(Intercept)"))
    }
  
  ## OLS final --------------------------------------------------------
  sel <- union(sel_y, sel_d); sel <- union(sel, colnames(X_mat)[fe_idx])
  X_fin  <- X_mat[, sel, drop = FALSE]
  df_fin <- data.frame(Y = y_vec, D_mat, X_fin)
  colnames(df_fin)[2:(1 + ncol(D_mat))] <- colnames(D_mat)
  
  lm_fit <- lm(as.formula(
    paste("Y ~", paste(names(df_fin)[-1], collapse = " + "))),
    data = df_fin)
  
  vc <- if (!is.null(se_cluster))
    sandwich::vcovCL(lm_fit, cluster = se_cluster)
  else sandwich::vcovHC(lm_fit, "HC1")
  attr(lm_fit, "vcov") <- vc        # <- correct
  
  list(model = lm_fit,
       selected = sel,
       robust  = lmtest::coeftest(lm_fit, vcov = vc))
}

############################################################################
## calculate_tests  --------------------------------------------------------
############################################################################
safe_p <- function(model, hypo) {
  out <- tryCatch(
    car::linearHypothesis(model, hypo, singular.ok = TRUE)$`Pr(>F)`[2],
    error = \(e) NA_real_)
  round(out, 3)
}

calculate_tests <- function(model, type) {
  if (type == "any_treatment") {
    list(
      pval1 = safe_p(model, "RES05_gender = 0"),
      pval2 = safe_p(model, "INT_treatment:RES05_gender = 0"),
      pval3 = safe_p(model, "INT_treatment = INT_treatment:RES05_gender")
    )
  } else {
    list(
      pval1 = safe_p(model, "INT_treatment_gender:RES05_gender = 0"),
      pval2 = safe_p(model, "INT_treatment_general:RES05_gender = 0"),
      pval3 = safe_p(model, "INT_treatment_gender = INT_treatment_general"),
      pval4 = safe_p(model, "INT_treatment_gender:RES05_gender = INT_treatment_general:RES05_gender")
    )
  }
}

############################################################################
## 3.  Estimations ---------------------------------------------------------
############################################################################
models_list <- list(); sel_list <- list(); test_results <- list()

## (A) Any-treatment -------------------------------------------------
for (i in seq_along(incum_dep_vars1)) {
  dep <- incum_dep_vars1[i]  ## <-- typo dans ton code ; corrige !
  dep <- incum_dep_vars1[i]  ## ligne correcte
  
  res <- double_lasso_fit(
    data_filtered, dep,
    d_names = c("INT_treatment","RES05_gender",
                "INT_treatment:RES05_gender"),
    x_candidates = all_candidates,
    fe_name = "district",
    se_cluster = data_filtered$district)
  
  models_list[[i]] <- res$model
  sel_list[[i]]    <- res$selected
  test_results[[i]]<- calculate_tests(res$model, "any_treatment")
}

## (B) Gender / General ---------------------------------------------
for (i in seq_along(incum_dep_vars1)) {
  dep <- incum_dep_vars1[i]; j <- i + length(incum_dep_vars1)
  
  res <- double_lasso_fit(
    data_filtered, dep,
    d_names = c("INT_treatment_gender","INT_treatment_general",
                "RES05_gender",
                "INT_treatment_gender:RES05_gender",
                "INT_treatment_general:RES05_gender"),
    x_candidates = all_candidates,
    fe_name = "district",
    se_cluster = data_filtered$district)
  
  models_list[[j]] <- res$model
  sel_list[[j]]    <- res$selected
  test_results[[j]]<- calculate_tests(res$model, "gender_general")
}

############################################################################
## 4.  Comparaison sélection vs baseline ----------------------------------
############################################################################
baseline_controls <- gpcontrols

controls_diff <- tibble(
  dep_var = rep(incum_dep_vars1, 2),
  spec    = c(rep("any_treatment", length(incum_dep_vars1)),
              rep("gender_general", length(incum_dep_vars1))),
  selected = sel_list
) %>%
  mutate(
    kept      = map(selected, intersect, baseline_controls),
    dropped   = map(selected, setdiff,  baseline_controls),
    added     = map(selected, setdiff,  baseline_controls),
    n_selected = map_int(selected, length)
  )

cat("\n==== COMPARAISON CONTROLES (baseline vs Lasso) ====\n")
for (r in seq_len(nrow(controls_diff))) {
  cat("\n--- DV:", controls_diff$dep_var[r],
      " | Spec:", controls_diff$spec[r], "---\n")
  cat("# Sélectionnés =", controls_diff$n_selected[r], "\n")
  cat("Kept    :", paste(controls_diff$kept[[r]],    collapse = ", "), "\n")
  cat("Dropped :", paste(controls_diff$dropped[[r]], collapse = ", "), "\n")
  cat("Added   :", paste(controls_diff$added[[r]],   collapse = ", "), "\n")
}
cat("\n==== FIN COMPARAISON ====\n")

############################################################################
## 5.  Affichage des coefficients + SE robustes ----------------------------
############################################################################
cat("\n==== COEFFICIENTS (SE robustes, cluster district) ====\n")
for (m in seq_along(models_list)) {
  
  spec <- if (m <= length(incum_dep_vars1))
    "Any-treatment" else "Gender/General"
  dv   <- incum_dep_vars1[(m-1) %% length(incum_dep_vars1) + 1]
  
  cat("\n-------------------------",
      "\nModèle", m, "–", spec,
      "\nDV :", dv, "\n")
  
  vc  <- attr(models_list[[m]], "vcov")
  tbl <- broom::tidy(models_list[[m]], conf.int = FALSE, vcov = vc)
  
  print(tbl[, c("term", "estimate", "std.error", "p.value")],
        row.names = FALSE, digits = 4)
}
cat("\n===============================================\n")

