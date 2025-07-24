## double lasso table 1 v2 ##############

# trying without loops because this is hell


############################################################
# DOUBLE LASSO – TABLE 1 (COMPLET, SANS BOUCLE) – CODE ROBUSTE
# ------------------------------------------------------------
# Ce script :
#   1. Charge les données et fait ton filtrage initial.
#   2. Détermine les contrôles candidats en excluant Y, traitements, FE, etc.
#   3. Construit X proprement (pas d'erreur de contrastes, pas d'UTF-8).
#   4. Effectue le double lasso pour CHAQUE régression (12 blocs) sans boucle.
#   5. Renomme les colonnes sélectionnées pour créer des formules courtes et sûres.
#   6. Estime l'OLS finale avec effets fixes (district) via fixest::feols.
#   7. Imprime les contrôles retenus (noms originaux), la différence vs ancien OLS.
#
# Remarque : Ce code n'utilise AUCUNE BOUCLE pour les régressions finales.
############################################################

# -------------------- 0. PACKAGES ------------------------------------------
library(tidyverse)
library(haven)
library(hdm)
library(fixest)
library(fastDummies)

# -------------------- 1. DONNÉES -------------------------------------------
# Adapter le chemin si besoin
# ATTENTION : garde le même nom d'objet 'data' que ton script initial pour cohérence

data <- read_dta("~/work/Electoral data cleaned.dta")

data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running   = INCorFAM05_running   - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won       = INCorFAM05_won       - INC05_won
  )

# -------------------- 2. VARIABLES -----------------------------------------
# Outcomes (Table 1)
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

# Traitements obligatoires
# Spec 1 (any treatment)
force_vars_any <- c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender")
# Spec 2 (gender & general)
force_vars_det <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender")

# Effets fixes
FE_var <- "district"

# Contrôles OLS initiaux (pour comparaison)
old_controls <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                  "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                  "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Variables de traitement simples (exclues de X candidates)
all_treat_vars <- c("INT_treatment", "INT_treatment_gender", "INT_treatment_general")

# -------------------- 3. UTILITAIRES ---------------------------------------
# Sélection des candidats : toutes les variables sauf Y, traitements, FE, etc.
get_X_candidates <- function(df){
  forbidden <- c(incum_dep_vars1,
                 all_treat_vars,
                 "RES05_gender",
                 FE_var,
                 # Interactions explicites au cas où elles seraient déjà présentes
                 "INT_treatment:RES05_gender",
                 "INT_treatment_gender:RES05_gender",
                 "INT_treatment_general:RES05_gender")
  setdiff(names(df), forbidden)
}

# Construction de X : encode facteurs, vire constantes, NA -> 0, variance nulle -> out
build_X <- function(df, vars){
  if(length(vars) == 0) return(matrix(0, nrow(df), 0))
  dfX <- df %>% select(all_of(vars))
  # haven_labelled -> numeric
  dfX <- dfX %>% mutate(across(where(~ inherits(.x, "haven_labelled")), as.numeric))
  # Retirer colonnes avec < 2 valeurs distinctes
  keep_var <- sapply(dfX, function(x) dplyr::n_distinct(na.omit(x)) > 1)
  dfX <- dfX[, keep_var, drop = FALSE]
  if(ncol(dfX) == 0) return(matrix(0, nrow(df), 0))
  # Dummy encoding propre
  fac_cols <- names(dfX)[sapply(dfX, function(x) is.factor(x) || is.character(x))]
  if(length(fac_cols) > 0){
    dfX <- fastDummies::dummy_cols(dfX,
                                   select_columns = fac_cols,
                                   remove_first_dummy = TRUE,
                                   remove_selected_columns = TRUE,
                                   ignore_na = TRUE)
  }
  # NA -> 0 pour rlasso
  dfX[is.na(dfX)] <- 0
  # Retirer variance nulle
  keep_col <- apply(dfX, 2, function(z) var(z, na.rm = TRUE) > 0)
  dfX <- dfX[, keep_col, drop = FALSE]
  as.matrix(dfX)
}

# Fonction d'impression récap
print_selection <- function(title, kept, old){
  cat("\n---", title, "---\n")
  cat("Contrôles retenus (Double-Lasso):\n", paste(kept, collapse = ", "), "\n")
  cat("Retirés vs ancien OLS: \n", paste(setdiff(old, kept), collapse = ", "), "\n")
  cat("Ajouts vs ancien OLS: \n", paste(setdiff(kept, old), collapse = ", "), "\n")
}

# Helper pour renommer les X sélectionnés et créer la data finale & formule
prepare_reg <- function(df, y, base_terms, X_mat, sel_vec){
  orig_names <- colnames(X_mat)[sel_vec]
  X_sel <- X_mat[, sel_vec, drop = FALSE]
  # Renommer pour la formule
  new_names <- paste0("ctrl_", seq_len(ncol(X_sel)))
  colnames(X_sel) <- new_names
  
  df_reg <- bind_cols(
    df %>% select(all_of(c(y, FE_var)), all_of(base_terms[grepl(":", base_terms, invert = TRUE)])),
    # NOTE : les interactions seront gérées dans la formule via reformulate
    as.data.frame(X_sel)
  )
  list(df_reg = df_reg, new_names = new_names, orig_names = orig_names)
}

# -------------------- 4. RÉGRESSIONS – ANY TREATMENT -----------------------
# Bloc 1 : INC05_running -----------------------------------------------------
Xcand_1_any <- get_X_candidates(data_filtered)
X_1_any <- build_X(data_filtered, Xcand_1_any)

l_y_1_any <- rlasso(X_1_any, data_filtered$INC05_running, post = TRUE)
l_d_1_any <- rlasso(X_1_any, data_filtered$INT_treatment, post = TRUE)
sel_1_any <- l_y_1_any$index | l_d_1_any$index

prep_1_any <- prepare_reg(
  df          = data_filtered,
  y           = "INC05_running",
  base_terms  = force_vars_any,
  X_mat       = X_1_any,
  sel_vec     = sel_1_any
)

form_1_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_1_any$new_names),
  response = "INC05_running"
)
reg_1_any <- feols(form_1_any, fixef = ~ district, data = prep_1_any$df_reg)

print_selection("INC05_running (Any)", prep_1_any$orig_names, old_controls)

# Bloc 2 : INC05_voteshare ---------------------------------------------------
Xcand_2_any <- get_X_candidates(data_filtered)
X_2_any <- build_X(data_filtered, Xcand_2_any)

l_y_2_any <- rlasso(X_2_any, data_filtered$INC05_voteshare, post = TRUE)
l_d_2_any <- rlasso(X_2_any, data_filtered$INT_treatment,   post = TRUE)
sel_2_any <- l_y_2_any$index | l_d_2_any$index

prep_2_any <- prepare_reg(
  df          = data_filtered,
  y           = "INC05_voteshare",
  base_terms  = force_vars_any,
  X_mat       = X_2_any,
  sel_vec     = sel_2_any
)

form_2_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_2_any$new_names),
  response = "INC05_voteshare"
)
reg_2_any <- feols(form_2_any, fixef = ~ district, data = prep_2_any$df_reg)

print_selection("INC05_voteshare (Any)", prep_2_any$orig_names, old_controls)

# Bloc 3 : INCSPOUSE05_running ----------------------------------------------
Xcand_3_any <- get_X_candidates(data_filtered)
X_3_any <- build_X(data_filtered, Xcand_3_any)

l_y_3_any <- rlasso(X_3_any, data_filtered$INCSPOUSE05_running, post = TRUE)
l_d_3_any <- rlasso(X_3_any, data_filtered$INT_treatment,       post = TRUE)
sel_3_any <- l_y_3_any$index | l_d_3_any$index

prep_3_any <- prepare_reg(
  df          = data_filtered,
  y           = "INCSPOUSE05_running",
  base_terms  = force_vars_any,
  X_mat       = X_3_any,
  sel_vec     = sel_3_any
)

form_3_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_3_any$new_names),
  response = "INCSPOUSE05_running"
)
reg_3_any <- feols(form_3_any, fixef = ~ district, data = prep_3_any$df_reg)

print_selection("INCSPOUSE05_running (Any)", prep_3_any$orig_names, old_controls)

# Bloc 4 : INCSPOUSE05_voteshare --------------------------------------------
Xcand_4_any <- get_X_candidates(data_filtered)
X_4_any <- build_X(data_filtered, Xcand_4_any)

l_y_4_any <- rlasso(X_4_any, data_filtered$INCSPOUSE05_voteshare, post = TRUE)
l_d_4_any <- rlasso(X_4_any, data_filtered$INT_treatment,        post = TRUE)
sel_4_any <- l_y_4_any$index | l_d_4_any$index

prep_4_any <- prepare_reg(
  df          = data_filtered,
  y           = "INCSPOUSE05_voteshare",
  base_terms  = force_vars_any,
  X_mat       = X_4_any,
  sel_vec     = sel_4_any
)

form_4_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_4_any$new_names),
  response = "INCSPOUSE05_voteshare"
)
reg_4_any <- feols(form_4_any, fixef = ~ district, data = prep_4_any$df_reg)

print_selection("INCSPOUSE05_voteshare (Any)", prep_4_any$orig_names, old_controls)

# Bloc 5 : INCOTHER05_running ------------------------------------------------
Xcand_5_any <- get_X_candidates(data_filtered)
X_5_any <- build_X(data_filtered, Xcand_5_any)

l_y_5_any <- rlasso(X_5_any, data_filtered$INCOTHER05_running, post = TRUE)
l_d_5_any <- rlasso(X_5_any, data_filtered$INT_treatment,      post = TRUE)
sel_5_any <- l_y_5_any$index | l_d_5_any$index

prep_5_any <- prepare_reg(
  df          = data_filtered,
  y           = "INCOTHER05_running",
  base_terms  = force_vars_any,
  X_mat       = X_5_any,
  sel_vec     = sel_5_any
)

form_5_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_5_any$new_names),
  response = "INCOTHER05_running"
)
reg_5_any <- feols(form_5_any, fixef = ~ district, data = prep_5_any$df_reg)

print_selection("INCOTHER05_running (Any)", prep_5_any$orig_names, old_controls)

# Bloc 6 : INCOTHER05_voteshare ---------------------------------------------
Xcand_6_any <- get_X_candidates(data_filtered)
X_6_any <- build_X(data_filtered, Xcand_6_any)

l_y_6_any <- rlasso(X_6_any, data_filtered$INCOTHER05_voteshare, post = TRUE)
l_d_6_any <- rlasso(X_6_any, data_filtered$INT_treatment,        post = TRUE)
sel_6_any <- l_y_6_any$index | l_d_6_any$index

prep_6_any <- prepare_reg(
  df          = data_filtered,
  y           = "INCOTHER05_voteshare",
  base_terms  = force_vars_any,
  X_mat       = X_6_any,
  sel_vec     = sel_6_any
)

form_6_any <- reformulate(
  c("INT_treatment", "RES05_gender", "INT_treatment:RES05_gender", prep_6_any$new_names),
  response = "INCOTHER05_voteshare"
)
reg_6_any <- feols(form_6_any, fixef = ~ district, data = prep_6_any$df_reg)

print_selection("INCOTHER05_voteshare (Any)", prep_6_any$orig_names, old_controls)

# -------------------- 5. RÉGRESSIONS – GENDER & GENERAL --------------------
# Bloc 7 : INC05_running -----------------------------------------------------
Xcand_1_det <- get_X_candidates(data_filtered)
X_1_det <- build_X(data_filtered, Xcand_1_det)

l_y_1_det  <- rlasso(X_1_det, data_filtered$INC05_running,         post = TRUE)
l_dg1_1_det<- rlasso(X_1_det, data_filtered$INT_treatment_gender,  post = TRUE)
l_dg2_1_det<- rlasso(X_1_det, data_filtered$INT_treatment_general, post = TRUE)
sel_1_det <- l_y_1_det$index | l_dg1_1_det$index | l_dg2_1_det$index

prep_1_det <- prepare_reg(
  df          = data_filtered,
  y           = "INC05_running",
  base_terms  = force_vars_det,
  X_mat       = X_1_det,
  sel_vec     = sel_1_det
)

form_1_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_1_det$new_names),
  response = "INC05_running"
)
reg_1_det <- feols(form_1_det, fixef = ~ district, data = prep_1_det$df_reg)

print_selection("INC05_running (Det)", prep_1_det$orig_names, old_controls)

# Bloc 8 : INC05_voteshare ---------------------------------------------------
Xcand_2_det <- get_X_candidates(data_filtered)
X_2_det <- build_X(data_filtered, Xcand_2_det)

l_y_2_det  <- rlasso(X_2_det, data_filtered$INC05_voteshare,       post = TRUE)
l_dg1_2_det<- rlasso(X_2_det, data_filtered$INT_treatment_gender,  post = TRUE)
l_dg2_2_det<- rlasso(X_2_det, data_filtered$INT_treatment_general, post = TRUE)
sel_2_det <- l_y_2_det$index | l_dg1_2_det$index | l_dg2_2_det$index

prep_2_det <- prepare_reg(
  df          = data_filtered,
  y           = "INC05_voteshare",
  base_terms  = force_vars_det,
  X_mat       = X_2_det,
  sel_vec     = sel_2_det
)

form_2_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_2_det$new_names),
  response = "INC05_voteshare"
)
reg_2_det <- feols(form_2_det, fixef = ~ district, data = prep_2_det$df_reg)

print_selection("INC05_voteshare (Det)", prep_2_det$orig_names, old_controls)

# Bloc 9 : INCSPOUSE05_running ----------------------------------------------
Xcand_3_det <- get_X_candidates(data_filtered)
X_3_det <- build_X(data_filtered, Xcand_3_det)

l_y_3_det  <- rlasso(X_3_det, data_filtered$INCSPOUSE05_running,    post = TRUE)
l_dg1_3_det<- rlasso(X_3_det, data_filtered$INT_treatment_gender,   post = TRUE)
l_dg2_3_det<- rlasso(X_3_det, data_filtered$INT_treatment_general,  post = TRUE)
sel_3_det <- l_y_3_det$index | l_dg1_3_det$index | l_dg2_3_det$index

prep_3_det <- prepare_reg(
  df          = data_filtered,
  y           = "INCSPOUSE05_running",
  base_terms  = force_vars_det,
  X_mat       = X_3_det,
  sel_vec     = sel_3_det
)

form_3_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_3_det$new_names),
  response = "INCSPOUSE05_running"
)
reg_3_det <- feols(form_3_det, fixef = ~ district, data = prep_3_det$df_reg)

print_selection("INCSPOUSE05_running (Det)", prep_3_det$orig_names, old_controls)

# Bloc 10 : INCSPOUSE05_voteshare -------------------------------------------
Xcand_4_det <- get_X_candidates(data_filtered)
X_4_det <- build_X(data_filtered, Xcand_4_det)

l_y_4_det  <- rlasso(X_4_det, data_filtered$INCSPOUSE05_voteshare,  post = TRUE)
l_dg1_4_det<- rlasso(X_4_det, data_filtered$INT_treatment_gender,   post = TRUE)
l_dg2_4_det<- rlasso(X_4_det, data_filtered$INT_treatment_general,  post = TRUE)
sel_4_det <- l_y_4_det$index | l_dg1_4_det$index | l_dg2_4_det$index

prep_4_det <- prepare_reg(
  df          = data_filtered,
  y           = "INCSPOUSE05_voteshare",
  base_terms  = force_vars_det,
  X_mat       = X_4_det,
  sel_vec     = sel_4_det
)

form_4_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_4_det$new_names),
  response = "INCSPOUSE05_voteshare"
)
reg_4_det <- feols(form_4_det, fixef = ~ district, data = prep_4_det$df_reg)

print_selection("INCSPOUSE05_voteshare (Det)", prep_4_det$orig_names, old_controls)

# Bloc 11 : INCOTHER05_running ----------------------------------------------
Xcand_5_det <- get_X_candidates(data_filtered)
X_5_det <- build_X(data_filtered, Xcand_5_det)

l_y_5_det  <- rlasso(X_5_det, data_filtered$INCOTHER05_running,     post = TRUE)
l_dg1_5_det<- rlasso(X_5_det, data_filtered$INT_treatment_gender,   post = TRUE)
l_dg2_5_det<- rlasso(X_5_det, data_filtered$INT_treatment_general,  post = TRUE)
sel_5_det <- l_y_5_det$index | l_dg1_5_det$index | l_dg2_5_det$index

prep_5_det <- prepare_reg(
  df          = data_filtered,
  y           = "INCOTHER05_running",
  base_terms  = force_vars_det,
  X_mat       = X_5_det,
  sel_vec     = sel_5_det
)

form_5_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_5_det$new_names),
  response = "INCOTHER05_running"
)
reg_5_det <- feols(form_5_det, fixef = ~ district, data = prep_5_det$df_reg)

print_selection("INCOTHER05_running (Det)", prep_5_det$orig_names, old_controls)

# Bloc 12 : INCOTHER05_voteshare --------------------------------------------
Xcand_6_det <- get_X_candidates(data_filtered)
X_6_det <- build_X(data_filtered, Xcand_6_det)

l_y_6_det  <- rlasso(X_6_det, data_filtered$INCOTHER05_voteshare,   post = TRUE)
l_dg1_6_det<- rlasso(X_6_det, data_filtered$INT_treatment_gender,   post = TRUE)
l_dg2_6_det<- rlasso(X_6_det, data_filtered$INT_treatment_general,  post = TRUE)
sel_6_det <- l_y_6_det$index | l_dg1_6_det$index | l_dg2_6_det$index

prep_6_det <- prepare_reg(
  df          = data_filtered,
  y           = "INCOTHER05_voteshare",
  base_terms  = force_vars_det,
  X_mat       = X_6_det,
  sel_vec     = sel_6_det
)

form_6_det <- reformulate(
  c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
    "INT_treatment_gender:RES05_gender", "INT_treatment_general:RES05_gender",
    prep_6_det$new_names),
  response = "INCOTHER05_voteshare"
)
reg_6_det <- feols(form_6_det, fixef = ~ district, data = prep_6_det$df_reg)

print_selection("INCOTHER05_voteshare (Det)", prep_6_det$orig_names, old_controls)

# -------------------- 6. OUTPUTS SUPPLÉMENTAIRES ---------------------------
# Tu peux faire :
# summary(reg_1_any); summary(reg_1_det); etc.
# Ou utiliser modelsummary :
# library(modelsummary)
# modelsummary(list(reg_1_any, reg_1_det, reg_2_any, reg_2_det, ...), stars = TRUE)
# Pour exporter la liste des contrôles retenus, concatène prep_X_any$orig_names dans un tibble.
############################################################



