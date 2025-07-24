# Trying the double lasso method on table 1 to find the best selection of controls

###############################################
# 0.  PACKAGES
###############################################
library(haven)   # read_dta + zap_labels
library(dplyr)
library(hdm)

###############################################
# 1.  LECTURE + FILTRES
###############################################
data_raw <- read_dta("~/work/Electoral data cleaned.dta")

d <- data_raw %>%                               # <-- mets ici tes filtres
  filter(RES10_gender == 0,
         SAMPLE_hhsurvey == 1,
         GP_tag == 1,
         INC05_can_run == 1)

###############################################
# 2.  TOUT EN NUMÉRIQUE   (PLUS AUCUN FACTOR)
###############################################
d_num <- d %>%
  mutate(across(everything(), haven::zap_labels)) %>%  # vire les labels STATA
  mutate(across(everything(), as.numeric)) %>%         # tout numeric
  select_if(~ !all(is.na(.)))                          # vire colonnes 100 % NA

# enlève les colonnes constantes (sinon le lasso plante)
d_num <- d_num %>%
  select(where(~ length(unique(na.omit(.))) > 1))

###############################################
# 3.  DÉFINIS OUTCOME & TRAITEMENT
###############################################
outcome <- "INC05_running"          # <-- change si besoin
treat   <- "INT_treatment"          # any-treatment  (0/1)

###############################################
# 4.  MATRICE X   (= traitement + TOUS LES AUTRES)
###############################################
controls <- setdiff(names(d_num), c(outcome, treat))
X_full   <- as.matrix(d_num[, c(treat, controls)])
Y        <- d_num[[outcome]]

###############################################
# 5.  DOUBLE-LASSO   (post-double-selection)
###############################################
mod <- rlassoEffects(x = X_full, y = Y, index = 1)   # 1 = colonne du traitement
summary(mod)                                         # coefficient, SE, p-value


run_dlasso <- function(outcome, treats, data_num) {
  controls <- setdiff(names(data_num), c(outcome, treats))
  X <- as.matrix(data_num[, c(treats, controls)])
  Y <- data_num[[outcome]]
  rlassoEffects(x = X, y = Y, index = 1:length(treats))
}

outcomes <- c("INC05_running", "INC05_voteshare",
              "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
              "INCOTHER05_running", "INCOTHER05_voteshare")

models_any <- lapply(outcomes, run_dlasso,
                     treats = "INT_treatment",
                     data_num = d_num)

models_det <- lapply(outcomes, run_dlasso,
                     treats = c("INT_treatment_gender",
                                "INT_treatment_general"),
                     data_num = d_num)



# combien d’observations au total ?
nrow(d_num)

# distribution de la dummy "any treatment"
table(d_num$INT_treatment, useNA = "ifany")

# écart-type (0 = pas de variation)
sd(d_num$INT_treatment, na.rm = TRUE)



## blocs pour corriger ce qui précède

###############################################
# 3 bis.  Sous-échantillon COMPLET (sans NA)
###############################################
vars_for_model <- c(outcome, treat, controls)     # toutes les colonnes utiles
d_clean <- d_num[complete.cases(d_num[, vars_for_model]), ]

cat("Observations gardées :", nrow(d_clean), "\n")   # contrôle visuel

###############################################
# 4 bis.  Matrice X et vecteur Y (propres)
###############################################
Y  <- d_clean[[outcome]]
X  <- as.matrix(d_clean[, c(treat, controls)])

###############################################
# 5.  Double-lasso
###############################################
mod <- rlassoEffects(x = X, y = Y, index = 1)
summary(mod)


run_dlasso <- function(outcome, treats, data_num) {
  controls <- setdiff(names(data_num), c(outcome, treats))
  vars     <- c(outcome, treats, controls)
  d_cl     <- data_num[complete.cases(data_num[, vars]), ]
  
  X <- as.matrix(d_cl[, c(treats, controls)])
  Y <- d_cl[[outcome]]
  
  rlassoEffects(x = X, y = Y, index = 1:length(treats))
}


models_any <- lapply(outcomes, run_dlasso,
                     treats   = "INT_treatment",
                     data_num = d_num)

models_det <- lapply(outcomes, run_dlasso,
                     treats   = c("INT_treatment_gender",
                                  "INT_treatment_general"),
                     data_num = d_num)

