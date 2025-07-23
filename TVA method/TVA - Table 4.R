## TVA TABLE 4 ################################################################

# Libraries
library(tidyverse)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(car)

install.packages("estimatr")    
library(estimatr)  

install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)

library(dplyr)


# Source to stock functions coded on Simon Yaspo's git (cloned needs to be cloned in your environment)
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")


# FORMATTING THE DATA #########################################################
# this time complicated because we have to merge electoral data and electoral 2015

electoral_data <- read_dta("Electoral data cleaned.dta")
electoral_2015 <- read_dta("Electoral data 2015 cleaned.dta")

# merge the two and create the arms

key_vars <- c("district", "gp")   


#data10 <- electoral_data %>% 
#  select(all_of(c(key_vars,
#                "RES10_gender", "SAMPLE_hhsurvey", "GP_tag", "INC05_can_run",
#                  "INT_treatment_code")))


data15 <- electoral_2015 %>% 
  select(all_of(key_vars),
         RES15_gender,                       
         ELEC15_nbcands, ELEC15_incum10_running, ELEC15_voteshare_incum10,
         ELEC15_prop_cand2010, ELEC15_voteshare_cand2010,
         ELEC15_prop_female,  ELEC15_voteshare_female,
         ELEC15_prop_nongen,  ELEC15_voteshare_nongen,
         RES10_gender, SAMPLE_hhsurvey, GP_tag, INT_treatment_code, RES05_gender)




# merged <- inner_join(data10, data15, by = key_vars)


data <- data15 %>% 
  filter(
    RES10_gender == 0,
    GP_tag        == 1,
    RES15_gender  == 0          # critère Table 4
  ) %>% 
  mutate(
    gender_arm = as.integer(INT_treatment_code %in% c(2, 4)),
    theme_arm  = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # routes
      INT_treatment_code %in% c(3, 4) ~ 2,   # eau
      TRUE                            ~ 0),
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI = as.integer(district == "KARAULI")
  ) %>% 
  select(gender_arm, theme_arm,
         district_DHOLPUR, district_KARAULI,
         ELEC15_nbcands, ELEC15_incum10_running, ELEC15_voteshare_incum10,
         ELEC15_prop_cand2010, ELEC15_voteshare_cand2010,
         ELEC15_prop_female,  ELEC15_voteshare_female,
         ELEC15_prop_nongen,  ELEC15_voteshare_nongen,
         RES10_gender, SAMPLE_hhsurvey, GP_tag, INT_treatment_code, RES05_gender) %>% 
  as.data.frame()

# vérif : doit donner 5 cellules non nulles (contrôle + 4 combos)
with(data, table(gender_arm, theme_arm))




outcomes <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")

#################################################################################

# there again, we try to find common cutoffs before doing tva
# if not, we stick to individual research of cutoff and loop over outcomes


### tva

# best cutoff  pour chaque outcome de la boucle 

cut_grid <- seq(0.05, 0.90, by = 0.05)


try_TVA <- function(y, cutoff, cmp0 = FALSE) {
  do_TVA(
    data            = data[ , c(y, arms, fes)],  
    arms            = arms,
    y               = y,
    fes             = fes,
    estim_func      = "pval_MSE",                
    cutoff          = cutoff,
    compare_to_zero = cmp0
  )
}


environment(do_TVA)$winners_curse <- function(...) NULL


tva_table4 <- list()

for (y in outcomes) {
  
  cat("\n================  OUTCOME :", y, " ================\n")
  
  best_res   <- NULL
  best_cut   <- NA_real_
  cmp0_used  <- FALSE
  
  
  for (cmp in c(FALSE, TRUE)) {
    for (ct in cut_grid) {
      best_res <- tryCatch(try_TVA(y, ct, cmp), error = function(e) NULL)
      if (!is.null(best_res) && nrow(best_res$marginal_support) > 0) {
        best_cut  <- ct
        cmp0_used <- cmp
        break     
      }
    }
    if (!is.null(best_res)) break  
  }
  
  
  if (is.null(best_res)) {
    cat("Aucun support non vide jusqu’à 0.90\n")
  } else {
    tva_table4[[y]] <- best_res
    
    cat("cutoff =", best_cut,
        "| cmp0 =", cmp0_used,
        "| support =", nrow(best_res$marginal_support),
        "| pools =", nrow(best_res$pools_summary), "\n\n")
    
    print(best_res$marginal_support)
    cat("\n--- Pools summary ------------------------------\n")
    print(best_res$pools_summary)
    cat("\n--- Pooled OLS (effet vs contrôle) --------------\n")
    print(best_res$pooled_ols)
    cat("-------------------------------------------------\n")
  }
}




# interprétation?
# routes est plus puissant que eau, reconfigure bcp les candidatures
# le message genre seul ne suffit pas. pas de truc significatif à lui seul.





## SEPARATING THE SAMPLES ALONG GENDER RESERVATION STATUS #####################


electoral_data   <- read_dta("Electoral data cleaned.dta")
electoral_2015   <- read_dta("Electoral data 2015 cleaned.dta")



key_vars <- c("district", "gp")

#data10 <- electoral_data %>% 
  select(all_of(c(key_vars,
                  "RES10_gender", "RES05_gender",          # <= on conserve !
                  "GP_tag", "INT_treatment_code")))

data15 <- electoral_2015 %>% 
  select(all_of(key_vars),
         RES15_gender,
         ELEC15_nbcands, ELEC15_incum10_running, ELEC15_voteshare_incum10,
         ELEC15_prop_cand2010, ELEC15_voteshare_cand2010,
         ELEC15_prop_female,  ELEC15_voteshare_female,
         ELEC15_prop_nongen,  ELEC15_voteshare_nongen)

merged <- inner_join(data10, data15, by = key_vars)

data <- merged %>% 
  filter(RES10_gender == 0,
         GP_tag        == 1,
         RES15_gender  == 0) %>%       # critère Table 4
  mutate(
    gender_arm = as.integer(INT_treatment_code %in% c(2, 4)),
    theme_arm  = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,
      INT_treatment_code %in% c(3, 4) ~ 2,
      TRUE                            ~ 0),
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI = as.integer(district == "KARAULI")
  ) %>% 
  select(gender_arm, theme_arm,
         district_DHOLPUR, district_KARAULI,
         RES05_gender,                             # <- à présent disponible
         starts_with("ELEC15_")) %>% 
  as.data.frame()


# bras, fe, outcome, cutoff
arms     <- c("gender_arm", "theme_arm")
fes      <- c("district_DHOLPUR", "district_KARAULI")
outcomes <- names(data)[grepl("^ELEC15_", names(data))]

cut_grid <- seq(0.05, 0.90, by = 0.05)
environment(do_TVA)$winners_curse <- function(...) NULL   # neutralise WC




# run tva

run_tva <- function(df, y, cutoff, cmp0 = FALSE) {
  tryCatch(
    do_TVA(
      data            = df[, c(y, arms, fes)],
      arms            = arms,
      y               = y,
      fes             = fes,
      estim_func      = "pval_MSE",
      cutoff          = cutoff,
      compare_to_zero = cmp0
    ),
    error = function(e) NULL
  )
}


################################################################################
## boucle sur gender reservation
################################################################################
samples <- list(
  prev_reserved   = subset(data, RES05_gender == 1),
  prev_unreserved = subset(data, RES05_gender == 0)
)

tva_split <- list()

for (s in names(samples)) {
  cat("\n####################  SAMPLE :", s, "####################\n")
  df_s <- samples[[s]]
  
  for (y in outcomes) {
    cat("\n=== OUTCOME :", y, "===\n")
    
    best <- NULL; best_cut <- NA; cmp0_flag <- FALSE
    
    for (cmp in c(FALSE, TRUE)) {
      for (ct in cut_grid) {
        best <- run_tva(df_s, y, ct, cmp)
        if (!is.null(best) && nrow(best$marginal_support) > 0) {
          best_cut  <- ct; cmp0_flag <- cmp
          break
        }
      }
      if (!is.null(best)) break          # on sort dès qu’on a un support non vide
    }
    
    if (is.null(best)) {
      cat("   → aucun support jusqu’à 0.90\n")
      next
    }
    
    tva_split[[s]][[y]] <- best
    cat("   ✔ cutoff =", best_cut,
        "| cmp0 =", cmp0_flag,
        "| support =", nrow(best$marginal_support),
        "| pools =",   nrow(best$pools_summary), "\n")
    
    print(best$marginal_support)
    cat("\n--- Pools summary\n");   print(best$pools_summary)
    cat("\n--- Pooled OLS\n");      print(best$pooled_ols)
  }
}



# interpretation
# on observe bien des différences selon l'historique de la gender reservation!
# quand reservation a eu lieu, ils sont plus sensibles au contenu politique uq'au genre



with(subset(data, RES05_gender == 1), table(gender_arm, theme_arm))  # prev_reserved
with(subset(data, RES05_gender == 0), table(gender_arm, theme_arm))  # prev_unreserved
with(data, table(gender_arm, theme_arm))  # tous



