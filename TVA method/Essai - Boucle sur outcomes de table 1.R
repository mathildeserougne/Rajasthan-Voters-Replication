# boucle TVA sur les outcomes de la table 1


library(dplyr)
library(estimatr)
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")


arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")


# données

electoral_data <- read_dta("Electoral data cleaned.dta")

base_df <- electoral_data %>% 
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    gender_arm = as.integer(INT_treatment_code %in% c(2, 4)),
    theme_arm  = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,  # eau
      INT_treatment_code %in% c(3, 4) ~ 2,  # routes
      TRUE                            ~ 0),
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI  = as.integer(district == "KARAULI")
  )


install.packages("TruncatedNormal")   
library(TruncatedNormal)              

# boucle sur les six outcomes de la table 1

incum_dep_vars1 <- c("INC05_running",       "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running",  "INCOTHER05_voteshare")

tva_list <- lapply(incum_dep_vars1, function(outcome) {
  
  message("----- TVA sur ", outcome, " -----")
  
  data_i <- base_df %>%                               
    select(all_of(outcome), all_of(arms), all_of(fes)) %>%
    drop_na()                                        
  
  res <- do_TVA(
    data       = as.data.frame(data_i),
    arms       = arms,
    y          = outcome,
    fes        = fes,
    estim_func = "pval_MSE",   # plus doux
    cutoff     = 0.30          # support plus réduit
  )
  
  ## imprimer les résultats
  print(res$marginal_support)
  print(res$pools_summary)
  print(res$pooled_ols)
  
  invisible(res)
})

names(tva_list) <- incum_dep_vars1  


# bon ça bloque à cause du passage winner curse masi j'arrive pas à m'en débarrasser
