# tentative avec directement un seul type de traitement

# Source to stock functions coded on Simon Yaspo's git (cloned needs to be cloned in your environment)
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")


# FORMATTING THE DATA #########################################################

electoral_data <- read_dta("Electoral data cleaned.dta")


data <- electoral_data %>%                                   
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    #gender_arm neutralisé
    # gender_arm = ifelse(INT_treatment_code %in% c(2, 4), 1, 0),
    
    #theme_arm (water, roads, nothing)
    theme_arm = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # routes
      INT_treatment_code %in% c(3, 4) ~ 2,   # eau
      TRUE                              ~ 0   # contrôle
    ),
    
    # as integer (just to be sure)
    #gender_arm = as.integer(gender_arm),
    theme_arm  = as.integer(theme_arm),
    
    # district fixed effects
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI  = as.integer(district == "KARAULI")
  ) %>%
  select(INC05_running, theme_arm,
         district_DHOLPUR, district_KARAULI, RES05_gender, INC05_voteshare,
         INCSPOUSE05_running, INCSPOUSE05_voteshare,
         INCOTHER05_running, INCOTHER05_voteshare)

arms <- c("theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")

data       = as.data.frame(data)


# check before running the tva function (how many cells? how many empty?)
with(data, table(theme_arm))


# essai avec un outcome

# example of working tva: y = INC05_running

result <- do_TVA(
  data       = data,   
  arms       = arms,                  
  y          = "INC05_running",
  fes        = fes,                   
  estim_func = "pval_MSE",
  cutoff     = 0.10
)


result$marginal_support
result$pools_summary
result$pooled_ols       
result$winners_effect  







## WHOLE SAMPLE FIRST

# Liste des outcomes
outcomes <- c("INC05_running",        "INC05_voteshare",
              "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
              "INCOTHER05_running",  "INCOTHER05_voteshare")

# cut-offs à tester
cut_grid <- seq(0.05, 0.90, by = 0.05)

# ne pas tenir compte de winners curse
environment(do_TVA)$winners_curse <- function(...) NULL

# fonction pour ne pas réécrire tva
try_TVA <- function(y, cutoff, cmp0 = FALSE) {
  do_TVA(
    data            = data[, c(y, arms, fes)],   
    arms            = arms,
    y               = y,
    fes             = fes,
    estim_func      = "pval_MSE",
    cutoff          = cutoff,
    compare_to_zero = cmp0
  )
}

# main loop
tva_table1 <- list()     

for (y in outcomes) {
  cat("\n================  OUTCOME :", y, " ================\n")
  
  best_res  <- NULL      # meilleur résultat trouvé
  best_cut  <- NA_real_  # cutoff associé
  cmp0_used <- FALSE     # compare_to_zero ?
  
  # On essaie d’abord sans compare_to_zero, puis si nécessaire avec
  for (cmp in c(FALSE, TRUE)) {
    for (ct in cut_grid) {
      best_res <- tryCatch(try_TVA(y, ct, cmp), error = function(e) NULL)
      if (!is.null(best_res) && nrow(best_res$marginal_support) > 0) {
        best_cut  <- ct
        cmp0_used <- cmp
        break                           # on sort de la boucle cut_grid
      }
    }
    if (!is.null(best_res)) break       # on sort de la boucle cmp
  }
  
  # 6. Affichage / stockage des résultats
  if (is.null(best_res)) {
    cat("Aucun support non vide jusqu’à", max(cut_grid), "\n")
  } else {
    tva_table1[[y]] <- best_res
    
    cat("✔ cutoff =", best_cut,
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

