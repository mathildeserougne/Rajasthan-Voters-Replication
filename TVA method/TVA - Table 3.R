## TVA ON TABLE 3 - candidate entry ############################################
################################################################################


# vérifier plus finement sur low caste challenger, percentage of candidates
# normalement la table donne un truc significatif


# same data specification as for table 1: 
# just change the outcomes studied !


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

electoral_data <- read_dta("Electoral data cleaned.dta")


data <- electoral_data %>%                                   
  filter(RES10_gender == 0,
         GP_tag == 1) %>%
  mutate(
    #gender_arm
    gender_arm = ifelse(INT_treatment_code %in% c(2, 4), 1, 0),
    
    #theme_arm (water, roads, nothing)
    theme_arm = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # routes
      INT_treatment_code %in% c(3, 4) ~ 2,   # eau
      TRUE                              ~ 0   # contrôle
    ),
    
    # as integer (just to be sure)
    gender_arm = as.integer(gender_arm),
    theme_arm  = as.integer(theme_arm),
    
    # district fixed effects
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI  = as.integer(district == "KARAULI")
  ) %>%
  select(gender_arm, theme_arm,
         ELEC10_nbcands, CHAL_nbchal, CHAL_prop_female,
         CHAL_voteshare_female, CHAL_prop_nongen, CHAL_voteshare_nongen,
         district_DHOLPUR, district_KARAULI, RES05_gender)

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")

data       = as.data.frame(data)


# check before running the tva function (how many cells? how many empty?)
with(data, table(gender_arm, theme_arm))
# six cells / one empty / 5 non empty situations (control + 4 combos)


################################################################################


# check if one common cutoff exists for all of the outcomes

outcomes <- c("ELEC10_nbcands",         
              "CHAL_nbchal",           
              "CHAL_prop_female",     
              "CHAL_voteshare_female",  
              "CHAL_prop_nongen",     
              "CHAL_voteshare_nongen") 

cut_grid <- seq(0.05, 0.50, by = 0.05)   

environment(do_TVA)$winners_curse <- function(...) NULL

find_support <- function(y, cut) {
  tryCatch({
    res <- do_TVA(data  = data %>% select(all_of(y), all_of(arms)) %>% drop_na(),
                  arms  = arms,
                  y     = y,
                  fes   = NULL,          # à cette étape on n'en tient pas compte
                  estim_func = "pval_MSE",
                  cutoff      = cut,
                  compare_to_zero = FALSE)
    nrow(res$marginal_support)           # taille du support
  }, error = function(e) 0)
}

# tableau outcome × cutoff
library(purrr); library(tidyr)
grid_res <- map_df(outcomes, ~{
  map_dbl(cut_grid, ~find_support(.x, .y)) %>%
    setNames(cut_grid) %>%               
    as.list()
}, .id = "outcome")

print(grid_res)

# bon on obtient une table nulle

# donc on fait autrement
# pour chaque outcome, on prend le meilleur cutoff...


tva_table3 <- list()

for (y in outcomes) {
  # boucle sur l'outcome pour trouver le meilleur cutoff
  best_cut <- NULL
  for (ct in cut_grid) {
    if (find_support(y, ct) > 0) { best_cut <- ct; break }
  }
  if (is.null(best_cut)) {             
    for (ct in cut_grid) {
      if (find_support(y, ct) > 0) { best_cut <- ct; cmp0 <- TRUE; break }
    }
  } else cmp0 <- FALSE                 
  
  if (is.null(best_cut)) {
    message(y, " : support vide même après 0.50 → ignoré")
    next
  }
  
  # lance TVA 
  tva_table3[[y]] <- do_TVA(
    data            = data %>% select(all_of(y), all_of(arms)) %>% drop_na(),
    arms            = arms,
    y               = y,
    fes             = NULL,
    estim_func      = "pval_MSE",
    cutoff          = best_cut,
    compare_to_zero = cmp0)
  
  cat("\n>>> ", y, " | cutoff ", best_cut,
      " | support ", nrow(tva_table3[[y]]$marginal_support),
      " | pools ",   nrow(tva_table3[[y]]$pools_summary), "\n", sep = "")
}


# bon premier essai nous donne rien
# soit pas assez de données, soit cutoff trop strict...
# mais tjs est-il que la tva ne reconnaît pas de signal assez significatif



## nouvel essai avec plus de tolérance sur le cutoff et incluant les fixed effects

try_TVA <- function(df, y, cut, cmp0) {
  do_TVA(
    data            = df %>% select(all_of(y), all_of(arms), all_of(fes)) %>% drop_na(),
    arms            = arms,
    y               = y,
    fes             = fes,
    estim_func      = "pval_MSE",
    cutoff          = cut,
    compare_to_zero = cmp0
  )
}


cut_grid <- seq(0.05, 0.90, by = 0.05)
res_list <- list()

for (y in outcomes) {
  
  message("\n=== SEARCH : ", y, " ===")
  best_res <- NULL
  best_cut <- NA
  cmp_used <- FALSE
  
  
  for (ct in cut_grid) {
    tmp <- suppressWarnings(
      tryCatch(try_TVA(data, y, ct, FALSE),
               error = function(e) NULL))
    if (!is.null(tmp)) { best_res <- tmp; best_cut <- ct; break }
  }
  
  
  if (is.null(best_res)) {
    for (ct in cut_grid) {
      tmp <- suppressWarnings(
        tryCatch(try_TVA(data, y, ct, TRUE),
                 error = function(e) NULL))
      if (!is.null(tmp)) { best_res <- tmp; best_cut <- ct; cmp_used <- TRUE; break }
    }
  }
  
  # result
  if (is.null(best_res)) {
    message("→ support toujours vide jusqu’à 0.90 (même cmp0=TRUE)")
  } else {
    res_list[[y]] <- best_res
    cat("→ cutoff ", best_cut,
        " | cmp0=", cmp_used,
        " | support ", nrow(best_res$marginal_support),
        " | pools ",   nrow(best_res$pools_summary), "\n", sep = "")
    
    print(best_res$marginal_support)
    print(best_res$pools_summary)
    print(best_res$pooled_ols)
  }
}


## toujours aucun signe significatif d'effet de l'activation d'un des bras
# peu d'observation? faible variation?





########################################
# autre méthode corrigée par chat gpt ##########

get_df <- function(y) {
  data %>% 
    dplyr::select(dplyr::all_of(c(y, arms, fes))) %>% 
    tidyr::drop_na() %>% 
    as.data.frame()
}

################################################################################
## 2.  Wrapper TVA  (avec gestion d’erreur silencieuse)
################################################################################
run_tva <- function(y, cutoff, cmp0 = FALSE) {
  tryCatch(
    do_TVA(
      data            = get_df(y),
      arms            = arms,
      y               = y,
      fes             = fes,           # on garde toujours les FE
      estim_func      = "pval_MSE",
      cutoff          = cutoff,
      compare_to_zero = cmp0
    ),
    error = function(e) NULL
  )
}

## On neutralise l’éventuelle correction winners-curse (optionnel)
environment(do_TVA)$winners_curse <- function(...) NULL

################################################################################
## 3.  Recherche du meilleur cut-off  + impression des résultats
################################################################################
cut_grid <- seq(0.05, 0.90, by = 0.05)
tva_table3 <- list()

for (y in outcomes) {
  cat("\n================  OUTCOME :", y, " ================\n")
  
  best <- NULL
  best_cut <- NA
  cmp0_flag <- FALSE
  
  ## (a) 1ʳᵉ passe : sans compare_to_zero
  for (ct in cut_grid) {
    best <- run_tva(y, ct, cmp0 = FALSE)
    if (!is.null(best) && nrow(best$marginal_support) > 0) {
      best_cut <- ct; break
    }
  }
  
  ## (b) 2ᵉ passe : on autorise compare_to_zero = TRUE
  if (is.null(best)) {
    for (ct in cut_grid) {
      best <- run_tva(y, ct, cmp0 = TRUE)
      if (!is.null(best) && nrow(best$marginal_support) > 0) {
        best_cut  <- ct
        cmp0_flag <- TRUE
        break
      }
    }
  }
  
  ## (c) sortie
  if (is.null(best)) {
    cat("Aucun support non vide, même avec cutoff 0.90\n")
    next
  }
  
  tva_table3[[y]] <- best
  
  cat("✔ cutoff =", best_cut,
      "| cmp0 =", cmp0_flag,
      "| support =", nrow(best$marginal_support),
      "| pools =", nrow(best$pools_summary), "\n\n")
  
  print(best$marginal_support)
  cat("\n--- Pools summary ------------------------------\n")
  print(best$pools_summary)
  cat("\n--- Pooled OLS (effet vs contrôle) --------------\n")
  print(best$pooled_ols)
  cat("-------------------------------------------------\n")
}



################################################################################

################################################################################

################################################################################

## DOING IT WITH A LOOP
# for each outcome, finding the best cutoff, and applying tva.

# Outcomes Table 3
outcomes_t3 <- c(
  "ELEC10_nbcands",
  "CHAL_nbchal",
  "CHAL_prop_female",
  "CHAL_voteshare_female",
  "CHAL_prop_nongen",
  "CHAL_voteshare_nongen"
)

# Grille de cut-offs
cut_grid <- seq(0.05, 0.90, by = 0.05)

# Désactiver winner's curse (comme Table 1)
environment(do_TVA)$winners_curse <- function(...) NULL

# Wrapper pratique
try_TVA_t3 <- function(y, cutoff, cmp0 = FALSE) {
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

# --- Boucle principale -------------------------------------------------------
tva_table3 <- list()

for (y in outcomes_t3) {
  cat("\n================  OUTCOME :", y, " ================\n")
  
  best_res  <- NULL
  best_cut  <- NA_real_
  cmp0_used <- FALSE
  
  # Essai sans puis avec compare_to_zero
  for (cmp in c(FALSE, TRUE)) {
    for (ct in cut_grid) {
      res_try <- tryCatch(try_TVA_t3(y, ct, cmp), error = function(e) NULL)
      if (!is.null(res_try) && nrow(res_try$marginal_support) > 0) {
        best_res  <- res_try
        best_cut  <- ct
        cmp0_used <- cmp
        break
      }
    }
    if (!is.null(best_res)) break
  }
  
  if (is.null(best_res)) {
    cat("Aucun support non vide jusqu’à", max(cut_grid), "\n")
  } else {
    tva_table3[[y]] <- best_res
    
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

## Résumé rapide : convertir en data.frame empilé (optionnel) -----------------
# Si vous souhaitez comparer les meilleurs cutoffs/outils dans un tableau:
t3_summary <- tibble::tibble(
  outcome = names(tva_table3),
  cutoff  = purrr::map_dbl(tva_table3, ~ attr(.x, "cutoff", exact = TRUE) %||% NA_real_),
  cmp0    = purrr::map_lgl(tva_table3, ~ attr(.x, "compare_to_zero", exact = TRUE) %||% NA),
  n_supp  = purrr::map_int(tva_table3, ~ nrow(.x$marginal_support)),
  n_pools = purrr::map_int(tva_table3, ~ nrow(.x$pools_summary))
)
print(t3_summary)




###############################################################################
### SPLITTING THE SAMPLE ACCORDING TO PREVIOUS GENDER RESERVATION STATUS ######


run_tva_t3 <- function(df, y, cutoff, cmp0 = FALSE) {
  # S'assurer que la variable outcome est présente et non vide
  if (!y %in% names(df)) return(NULL)
  # Sous-set colonnes requises; drop cases complètement NA sur outcome
  tmp <- df[, c(y, arms, fes)]
  if (all(is.na(tmp[[y]]))) return(NULL)
  tmp <- tmp[!is.na(tmp[[y]]), , drop = FALSE]
  if (nrow(tmp) == 0) return(NULL)
  
  tryCatch(
    do_TVA(
      data            = tmp,
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

samples_t3 <- list(
  prev_reserved   = subset(data, RES05_gender == 1),
  prev_unreserved = subset(data, RES05_gender == 0)
)


with(subset(data, RES05_gender == 1), table(gender_arm, theme_arm))  # prev_reserved
with(subset(data, RES05_gender == 0), table(gender_arm, theme_arm))  # prev_unreserved

# split loop

tva_t3_split <- list()

for (s in names(samples_t3)) {
  cat("\n####################  SAMPLE :", s, "####################\n")
  df_s <- samples_t3[[s]]
  
  for (y in outcomes_t3) {
    cat("\n=== OUTCOME :", y, "===\n")
    
    best <- NULL
    best_cut <- NA_real_
    cmp0_flag <- FALSE
    
    for (cmp in c(FALSE, TRUE)) {
      for (ct in cut_grid) {
        res_try <- run_tva_t3(df_s, y, ct, cmp)
        if (!is.null(res_try) && nrow(res_try$marginal_support) > 0) {
          best       <- res_try
          best_cut   <- ct
          cmp0_flag  <- cmp
          break
        }
      }
      if (!is.null(best)) break
    }
    
    if (is.null(best)) {
      cat("   → aucun support jusqu’à", max(cut_grid), "\n")
      next
    }
    
    # Stockage
    if (is.null(tva_t3_split[[s]])) tva_t3_split[[s]] <- list()
    tva_t3_split[[s]][[y]] <- best
    
    cat("   ✔ cutoff =", best_cut,
        "| cmp0 =", cmp0_flag,
        "| support =", nrow(best$marginal_support),
        "| pools =",   nrow(best$pools_summary), "\n")
    
    print(best$marginal_support)
    cat("\n--- Pools summary\n"); print(best$pools_summary)
    cat("\n--- Pooled OLS\n");    print(best$pooled_ols)
  }
}





# --------------------------------------------------------------------------- #
# (Optionnel) tableau récap pour chaque sous-échantillon
# --------------------------------------------------------------------------- #
summ_list <- lapply(names(tva_t3_split), function(s) {
  lst <- tva_t3_split[[s]]
  if (is.null(lst) || !length(lst)) return(NULL)
  tibble::tibble(
    sample  = s,
    outcome = names(lst),
    # si do_TVA attache ces attributs :
    cutoff  = purrr::map_dbl(lst, ~ attr(.x, "cutoff", exact = TRUE) %||% NA_real_),
    cmp0    = purrr::map_lgl(lst, ~ attr(.x, "compare_to_zero", exact = TRUE) %||% NA),
    n_supp  = purrr::map_int(lst, ~ nrow(.x$marginal_support)),
    n_pools = purrr::map_int(lst, ~ nrow(.x$pools_summary))
  )
})
t3_split_summary <- dplyr::bind_rows(summ_list)
print(t3_split_summary)
