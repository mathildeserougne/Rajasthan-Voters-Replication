## TVA ON TABLE 1 ##############################################################

# using the TVA to determine relevant pools and controls for the studied outcomes
# estimation per outcome
# then attempt to do a loop over the outcomes
# then attempt to do a split between samples (whole/ gender reserved/not reserved)


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
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
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
  select(INC05_running, gender_arm, theme_arm,
         district_DHOLPUR, district_KARAULI, RES05_gender, INC05_voteshare,
         INCSPOUSE05_running, INCSPOUSE05_voteshare,
         INCOTHER05_running, INCOTHER05_voteshare)

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")

data       = as.data.frame(data)


# check before running the tva function (how many cells? how many empty?)
with(data, table(gender_arm, theme_arm))
# six cells / one empty / 5 non empty situations (control + 4 combos)


################################################################################

### PART 1 - Doing the outcomes separately #####################################


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




result <- do_TVA(
  data       = data,   
  arms       = arms,                  
  y          = "INC05_voteshare",
  fes        = fes,                   
  estim_func = "pval_MSE",
  cutoff     = 0.10
)
result$marginal_support
result$pools_summary
result$pooled_ols       
result$winners_effect  


result <- do_TVA(
  data       = data,   
  arms       = arms,                  
  y          = "INC05spouse_running",
  fes        = fes,                   
  estim_func = "pval_MSE",
  cutoff     = 0.60
)


result$marginal_support
result$pools_summary
result$pooled_ols       
result$winners_effect  


# cutoffs that work for all of the outcomes, separately?

# list of outcomes and range by which we test the cutoff value 
outcomes <- c("INC05_running", "INC05_voteshare",
              "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
              "INCOTHER05_running",  "INCOTHER05_voteshare")

cut_grid <- seq(0.05, 0.50, by = 0.05)          

# a function that finds the list of valid cutoffs, for a fixed y
find_valid_cutoffs <- function(y) {
  ok <- c()
  for (ct in cut_grid) {
    res <- tryCatch(
      do_TVA(data       = data %>% select(all_of(y), all_of(arms), all_of(fes)) %>% drop_na() %>% as.data.frame(),
             arms       = arms,
             y          = y,
             fes        = fes,
             estim_func = "pval_MSE",          # MSE = plus robuste
             cutoff     = ct),
      error = function(e) NULL)
    if (!is.null(res)) ok <- c(ok, ct)
  }
  ok
}

#list of candidates
valid_list <- lapply(outcomes, find_valid_cutoffs)
names(valid_list) <- outcomes
print(valid_list) 

common_cutoffs <- Reduce(intersect, valid_list)

if (length(common_cutoffs) == 0) {
  stop("No cutoff is kept, need to change the grid")
}

chosen_cutoff <- max(common_cutoffs)   
message("Most strict common cutoff: ", chosen_cutoff)


# with the grid cut_grid <- seq(0.05, 0.50, by = 0.05) , we do not find a result
# pb with the variables: INCSPOUSE05_voteshare et les deux INCOTHER05_* 

# en gardant seulement celles qui marchent bien?





# PART 2 - Looping on the outcomes #############################################


# well i did not manage to do something just on the cutoffs. 

outcomes   <- c("INC05_running",       "INC05_voteshare",
                "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                "INCOTHER05_running",  "INCOTHER05_voteshare")

cutoff_use <- 0.30          # same for all
estim_use  <- "pval_MSE"    # more robust says the documentation
cmp0_use   <- FALSE         # to adjust if necessary

## TVA LOOP

tva_res <- list()        

for (y in outcomes) {
  message("\n=== TVA sur ", y, " ===")
  
  data_i <- data %>% 
    select(all_of(y), all_of(arms), all_of(fes)) %>% 
    drop_na() %>%                        
    as.data.frame()
  
  # tryCatch : if one outcome goes crazy, keep going
  tva_res[[y]] <- tryCatch({
    res <- do_TVA(
      data              = data_i,
      arms              = arms,
      y                 = y,
      fes               = fes,
      estim_func        = estim_use,
      cutoff            = cutoff_use,
      compare_to_zero   = cmp0_use)
    
    cat(" → support =", nrow(res$marginal_support),
        " | pools =", nrow(res$pools_summary), "\n")
    res
  },
  error = function(e) {
    cat(" !!! fail :", e$message, "\n")
    NULL
  })
}



# what we get with this: works for INC05_running et INC05_voteshare, mais pas les autres
# empty support for INCSPOUSE05_running/voteshare et INCOTHER_05 (les deux)
# pas assez d'obs for the first
# pbm of instability when winners curse for the three other ones

# for the three problematic ones: 
for (y in c("INCSPOUSE05_running",
            "INCSPOUSE05_voteshare",
            "INCOTHER05_running",
            "INCOTHER05_voteshare")) {
  
  message("\n-- Retry on ", y)
  data_i <- data %>% select(all_of(y), all_of(arms), all_of(fes)) %>% drop_na() %>% as.data.frame()
  
  tva_res[[y]] <- tryCatch(
    do_TVA(data = data_i,
           arms = arms,
           y    = y,
           fes  = fes,
           estim_func      = "pval_MSE",
           cutoff          = 0.60,           # larger
           compare_to_zero = TRUE),          # more tolerant
    error = function(e) { cat(" still fails\n"); NULL }
  )
}

# only issue is the winners curse !
# we neutralise it for now...

environment(do_TVA)$winners_curse <- function(...) NULL

### redo TVA for the problematic outcomes
for (y in c("INCSPOUSE05_running","INCSPOUSE05_voteshare",
            "INCOTHER05_running","INCOTHER05_voteshare")) {
  
  data_i <- data %>% select(all_of(y), all_of(arms), all_of(fes)) %>% 
    drop_na() %>% as.data.frame()
  
  tva_res[[y]] <- do_TVA(
    data            = data_i,
    arms            = arms,
    y               = y,
    fes             = fes,
    estim_func      = "pval_MSE",
    cutoff          = 0.60,          # large
    compare_to_zero = TRUE)          # more flexible
}

# display results for all outcomes

for (nm in names(tva_res)) {
  
  cat("\n==============================\n")
  cat("   OUTCOME :", nm, "\n")
  cat("==============================\n")
  
  obj <- tva_res[[nm]]
  if (is.null(obj)) { cat("tva failed.\n"); next }
  
  cat("\n--- Marginal support (", nrow(obj$marginal_support), ")\n", sep = "")
  print(obj$marginal_support)
  
  cat("\n--- Pools summary\n")
  print(obj$pools_summary)
  
  cat("\n--- Pooled OLS (effet vs contrôle)\n")
  print(obj$pooled_ols)
}


# RESUME DE CE QU'ON TROUVE

# inc05_running ? Montrer routes/eau sans discours genre réduit fortement la candidature du sortant.
# inc05_voteshare ? Même pattern. effet principal significatif.
# pas significatif sur inc05spouse_running
# pas certain mais négatif sur spouse voteshare
# faible effet pour other running
# non sur voteshare de other, pas assez de données de tte façon



## là on ssaye de justifier notre pooling
## bons arguments pour pooler roads and water




# PART 3 - Separating the sample by gender reservation status ##################


outcomes <- c("INC05_running",       "INC05_voteshare",
              "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
              "INCOTHER05_running",  "INCOTHER05_voteshare")

# cutoffs for the nice guys and the problematic ones
param_tbl <- tibble::tribble(
  ~y,                    ~cutoff, ~cmp0,
  "INC05_running",        0.10,    FALSE,
  "INC05_voteshare",      0.10,    FALSE,
  "INCSPOUSE05_running",  0.60,    TRUE,
  "INCSPOUSE05_voteshare",0.60,    TRUE,
  "INCOTHER05_running",   0.60,    TRUE,
  "INCOTHER05_voteshare", 0.60,    TRUE
)

# neutralising winner's curse
environment(do_TVA)$winners_curse <- function(...) NULL

# two samples
samples <- list(
  prev_reserved    = data %>% filter(RES05_gender == 1),
  prev_unreserved  = data %>% filter(RES05_gender == 0)
)

tva_split <- list()   


# loop 

for (s in names(samples)) {
  cat("\n####################  SAMPLE :", s, "####################\n")
  dat_s <- samples[[s]]
  
  for (y in outcomes) {
    
    cat("\n=== TVA sur", y, "===\n")
    pars <- dplyr::filter(param_tbl, y == !!y)
    
    data_i <- dat_s %>%
      dplyr::select(all_of(y), all_of(arms), all_of(fes)) %>%
      tidyr::drop_na() %>% as.data.frame()
    
    res <- tryCatch(
      do_TVA(
        data            = data_i,
        arms            = arms,
        y               = y,
        fes             = fes,
        estim_func      = "pval_MSE",
        cutoff          = pars$cutoff,
        compare_to_zero = pars$cmp0),
      error = function(e) {cat("tva fails:", e$message, "\n"); NULL}
    )
    
    tva_split[[s]][[y]] <- res       
    
    if (!is.null(res)) {
      cat("-- support :", nrow(res$marginal_support),
          "| pools :",   nrow(res$pools_summary), "\n")
      
      cat("\n> Support\n");        print(res$marginal_support)
      cat("\n> Pools\n");          print(res$pools_summary)
      cat("\n> Pooled OLS\n");     print(res$pooled_ols)
    }
  }
}



# résultat de la comparaison entre les deux échantillons ?
# inc05_running: sanction du visuel sans genre n’apparaît que chez les “non-réservés”.
# inc05_voteshare: idem, plus fort chez non reserves
# incspouse running: effet négatif fort sur l’entrée du conjoint seulement là où une femme a déjà été sarpanch.
# spouse voteshare: même pattern : sanction présente uniquement chez “réservés”.
# pas solide sur les coefficients "others"

# avoir eu la reservation ça diminue l'effet additionnel de l'accent genre de la pp
# sans historique de reservation, le constat technique sur les accomplissements prédomine sur les considérations de genre





##
# si on décide de pooler gender emssage avec controle, on accepte que ça backfire






################################################################################

################################################################################

################################################################################

# FAIRE SOUS FORME DE BOUCLE.
# for (chaque outcome) on fait une boucle sur la valeur du cutoff, puis l'estimation


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



#####################################################################
## NOW, SPLITTING THE SAMPLE BASED ON THE PREVIOUS GENDER RESERVATION


cut_grid <- seq(0.05, 0.90, by = 0.05)
environment(do_TVA)$winners_curse <- function(...) NULL

# calling tva
run_tva <- function(df, y, cutoff, cmp0 = FALSE) {
  tryCatch(
    do_TVA(
      data            = df[ , c(y, arms, fes)],
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


# loop on split sample (women reservation 2005)

samples <- list(
  prev_reserved   = subset(data, RES05_gender == 1),
  prev_unreserved = subset(data, RES05_gender == 0)
)

tab_res  <- with(subset(data, RES05_gender == 1), table(gender_arm, theme_arm))
tab_unr  <- with(subset(data, RES05_gender == 0), table(gender_arm, theme_arm))
cat("\n--- RES05=1 (prev_reserved) ---\n"); print(tab_res)
cat("\n--- RES05=0 (prev_unreserved) ---\n"); print(tab_unr)

tva_split <- list()          

for (s in names(samples)) {
  cat("\n####################  SAMPLE :", s, "####################\n")
  df_s <- samples[[s]]
  
  # On saute le tour si le sous-échantillon est vide
  if (nrow(df_s) == 0) { cat("   (aucune observation)\n"); next }
  
  for (y in outcomes) {
    
    cat("\n=== OUTCOME :", y, "===\n")
    best_res <- NULL; best_cut <- NA; cmp0_flag <- FALSE
    
    # On cherche d’abord sans comparaison-à-zéro, puis avec
    for (cmp in c(FALSE, TRUE)) {
      for (ct in cut_grid) {
        best_res <- run_tva(df_s, y, ct, cmp)
        if (!is.null(best_res) && nrow(best_res$marginal_support) > 0) {
          best_cut  <- ct; cmp0_flag <- cmp
          break
        }
      }
      if (!is.null(best_res)) break
    }
    
    if (is.null(best_res)) {
      cat("   → aucun support non vide jusqu’à 0.90\n")
      next
    }
    
    # On stocke et on imprime un résumé
    tva_split[[s]][[y]] <- best_res
    cat(sprintf(
      "   ✔ cutoff = %.2f | cmp0 = %s | support = %d | pools = %d\n",
      best_cut, cmp0_flag, nrow(best_res$marginal_support),
      nrow(best_res$pools_summary)
    ))
    
    print(best_res$marginal_support)
    cat("\n--- Pools summary\n"); print(best_res$pools_summary)
    cat("\n--- Pooled OLS\n");    print(best_res$pooled_ols)
  }
}







