### TVA METHOD ON ALL THREE TABLES #######


########## LIBRARIES AND FUNCTIONS FROM SIMON YASPO'S GITHUB ##################
install.packages(c("stargazer","jnitr","broom","haven","fixest","modelsummary","gt","webshot2","car"))
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
source("~/work/TVA/R/pooling.R")
source("~/work/TVA/R/support_estimation.R")
source("~/work/TVA/R/threshold_functions.R")
source("~/work/TVA/R/winners_curse.R")


############### TABLE 1 #####################################################

# using the TVA to determine relevant pools and controls for the studied outcomes
# estimation per outcome
# then attempt to do a loop over the outcomes
# then attempt to do a split between samples (whole/ gender reserved/not reserved)


# FORMATTING THE DATA

electoral_data <- read_dta("~/work/Electoral data cleaned.dta")


data <- electoral_data %>%
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    #gender_arm
    gender_arm = ifelse(INT_treatment_code %in% c(2, 4), 1, 0),

    #theme_arm (water, roads, nothing)
    theme_arm = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # roads
      INT_treatment_code %in% c(3, 4) ~ 2,   # water
      TRUE                              ~ 0   # control
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

## WHOLE SAMPLE FIRST

# Outcomes list
outcomes <- c("INC05_running",        "INC05_voteshare",
              "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
              "INCOTHER05_running",  "INCOTHER05_voteshare")

# cut-offs to test
cut_grid <- seq(0.05, 0.90, by = 0.05)

# no need of winner's curse here
environment(do_TVA)$winners_curse <- function(...) NULL

# no need to rewrite tva
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

  best_res  <- NULL      # best found result
  best_cut  <- NA_real_  # cutoff
  cmp0_used <- FALSE     # compare_to_zero ?

  # first without compare to zero
  for (cmp in c(FALSE, TRUE)) {
    for (ct in cut_grid) {
      best_res <- tryCatch(try_TVA(y, ct, cmp), error = function(e) NULL)
      if (!is.null(best_res) && nrow(best_res$marginal_support) > 0) {
        best_cut  <- ct
        cmp0_used <- cmp
        break                           # out of loop cut_grid
      }
    }
    if (!is.null(best_res)) break       # out of loop cmp
  }

  # display and stock results
  if (is.null(best_res)) {
    cat("No non-empty support until: ", max(cut_grid), "\n")
  } else {
    tva_table1[[y]] <- best_res

    cat("✔ cutoff =", best_cut,
        "| cmp0 =", cmp0_used,
        "| support =", nrow(best_res$marginal_support),
        "| pools =", nrow(best_res$pools_summary), "\n\n")

    print(best_res$marginal_support)
    cat("\n--- Pools summary ------------------------------\n")
    print(best_res$pools_summary)
    cat("\n--- Pooled OLS (effect vs control) --------------\n")
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
  if (nrow(df_s) == 0) { cat("   (no observation)\n"); next }

  for (y in outcomes) {

    cat("\n=== OUTCOME :", y, "===\n")
    best_res <- NULL; best_cut <- NA; cmp0_flag <- FALSE

    # first without comparison to zero
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
      cat("   → no non-empty support until 0.90\n")
      next
    }

    # stock and display a summary
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



############### TABLE 3 #####################################################

# FORMATTING THE DATA

electoral_data <- read_dta("~/work/Electoral data cleaned.dta")


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

# cut-offs
cut_grid <- seq(0.05, 0.90, by = 0.05)

# no winner's curse (as in Table 1)
environment(do_TVA)$winners_curse <- function(...) NULL

# Wrapper
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

# main loop
tva_table3 <- list()

for (y in outcomes_t3) {
  cat("\n================  OUTCOME :", y, " ================\n")

  best_res  <- NULL
  best_cut  <- NA_real_
  cmp0_used <- FALSE

  # without, then with, compare_to_zero
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
    cat("No non-empty support until", max(cut_grid), "\n")
  } else {
    tva_table3[[y]] <- best_res

    cat("✔ cutoff =", best_cut,
        "| cmp0 =", cmp0_used,
        "| support =", nrow(best_res$marginal_support),
        "| pools =", nrow(best_res$pools_summary), "\n\n")

    print(best_res$marginal_support)
    cat("\n--- Pools summary ------------------------------\n")
    print(best_res$pools_summary)
    cat("\n--- Pooled OLS (effect vs control) --------------\n")
    print(best_res$pooled_ols)
    cat("-------------------------------------------------\n")
  }
}



t3_summary <- tibble::tibble(
  outcome = names(tva_table3),
  cutoff  = purrr::map_dbl(tva_table3, ~ attr(.x, "cutoff", exact = TRUE) %||% NA_real_),
  cmp0    = purrr::map_lgl(tva_table3, ~ attr(.x, "compare_to_zero", exact = TRUE) %||% NA),
  n_supp  = purrr::map_int(tva_table3, ~ nrow(.x$marginal_support)),
  n_pools = purrr::map_int(tva_table3, ~ nrow(.x$pools_summary))
)
print(t3_summary)





### SPLITTING THE SAMPLE ACCORDING TO PREVIOUS GENDER RESERVATION STATUS


run_tva_t3 <- function(df, y, cutoff, cmp0 = FALSE) {
  # present and non empty outcome variable?
  if (!y %in% names(df)) return(NULL)
  # required parts, drop if na
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
      cat("   → no support", max(cut_grid), "\n")
      next
    }

    # stock
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








############### TABLE 4 #####################################################

# FORMATTING THE DATA
# electoral data and electoral 2015

electoral_data <- read_dta("~/work/Electoral data cleaned.dta")
electoral_2015 <- read_dta("~/work/Electoral data 2015 cleaned.dta")

# merge the two and create the arms

key_vars <- c("district", "gp")


data15 <- electoral_2015 %>%
  select(all_of(key_vars),
         RES15_gender,
         ELEC15_nbcands, ELEC15_incum10_running, ELEC15_voteshare_incum10,
         ELEC15_prop_cand2010, ELEC15_voteshare_cand2010,
         ELEC15_prop_female,  ELEC15_voteshare_female,
         ELEC15_prop_nongen,  ELEC15_voteshare_nongen,
         RES10_gender, SAMPLE_hhsurvey, GP_tag, INT_treatment_code, RES05_gender)




data <- data15 %>%
  filter(
    RES10_gender == 0,
    GP_tag        == 1,
    RES15_gender  == 0          # table 4 filter
  ) %>%
  mutate(
    gender_arm = as.integer(INT_treatment_code %in% c(2, 4)),
    theme_arm  = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # roads
      INT_treatment_code %in% c(3, 4) ~ 2,   # water
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


with(data, table(gender_arm, theme_arm))



outcomes <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_voteshare_incum10",
              "ELEC15_prop_cand2010", "ELEC15_voteshare_cand2010", "ELEC15_prop_female",
              "ELEC15_voteshare_female", "ELEC15_prop_nongen", "ELEC15_voteshare_nongen")

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")


# best cutoff for each outcome in the loop

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
    cat("No non-empty support until 0.90\n")
  } else {
    tva_table4[[y]] <- best_res

    cat("cutoff =", best_cut,
        "| cmp0 =", cmp0_used,
        "| support =", nrow(best_res$marginal_support),
        "| pools =", nrow(best_res$pools_summary), "\n\n")

    print(best_res$marginal_support)
    cat("\n--- Pools summary ------------------------------\n")
    print(best_res$pools_summary)
    cat("\n--- Pooled OLS (effect vs control) --------------\n")
    print(best_res$pooled_ols)
    cat("-------------------------------------------------\n")
  }
}



## SEPARATING THE SAMPLES ALONG GENDER RESERVATION STATUS


electoral_data   <- read_dta("~/work/Electoral data cleaned.dta")
electoral_2015   <- read_dta("~/work/Electoral data 2015 cleaned.dta")



key_vars <- c("district", "gp")

data10 <- electoral_data %>%
select(all_of(c(key_vars,
                "RES10_gender", "RES05_gender",
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
         RES15_gender  == 0) %>%
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
         RES05_gender,
         starts_with("ELEC15_")) %>%
  as.data.frame()


# arm, fe, outcome, cutoff
arms     <- c("gender_arm", "theme_arm")
fes      <- c("district_DHOLPUR", "district_KARAULI")
outcomes <- names(data)[grepl("^ELEC15_", names(data))]

cut_grid <- seq(0.05, 0.90, by = 0.05)
environment(do_TVA)$winners_curse <- function(...) NULL




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


## loop on  gender reservation
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
      if (!is.null(best)) break
    }

    if (is.null(best)) {
      cat("   → no support until 0.90\n")
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



with(subset(data, RES05_gender == 1), table(gender_arm, theme_arm))  # prev_reserved
with(subset(data, RES05_gender == 0), table(gender_arm, theme_arm))  # prev_unreserved
with(data, table(gender_arm, theme_arm))  # all
