# Table 4 - pure effect - sep outcomes

library(tidyverse)
library(stargazer)
library(haven)

# controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# DATA
data <- read_dta("~/work/Electoral data 2015 cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0, GP_tag == 1, RES15_gender == 0) %>%
  mutate(
    INC10_can_run = 1,
    INC10_can_run = ifelse(ELEC10_won_female == 0 & RES15_gender == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_sc == 0 & RES15_sc == 1, 0, INC10_can_run),
    INC10_can_run = ifelse(ELEC10_won_st == 0 & RES15_st == 1, 0, INC10_can_run)
  )

# new variables
for (var in c("INT_treatment", "X_anytr_genderres05", "RES05_gender")) {
  data_filtered <- data_filtered %>%
    mutate(!!paste0("X15_", var) := get(var) * (RES15_gender == 1))
}

# dep variables for 1st family
dep_vars_family1 <- c("ELEC15_nbcands", "ELEC15_incum10_running", "ELEC15_prop_cand2010",
                      "ELEC15_prop_female", "ELEC15_prop_nongen")
# dep variables for 2nd family
dep_vars_family2 <- c("ELEC15_nbcands", "ELEC15_voteshare_incum10", "ELEC15_voteshare_cand2010",
                      "ELEC15_voteshare_female", "ELEC15_voteshare_nongen")

# labels for variables
depvar_labels_family1 <- c(
  "Number of Candidates",
  "Incumbent Running",
  "Proportion of 2010 Candidates (Candidates)",
  "Proportion of Female Candidates",
  "Proportion of Non-General Candidates"
)

depvar_labels_family2 <- c(
  "Number of Candidates",
  "Incumbent Vote Share",
  "Vote Share of 2010 Candidates",
  "Vote Share of Female Candidates",
  "Vote Share of Non-General Candidates"
)


# REGRESSION
create_formula <- function(dep_var) {
  formula_str <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols15, collapse = " + "), "+ factor(district)"))
  return(formula_str)
}

# model estimation for a given subsample
estimate_models <- function(data_subset, dep_vars) {
  models <- list()
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    formula <- create_formula(dep_var)
    model <- tryCatch(lm(formula, data = data_subset), error = function(e) NULL)
    models[[i]] <- model
  }
  return(models)
}

# model estimation for each panel and family
panel_A_family1 <- estimate_models(data_filtered, dep_vars_family1)
panel_B_family1 <- estimate_models(data_filtered %>% filter(RES05_gender == 0), dep_vars_family1)
panel_C_family1 <- estimate_models(data_filtered %>% filter(RES05_gender == 1), dep_vars_family1)

panel_A_family2 <- estimate_models(data_filtered, dep_vars_family2)
panel_B_family2 <- estimate_models(data_filtered %>% filter(RES05_gender == 0), dep_vars_family2)
panel_C_family2 <- estimate_models(data_filtered %>% filter(RES05_gender == 1), dep_vars_family2)



# OUTPUT
sink("~/work/Table4_SixPanels_Stacked.txt")

cat("Table 4: Effects of INT_treatment on Candidate Entry in 2015 by Gender Reservation Status (Six Panels)\n")
cat("====================================================================================\n\n")

# 1st family
cat("FAMILY 1: Proportions (Candidates)\n")
cat("------------------------------------------------------------\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_family1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_family1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_family1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family1, model.numbers = FALSE, omit = "Intercept")

cat("\n\n")

# 2nd family
cat("FAMILY 2: Proportions (Votes)\n")
cat("------------------------------------------------------------\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_family2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_family2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_family2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_family2, model.numbers = FALSE, omit = "Intercept")

sink()
