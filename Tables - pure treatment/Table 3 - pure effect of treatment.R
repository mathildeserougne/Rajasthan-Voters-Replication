# T3: pure effect of treatment
# separating in three panels: whole, res05_gender==0, res05_gender==1
# no interactions

library(dplyr)
library(fixest)
library(stargazer)
library(haven)

# Controls and dep variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# DATA and filter
data_path <- "~/work/Electoral data cleaned.dta"
data <- read_dta(data_path)
data_filtered <- data %>% filter(RES10_gender == 0, GP_tag == 1)



# Regression formula
create_formula <- function(dep_var) {
  formula_str <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols, collapse = " + "), "+ factor(district)"))
  return(formula_str)
}

# model estimation for a given subsample
estimate_models <- function(data_subset) {
  models <- list()
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    formula <- create_formula(dep_var)
    model <- tryCatch({
      lm(formula, data = data_subset)
    }, error = function(e) NULL)
    models[[i]] <- model
  }
  return(models)
}


# Panel A: Whole sample
panel_A <- estimate_models(data_filtered)

# Panel B: RES05_gender == 0
panel_B <- estimate_models(data_filtered %>% filter(RES05_gender == 0))

# Panel C: RES05_gender == 1
panel_C <- estimate_models(data_filtered %>% filter(RES05_gender == 1))



## OUTPUT ##

# col names for the table
depvar_labels <- c(
  "Number of Candidates",
  "Number of Challengers",
  "Proportion of Female Challengers",
  "Vote Share of Female Challengers",
  "Proportion of Non-General Challengers",
  "Vote Share of Non-General Challengers"
)

# generating the table
stargazer(
  panel_A, panel_B, panel_C,
  type = "text",
  column.labels = c("Panel A: Whole Sample", "Panel B: Gender Reservation = 0", "Panel C: Gender Reservation = 1"),
  dep.var.labels = depvar_labels,
  keep = "INT_treatment",  
  digits = 3,
  title = "Table 3: Effects of INT_treatment on Challenger Entry by Gender Reservation Status",
  out = "~/work/Table3_Panels_ABC.txt",
  model.numbers = FALSE,
  omit = "Intercept"  
)



# stacking it vertically


sink("~/work/Table3_Panels_Stacked.txt")

cat("Table 3: Effects of INT_treatment on Challenger Entry by Gender Reservation Status (Stacked)\n")
cat("====================================================================================\n\n")

cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels, model.numbers = FALSE, omit = "Intercept")

sink()  