# Table 3 - pure effect - sep outcomes

library(dplyr)
library(fixest)
library(stargazer)
library(haven)

# controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

# dep variables for first series
dep_vars_series1 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female", "CHAL_prop_nongen")

# dep variables for second series
dep_vars_series2 <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_voteshare_female", "CHAL_voteshare_nongen")

# labels
depvar_labels_series1 <- c(
  "Number of Candidates",
  "Number of Challengers",
  "Proportion of Female Challengers (Candidates)",
  "Proportion of Non-General Challengers (Candidates)"
)

depvar_labels_series2 <- c(
  "Number of Candidates",
  "Number of Challengers",
  "Vote Share of Female Challengers",
  "Vote Share of Non-General Challengers"
)



# DATA

data_path <- "~/work/Electoral data cleaned.dta"
data <- read_dta(data_path)
data_filtered <- data %>% filter(RES10_gender == 0, GP_tag == 1)



# REGRESSION
create_formula <- function(dep_var) {
  formula_str <- as.formula(paste(dep_var, "~ INT_treatment +", paste(gpcontrols, collapse = " + "), "+ factor(district)"))
  return(formula_str)
}

# MODEL ESTIMATION FOR A GIVEN SUBSAMPLE
estimate_models <- function(data_subset, dep_vars) {
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

# models for each panel and each series of variables
panel_A_series1 <- estimate_models(data_filtered, dep_vars_series1)
panel_B_series1 <- estimate_models(data_filtered %>% filter(RES05_gender == 0), dep_vars_series1)
panel_C_series1 <- estimate_models(data_filtered %>% filter(RES05_gender == 1), dep_vars_series1)

panel_A_series2 <- estimate_models(data_filtered, dep_vars_series2)
panel_B_series2 <- estimate_models(data_filtered %>% filter(RES05_gender == 0), dep_vars_series2)
panel_C_series2 <- estimate_models(data_filtered %>% filter(RES05_gender == 1), dep_vars_series2)



# SEPARATE OUTPUTS

# 1st family
sink("~/work/Table3_Series1_Panels_ABC.txt")
cat("Table: Effects of INT_treatment on Challenger Entry (Candidates) by Gender Reservation Status\n")
cat("====================================================================================\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")
sink()

# 2nd family
sink("~/work/Table3_Series2_Panels_ABC.txt")
cat("Table: Effects of INT_treatment on Challenger Entry (Votes) by Gender Reservation Status\n")
cat("====================================================================================\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")
sink()





# both families together

sink("~/work/Table3_All_Panels_Stacked.txt")

cat("Table: Effects of INT_treatment on Challenger Entry by Gender Reservation Status\n")
cat("====================================================================================\n\n")

# Série 1 : Proportions en % cand
cat("SERIES 1: Proportions (Candidates)\n")
cat("------------------------------------------------------------\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_series1, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series1, model.numbers = FALSE, omit = "Intercept")

cat("\n\n")

# Série 2 : Proportions en % votes
cat("SERIES 2: Proportions (Votes)\n")
cat("------------------------------------------------------------\n\n")
cat("Panel A: Whole Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel B: Gender Reservation = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")
cat("\nPanel C: Gender Reservation = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_series2, type = "text", keep = "INT_treatment", digits = 3,
          dep.var.labels = depvar_labels_series2, model.numbers = FALSE, omit = "Intercept")

sink()

# deleting temporary files
file.remove(c("~/work/Table3_Series1_Panels_ABC.txt", "~/work/Table3_Series2_Panels_ABC.txt"))


## .TEX OUTPUT ##


## OUTPUT IN .TEX ##
sink("~/work/Table3_All_Panels.tex", append = FALSE, split = TRUE)

cat("\\documentclass{article}
\\usepackage{booktabs}
\\usepackage{caption}
\\usepackage{adjustbox}
\\usepackage{pdflscape}
\\begin{document}
\\begin{landscape}
\\begin{table}[htbp]
\\centering
\\caption{Effects of INT_treatment on Challenger Entry by Gender Reservation Status}
\\label{tab:challenger_entry}

\\begin{tabular}{lcccc}
\\toprule
& \\multicolumn{4}{c}{Series 1: Proportions (Candidates)} \\\\
\\cmidrule(lr){2-5}
Variable & Whole Sample & Gender Reservation = 0 & Gender Reservation = 1 \\
& (1) & (2) & (3) \\
\\midrule", sep = "\n")

# Panel A, B, C for Series 1
for (var in "INT_treatment") {
  cat(gsub("_", " ", var))
  for (panel in list(panel_A_series1, panel_B_series1, panel_C_series1)) {
    model <- panel[[1]]
    if (!is.null(model) && var %in% rownames(coef(summary(model)))) {
      coef_val <- round(coef(summary(model))[var, "Estimate"], 4)
      se_val <- round(coef(summary(model))[var, "Std. Error"], 4)
      cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
    } else {
      cat(" & ", sep = "")
    }
  }
  cat(" \\\\", sep = "")
}

cat("\\midrule
Observations ")
for (panel in list(panel_A_series1, panel_B_series1, panel_C_series1)) {
  model <- panel[[1]]
  if (!is.null(model)) cat(" & ", nobs(model), sep = "")
}
cat(" \\\\", sep = "")

cat("\\bottomrule
\\end{tabular}

\\quad

\\begin{tabular}{lcccc}
\\toprule
& \\multicolumn{4}{c}{Series 2: Proportions (Votes)} \\\\
\\cmidrule(lr){2-5}
Variable & Whole Sample & Gender Reservation = 0 & Gender Reservation = 1 \\
& (1) & (2) & (3) \\
\\midrule", sep = "\n")

# Panel A, B, C for Series 2
for (var in "INT_treatment") {
  cat(gsub("_", " ", var))
  for (panel in list(panel_A_series2, panel_B_series2, panel_C_series2)) {
    model <- panel[[1]]
    if (!is.null(model) && var %in% rownames(coef(summary(model)))) {
      coef_val <- round(coef(summary(model))[var, "Estimate"], 4)
      se_val <- round(coef(summary(model))[var, "Std. Error"], 4)
      cat(" & $", coef_val, " (", se_val, ")", "$", sep = "")
    } else {
      cat(" & ", sep = "")
    }
  }
  cat(" \\\\", sep = "")
}

cat("\\midrule
Observations ")
for (panel in list(panel_A_series2, panel_B_series2, panel_C_series2)) {
  model <- panel[[1]]
  if (!is.null(model)) cat(" & ", nobs(model), sep = "")
}
cat(" \\\\", sep = "")

cat("\\bottomrule
\\end{tabular}

\\end{table}
\\end{landscape}
\\end{document}", sep = "\n")

sink()





