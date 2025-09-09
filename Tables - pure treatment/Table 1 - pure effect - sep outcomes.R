# Table 1 pure effect of treatment BUT separate families of outcomes

library(tidyverse)
library(fixest)
library(modelsummary)

# DATA
data <- read_dta("~/work/Electoral data cleaned.dta")

# Original filters
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Dep variables: séparer "runs" et "voteshare"
incum_dep_vars_runs <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
incum_dep_vars_voteshare <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# REGRESSION FORMULA
create_formula <- function(dep_var) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  formula_str <- paste(dep_var, "~ INT_treatment +", base_controls, "+ factor(district)")
  return(as.formula(formula_str))
}

# Estimate models on a given subsample and for a given set of dep vars
estimate_models <- function(data_subset, dep_vars) {
  models <- list()
  for (i in 1:length(dep_vars)) {
    dep_var <- dep_vars[i]
    formula <- create_formula(dep_var)
    model <- lm(formula, data = data_subset)
    models[[i]] <- model
  }
  return(models)
}

# Panel A : whole sample
panel_A_runs <- estimate_models(data_filtered, incum_dep_vars_runs)
panel_A_voteshare <- estimate_models(data_filtered, incum_dep_vars_voteshare)

# Panel B : RES05_gender == 0
panel_B_runs <- estimate_models(filter(data_filtered, RES05_gender == 0), incum_dep_vars_runs)
panel_B_voteshare <- estimate_models(filter(data_filtered, RES05_gender == 0), incum_dep_vars_voteshare)

# Panel C : RES05_gender == 1
panel_C_runs <- estimate_models(filter(data_filtered, RES05_gender == 1), incum_dep_vars_runs)
panel_C_voteshare <- estimate_models(filter(data_filtered, RES05_gender == 1), incum_dep_vars_voteshare)

# Noms des colonnes pour modelsummary
col_names_runs <- c("Incumbent Runs", "Incumbent Spouse Runs", "Other Family Member Runs")
col_names_voteshare <- c("Incumbent Vote Share", "Incumbent Spouse Vote Share", "Other Family Member Vote Share")

# Combiner tous les modèles dans une liste pour modelsummary
all_models <- list(
  "Panel A (Full Sample)" = list(
    "Runs" = panel_A_runs,
    "Vote Share" = panel_A_voteshare
  ),
  "Panel B (RES05_gender = 0)" = list(
    "Runs" = panel_B_runs,
    "Vote Share" = panel_B_voteshare
  ),
  "Panel C (RES05_gender = 1)" = list(
    "Runs" = panel_C_runs,
    "Vote Share" = panel_C_voteshare
  )
)


## TXT OUTPUT

sink("~/work/Table1_sep_outcomes_Combined.txt")


cat("Table: Effects of INT_treatment by Gender Reservation Status\n")
cat("========================================================================\n\n")

# runs
cat("Runs Outcomes\n")
cat("----------------------------------------------------------------------\n\n")

cat("Panel A: Full Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_runs, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel B: RES05_gender = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_runs, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel C: RES05_gender = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_runs, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept")


# voteshare
cat("\nVote Share Outcomes\n")
cat("----------------------------------------------------------------------\n\n")

cat("Panel A: Full Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A_voteshare, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel B: RES05_gender = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B_voteshare, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel C: RES05_gender = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C_voteshare, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept")



sink()




## TEX OUTPUT


# Noms des colonnes pour stargazer avec des espaces
col_names_runs <- c("Incumbent Runs", "Incumbent Spouse Runs", "Other Family Member Runs")
col_names_voteshare <- c("Incumbent Vote Share", "Incumbent Spouse Vote Share", "Other Family Member Vote Share")

# TEX OUTPUT
sink("~/work/Table1_sep_outcomes_Combined.tex")

# Début du document LaTeX
cat("\\documentclass{article}
\\usepackage{booktabs}
\\begin{document}
\\section*{Effects of INT treatment by Gender Reservation Status}
\\subsection*{Runs Outcomes}
\\begin{center}
")

# Runs Outcomes
stargazer(panel_A_runs, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept",
          title = "Panel A: Full Sample")

stargazer(panel_B_runs, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept",
          title = "Panel B: RES05 gender = 0")

stargazer(panel_C_runs, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_runs, model.numbers = FALSE, omit = "Intercept",
          title = "Panel C: RES05 gender = 1")

# Vote Share Outcomes
cat("\\subsection*{Vote Share Outcomes}
")

stargazer(panel_A_voteshare, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept",
          title = "Panel A: Full Sample")

stargazer(panel_B_voteshare, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept",
          title = "Panel B: RES05 gender = 0")

stargazer(panel_C_voteshare, type = "latex", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names_voteshare, model.numbers = FALSE, omit = "Intercept",
          title = "Panel C: RES05 gender = 1")

# Fin du document LaTeX
cat("\\end{center}
\\end{document}")

# Fermer la sortie tex
sink()
