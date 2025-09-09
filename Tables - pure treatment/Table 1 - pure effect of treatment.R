# T1 - pure effect of treatment
# effect of the treatment but on separate subsamples.
# no interaction anymore between treatment and previous gender quotas.


library(tidyverse)
library(stargazer)
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

# Dep variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                     "INCOTHER05_running", "INCOTHER05_voteshare")

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

# Estimate models on a given subsample: 
estimate_models <- function(data_subset) {
  models <- list()
  for (i in 1:length(incum_dep_vars1)) {
    dep_var <- incum_dep_vars1[i]
    formula <- create_formula(dep_var)
    model <- lm(formula, data = data_subset)
    models[[i]] <- model
  }
  return(models)
}

# Panel A :whole sample
panel_A <- estimate_models(data_filtered)

# Panel B : RES05_gender == 0
panel_B <- estimate_models(filter(data_filtered, RES05_gender == 0))

# Panel C : RES05_gender == 1
panel_C <- estimate_models(filter(data_filtered, RES05_gender == 1))


## OUTPUT ## 

# column names for stargazer
col_names <- c("Incumbent Runs", "Incumbent Vote Share",
               "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
               "Other Family Member Runs", "Other Family Member Vote Share")

# variables to display
outregvar <- "INT_treatment"



## vertical stacking of the panels


sink("~/work/Table1_Panels_ABC_Stacked.txt")

cat("Table: Effects of INT_treatment by Gender Reservation Status (Stacked)\n")
cat("========================================================================\n\n")

cat("Panel A: Full Sample\n")
cat("------------------------------------------------------------\n")
stargazer(panel_A, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel B: RES05_gender = 0\n")
cat("------------------------------------------------------------\n")
stargazer(panel_B, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names, model.numbers = FALSE, omit = "Intercept")

cat("\nPanel C: RES05_gender = 1\n")
cat("------------------------------------------------------------\n")
stargazer(panel_C, type = "text", keep = "INT_treatment", digits = 2,
          dep.var.labels = col_names, model.numbers = FALSE, omit = "Intercept")

sink()




## .TEX OUTPUT


# Charger la bibliothèque
library(stargazer)

# Créer un fichier .tex avec la table
sink("~/work/Table1_Panels_ABC_Stacked.tex")

# En-tête LaTeX minimal
cat(
  '
\\documentclass{article}
\\begin{document}
'
)

# Panel A
cat("\\textbf{Panel A: Full Sample}\\\\")
stargazer(
  panel_A,
  type = "latex",
  keep = "INT_treatment",
  digits = 2,
  dep.var.labels = col_names,
  model.numbers = FALSE,
  omit = "Intercept",
  covariate.labels = c("INT treatment"),  # Remplace les underscores
  out = "temp_A.tex"  # Sauvegarde temporaire
)

# Panel B
cat("\\\\ \\textbf{Panel B: RES05 gender = 0}\\\\")
stargazer(
  panel_B,
  type = "latex",
  keep = "INT_treatment",
  digits = 2,
  dep.var.labels = col_names,
  model.numbers = FALSE,
  omit = "Intercept",
  covariate.labels = c("INT treatment"),
  out = "temp_B.tex"
)

# Panel C
cat("\\\\ \\textbf{Panel C: RES05 gender = 1}\\\\")
stargazer(
  panel_C,
  type = "latex",
  keep = "INT_treatment",
  digits = 2,
  dep.var.labels = col_names,
  model.numbers = FALSE,
  omit = "Intercept",
  covariate.labels = c("INT treatment"),
  out = "temp_C.tex"
)

# Fermeture du document LaTeX
cat(
  '
\\end{document}
'
)

sink()

# Combiner les fichiers temporaires en un seul
combined_table <- c(
  readLines("Table1_Panels_ABC_Stacked.tex"),
  readLines("temp_A.tex"),
  readLines("temp_B.tex"),
  readLines("temp_C.tex")
)

# Sauvegarder le fichier final
writeLines(combined_table, "Table1_Panels_ABC_Stacked_final.tex")

# Supprimer les fichiers temporaires
file.remove(c("temp_A.tex", "temp_B.tex", "temp_C.tex"))






## tentative version paysage



