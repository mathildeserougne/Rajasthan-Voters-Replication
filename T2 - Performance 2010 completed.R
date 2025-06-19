## TABLE 2 PERFORMANCE COMPLETED ###############

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

# Load the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Define control variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Define dependent variables
incum_dep_vars1 <- c("INC05_running", "INC05_voteshare", "INC05_won",
                     "INCSPOUSE05_running", "INCSPOUSE05_voteshare", "INCSPOUSE05_won",
                     "INCOTHER05_running", "INCOTHER05_voteshare", "INCOTHER05_won")

# Filter the data
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )

# Create indices
data_filtered <- data_filtered %>%
  mutate(
    index_empl_svy_0 = rowMeans(select(., starts_with("std_HH_NREGA_")), na.rm = TRUE),
    index_empl_svy_1 = rowMeans(select(., std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f, std_HH_NREGA_waiting_time_m, std_HH_NREGA_waiting_time_f, std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_2 = rowMeans(select(., std_HH_NREGA, std_HH_NREGA_work_m, std_HH_NREGA_work_f), na.rm = TRUE),
    index_empl_svy_3 = rowMeans(select(., std_HH_NREGA_unmet_demand_m, std_HH_NREGA_unmet_demand_f), na.rm = TRUE)
  )

# Function for regression formulas
create_formula <- function(dep_var, index_var) {
  as.formula(paste(dep_var, "~ INT_treatment +", index_var, "+ INT_treatment:", index_var, "+", paste(gpcontrols, collapse = " + "), "+ factor(district)"))
}

# Function for statistical tests
# Function for statistical tests
calculate_tests <- function(model, mean_index, index_var) {
  # Test 1: INT_treatment + INT_treatment:index_var = 0
  test1 <- tryCatch({
    car::linearHypothesis(model, paste0("INT_treatment + INT_treatment:", index_var, " = 0"))
  }, error = function(e) {
    return(list(PrF = NA))
  })
  pval1 <- if (!is.null(test1$PrF)) round(test1$`Pr(>F)`[2], 2) else NA
  
  # Test 2: INT_treatment + INT_treatment:index_var * mean_index = 0
  test2 <- tryCatch({
    car::linearHypothesis(model, c("INT_treatment = 0", paste0("INT_treatment:", index_var, " = 0")))
  }, error = function(e) {
    return(list(PrF = NA))
  })
  pval2 <- if (!is.null(test2$PrF)) round(test2$`Pr(>F)`[2], 2) else NA
  
  return(list(pval1 = pval1, pval2 = pval2))
}

# Estimating the models
# Estimating the models
models_list <- list()
control_means <- list()
test_results <- list()

for (dep_var in incum_dep_vars1) {
  for (index_var in c("index_empl_svy_0", "index_empl_svy_1", "index_empl_svy_2", "index_empl_svy_3")) {
    control_mean <- data_filtered %>%
      filter(INT_treatment == 0 & RES05_gender == 0) %>%
      summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean) %>%
      round(2)
    
    control_means[[paste(dep_var, index_var, sep = "_")]] <- control_mean
    
    formula <- create_formula(dep_var, index_var)
    model <- lm(formula, data = data_filtered)
    models_list[[paste(dep_var, index_var, sep = "_")]] <- model
    
    mean_index <- mean(data_filtered[[index_var]], na.rm = TRUE)
    test_results[[paste(dep_var, index_var, sep = "_")]] <- calculate_tests(model, mean_index, index_var)
  }
}







## TABLE


# Préparation des modèles pour stargazer
# Assurez-vous que chaque modèle est nommé correctement pour correspondre à votre structure de tableau
model_names <- c(
  "INC05_running", "INC05_voteshare",
  "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
  "INCOTHER05_running", "INCOTHER05_voteshare"
)

# Utilisation de stargazer pour créer le tableau
stargazer(
  models_list,
  type = "text",
  column.labels = model_names,
  covariate.labels = c(
    "INT_treatment" = "Treatment",
    "index_empl_svy_0" = "Performance Index",
    "INT_treatment:index_empl_svy_0" = "Treatment:Performance"
  ),
  add.lines = list(
    c("Observations", sapply(models_list, function(x) if (!is.null(x)) nobs(x) else NA)),
    c("Mean in Control without GQ", sapply(control_means, function(x) if (!is.null(x)) x else NA))
  ),
  digits = 2,
  title = "Table 2: Performance",
  out = "Table2_Performance_2010_completed.txt"
)



## problème
# Assurez-vous que chaque modèle est valide et ne contient pas de valeurs manquantes
valid_models <- sapply(models_list, function(model) {
  !is.null(model) && all(!is.na(coef(model)))
})

models_list <- models_list[valid_models]

# Assurez-vous que les noms des modèles sont corrects
names(models_list) <- c(
  "Incumbent Runs", "Incumbent Vote Share",
  "Incumbent Spouse Runs", "Incumbent Spouse Vote Share",
  "Other Family Member Runs", "Other Family Member Vote Share"
)

# Utilisez stargazer pour créer le tableau
stargazer(
  models_list,
  type = "text",
  column.labels = names(models_list),
  covariate.labels = c(
    "INT_treatment" = "Treatment",
    "index_empl_svy_0" = "Performance Index",
    "INT_treatment:index_empl_svy_0" = "Treatment:Performance"
  ),
  add.lines = list(
    c("Observations", sapply(models_list, function(x) nobs(x))),
    c("Mean in Control without GQ", unlist(control_means))
  ),
  digits = 2,
  title = "Table 2: Performance",
  out = "Table2_Performance_2010_completed.txt"
)


