### REPLICATION - TABLE 3 - CANDIDATE CHARACTERISTICS ###############


# ok that was so bad it's actually embarrassing
# do not look any further if opened 





# PACKAGES (si pas déjà installés)
library(tidyverse)
library(fixest)
library(modelsummary)
library(gt)
library(car)
library(haven)

# VARIABLES -------------------------------

# Chemin vers les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtres comme dans le code Stata
data_filtered_t3 <- data %>%
  filter(RES10_gender == 0, GP_tag == 1)  # Ne pas filtrer INC05_can_run ici !

# Variables dépendantes de la première boucle Stata
dep_vars_t3_partA <- c("ELEC10_nbcands", "CHAL_nbchal", 
                       "CHAL_prop_female", "CHAL_voteshare_female",
                       "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# GP controls (identique à Table 1)
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", 
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", 
                "RES10_obc", "RES10_sc", "RES10_st", 
                "RES05_obc", "RES05_sc", "RES05_st")

# Fonction de formule pour modèle "any treatment"
create_formula_t3 <- function(dep_var) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  formula_str <- paste(dep_var, "~ INT_treatment + RES05_gender + X_anytr_genderres05 +",
                       base_controls, "+ factor(district)")
  as.formula(formula_str)
}

# Fonction de test: RES05_gender + X_anytr_genderres05 = 0
calculate_t3_test <- function(model) {
  test <- car::linearHypothesis(model, "RES05_gender + X_anytr_genderres05 = 0")
  pval <- round(test$`Pr(>F)`[2], 2)
  return(pval)
}

# Boucle d'estimation
models_t3_partA <- list()
control_means_t3 <- list()
test_results_t3 <- list()

for (i in seq_along(dep_vars_t3_partA)) {
  dep_var <- dep_vars_t3_partA[i]
  
  # moyenne dans le groupe de contrôle
  mean_control <- data_filtered_t3 %>%
    filter(INT_treatment == 0, RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean) %>%
    round(2)
  
  control_means_t3[[i]] <- mean_control
  
  # estimation
  model_formula <- create_formula_t3(dep_var)
  model <- lm(model_formula, data = data_filtered_t3)
  models_t3_partA[[i]] <- model
  
  # test d'interaction
  test_results_t3[[i]] <- calculate_t3_test(model)
}

# Création de la table (version simple)
table3_partA <- modelsummary(models_t3_partA,
                             output = "gt",
                             statistic = "std.error",
                             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                             coef_omit = paste(gpcontrols, collapse = "|"),
                             gof_omit = ".*") %>%
  tab_header(
    title = "Table 3 - Candidate Characteristics (2010)",
    subtitle = "Part A: All GPs (including with incumbents)"
  ) %>%
  tab_footnote(
    footnote = "Significance: *** p<0.01, ** p<0.05, * p<0.1",
    locations = cells_title("subtitle")
  )

# Afficher
table3_partA

# Sauvegarde
gtsave(table3_partA, "~/work/Table3_PartA.html")
