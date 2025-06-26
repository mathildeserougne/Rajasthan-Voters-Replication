# packages
library(tidyverse)
library(haven)
library(data.table)
library(fixest)
library(broom)

## premier fichier

# Charger les données électorales nettoyées
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")

# Variables à garder
gpcontrols <- c("GP_population", "ELEC10_electorate_total", "ELEC10_electorate_total_missing")  # remplacer par vos contrôles GP exacts
indices <- c("index_empl_pre_svysample")

# Garder uniquement les variables nécessaires
electoral_data <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

# Sauvegarde temporaire
temp <- electoral_data


## survey and merge

hh_data <- read_dta("~/work/Household survey data cleaned.dta")

merged <- hh_data %>%
  left_join(temp, by = c("district", "ps", "gp")) %>%
  filter(!is.na(index_empl_pre_svysample))  # drop if _m==2


## reshape

merged <- merged %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("A_age") | starts_with("A_educ") | starts_with("A_literacy") |
      starts_with("D_NREGA_work") | starts_with("E_know") | starts_with("E_rate") |
      starts_with("F_rank") | starts_with("F_rate_publicgoods") | starts_with("F_optimistic"),
    names_to = c(".value", "gender"),
    names_pattern = "(.*)_([mf])"
  )

## indicatrices individuelles
merged <- merged %>%
  mutate(
    C_I_AgeBelow25 = A_age < 25,
    C_I_Age2535 = A_age >= 25 & A_age < 35,
    C_I_Age3545 = A_age >= 35 & A_age < 45,
    C_I_AgeAbove45 = A_age >= 45 & !is.na(A_age),
    C_I_Female = gender == "f",
    C_I_Literate = A_literacy == 4,
    C_I_EducNone = A_educ == 0,
    C_I_EducPrimary = A_educ > 0 & A_educ <= 5,
    C_I_EducLowerSec = A_educ > 5 & A_educ <= 9,
    C_I_EducUpperSec = A_educ > 9 & A_educ <= 12,
    C_I_EducTertiary = A_educ > 12 & !is.na(A_educ),
    C_I_Missing = if_else(is.na(A_age) | is.na(A_educ) | is.na(A_literacy), 1, 0)
  )


## indicatrices ménage

merged <- merged %>%
  mutate(
    C_H_bpl = H_bpl == 1,
    C_H_ownland = H_ownland == 1,
    C_H_hindu = H_religion == 1,
    C_H_CasteGen = H_caste %in% c(1, 5),
    C_H_CasteOBC = H_caste %in% c(2, 6),
    C_H_CasteSC = H_caste == 3,
    C_H_CasteST = H_caste == 4,
    C_H_Missing = if_else(is.na(H_bpl) | is.na(H_ownland) | is.na(H_religion) | is.na(H_caste), 1, 0)
  )

## normalisation

to_scale <- grep("^(E_know|F_rate|E_rate)", names(merged), value = TRUE)
merged <- merged %>%
  mutate(across(all_of(to_scale), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "{.col}_scaled"))



## composites

merged <- merged %>%
  rowwise() %>%
  mutate(
    E_know_nregarules = mean(c_across(c(E_know_minimumwage, E_know_maximumdays)), na.rm = TRUE),
    E_know_sarpanchrole = mean(c_across(starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = mean(c(E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = mean(c(F_rate_publicgoods_road, F_rate_publicgoods_pump, F_rate_publicgoods_school), na.rm = TRUE)
  ) %>%
  ungroup()



## interactions à créer

merged <- merged %>%
  mutate(
    TEMP_index = index_empl_pre_svysample,
    TEMP_X_res_index = RES05_gender * index_empl_pre_svysample,
    TEMP_X_anytr_index = INT_treatment * index_empl_pre_svysample,
    TEMP_X_grltr_index = INT_treatment_general * index_empl_pre_svysample,
    TEMP_X_gndtr_index = INT_treatment_gender * index_empl_pre_svysample,
    TEMP_X_anytr_res_index = INT_treatment * RES05_gender * index_empl_pre_svysample,
    TEMP_X_grltr_res_index = INT_treatment_general * RES05_gender * index_empl_pre_svysample,
    TEMP_X_gndtr_res_index = INT_treatment_gender * RES05_gender * index_empl_pre_svysample
  )


## boucles de régression


merged <- merged %>%
  rename(GP_population = GP_population.x) %>%
  select(-GP_population.y)

merged <- merged %>%
  rename(ELEC10_electorate_total = ELEC10_electorate_total.x) %>%
  select(-ELEC10_electorate_total.y)

merged <- merged %>%
  rename(ELEC10_electorate_total_missing = ELEC10_electorate_total_missing.x) %>%
  select(-ELEC10_electorate_total_missing.y)


outcomes <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")
results <- list()


## VERSION 1 DES REGRESSIONS ###################################################

for (i in seq_along(outcomes)) {
  dep <- outcomes[i]
  
  control_mean1 <- mean(merged[[dep]][merged$INT_treatment == 0 & merged$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(merged[[dep]][merged$INT_treatment == 0 & merged$RES05_gender == 1], na.rm = TRUE)
  
  f <- reformulate(
    c("TEMP_index", gpcontrols,
      grep("^C_I_", names(merged), value = TRUE),
      grep("^C_H_", names(merged), value = TRUE),
      "factor(district)"),
    response = dep
  )
  
  model <- feols(f, data = merged, cluster = "ID_gp_no")
  results[[i]] <- tidy(model) %>%
    filter(term == "TEMP_index") %>%
    mutate(dep_var = dep,
           control_mean_WR_0 = control_mean1,
           control_mean_WR_1 = control_mean2)
}




# avec affichage?

for (i in seq_along(outcomes)) {
  dep <- outcomes[i]
  
  control_mean1 <- mean(merged[[dep]][merged$INT_treatment == 0 & merged$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(merged[[dep]][merged$INT_treatment == 0 & merged$RES05_gender == 1], na.rm = TRUE)
  
  f <- reformulate(
    c("TEMP_index", gpcontrols,
      grep("^C_I_", names(merged), value = TRUE),
      grep("^C_H_", names(merged), value = TRUE),
      "factor(district)"),
    response = dep
  )
  
  model <- feols(f, data = merged, cluster = "ID_gp_no")
  
  # Affiche le résumé de la régression dans la console
  print(summary(model))
  
  results[[i]] <- tidy(model) %>%
    filter(term == "TEMP_index") %>%
    mutate(dep_var = dep,
           control_mean_WR_0 = control_mean1,
           control_mean_WR_1 = control_mean2)
}

##############################################################################


# VERSION 2 DES REGRESSIONS

# Assurez-vous que les variables de contrôle sont bien définies
gpcontrols <- c("GP_population", "ELEC10_electorate_total", "ELEC10_electorate_total_missing")
indcontrols <- grep("^C_I_", names(merged), value = TRUE)
hhcontrols <- grep("^C_H_", names(merged), value = TRUE)

# Liste des variables dépendantes
outcomes <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Initialiser une liste pour stocker les résultats
results <- list()

# Boucle sur les variables dépendantes
for (dep_var in outcomes) {
  # Calculer les moyennes des groupes de contrôle
  control_mean1 <- merged %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_mean2 <- merged %>%
    filter(INT_treatment == 0 & RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  # Exécuter la régression
  regression <- feols(
    reformulate(termlabels = c("TEMP_index", gpcontrols, indcontrols, hhcontrols, "district"),
                response = dep_var),
    data = merged,
    cluster = ~ID_gp_no
  )
  
  # Stocker les résultats
  results[[dep_var]] <- list(
    model = tidy(regression),
    control_mean1 = control_mean1,
    control_mean2 = control_mean2
  )
}

# Afficher les résultats
for (dep_var in outcomes) {
  cat("Results for", dep_var, "\n")
  print(results[[dep_var]]$model)
  cat("Mean in Control not WR in 2005:", results[[dep_var]]$control_mean1, "\n")
  cat("Mean in Control WR in 2005:", results[[dep_var]]$control_mean2, "\n\n")
}




###############################################################################

# VERSION 3 DES REGRESSIONS PARCE QUE RIEN NE FONCTIONNE





library(tidyverse)
library(fixest)

# Assurez-vous que les variables de contrôle sont bien définies
gpcontrols <- c("GP_population", "ELEC10_electorate_total", "ELEC10_electorate_total_missing")
indcontrols <- grep("^C_I_", names(merged), value = TRUE)
hhcontrols <- grep("^C_H_", names(merged), value = TRUE)

# Liste des variables dépendantes
outcomes <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Initialiser une liste pour stocker les résultats
results <- list()

# Boucle sur les variables dépendantes
for (dep_var in outcomes) {
  # Calculer les moyennes des groupes de contrôle
  control_mean1 <- merged %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_mean2 <- merged %>%
    filter(INT_treatment == 0 & RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  # Exécuter la régression
  regression <- feols(
    as.formula(paste(dep_var, "~ TEMP_X_anytr_index + TEMP_X_anytr_res_index + TEMP_X_res_index + INT_treatment + RES05_gender + X_anytr_genderres05 + TEMP_index +",
                     paste(c(gpcontrols, indcontrols, hhcontrols), collapse = " + "),
                     "+ factor(district)")),
    data = merged,
    cluster = ~ID_gp_no
  )
  
  # Stocker les résultats
  results[[dep_var]] <- list(
    model = summary(regression),
    control_mean1 = control_mean1,
    control_mean2 = control_mean2
  )
}

# Afficher les résultats
for (dep_var in outcomes) {
  cat("Results for", dep_var, "\n")
  print(results[[dep_var]]$model)
  cat("Mean in Control not WR in 2005:", results[[dep_var]]$control_mean1, "\n")
  cat("Mean in Control WR in 2005:", results[[dep_var]]$control_mean2, "\n\n")
}


##############################################################################
# VERSION 4


library(fixest)

# Assurez-vous que les variables de contrôle sont bien définies
gpcontrols <- c("GP_population", "ELEC10_electorate_total", "ELEC10_electorate_total_missing")
indcontrols <- grep("^C_I_", names(merged), value = TRUE)
hhcontrols <- grep("^C_H_", names(merged), value = TRUE)

# Liste des variables dépendantes
outcomes <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Initialiser une liste pour stocker les résultats
results <- list()

# Boucle sur les variables dépendantes
for (dep_var in outcomes) {
  # Calculer les moyennes des groupes de contrôle
  control_mean1 <- mean(merged$`{{dep_var}}`[merged$INT_treatment == 0 & merged$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(merged$`{{dep_var}}`[merged$INT_treatment == 0 & merged$RES05_gender == 1], na.rm = TRUE)
  
  # Exécuter la régression
  regression <- feols(
    as.formula(paste(dep_var, "~ TEMP_X_anytr_index + TEMP_X_anytr_res_index + TEMP_X_res_index + INT_treatment + RES05_gender + X_anytr_genderres05 + TEMP_index +",
                     paste(c(gpcontrols, indcontrols, hhcontrols), collapse = " + "),
                     "+ factor(district)")),
    data = merged,
    cluster = ~ID_gp_no
  )
  
  # Stocker les résultats
  results[[dep_var]] <- list(
    model = summary(regression),
    control_mean1 = control_mean1,
    control_mean2 = control_mean2
  )
}

# Afficher les résultats
for (dep_var in outcomes) {
  cat("Results for", dep_var, "\n")
  print(results[[dep_var]]$model)
  cat("Mean in Control not WR in 2005:", results[[dep_var]]$control_mean1, "\n")
  cat("Mean in Control WR in 2005:", results[[dep_var]]$control_mean2, "\n\n")
}








