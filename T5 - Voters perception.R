## REPLICATION - TABLE 5: KNOWLEDGE AND PERCEPTION OF THE NREGA ################
################################################################################

# Installer les packages nécessaires
install.packages(c("dplyr", "tidyr", "fixest", "stargazer", "haven"))

# Charger les bibliothèques
library(dplyr)
library(tidyr)
library(fixest)
library(stargazer)
library(haven)

# Charger les données
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")
household_data <- read_dta("~/work/Household survey data cleaned.dta")

# Vérifier les données chargées
head(electoral_data)
head(household_data)




# Nettoyer les données électorales
electoral_data_clean <- electoral_data %>%
  select(district, ps, gp, GP_population, GP_lit, GP_sc, GP_st, GP_nbvillages,
         RES00_gender, RES00_obc, RES00_sc, RES00_st, RES10_obc, RES10_sc, RES10_st,
         RES05_obc, RES05_sc, RES05_st, index_empl_pre_svysample, starts_with("std_HH_NREGA"))

# Fusionner les données
merged_data <- household_data %>%
  inner_join(electoral_data_clean, by = c("district", "ps", "gp")) %>%
  filter(!duplicated(paste(district, ps, gp)))

# Vérifier les données fusionnées
head(merged_data)

# relationship normale on va dire
merged_data <- household_data %>%
  inner_join(electoral_data_clean, by = c("district", "ps", "gp"), relationship = "many-to-many") %>%
  filter(!duplicated(paste(district, ps, gp)))




## individual transformation

# Sélectionner les colonnes nécessaires
individual_data <- merged_data %>%
  select(district, ps, gp, ID_gp_no, starts_with("A_"), starts_with("D_"), starts_with("E_"),
         starts_with("F_"), starts_with("H_"), starts_with("GP_"), starts_with("ELEC10_"),
         starts_with("RES"), starts_with("INT_"), starts_with("X_"), starts_with("index_"),
         starts_with("std_HH_NREGA"))

# Ajouter un identifiant temporaire
individual_data <- individual_data %>%
  mutate(TEMP_id = row_number())

# Transformer les données en format long
individual_data_long <- individual_data %>%
  pivot_longer(cols = c(starts_with("A_"), starts_with("D_"), starts_with("E_"), starts_with("F_")),
               names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("prefix", "suffix"), sep = "_", remove = FALSE) %>%
  pivot_wider(names_from = suffix, values_from = value)

# Vérifier les données transformées
head(individual_data_long)





## composite variables

# Créer les variables composites
individual_data_long <- individual_data_long %>%
  mutate(
    E_know_nregarules = rowMeans(select(., starts_with("E_know_minimumwage"), starts_with("E_know_maximumdays")), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., starts_with("E_rate_NREGAimplementation"), starts_with("E_rate_sarpanchperformance")), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., starts_with("F_rate_publicgoods_road"), starts_with("F_rate_publicgoods_pump"), starts_with("F_rate_publicgoods_school")), na.rm = TRUE)
  )

# Vérifier les variables composites
head(individual_data_long)





## doing the regressions

# Définir les variables de contrôle
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
indcontrols <- grep("C_I_", names(individual_data_long), value = TRUE)
hhcontrols <- grep("C_H_", names(individual_data_long), value = TRUE)

# Définir les variables dépendantes
dep_vars <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Exécuter les régressions
regression_results <- list()
for (dep_var in dep_vars) {
  # Calculer les moyennes de contrôle
  control_mean1 <- individual_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_mean2 <- individual_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 1) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  # Exécuter la régression
  regression <- feols(
    reformulate(termlabels = c("index_empl_pre_svysample", gpcontrols, indcontrols, hhcontrols, "district"), response = dep_var),
    data = individual_data_long,
    cluster = ~ID_gp_no
  )
  
  # Stocker les résultats
  regression_results[[dep_var]] <- list(
    regression = regression,
    control_mean1 = control_mean1,
    control_mean2 = control_mean2
  )
}

# Vérifier les résultats des régressions
str(regression_results)


