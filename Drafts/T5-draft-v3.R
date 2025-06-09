# Installation des bibliothèques nécessaires
install.packages(c("tidyverse", "haven", "fixest", "stargazer", "glmnet"))

# Chargement des bibliothèques
library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(glmnet)

# Définition des variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# Chargement des données
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")
household_data <- read_dta("~/work/Household survey data cleaned.dta")

# Préparation des données
electoral_data_filtered <- electoral_data %>%
  select(all_of(gpcontrols), district, ps, gp, starts_with("index_empl_pre_svysample"), starts_with("std_HH_NREGA")) %>%
  distinct()

# Sauvegarde temporaire
temp_file <- tempfile()
write_dta(electoral_data_filtered, temp_file)

# Fusion des données
household_data_processed <- household_data %>%
  arrange(district, ps, gp) %>%
  left_join(read_dta(temp_file), by = c("district", "ps", "gp"), suffix = c("", "_y")) %>%
  filter(!is.na(district))

# Suppression du fichier temporaire
unlink(temp_file)

# Transformation des données
# donne un warning mais passons
household_data_long <- household_data_processed %>%
  select(district, ps, gp, ID_gp_no, starts_with("A_age"), starts_with("A_educ"),
         starts_with("A_literacy"), starts_with("D_NREGA_work"), starts_with("E_know"),
         starts_with("E_rate"), starts_with("F_rank"), starts_with("F_rate_publicgoods"),
         starts_with("F_optimistic"), starts_with("H_"), ends_with("GP_population"), ELEC10_electorate_total,
         ELEC10_electorate_total_missing, starts_with("RES"), starts_with("INT_"),
         starts_with("X_"), starts_with("index"), starts_with("GP_"), starts_with("std_HH_NREGA")) %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(cols = c(starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
                        starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
                        starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")),
               names_to = c("prefix", "suffix"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = suffix, values_from = value, values_fn = list(value = first)) %>%
  mutate(gender = ifelse(grepl("female", prefix, ignore.case = TRUE), "_f", "_m"))


# Transformation finale
household_data_long <- household_data_long %>%
  mutate(across(c(age, educ, literacy), ~ ifelse(. == 0, NA, .))) %>%
  mutate(C_I_AgeBelow25 = age < 25,
         C_I_Age2535 = age >= 25 & age < 35,
         C_I_Age3545 = age >= 35 & age < 45,
         C_I_AgeAbove45 = age >= 45 & !is.na(age),
         C_I_Female = gender == "_f",
         C_I_Literate = literacy == 4,
         C_I_EducNone = educ == 0,
         C_I_EducPrimary = educ > 0 & educ <= 5,
         C_I_EducLowerSec = educ > 5 & educ <= 9,
         C_I_EducUpperSec = educ > 9 & educ <= 12,
         C_I_EducTertiary = educ > 12 & !is.na(educ),
         C_I_Missing = if_any(c(age, educ, literacy), is.na),
         C_H_bpl = H_bpl == 1,
         C_H_ownland = H_ownland == 1,
         C_H_hindu = H_religion == 1,
         C_H_CasteGen = H_caste == 1 | H_caste == 5,
         C_H_CasteOBC = H_caste == 2 | H_caste == 6,
         C_H_CasteSC = H_caste == 3,
         C_H_CasteST = H_caste == 4,
         C_H_Missing = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na))



## RESOLUTION DU PROBLEME 

# Solution 1: Vérifier quelles variables existent réellement
print("Variables disponibles dans le dataframe :")
print(names(household_data_long))



# Remplacer votre code de standardisation par :
# non ne faites pas ça
standardize_vars <- c("know", "rate", "rank", "optimistic")

household_data_long <- household_data_long %>%
  group_by(INT_treatment, RES05_gender) %>%
  mutate(across(all_of(standardize_vars), 
                ~ ifelse(INT_treatment == 0 & RES05_gender == 0,
                         as.numeric(scale(., na.rm = TRUE)[,1]), .))) %>%
  ungroup()



# Ou encore plus simple - remplacer les variables originales :
# oui
household_data_long <- household_data_long %>%
  group_by(INT_treatment, RES05_gender) %>%
  mutate(
    know = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((know - mean(know, na.rm = TRUE)) / sd(know, na.rm = TRUE)),
                  know),
    rate = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((rate - mean(rate, na.rm = TRUE)) / sd(rate, na.rm = TRUE)),
                  rate),
    rank = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((rank - mean(rank, na.rm = TRUE)) / sd(rank, na.rm = TRUE)),
                  rank),
    optimistic = ifelse(INT_treatment == 0 & RES05_gender == 0,
                        as.numeric((optimistic - mean(optimistic, na.rm = TRUE)) / sd(optimistic, na.rm = TRUE)),
                        optimistic)
  ) %>%
  ungroup()

print("Standardisation terminée avec succès !")




# Standardisation des variables NE PAS FAIRE CE BLOC
standardize_vars <- c("E_know_minimumwage", "E_know_maximumdays", "E_rate_NREGAimplementation",
                      "E_rate_sarpanchperformance", "F_rate_publicgoods_road", "F_rate_publicgoods_pump",
                      "F_rate_publicgoods_school")

household_data_long <- household_data_long %>%
  group_by(INT_treatment, RES05_gender) %>%
  mutate(across(all_of(standardize_vars), ~ ifelse(INT_treatment == 0 & RES05_gender == 0,
                                                   scale(.)[,"scaled"], .))) %>%
  ungroup()







# Création de nouvelles variables
household_data_long <- household_data_long %>%
  mutate(E_know_nregarules = rowMeans(select(., starts_with("E_know_minimumwage"), starts_with("E_know_maximumdays")), na.rm = TRUE),
         E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole")), na.rm = TRUE),
         E_rate_nrega = rowMeans(select(., starts_with("E_rate_NREGAimplementation"), starts_with("E_rate_sarpanchperformance")), na.rm = TRUE),
         F_rate_publicgoods = rowMeans(select(., starts_with("F_rate_publicgoods")), na.rm = TRUE))

# Définition des variables dépendantes
dep_vars <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Liste pour stocker les résultats
models_list <- list()
control_means <- list()

# Exécution des régressions
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Moyenne de contrôle
  control_mean <- household_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_means[[i]] <- control_mean
  
  # Régression
  formula <- as.formula(paste(dep_var, "~ GP_population +", paste(c(gpcontrols, "C_I_AgeBelow25", "C_I_Age2535",
                                                                    "C_I_Age3545", "C_I_AgeAbove45", "C_I_Female", "C_I_Literate",
                                                                    "C_I_EducNone", "C_I_EducPrimary", "C_I_EducLowerSec",
                                                                    "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                                                                    "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen",
                                                                    "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing"), collapse = " + "), "+ factor(district)"))
  
  model <- feols(formula, data = household_data_long, cluster = ~ID_gp_no)
  models_list[[i]] <- model
}

# Génération de la table de résultats
stargazer(models_list,
          type = "text",
          column.labels = paste("Model", 1:length(dep_vars)),
          keep = "GP_population",
          add.lines = list(
            c("District FE", "Yes", "Yes", "Yes", "Yes"),
            c("Individual Controls", "Yes", "Yes", "Yes", "Yes"),
            c("HH Controls", "Yes", "Yes", "Yes", "Yes"),
            c("GP Controls", "Yes", "Yes", "Yes", "Yes"),
            c("Mean in Control not WR in 2005", control_means)
          ),
          digits = 2,
          title = "Regression Results",
          out = "regression_results.txt")
