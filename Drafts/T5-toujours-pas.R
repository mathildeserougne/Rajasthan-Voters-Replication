## REPLICATION - TABLE 5: KNOWLEDGE AND PERCEPTION OF THE NREGA ################
################################################################################

# Installer les packages nécessaires
install.packages(c("foreign","dplyr","tidyr","lmtest","sandwich","lfe","haven"))


# Bibliothèques
library(foreign) # Pour lire les fichiers .dta
library(dplyr) # Pour la manipulation des données
library(tidyr) # Pour le remodelage des données
library(lmtest) # Pour les régressions
library(sandwich) # Pour les erreurs standard robustes
library(lfe) # Pour les effets fixes
library(haven)

# extraction des données en utilisant haven
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")
household_data <- read_dta("~/work/Household survey data cleaned.dta")

# preparation des donnees

# fusion des données
merged_data <- household_data %>%
  inner_join(electoral_data, by = c("district", "ps", "gp"))



# moment où je dois reformatter des variables qui sont genrées...

long_data <- merged_data %>%
  pivot_longer(
    cols = c(A_age_m, A_age_f, A_literacy_m, A_literacy_f, A_educ_m, A_educ_f, D_NREGA_work_m, D_NREGA_work_f),
    names_to = c("variable", "gender"),
    names_pattern = "(.*)_(.)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    names_repair = "universal"
  )

# nouvelles variables
long_data <- long_data %>%
  mutate(
    C_I_AgeBelow25 = ifelse(A_age < 25, 1, 0),
    C_I_Age2535 = ifelse(A_age >= 25 & A_age < 35, 1, 0),
    C_I_Age3545 = ifelse(A_age >= 35 & A_age < 45, 1, 0),
    C_I_AgeAbove45 = ifelse(A_age >= 45 & !is.na(A_age), 1, 0),
    C_I_Female = ifelse(gender == "f", 1, 0),
    C_I_Literate = ifelse(A_literacy == 4, 1, 0),
    C_I_EducNone = ifelse(A_educ == 0, 1, 0),
    C_I_EducPrimary = ifelse(A_educ > 0 & A_educ <= 5, 1, 0),
    C_I_EducLowerSec = ifelse(A_educ > 5 & A_educ <= 9, 1, 0),
    C_I_EducUpperSec = ifelse(A_educ > 9 & A_educ <= 12, 1, 0),
    C_I_EducTertiary = ifelse(A_educ > 12 & !is.na(A_educ), 1, 0),
    C_I_Missing = ifelse(is.na(A_age) | is.na(A_educ) | is.na(A_literacy), 1, 0),
    C_H_bpl = ifelse(H_bpl == 1, 1, 0),
    C_H_ownland = ifelse(H_ownland == 1, 1, 0),
    C_H_hindu = ifelse(H_religion == 1, 1, 0),
    C_H_CasteGen = ifelse(H_caste == 1 | H_caste == 5, 1, 0),
    C_H_CasteOBC = ifelse(H_caste == 2 | H_caste == 6, 1, 0),
    C_H_CasteSC = ifelse(H_caste == 3, 1, 0),
    C_H_CasteST = ifelse(H_caste == 4, 1, 0),
    C_H_Missing = ifelse(is.na(H_bpl) | is.na(H_ownland) | is.na(H_religion) | is.na(H_caste), 1, 0)
  )



# Standardiser certaines variables
# Standardiser certaines variables
long_data <- long_data %>%
  group_by(`INT_treatment.x`, `RES05_gender.y`) %>%
  mutate(across(c(E_know_minimumwage_m, E_know_maximumdays_m, E_know_sarpanchrole_projects_m,
                  E_know_sarpanchrole_jobcard_m, E_know_sarpanchrole_work_m, E_know_jobcardapplication_m,
                  E_know_waitingdays_m, E_know_unemploymentallowance_m, E_know_postofficepay_m,
                  E_rate_NREGAimplementation_m, E_rate_NREGAimplementation_g_m, E_rate_NREGAimplementation_vg_m,
                  E_rate_sarpanchperformance_m, E_rate_sarpanchperformance_g_m, E_rate_sarpanchperformance_vg_m,
                  E_know_minimumwage_f, E_know_maximumdays_f, E_know_sarpanchrole_projects_f,
                  E_know_sarpanchrole_jobcard_f, E_know_sarpanchrole_work_f, E_know_jobcardapplication_f,
                  E_know_waitingdays_f, E_know_unemploymentallowance_f, E_know_postofficepay_f,
                  E_rate_NREGAimplementation_f, E_rate_NREGAimplementation_g_f, E_rate_NREGAimplementation_vg_f,
                  E_rate_sarpanchperformance_f, E_rate_sarpanchperformance_g_f, E_rate_sarpanchperformance_vg_f),
                ~ ifelse(`INT_treatment.x` == 0 & `RES05_gender.y` == 0, scale(.x), .x))) %>%
  ungroup()



# Création des variables d'index
long_data <- long_data %>%
  mutate(
    E_know_nregarules = rowMeans(select(., starts_with("E_know_")), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., E_know_sarpanchrole_projects_m, E_know_sarpanchrole_jobcard_m, E_know_sarpanchrole_work_m,
                                          E_know_sarpanchrole_projects_f, E_know_sarpanchrole_jobcard_f, E_know_sarpanchrole_work_f), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., E_rate_NREGAimplementation_m, E_rate_sarpanchperformance_m,
                                   E_rate_NREGAimplementation_f, E_rate_sarpanchperformance_f), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., F_rate_publicgoods_road_m, F_rate_publicgoods_pump_m, F_rate_publicgoods_school_m,
                                         F_rate_publicgoods_road_f, F_rate_publicgoods_pump_f, F_rate_publicgoods_school_f), na.rm = TRUE)
  )


# Exemple de création de TEMP_index et d'autres variables nécessaires
long_data <- long_data %>%
  mutate(
    TEMP_index = index_empl_pre_svysample, # Assurez-vous que cette variable existe dans votre jeu de données
    TEMP_X_res_index = RES05_gender.y * TEMP_index,
    TEMP_X_anytr_index = `INT_treatment.x` * TEMP_index,
    TEMP_X_grltr_index = `INT_treatment_general.x` * TEMP_index,
    TEMP_X_gndtr_index = `INT_treatment_gender.x` * TEMP_index,
    TEMP_X_anytr_res_index = `INT_treatment.x` * `RES05_gender.y` * TEMP_index,
    TEMP_X_grltr_res_index = `INT_treatment_general.x` * `RES05_gender.y` * TEMP_index,
    TEMP_X_gndtr_res_index = `INT_treatment_gender.x` * `RES05_gender.y` * TEMP_index
  )



library(lfe)


### CA MARCHE PAS

# Vérifiez les variables
summary(long_data[, c("E_know_nregarules", "TEMP_X_res_index", "district", "ID_gp_no")])

# Essayez une régression simple sans effets fixes
simple_regression <- lm(E_know_nregarules ~ TEMP_X_res_index, data = long_data)
summary(simple_regression)

# Si la régression simple fonctionne, essayez d'ajouter les effets fixes
regression_results <- try(felm(E_know_nregarules ~ TEMP_X_res_index | ID_gp_no, data = long_data))
if (class(regression_results) == "try-error") {
  print("Une erreur s'est produite avec les effets fixes.")
} else {
  summary(regression_results)
}

###

## MARCHE PAS 

# Vérifiez les valeurs manquantes
summary(long_data[, c("E_know_nregarules", "TEMP_X_res_index", "ID_gp_no")])

# Vérifiez la variabilité des variables
table(long_data$ID_gp_no)

# Essayez un sous-ensemble de données
subset_data <- long_data[1:1000, ] # Utilisez un sous-ensemble de données
regression_results <- try(felm(E_know_nregarules ~ TEMP_X_res_index | ID_gp_no, data = subset_data))
if (class(regression_results) == "try-error") {
  print("Une erreur s'est produite avec les effets fixes.")
} else {
  summary(regression_results)
}

####


###

# Vérifiez la colinéarité
cor(subset_data[, c("E_know_nregarules", "TEMP_X_res_index")], use = "complete.obs")

# Essayez un modèle simplifié sans effets fixes
simple_regression <- lm(E_know_nregarules ~ TEMP_X_res_index, data = subset_data)
summary(simple_regression)

# Si le modèle simplifié fonctionne, essayez d'ajouter les effets fixes avec un sous-ensemble plus petit
small_subset_data <- subset_data[1:500, ] # Utilisez un sous-ensemble plus petit
regression_results <- try(felm(E_know_nregarules ~ TEMP_X_res_index | ID_gp_no, data = small_subset_data))
if (class(regression_results) == "try-error") {
  print("Une erreur s'est produite avec les effets fixes.")
} else {
  summary(regression_results)
}

