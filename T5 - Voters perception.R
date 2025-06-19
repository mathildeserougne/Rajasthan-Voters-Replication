## REPLICATION - TABLE 5: KNOWLEDGE AND PERCEPTION OF THE NREGA ################
################################################################################

### LIBRARIES

# Un-comment the packages installation if necessary
install.packages("haven")
install.packages("dplyr")
install.packages("tidyverse")

# Required libraries
library(haven)
library(dplyr)
library(tidyverse)



### PATH

path <- "~/work"

### MACROS GLOBALES

# Global variables for controls and output variables
gpcontrols <- "GP_population GP_lit GP_sc GP_st GP_nbvillages RES00_gender RES00_obc RES00_sc RES00_st RES10_obc RES10_sc RES10_st RES05_obc RES05_sc RES05_st"
gpcontrols15 <- paste(gpcontrols, "RES15_obc RES15_sc RES15_st")

outregvar0 <- "RES05_gender_control X_anytr_nogenderres05 X_anytr_genderres05"
outregvar1 <- "RES05_gender INT_treatment"
outregvar2 <- "INT_treatment RES05_gender X_anytr_genderres05"
outregvar3 <- "INT_treatment_gender INT_treatment_general RES05_gender"
outregvar4 <- "INT_treatment_gender INT_treatment_general RES05_gender X_generaltr_genderres05 X_gendertr_genderres05"
outregvar5 <- "INT_treatment RES05_gender X_anytr_genderres05 INC05_can_run X_anytr_inc_can X_inc_can_genderres05 X_anytr_inc_can_genderres05"

# Variables for indices
TEMP_index <- "TEMP_index"
TEMP_X_res_index <- "TEMP_X_res_index"
TEMP_X_anytr_index <- "TEMP_X_anytr_index"
TEMP_X_anytr_res_index <- "TEMP_X_anytr_res_index"
TEMP_X_gender_index <- "TEMP_X_gender_index"
TEMP_X_general_index <- "TEMP_X_general_index"
TEMP_X_gender_res_index <- "TEMP_X_gender_res_index"
TEMP_X_general_res_index <- "TEMP_X_general_res_index"

outregvarindex1 <- paste(TEMP_X_res_index, TEMP_X_anytr_index, outregvar1, TEMP_index)
outregvarindex2 <- paste(TEMP_X_anytr_index, TEMP_X_anytr_res_index, TEMP_X_res_index, outregvar2, TEMP_index)
outregvarindex3 <- paste(TEMP_X_gender_index, TEMP_X_general_index, TEMP_X_gender_res_index, TEMP_X_general_res_index, TEMP_X_res_index, outregvar4, TEMP_index)




## version du code du 19/06


# Chemins des fichiers
electoral_data_path <- "~/work/Electoral data cleaned.dta"
household_data_path <- "~/work/Household survey data cleaned.dta"
performance_data_path <- "~/work/Performance Indexes.dta"

# Charger les données électorales
electoral_data <- read_dta(electoral_data_path)

# Définir les variables à conserver
indices <- "index_empl_pre_svysample"
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

# Conserver uniquement les variables spécifiées
electoral_data_filtered <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

# Sauvegarder le jeu de données filtré dans un fichier temporaire
temp_file <- tempfile(fileext = ".dta")
write_dta(electoral_data_filtered, temp_file)

# Charger les données des ménages
household_data <- read_dta(household_data_path)

# Charger les données de performance
performance_data <- read_dta(performance_data_path)

# Fusionner les données des ménages avec les données de performance
household_data <- household_data %>%
  arrange(district, ps, gp) %>%
  inner_join(performance_data, by = c("district", "ps", "gp"))

# Fusionner les données avec les données électorales filtrées
merged_data <- household_data %>%
  inner_join(read_dta(temp_file), by = c("district", "ps", "gp"))

# Transformation des données de wide à long
merged_data_long <- merged_data %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      D_NREGA_work_m, D_NREGA_work_f, starts_with("E_know"),
      starts_with("E_rate"), starts_with("F_rank"),
      starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c("var_prefix", "gender"),
    values_to = "value",
    names_pattern = "([^_]+)_([^_]+)"
  ) %>%
  mutate(
    A_age = as.numeric(ifelse(var_prefix == "A_age", value, NA)),
    A_educ = as.numeric(ifelse(var_prefix == "A_educ", value, NA)),
    A_literacy = as.numeric(ifelse(var_prefix == "A_literacy", value, NA))
  ) %>%
  group_by(TEMP_id) %>%
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
    C_I_Missing = as.integer(is.na(A_age) | is.na(A_educ) | is.na(A_literacy)),
    C_H_bpl = ifelse(H_bpl == 1, 1, 0),
    C_H_ownland = ifelse(H_ownland == 1, 1, 0),
    C_H_hindu = ifelse(H_religion == 1, 1, 0),
    C_H_CasteGen = ifelse(H_caste == 1 | H_caste == 5, 1, 0),
    C_H_CasteOBC = ifelse(H_caste == 2 | H_caste == 6, 1, 0),
    C_H_CasteSC = ifelse(H_caste == 3, 1, 0),
    C_H_CasteST = ifelse(H_caste == 4, 1, 0),
    C_H_Missing = as.integer(is.na(H_bpl) | is.na(H_ownland) | is.na(H_religion) | is.na(H_caste))
  ) %>%
  ungroup()

# Afficher les premières lignes du jeu de données transformé
head(merged_data_long)





## passons à la suite; standardisation des variables

# Liste des variables à standardiser
vars_to_standardize <- c(
  grep("^E_know_", colnames(merged_data_long), value = TRUE),
  grep("^F_rate_", colnames(merged_data_long), value = TRUE),
  grep("^E_rate", colnames(merged_data_long), value = TRUE)
)

# Standardisation des variables pour le sous-ensemble spécifié
merged_data_long <- merged_data_long %>%
  group_by(TEMP_id) %>%
  mutate(
    across(
      all_of(vars_to_standardize),
      ~ ifelse(
        INT_treatment == 0 & RES05_gender == 0,
        (`_` - mean(`_`[INT_treatment == 0 & RES05_gender == 0], na.rm = TRUE)) /
          sd(`_`[INT_treatment == 0 & RES05_gender == 0], na.rm = TRUE),
        `_`
      )
    )
  ) %>%
  ungroup()


## jusqu'ici ça fonctionne


# ce bloc non: 
# Calcul des moyennes de lignes pour créer de nouvelles variables
merged_data_long <- merged_data_long %>%
  mutate(
    E_know_nregarules = rowMeans(select(., E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., F_rate_publicgoods_road, F_rate_publicgoods_pump, F_rate_publicgoods_school), na.rm = TRUE)
  )


# tentatives de m'en sortir sinon je vais mourir

print(colnames(merged_data_long))









# controls

# normalisation of variables



## regression 1
## regression 2
## regression 3


## Output file













