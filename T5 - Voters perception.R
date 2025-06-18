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


### DATA 

# Charger les bibliothèques nécessaires
library(haven)
library(dplyr)
library(tidyr)

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




#Transformation des données de wide à long
merged_data_long <- merged_data %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      D_NREGA_work_m, D_NREGA_work_f, starts_with("E_know"),
      starts_with("E_rate"), starts_with("F_rank"),
      starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c("variable", "gender"),
    values_to = "value",
    names_pattern = "([^_]+)_([^_]+)"
  ) %>%
  mutate(value = ifelse(variable == "A_age" & value == 0, NA, value))

# Afficher les premières lignes du jeu de données transformé
head(merged_data_long)

colnames(performance_data)
colnames(electoral_data)
colnames(household_data)
colnames(merged_data)

colnames(merged_data_long)




## new variables

merged_data_long <- merged_data_long %>%
  mutate(
    C_I_AgeBelow25 = ifelse(A_age < 25, 1, 0),
    C_I_Age2535 = ifelse(A_age >= 25 & A_age < 35, 1, 0),
    C_I_Age3545 = ifelse(A_age >= 35 & A_age < 45, 1, 0),
    C_I_AgeAbove45 = ifelse(A_age >= 45 & !is.na(A_age), 1, 0),
    C_I_Female = ifelse(gender == "_f", 1, 0),
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
  )


## marche pas donc autre essai

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
    names_to = "variable",
    values_to = "value",
    names_pattern = "([^_]+)_(.*)"
  ) %>%
  mutate(
    A_age = ifelse(variable == "A_age", value, NA),
    gender = ifelse(variable == "gender", as.character(value), NA),
    A_literacy = ifelse(variable == "A_literacy", value, NA),
    A_educ = ifelse(variable == "A_educ", value, NA)
  ) %>%
  group_by(TEMP_id) %>%
  mutate(
    C_I_AgeBelow25 = ifelse(A_age < 25, 1, 0),
    C_I_Age2535 = ifelse(A_age >= 25 & A_age < 35, 1, 0),
    C_I_Age3545 = ifelse(A_age >= 35 & A_age < 45, 1, 0),
    C_I_AgeAbove45 = ifelse(A_age >= 45 & !is.na(A_age), 1, 0),
    C_I_Female = ifelse(gender == "_f", 1, 0),
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
  )




# controls

# normalisation of variables



## regression 1
## regression 2
## regression 3


## Output file













