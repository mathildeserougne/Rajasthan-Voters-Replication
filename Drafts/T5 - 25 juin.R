### replication tentative 25 juin  ######################################

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

### MACROS

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




# Paths
electoral_data_path <- "~/work/Electoral data cleaned.dta"
household_data_path <- "~/work/Household survey data cleaned.dta"

# Load data
electoral_data <- read_dta(electoral_data_path)

# Variables to keep
indices <- "index_empl_pre_svysample"
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

# Filtering the data
electoral_data_filtered <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

# Save filtered dataframe in a temporary file
temp_file <- tempfile(fileext = ".dta")
write_dta(electoral_data_filtered, temp_file)

# Load household survey data
household_data <- read_dta(household_data_path)

# Merge with filtered electoral data
merged_data <- household_data %>%
  arrange(district, ps, gp) %>%
  inner_join(read_dta(temp_file), by = c("district", "ps", "gp"))



## reshape ##

df <- merged_data %>%
  select(
    district, ps, gp, ID_gp_no,
    starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
    starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
    starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic"),
    H_bpl, H_ownland, H_participates_nrega, H_religion, H_caste,
    ELEC10_electorate_total, ELEC10_electorate_total_missing,
    starts_with("RES00"), starts_with("RES10"), starts_with("RES05"),
    starts_with("INT_"), starts_with("X_"), starts_with("index"),
    starts_with("std_HH_NREGA")
  ) %>%
  mutate(TEMP_id = row_number())




# temporary id
df <- df %>%
  mutate(TEMP_id = row_number())


# VERSION CORRIGEE DE L'UTILISATION DE PIVOT ::
# question of pivot
df_long <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c(".value","gender"),
    names_sep="_(?=[mf]$)"
    
  ) 
# %>%
#   separate(variable, into = c("prefix", "gender"), sep = "_(?=[mf]$)", fill = "right") %>%
#   pivot_wider(
#     names_from = prefix,
#     values_from = value
#   )



# Après avoir utilisé pivot_longer pour transformer les données, supprimer la ligne bizarre non-genrée
df_long <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c(".value", "gender"),
    names_sep = "_(?=[mf]$)"
  )

# Filtrer pour ne garder que les lignes où gender est "m" ou "f"
df_long <- df_long %>%
  filter(gender %in% c("m", "f"))

# Vérifiez les noms de colonnes
print(colnames(df_long))

# Renommer les colonnes si nécessaire
df_long <- df_long %>%
  rename(
    A_age = A_age,
    A_educ = A_educ,
    A_literacy = A_literacy
  )



# préparation des variables indicatrices

library(dplyr)

df <- df_long %>%
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
    C_I_Missing = if_any(c(A_age, A_educ, A_literacy), is.na),
    
    C_H_bpl = H_bpl == 1,
    C_H_ownland = H_ownland == 1,
    C_H_hindu = H_religion == 1,
    C_H_CasteGen = H_caste %in% c(1,5),
    C_H_CasteOBC = H_caste %in% c(2,6),
    C_H_CasteSC = H_caste == 3,
    C_H_CasteST = H_caste == 4,
    C_H_Missing = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na)
  )


# standardisation

vars_std <- grep("^(E_know_|E_rate_)", names(df), value = TRUE)

means_sds <- df %>%
  filter(INT_treatment == 0, RES05_gender == 0) %>%
  summarise(across(all_of(vars_std), list(m = mean, s = sd), na.rm = TRUE))

df <- df %>%
  mutate(across(all_of(vars_std), 
                ~ (.x - means_sds[[paste0(cur_column(), "_m")]]) / means_sds[[paste0(cur_column(), "_s")]] ))



# création des indices composites MAIS c'est au mauvais endroit lol???

df <- df %>%
  rowwise() %>%
  mutate(
    E_know_nregarules = mean(c(E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = mean(c(E_know_sarpanchrole_projects,
                                 E_know_sarpanchrole_jobcard,
                                 E_know_sarpanchrole_work), na.rm = TRUE),
    E_rate_nrega = mean(c(E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = mean(c(F_rate_publicgoods_road,
                                F_rate_publicgoods_pump,
                                F_rate_publicgoods_school), na.rm = TRUE)
  ) %>%
  ungroup()




# génération des variables TEMP

df <- df %>%
  mutate(TEMP_index = index_empl_pre_svysample,
         TEMP_X_res_index = RES05_gender * TEMP_index,
         TEMP_X_anytr_index = INT_treatment * TEMP_index,
         TEMP_X_gndtr_index = INT_treatment_gender * TEMP_index,
         TEMP_X_grltr_index = INT_treatment_general * TEMP_index,
         TEMP_X_anytr_res_index = INT_treatment * RES05_gender * TEMP_index,
         TEMP_X_gndtr_res_index = INT_treatment_gender * RES05_gender * TEMP_index,
         TEMP_X_grltr_res_index = INT_treatment_general * RES05_gender * TEMP_index)





#### SUITE, FAITE LE 25 JUIN

## vérification sur les données

electoral_data <- read_dta(electoral_data_path)
household_data <- read_dta(household_data_path)

# Filtrer et sauvegarder les données électorales
electoral_data_filtered <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

temp_file <- tempfile(fileext = ".dta")
write_dta(electoral_data_filtered, temp_file)

# Fusionner les données
merged_data <- household_data %>%
  arrange(district, ps, gp) %>%
  inner_join(read_dta(temp_file), by = c("district", "ps", "gp"))



## vérification sur transformation des données

df <- merged_data %>%
  select(
    district, ps, gp, ID_gp_no,
    starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
    starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
    starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic"),
    H_bpl, H_ownland, H_participates_nrega, H_religion, H_caste,
    ELEC10_electorate_total, ELEC10_electorate_total_missing,
    starts_with("RES00"), starts_with("RES10"), starts_with("RES05"),
    starts_with("INT_"), starts_with("X_"), starts_with("index"),
    starts_with("std_HH_NREGA")
  ) %>%
  mutate(TEMP_id = row_number())

df_long <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c(".value", "gender"),
    names_sep = "_(?=[mf]$)"
  ) %>%
  filter(gender %in% c("m", "f"))



## spécification des régressions



# correction sur les variables composites?

library(dplyr)

# Assurez-vous que df_long est le DataFrame avec le bon nombre d'observations
print(nrow(df_long))

# Calculer les variables composites dans df_long
df_long <- df_long %>%
  mutate(
    E_know_nregarules = rowMeans(select(., starts_with("E_know_minimumwage"), starts_with("E_know_maximumdays")), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., starts_with("E_rate_NREGAimplementation"), starts_with("E_rate_sarpanchperformance")), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., starts_with("F_rate_publicgoods_")), na.rm = TRUE)
  )

# Vérifiez à nouveau le nombre d'observations pour vous assurer qu'aucune n'a été perdue
print(nrow(df_long))

# Vérifiez les noms de colonnes pour confirmer que les variables ont été ajoutées
print(colnames(df_long))












###############################################################################
################### tentative B - 25 juin #####################################
################################################################################

### LIBRARIES
library(haven)
library(dplyr)
library(tidyverse)

### PATH
path <- "~/work"

### MACROS
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

### Load data
electoral_data_path <- "~/work/Electoral data cleaned.dta"
household_data_path <- "~/work/Household survey data cleaned.dta"

electoral_data <- read_dta(electoral_data_path)
household_data <- read_dta(household_data_path)

### Filtering the data
electoral_data_filtered <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), index_empl_pre_svysample, starts_with("std_HH_NREGA")) %>%
  distinct()

temp_file <- tempfile(fileext = ".dta")
write_dta(electoral_data_filtered, temp_file)

### Merge with filtered electoral data
merged_data <- household_data %>%
  arrange(district, ps, gp) %>%
  inner_join(read_dta(temp_file), by = c("district", "ps", "gp"))

### Reshape and filter
df <- merged_data %>%
  select(
    district, ps, gp, ID_gp_no,
    starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
    starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
    starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic"),
    H_bpl, H_ownland, H_participates_nrega, H_religion, H_caste,
    ELEC10_electorate_total, ELEC10_electorate_total_missing,
    starts_with("RES00"), starts_with("RES10"), starts_with("RES05"),
    starts_with("INT_"), starts_with("X_"), starts_with("index"),
    starts_with("std_HH_NREGA")
  ) %>%
  mutate(TEMP_id = row_number())

df_long <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c(".value", "gender"),
    names_sep = "_(?=[mf]$)"
  ) %>%
  filter(gender %in% c("m", "f"))

### Create indicator variables
df_long <- df_long %>%
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
    C_I_Missing = if_any(c(A_age, A_educ, A_literacy), is.na),
    C_H_bpl = H_bpl == 1,
    C_H_ownland = H_ownland == 1,
    C_H_hindu = H_religion == 1,
    C_H_CasteGen = H_caste %in% c(1, 5),
    C_H_CasteOBC = H_caste %in% c(2, 6),
    C_H_CasteSC = H_caste == 3,
    C_H_CasteST = H_caste == 4,
    C_H_Missing = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na)
  )

### Standardization
vars_std <- grep("^(E_know_|E_rate_)", names(df_long), value = TRUE)
means_sds <- df_long %>%
  filter(INT_treatment == 0, RES05_gender == 0) %>%
  summarise(across(all_of(vars_std), list(m = mean, s = sd), na.rm = TRUE))

df_long <- df_long %>%
  mutate(across(all_of(vars_std),
                ~ (.x - means_sds[[paste0(cur_column(), "_m")]]) / means_sds[[paste0(cur_column(), "_s")]] ))

### Create composite variables
df_long <- df_long %>%
  mutate(
    E_know_nregarules = rowMeans(select(., starts_with("E_know_minimumwage"), starts_with("E_know_maximumdays")), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., starts_with("E_rate_NREGAimplementation"), starts_with("E_rate_sarpanchperformance")), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., starts_with("F_rate_publicgoods_")), na.rm = TRUE)
  )

### Generate TEMP variables
df_long <- df_long %>%
  mutate(
    TEMP_index = index_empl_pre_svysample,
    TEMP_X_res_index = RES05_gender * TEMP_index,
    TEMP_X_anytr_index = INT_treatment * TEMP_index,
    TEMP_X_gndtr_index = INT_treatment_gender * TEMP_index,
    TEMP_X_grltr_index = INT_treatment_general * TEMP_index,
    TEMP_X_anytr_res_index = INT_treatment * RES05_gender * TEMP_index,
    TEMP_X_gndtr_res_index = INT_treatment_gender * RES05_gender * TEMP_index,
    TEMP_X_grltr_res_index = INT_treatment_general * RES05_gender * TEMP_index
  )

### Verify the data
print(nrow(df_long))
print(colnames(df_long))





###############################################################################

###############################################################################


# Charger les bibliothèques nécessaires
library(dplyr)
library(lmtest)
library(sandwich)
library(lfe)

# Supposons que df_long est déjà chargé et préparé

# Liste des variables dépendantes
dep_vars <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Liste des variables de contrôle
gpcontrols <- c("GP_population", "ELEC10_electorate_total", "ELEC10_electorate_total_missing")
indcontrols <- grep("^C_I_", names(df_long), value = TRUE)
hhcontrols <- grep("^C_H_", names(df_long), value = TRUE)

# Ajouter les effets fixes de district
df_long$district_factor <- as.factor(df_long$district)


colnames(electoral_data)

colnames(electoral_data_filtered)
colnames(merged_data)






# Exécuter les régressions
for (dep_var in dep_vars) {
  # Calculer les moyennes de contrôle
  control_mean1 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 1], na.rm = TRUE)
  
  # Formule de régression
  formula <- as.formula(paste(dep_var, "~ TEMP_index +", paste(gpcontrols, collapse = " + "), "+", paste(indcontrols, collapse = " + "), "+", paste(hhcontrols, collapse = " + "), "+ district_factor"))
  
  # Exécuter la régression avec des erreurs standard groupées
  model <- felm(formula, data = df_long, cluster = df_long$ID_gp_no)
  
  # Afficher les résultats
  summary(model)
  
  # Ajouter les moyennes de contrôle et d'autres informations
  cat("Mean in Control not WR in 2005:", control_mean1, "\n")
  cat("Mean in Control WR in 2005:", control_mean2, "\n")
}

# Pour les autres régressions avec différentes variables indépendantes
outregvar2 <- c("TEMP_X_res_index", "TEMP_X_anytr_index", "TEMP_X_grltr_index", "TEMP_X_gndtr_index", "TEMP_X_anytr_res_index", "TEMP_X_grltr_res_index", "TEMP_X_gndtr_res_index")
outregvarindex2 <- grep("TEMP_X", names(df_long), value = TRUE)

for (dep_var in dep_vars) {
  # Calculer les moyennes de contrôle
  control_mean1 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 1], na.rm = TRUE)
  
  # Formule de régression
  formula <- as.formula(paste(dep_var, "~", paste(outregvar2, collapse = " + "), "+", paste(gpcontrols, collapse = " + "), "+", paste(indcontrols, collapse = " + "), "+", paste(hhcontrols, collapse = " + "), "+ district_factor"))
  
  # Exécuter la régression avec des erreurs standard groupées
  model <- felm(formula, data = df_long, cluster = df_long$ID_gp_no)
  
  # Afficher les résultats
  summary(model)
  
  # Ajouter les moyennes de contrôle et d'autres informations
  cat("Mean in Control not WR in 2005:", control_mean1, "\n")
  cat("Mean in Control WR in 2005:", control_mean2, "\n")
}

for (dep_var in dep_vars) {
  # Calculer les moyennes de contrôle
  control_mean1 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(df_long[[dep_var]][df_long$INT_treatment == 0 & df_long$RES05_gender == 1], na.rm = TRUE)
  
  # Formule de régression
  formula <- as.formula(paste(dep_var, "~", paste(outregvarindex2, collapse = " + "), "+", paste(gpcontrols, collapse = " + "), "+", paste(indcontrols, collapse = " + "), "+", paste(hhcontrols, collapse = " + "), "+ district_factor"))
  
  # Exécuter la régression avec des erreurs standard groupées
  model <- felm(formula, data = df_long, cluster = df_long$ID_gp_no)
  
  # Afficher les résultats
  summary(model)
  
  # Ajouter les moyennes de contrôle et d'autres informations
  cat("Mean in Control not WR in 2005:", control_mean1, "\n")
  cat("Mean in Control WR in 2005:", control_mean2, "\n")
}
















