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




## version du code du 19/06 ####################


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




# until now, pretty sure it's normal


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
%>%
  separate(variable, into = c("prefix", "gender"), sep = "_(?=[mf]$)", fill = "right") %>%
  pivot_wider(
    names_from = prefix,
    values_from = value
  )

# colnames to check
print(colnames(df_long))

df_long <- df_long %>%
  rename(
    A_age = A_age,
    A_educ = A_educ,
    A_literacy = A_literacy
  )

# cleaning and interest variables
df_reshaped <- df_long %>%
  mutate(
    A_age = as.numeric(A_age),
    A_educ = as.numeric(A_educ),
    A_literacy = as.numeric(A_literacy),
    A_age = ifelse(A_age == 0, NA, A_age),
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
    C_I_Missing = is.na(A_age) | is.na(A_educ) | is.na(A_literacy),
    C_H_bpl = H_bpl == 1,
    C_H_ownland = H_ownland == 1,
    C_H_hindu = H_religion == 1,
    C_H_CasteGen = H_caste == 1 | H_caste == 5,
    C_H_CasteOBC = H_caste == 2 | H_caste == 6,
    C_H_CasteSC = H_caste == 3,
    C_H_CasteST = H_caste == 4,
    C_H_Missing = is.na(H_bpl) | is.na(H_ownland) | is.na(H_religion) | is.na(H_caste)
  )

# check
head(df_reshaped)





## controls ##

library(dplyr)
library(stringr)

# Centrage et réduction des variables E_know_*, F_rate_*, E_rate* pour les individus de contrôle (INT_treatment==0 & RES05_gender==0)
vars_to_scale <- df_reshaped %>%
  select(starts_with("E_know_"), starts_with("F_rate_"), starts_with("E_rate")) %>%
  select(where(is.numeric)) %>%
  colnames()

df_reshaped <- df_reshaped %>%
  mutate(across(all_of(vars_to_scale), ~ {
    control_group <- df_reshaped %>%
      filter(INT_treatment == 0, RES05_gender == 0) %>%
      pull(cur_column())
    mu <- mean(control_group, na.rm = TRUE)
    sigma <- sd(control_group, na.rm = TRUE)
    (. - mu) / sigma
  }, .names = "{.col}"))



# composites
df_reshaped <- df_reshaped %>%
  mutate(
    E_know_nregarules = rowMeans(select(., E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., F_rate_publicgoods_road, F_rate_publicgoods_pump, F_rate_publicgoods_school), na.rm = TRUE)
  )


# variables d'interaction

indices <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

for (index in indices) {
  df_reshaped <- df_reshaped %>%
    mutate(
      !!paste0("TEMP_index_", index) := .data[[index]],
      !!paste0("TEMP_X_res_index_", index) := RES05_gender * .data[[index]],
      !!paste0("TEMP_X_anytr_index_", index) := INT_treatment * .data[[index]],
      !!paste0("TEMP_X_grltr_index_", index) := INT_treatment_general * .data[[index]],
      !!paste0("TEMP_X_gndtr_index_", index) := INT_treatment_gender * .data[[index]],
      !!paste0("TEMP_X_anytr_res_index_", index) := INT_treatment * RES05_gender * .data[[index]],
      !!paste0("TEMP_X_grltr_res_index_", index) := INT_treatment_general * RES05_gender * .data[[index]],
      !!paste0("TEMP_X_gndtr_res_index_", index) := INT_treatment_gender * RES05_gender * .data[[index]]
    )
}



## control variables

library(dplyr)
library(fixest)
library(modelsummary)

# Définir les variables de contrôle
indcontrols <- grep("^C_I_", names(data), value = TRUE)
hhcontrols  <- grep("^C_H_", names(data), value = TRUE)
gpcontrols  <- grep("^std_HH_NREGA", names(data), value = TRUE)

standardize_vars <- grep("^E_know_|^F_rate_|^E_rate", names(df_reshaped), value = TRUE)

df_reshaped <- df_reshaped %>%
  mutate(across(
    all_of(standardize_vars),
    ~ ifelse(INT_treatment == 0 & RES05_gender == 0, as.numeric(scale(.x)), .x)
  ))








# regressions

results_list <- list()
i <- 1

for (dep_var in indices) {
  # Moyennes dans le groupe de contrôle
  control_mean1 <- mean(df_reshaped[[dep_var]][df_reshaped$INT_treatment == 0 & df_reshaped$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(df_reshaped[[dep_var]][df_reshaped$INT_treatment == 0 & df_reshaped$RES05_gender == 1], na.rm = TRUE)
  
  # Variable TEMP_index spécifique
  temp_index_var <- paste0("TEMP_index_", dep_var)
  
  # Construire la formule classique (sans effets fixes)
  rhs_vars <- c(temp_index_var, gpcontrols, indcontrols, hhcontrols)
  fmla_str <- paste(dep_var, "~", paste(rhs_vars, collapse = " + "))
  formula <- as.formula(fmla_str)
  
  # Faire la régression avec fixef en vecteur de noms de colonnes
  model <- feols(formula, fixef = "district", cluster = "ID_gp_no", data = df_reshaped)
  
  # Stocker le résultat + moyennes contrôle
  results_list[[paste0("Model_", i)]] <- model
  attr(results_list[[paste0("Model_", i)]], "control_mean1") <- control_mean1
  attr(results_list[[paste0("Model_", i)]], "control_mean2") <- control_mean2
  
  i <- i + 1
}






### poubelle



# Utilisation du dataframe merged_data
df <- merged_data

# Sélection des variables disponibles
df <- df %>%
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
  )

# Ajout d'un identifiant temporaire
df <- df %>%
  mutate(TEMP_id = row_number())

# Remodelage des données
df_reshaped <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  separate(variable, into = c("variable_prefix", "gender"), sep = "_", fill = "right") %>%
  pivot_wider(
    names_from = variable_prefix,
    values_from = value
  )


## poubelle




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













