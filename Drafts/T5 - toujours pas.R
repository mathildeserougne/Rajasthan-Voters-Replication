# Charger les bibliothèques nécessaires
library(haven)
library(dplyr)
library(tidyverse)
library(fixest)
library(modelsummary)

# Chemins des fichiers
electoral_data_path <- "~/work/Electoral data cleaned.dta"
household_data_path <- "~/work/Household survey data cleaned.dta"

# Charger les données électorales
electoral_data <- read_dta(electoral_data_path)

# Variables à conserver
indices <- "index_empl_pre_svysample"
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc",
                "RES05_sc", "RES05_st")

# Filtrer les données électorales
electoral_data_filtered <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

# Sauvegarder le dataframe filtré dans un fichier temporaire
temp_file <- tempfile(fileext = ".dta")
write_dta(electoral_data_filtered, temp_file)

# Charger les données des ménages
household_data <- read_dta(household_data_path)

# Fusionner avec les données électorales filtrées
merged_data <- household_data %>%
  arrange(district, ps, gp) %>%
  inner_join(read_dta(temp_file), by = c("district", "ps", "gp"))

# Remodeler les données
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

# Remodeler les données
df_long <- df %>%
  pivot_longer(
    cols = c(
      starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
      starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
      starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")
    ),
    names_to = c("prefix", "gender"),
    names_sep = "_(?=[mf]$)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = prefix,
    values_from = value
  )

# Nettoyer et créer les variables d'intérêt
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

# Centrage et réduction des variables E_know_*, F_rate_*, E_rate* pour les individus de contrôle
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
    ifelse(INT_treatment == 0 & RES05_gender == 0, (. - mu) / sigma, .)
  }))

# Création des indices composites
df_reshaped <- df_reshaped %>%
  mutate(
    E_know_nregarules = rowMeans(select(., E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., F_rate_publicgoods_road, F_rate_publicgoods_pump, F_rate_publicgoods_school), na.rm = TRUE)
  )

# Variables d'interaction
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

# Définir les variables de contrôle
indcontrols <- grep("^C_I_", names(df_reshaped), value = TRUE)
hhcontrols <- grep("^C_H_", names(df_reshaped), value = TRUE)
gpcontrols <- grep("^std_HH_NREGA", names(df_reshaped), value = TRUE)

# Régressions
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
  
  # Faire la régression avec feols
  model <- feols(formula, fixef = "district", cluster = "ID_gp_no", data = df_reshaped)
  
  # Stocker le résultat + moyennes contrôle
  results_list[[paste0("Model_", i)]] <- model
  attr(results_list[[paste0("Model_", i)]], "control_mean1") <- control_mean1
  attr(results_list[[paste0("Model_", i)]], "control_mean2") <- control_mean2
  
  i <- i + 1
}

# Afficher les résultats
modelsummary(results_list, output = "default")



# Calculer les VIF
vif_model <- lm(E_know_nregarules ~ ., data = df_reshaped)
vif(vif_model)


# Calculer la matrice de corrélation
cor_matrix <- cor(df_reshaped %>% select(-c(E_know_nregarules)))
print(cor_matrix)


