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




## VERSION OF 24TH OF JUNE #####################################################


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



# création des indices composites

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



## REGRESSIONS


## y croit-on encore (non)


# bibliothèques
library(dplyr)
library(tidyr)
library(broom)
library(fixest)
library(modelsummary)

# centrer réduire les variables
vars_to_scale <- c(grep("^E_know_", names(df), value = TRUE),
                   grep("^F_rate_", names(df), value = TRUE),
                   grep("^E_rate", names(df), value = TRUE))

df <- df %>%
  mutate(across(all_of(vars_to_scale), ~
                  ifelse(INT_treatment == 0 & RES05_gender == 0,
                         (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                         .),
                .names = "std_{.col}"))


# indices composites

df <- df %>%
  mutate(
    E_know_nregarules = rowMeans(select(., E_know_minimumwage, E_know_maximumdays), na.rm = TRUE),
    E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole_")), na.rm = TRUE),
    E_rate_nrega = rowMeans(select(., E_rate_NREGAimplementation, E_rate_sarpanchperformance), na.rm = TRUE),
    F_rate_publicgoods = rowMeans(select(., F_rate_publicgoods_road,
                                         F_rate_publicgoods_pump,
                                         F_rate_publicgoods_school), na.rm = TRUE)
  )


# interactions avec traitement et genre

indices <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

for (index in indices) {
  df[[paste0("TEMP_index_", index)]] <- df[[index]]
  df[[paste0("TEMP_X_res_index_", index)]] <- df[[index]] * df$RES05_gender
  df[[paste0("TEMP_X_anytr_index_", index)]] <- df[[index]] * df$INT_treatment
  df[[paste0("TEMP_X_grltr_index_", index)]] <- df[[index]] * df$INT_treatment_general
  df[[paste0("TEMP_X_gndtr_index_", index)]] <- df[[index]] * df$INT_treatment_gender
  df[[paste0("TEMP_X_anytr_res_index_", index)]] <- df[[index]] * df$INT_treatment * df$RES05_gender
  df[[paste0("TEMP_X_grltr_res_index_", index)]] <- df[[index]] * df$INT_treatment_general * df$RES05_gender
  df[[paste0("TEMP_X_gndtr_res_index_", index)]] <- df[[index]] * df$INT_treatment_gender * df$RES05_gender
}



# régressions avec contrôles et effets fixes de districts

gpcontrols <- c("gp_control1", "gp_control2")  # à adapter
indcontrols <- grep("^C_I_", names(df), value = TRUE)
hhcontrols <- grep("^C_H_", names(df), value = TRUE)
controls <- c(gpcontrols, indcontrols, hhcontrols)


# regressions

results_list <- list()
i <- 1

for (dep_var in indices) {
  control_mean1 <- mean(df[[dep_var]][df$INT_treatment == 0 & df$RES05_gender == 0], na.rm = TRUE)
  control_mean2 <- mean(df[[dep_var]][df$INT_treatment == 0 & df$RES05_gender == 1], na.rm = TRUE)
  
  fml <- as.formula(paste0(
    dep_var, " ~ TEMP_index_", dep_var, " + ",
    paste(controls, collapse = " + "),
    " | district"
  ))
  
  model <- feols(fml, cluster = ~ID_gp_no, data = df)
  results_list[[paste0("Model_", i)]] <- model
  attr(results_list[[paste0("Model_", i)]], "control_mean1") <- control_mean1
  attr(results_list[[paste0("Model_", i)]], "control_mean2") <- control_mean2
  i <- i + 1
}


names(df)


## on rattrape tout ça

indcontrols <- grep("^C_I_", names(df), value = TRUE)
hhcontrols  <- grep("^C_H_", names(df), value = TRUE)

controls <- c(indcontrols, hhcontrols)

fml <- as.formula(
  paste0("std_E_know_nregarules ~ INT_treatment + ", 
         paste(controls, collapse = " + "), 
         " | ps")
)


library(fixest)
est <- feols(fml, cluster = ~ID_gp_no, data = df)
summary(est)




## creation des bons modèles


library(fixest)
library(modelsummary)

# --- Définition des formules pour chaque Panel ---
formula_template <- function(outcome) {
  as.formula(paste0(
    outcome, " ~ INT_treatment_gender + INT_treatment_general + RES05_gender + ",
    "INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender | district + ps"
  ))
}

# Liste des modèles (nom = panel)
models <- list(
  "Panel A: NREGA rules" = feols(formula_template("E_know_nregarules"), df, cluster = "gp"),
  "Panel B: Sarpanch role" = feols(formula_template("E_know_sarpanchrole"), df, cluster = "gp"),
  "Panel C1: Evaluation - NREGA implementation" = feols(formula_template("E_rate_nrega"), df, cluster = "gp"),
  "Panel C2: Evaluation - Public goods" = feols(formula_template("F_rate_publicgoods"), df, cluster = "gp")
)

# --- Affichage du tableau résumé ---
modelsummary(models,
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC|Log|F|Within|Pseudo|R2_within|R2_between",
             coef_rename = c(
               "INT_treatment_gender" = "Treatment × Female (quota)",
               "INT_treatment_general" = "Treatment × Female (general)",
               "RES05_gender" = "Female",
               "INT_treatment_gender:RES05_gender" = "Treatment × Female × Quota",
               "INT_treatment_general:RES05_gender" = "Treatment × Female × General"
             ),
             notes = list("All models include district and polling station fixed effects.",
                          "Standard errors clustered at the GP level."))




## stargazer
install.packages("stargazer")
library(fixest)
library(stargazer)

# --- Régressions (mêmes spécifications) ---
reg1 <- feols(E_know_nregarules ~ INT_treatment_gender + INT_treatment_general + RES05_gender +
                INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender | district + ps,
              data = df, cluster = "gp")

reg2 <- feols(E_know_sarpanchrole ~ INT_treatment_gender + INT_treatment_general + RES05_gender +
                INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender | district + ps,
              data = df, cluster = "gp")

reg3 <- feols(E_rate_nrega ~ INT_treatment_gender + INT_treatment_general + RES05_gender +
                INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender | district + ps,
              data = df, cluster = "gp")

reg4 <- feols(F_rate_publicgoods ~ INT_treatment_gender + INT_treatment_general + RES05_gender +
                INT_treatment_gender:RES05_gender + INT_treatment_general:RES05_gender | district + ps,
              data = df, cluster = "gp")

# --- Tableau avec Stargazer ---
stargazer(reg1, reg2, reg3, reg4,
          title = "Table 5: Knowledge and Perception of the NREGA",
          type = "text", # ou "latex" ou "html"
          column.labels = c("NREGA rules", "Sarpanch role", "Eval: NREGA", "Eval: Public goods"),
          covariate.labels = c("Treatment × Female (quota)",
                               "Treatment × Female (general)",
                               "Female",
                               "Treatment × Female × Quota",
                               "Treatment × Female × General"),
          keep.stat = c("n", "rsq"),
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          notes = c("District and polling station fixed effects included.",
                    "Standard errors clustered at GP level."),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)







## plus simple

etable(list(
  "NREGA rules" = reg1,
  "Sarpanch role" = reg2,
  "Eval: NREGA" = reg3,
  "Eval: Public goods" = reg4
),
se = "cluster", # déjà le défaut pour feols avec cluster
dict = c(
  "INT_treatment_gender" = "Treatment (quota)",
  "INT_treatment_general" = "Treatment (general)",
  "RES05_gender" = "Female",
  "INT_treatment_gender:RES05_gender" = "Treatment × Female (quota)",
  "INT_treatment_general:RES05_gender" = "Treatment × Female (general)"
))





