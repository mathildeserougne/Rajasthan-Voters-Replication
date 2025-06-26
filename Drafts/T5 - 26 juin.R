## 26 juin ####################################################################

# packages
library(tidyverse)
library(haven)
library(data.table)
library(fixest)
library(broom)


# globals from the master do 

gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")
outregvar1 <- c("RES05_gender", "INT_treatment")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar3 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender")
outregvar4 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender",
                "X_generaltr_genderres05", "X_gendertr_genderres05")
outregvar5 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05", "INC05_can_run",
                "X_anytr_inc_can", "X_inc_can_genderres05", "X_anytr_inc_can_genderres05")

outregvarindex1 <- c("TEMP_X_res_index", "TEMP_X_anytr_index", outregvar1, "TEMP_index")
outregvarindex2 <- c("TEMP_X_anytr_index", "TEMP_X_anytr_res_index", "TEMP_X_res_index",
                     outregvar2, "TEMP_index")
outregvarindex3 <- c("TEMP_X_gender_index", "TEMP_X_general_index", "TEMP_X_gender_res_index",
                     "TEMP_X_general_res_index", "TEMP_X_res_index", outregvar4, "TEMP_index")





# ELECTORAL DATA
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")


gpcontrols <- c("GP_population.x", "ELEC10_electorate_total.x", "ELEC10_electorate_total_missing.x")  # remplacer par vos contrôles GP exacts
indices <- c("index_empl_pre_svysample")

# necessary variables
electoral_data <- electoral_data %>%
  select(district, ps, gp, all_of(gpcontrols), all_of(indices), starts_with("std_HH_NREGA")) %>%
  distinct()

# temp save
temp <- electoral_data


# SURVEY DATA AND MERGE

hh_data <- read_dta("~/work/Household survey data cleaned.dta")

merged <- hh_data %>%
  left_join(temp, by = c("district", "ps", "gp")) %>%
  filter(!is.na(index_empl_pre_svysample))  # drop if _m==2


# RESHAPING

merged <- merged %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("A_age") | starts_with("A_educ") | starts_with("A_literacy") |
      starts_with("D_NREGA_work") | starts_with("E_know") | starts_with("E_rate") |
      starts_with("F_rank") | starts_with("F_rate_publicgoods") | starts_with("F_optimistic"),
    names_to = c(".value", "gender"),
    names_pattern = "(.*)_([mf])"
  )


# indicators (individual)

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


## indicators (household)

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



## interactions

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



colnames(merged)


## WE DO HAVE THE SAME CONTROLS AND VARIABLES AS IN THE STATA CODE 
## NOW MOVING ONTO THE REGRESSIONS




# 1) FIRST BLOCK OF REGRESSION ( premier quietly) ###################


library(fixest)
library(dplyr)


# controls
gpcontrols <- c("GP_population.x", "ELEC10_electorate_total.x", "ELEC10_electorate_total_missing.x")

# depdt variables
dep_vars <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# control means empty frame
control_means <- data.frame(dep_var = character(), control_mean1 = numeric(), control_mean2 = numeric())

# list for regression results
results_list <- list()


# loop on dependent variables
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # control mean
  control_mean1 <- mean(merged %>% filter(INT_treatment == 0 & RES05_gender == 0) %>% pull(all_of(dep_var)), na.rm = TRUE)
  control_mean2 <- mean(merged %>% filter(INT_treatment == 0 & RES05_gender == 1) %>% pull(all_of(dep_var)), na.rm = TRUE)
  # means stocked
  control_means[i, ] <- data.frame(dep_var = dep_var, control_mean1 = control_mean1, control_mean2 = control_mean2)
  
  # regression with fixed effect at district level
  regression <- feols(
    as.formula(paste(dep_var, "~ TEMP_index +", paste(gpcontrols, collapse = " + "), "+",
                     paste(grep("^C_I_", names(merged), value = TRUE), collapse = " + "), "+",
                     paste(grep("^C_H_", names(merged), value = TRUE), collapse = " + "), "| district")),
    data = merged,
    cluster = ~ID_gp_no
  )
  
  # save results
  results_list[[i]] <- regression
}

# print ctrl means
print(control_means)

# print reg results
for (i in seq_along(results_list)) {
  cat("Regression for", dep_vars[i], ":\n")
  print(summary(results_list[[i]]))
}




## bloc 2 (quietly 2)


library(fixest)
library(dplyr)



# controls and globals
gpcontrols <- c("GP_population.x", "ELEC10_electorate_total.x", "ELEC10_electorate_total_missing.x")
indcontrols <- grep("^C_I_", names(merged), value = TRUE)
hhcontrols <- grep("^C_H_", names(merged), value = TRUE)
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvarindex2 <- c("TEMP_X_anytr_index", "TEMP_X_anytr_res_index", "TEMP_X_res_index", outregvar2, "TEMP_index")

# dpdt
dep_vars <- c("index_empl_pre_svysample", "E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# control means
control_means <- data.frame(dep_var = character(), control_mean1 = numeric(), control_mean2 = numeric())

# reg results
results_list <- list()

# function for regressions
run_regressions <- function(outreg_vars) {
  results <- list()
  for (i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    # control means
    control_mean1 <- mean(merged %>% filter(INT_treatment == 0 & RES05_gender == 0) %>% pull(.data[[dep_var]]), na.rm = TRUE)
    control_mean2 <- mean(merged %>% filter(INT_treatment == 0 & RES05_gender == 1) %>% pull(.data[[dep_var]]), na.rm = TRUE)
    
    # means
    control_means[i, ] <- data.frame(dep_var = dep_var, control_mean1 = control_mean1, control_mean2 = control_mean2)
    
    # regression with district fixed effect
    regression <- feols(
      as.formula(paste(dep_var, "~", paste(c(outreg_vars, gpcontrols, indcontrols, hhcontrols), collapse = " + "), "| district")),
      data = merged,
      cluster = ~ID_gp_no
    )
    
    # add result to list
    results[[i]] <- regression
  }
  return(results)
}

# reg for outregvar2
results_list <- run_regressions(outregvar2)

# control means
print(control_means)

# reg results
for (i in seq_along(results_list)) {
  cat("Regression for", dep_vars[i], ":\n")
  print(summary(results_list[[i]]))
}

# initialise list for next reg
results_list <- list()

# regs for outregvarindex2
results_list <- run_regressions(outregvarindex2)

# reg results
for (i in seq_along(results_list)) {
  cat("Regression for", dep_vars[i], ":\n")
  print(summary(results_list[[i]]))
}






