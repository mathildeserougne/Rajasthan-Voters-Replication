## REPLICATION - TABLE 5: KNOWLEDGE AND PERCEPTION OF THE NREGA ################
################################################################################

### LIBRARIES

# Package installation if necessary
# install.packages(c("tidyverse", "fixest", "modelsummary", "haven", "janitor"))



## DATA

path      <- file.path("~/work")

# variables
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st",
                "RES05_obc", "RES05_sc", "RES05_st")

## macros
outregvar2      <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvarindex2 <- c("TEMP_X_anytr_index", "TEMP_X_anytr_res_index",
                     "TEMP_X_res_index",  outregvar2, "TEMP_index")

indices <- "index_empl_pre_svysample"   

## Loading and merging the data
electoral <- read_dta(file.path("Electoral data cleaned.dta")) |>
  select(district, ps, gp, all_of(gpcontrols),
         all_of(indices), starts_with("std_HH_NREGA")) |>
  distinct()

household <- read_dta(file.path("Household survey data cleaned.dta"))

df <- household |>
  left_join(electoral, by = c("district", "ps", "gp")) |>
  filter(!is.na(index_empl_pre_svysample))               


# deleting the duplicates when a .x or .y appeared during the merge
dups <- names(df) %>% str_subset("\\.x$")

for (vx in dups) {
  base <- str_remove(vx, "\\.x$")
  vy   <- paste0(base, ".y")
  
  df <- df %>%
    mutate(
      !!base := coalesce(              
        if (vx %in% names(df)) .data[[vx]] else NULL,
        if (vy %in% names(df)) .data[[vy]] else NULL
      )
    )
}

## deleting all remaining .x or .y
df <- df %>%
  select(-matches("\\.(x|y)$"))





# RESHAPING AT INDIVIDUAL LEVEL 

# minding the suffix gender = "_m", "_f"
long_vars <- c("A_age", "A_educ", "A_literacy",
               "D_NREGA_work",
               "E_know_minimumwage", "E_know_maximumdays",
               "E_know_sarpanchrole_projects", "E_know_sarpanchrole_jobcard",
               "E_know_sarpanchrole_work", "E_know_jobcardapplication",
               "E_know_waitingdays", "E_know_unemploymentallowance",
               "E_know_postofficepay",
               "E_rate_NREGAimplementation", "E_rate_NREGAimplementation_g",
               "E_rate_NREGAimplementation_vg", "E_rate_sarpanchperformance",
               "E_rate_sarpanchperformance_g", "E_rate_sarpanchperformance_vg",
               "F_rank_publicgoods_road", "F_rank_publicgoods_pump",
               "F_rank_publicgoods_school",
               "F_rate_publicgoods_road", "F_rate_publicgoods_pump",
               "F_rate_publicgoods_school",
               "F_optimistic_sarpanch", "F_optimistic_govprograms")

# seeing what already exists without gender
base_dups <- long_vars[ long_vars %in% names(df) ]   # ex. "D_NREGA_work"
df <- df %>% select(-all_of(base_dups))

# pivot
df_long <- df %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(
    cols         = matches(paste0("^(", paste(long_vars, collapse = "|"), ")_(m|f)$")),
    names_to     = c(".value", "gender"),
    names_pattern = "^(.*)_(m|f)$"
  )




# INDICES FOR INDVIDIUAL

df_long <- df_long |>
  mutate(across(A_age, ~ na_if(., 0))) %>%           # 0 becomes NA
  mutate(
    C_I_AgeBelow25   = A_age  < 25,
    C_I_Age2535      = between(A_age, 25, 34),
    C_I_Age3545      = between(A_age, 35, 44),
    C_I_AgeAbove45   = A_age  >= 45 & !is.na(A_age),
    C_I_Female       = gender == "f",
    C_I_Literate     = A_literacy == 4,
    C_I_EducNone     = A_educ == 0,
    C_I_EducPrimary  = A_educ  > 0 & A_educ <= 5,
    C_I_EducLowerSec = A_educ  > 5 & A_educ <= 9,
    C_I_EducUpperSec = A_educ  > 9 & A_educ <= 12,
    C_I_EducTertiary = A_educ  > 12 & !is.na(A_educ),
    C_I_Missing      = if_any(c(A_age, A_educ, A_literacy), is.na)
  ) |>
  mutate(
    C_H_bpl       = H_bpl      == 1,
    C_H_ownland   = H_ownland  == 1,
    C_H_hindu     = H_religion == 1,
    C_H_CasteGen  = H_caste %in% c(1, 5),
    C_H_CasteOBC  = H_caste %in% c(2, 6),
    C_H_CasteSC   = H_caste == 3,
    C_H_CasteST   = H_caste == 4,
    C_H_Missing   = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na)
  )

indcontrols <- grep("^C_I_", names(df_long), value = TRUE)
hhcontrols  <- grep("^C_H_", names(df_long), value = TRUE)



# to avoid na errors
dummy_vars <- c(indcontrols, hhcontrols)   # dummies
df_long <- df_long %>%
  mutate(across(all_of(dummy_vars),
                ~ as.numeric(replace_na(., 0))))   



# NORMALISING THE VARIABLES

to_z   <- grep("^(E_know_|F_rate_|E_rate)", names(df_long), value = TRUE)

ref_mu <- df_long |>
  filter(INT_treatment == 0, RES05_gender == 0) |>
  summarise(across(all_of(to_z), mean,  na.rm = TRUE))
ref_sd <- df_long |>
  filter(INT_treatment == 0, RES05_gender == 0) |>
  summarise(across(all_of(to_z), sd, na.rm = TRUE))

df_long[to_z] <- map2_dfc(df_long[to_z], names(df_long[to_z]), \(x, v)
                          (x - ref_mu[[v]]) / ref_sd[[v]]
)



# COMPOSITE INDICES

df_long <- df_long %>% 
  mutate(
    E_know_nregarules  = rowMeans(cbind(E_know_minimumwage,
                                        E_know_maximumdays), na.rm = TRUE),
    

    E_know_sarpanchrole = rowMeans(pick(starts_with("E_know_sarpanchrole_")),
                                   na.rm = TRUE),
    E_rate_nrega        = rowMeans(cbind(E_rate_NREGAimplementation,
                                         E_rate_sarpanchperformance),
                                   na.rm = TRUE),
    F_rate_publicgoods  = rowMeans(pick(starts_with("F_rate_publicgoods_")),
                                   na.rm = TRUE)
  )


# INTERACTION VARIABLES

df_long <- df_long |>
  mutate(
    TEMP_index              = !!sym(indices),
    TEMP_X_res_index        = RES05_gender * TEMP_index,
    TEMP_X_anytr_index      = INT_treatment * TEMP_index,
    TEMP_X_anytr_res_index  = INT_treatment * RES05_gender * TEMP_index
  )



# REGRESSIONS

dep_set1 <- c("E_know_nregarules", "E_know_sarpanchrole",
              "E_rate_nrega", "F_rate_publicgoods")

##  Reg 1  – TEMP_index
models1 <- map(dep_set1, \(y) {
  feols(
    reformulate(c("TEMP_index", gpcontrols, indcontrols, hhcontrols),
                response = y),
    data    = df_long,
    cluster = "ID_gp_no",   
    fixef   = "district"   
  )
})

names(models1) <- paste0("(", seq_along(models1), ") Reg1: ", dep_set1)

##  Reg 2  – INT_treatment + RES05_gender + interaction
dep_set2 <- c(indices, dep_set1)   
models2 <- map(dep_set2, \(y) {
  feols(
    reformulate(c(outregvar2, gpcontrols, indcontrols, hhcontrols),
                response = y),
    data    = df_long,
    cluster = "ID_gp_no",
    fixef   = "district")
})
names(models2) <- paste0("(", seq_along(models2), ") Reg2: ", dep_set2)

##  Reg 3  – index, complete
models3 <- map(dep_set1, \(y) {
  feols(
    reformulate(c(outregvarindex2, gpcontrols, indcontrols, hhcontrols),
                response = y),
    data    = df_long,
    cluster = "ID_gp_no",
    fixef   = "district")
})
names(models3) <- paste0("(", seq_along(models3), ") Reg3: ", dep_set1)


# adding the means

control_means <- function(v) {
  m1 <- mean(df_long[df_long$INT_treatment == 0 &
                       df_long$RES05_gender == 0, v], na.rm = TRUE)
  m2 <- mean(df_long[df_long$INT_treatment == 0 &
                       df_long$RES05_gender == 1, v], na.rm = TRUE)
  c(`Mean Control not WR 05` = m1,
    `Mean Control WR 05`     = m2)
}


## one list per model
gof_add1 <- imap(models1, \(m, n) control_means(dep_set1[as.numeric(str_extract(n, "\\d+"))]))
gof_add2 <- imap(models2, \(m, n) {
  var <- dep_set2[as.numeric(str_extract(n, "\\d+"))]
  control_means(var)
})
gof_add3 <- imap(models3, \(m, n) control_means(dep_set1[as.numeric(str_extract(n, "\\d+"))]))


#  mean in control rows
row_add <- rbind(
  map_dbl(c(gof_add1, gof_add2, gof_add3), 1),   # 1re ligne
  map_dbl(c(gof_add1, gof_add2, gof_add3), 2)    # 2e ligne
)
rownames(row_add) <- c("Mean in Control not WR in 2005",
                       "Mean in Control WR in 2005")



## OUTPUT TABLE


# listing the models
mods <- c(models1, models2, models3)

# variables to display
vars <- c(
  TEMP_index              = "STD Index (2005)",
  INT_treatment           = "Any treat.",
  RES05_gender            = "Female",
  X_anytr_genderres05     = "Any × Female",
  TEMP_X_anytr_index      = "Any × Index",
  TEMP_X_anytr_res_index  = "Any × Index × F",
  TEMP_X_res_index        = "Index × Female"
)

# mean control
mean_control <- function(dep, female)
  mean(df_long[[dep]][df_long$INT_treatment == 0 &
                        df_long$RES05_gender  == female], na.rm = TRUE)

# output file
outfile <- "T5_Voters_complete.txt"
sink(outfile)     

for (i in seq_along(mods)) {
  
  m   <- mods[[i]]
  dep <- all.vars(formula(m))[1]
  
  cat("\n================  MODEL", i, " — ", dep, "  ================\n")
  
  ct <- coeftable(m)
  
  for (v in names(vars)) {
    if (v %in% rownames(ct)) {
      cat(sprintf("%-25s %8.4f  (%7.4f)\n",
                  vars[v], ct[v, "Estimate"], ct[v, "Std. Error"]))
    }
  }
  
  cat(sprintf("\nObservations                    %d", nobs(m)))
  cat(sprintf("\nR-squared                       %.3f", fitstat(m, 'r2')))
  cat("\nDistrict FE                     Yes")
  cat("\nIndividual Controls             YES")
  cat("\nHH Controls                     YES")
  cat("\nGP controls                     Yes")
  cat(sprintf("\nMean in Control not WR in 2005  %.4f",
              mean_control(dep, 0)))
  cat(sprintf("\nMean in Control WR in 2005      %.4f\n",
              mean_control(dep, 1)))
}

sink()       



# other presentation

vars_keep <- c("TEMP_index",
               "INT_treatment",
               "RES05_gender",
               "X_anytr_genderres05",
               "TEMP_X_anytr_index",
               "TEMP_X_anytr_res_index",
               "TEMP_X_res_index")


library(modelsummary)
library(openxlsx)

models_all <- c(models1, models2, models3)   

coef_map <- c(
  "TEMP_index"             = "STD Index (2005)",
  "INT_treatment"          = "Any treat.",
  "RES05_gender"           = "Female",
  "X_anytr_genderres05"    = "Any × Female",
  "TEMP_X_anytr_index"     = "Any × Index",
  "TEMP_X_anytr_res_index" = "Any × Index × F",
  "TEMP_X_res_index"       = "Index × Female"
)

tab5 <- msummary(
  models_all,
  coef_map   = coef_map,          
  gof_omit   = "Adj|Within|Pseudo",
  gof_map    = c("nobs" = "Observations",
                 "FE: district" = "District FE"),
  stars      = c('***' = .01, '**' = .05, '*' = .1),
  output     = "data.frame"    
)


library(modelsummary)

tab5_md <- msummary(
  models_all,
  coef_map = vars_keep,
  gof_omit = "Adj|Within|Pseudo",
  gof_map  = c("nobs" = "Observations",
               "FE: district" = "District FE"),
  stars    = c('***' = .01, '**' = .05, '*' = .1),
  output   = "markdown"
)

tab5_txt <- as.character(tab5_md)

outfile <- "T5_Voters.txt"
writeLines(tab5_txt, con = outfile)

