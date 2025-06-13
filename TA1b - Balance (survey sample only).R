## TA1-B SURVEY SAMPLE ONLY BALANCE REPLICATION #####


# Required libraries
# Uncomment packages installation if necessary
# install.packages(c("foreign","dplyr","stargazer","haven"))
library(foreign)
library(dplyr)
library(stargazer)
library(haven)

# Path, data path, output path
path <- "~/work"
Data <- file.path(path, "Electoral data cleaned.dta")
output_file_path <- file.path("~/work/Rajasthan-Voters-Replication", "TA1b-replication-survey-only.txt")

# Output directory
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Macros
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")
outregvar1 <- c("RES05_gender", "INT_treatment")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")
outregvar3 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender")
outregvar4 <- c("INT_treatment_gender", "INT_treatment_general", "RES05_gender", "X_generaltr_genderres05", "X_gendertr_genderres05")
outregvar5 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05", "INC05_can_run", "X_anytr_inc_can", "X_inc_can_genderres05", "X_anytr_inc_can_genderres05")





# Loading the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filter the data (survey sample only)
filtered_data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1 & SAMPLE_hhsurvey == 1)

# table data RES05_gender and INT_treatment (gives the number of GPs)
table_data <- table(filtered_data$RES05_gender, filtered_data$INT_treatment)
print(table_data)

# Macros
outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")

# Dependent variables
dep_vars <- c("GP_nbvillages", "GP_population", "GP_sc", "GP_st", "GP_lit", "GP_cult", "GP_aglb",
              "CENSUS_VD2011_drnk_wat_f", "CENSUS_VD2011_app_pr", "CENSUS_VD2011_power_dom",
              "ELEC10_electorate_total", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
              "RES05_obc", "RES05_sc", "RES05_st", "RES10_obc", "RES10_sc", "RES10_st", "INC05_can_run")

# List to stock the models
models <- list()

# Regression for each dependent variable
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # mean and se for control group
  control_data <- filtered_data %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    pull(.data[[dep_var]])
  
  control_mean <- mean(control_data, na.rm = TRUE)
  control_sd <- sd(control_data, na.rm = TRUE)
  
  # regression
  formula <- as.formula(paste(dep_var, "~", paste(outregvar0, collapse = " + ")))
  models[[i]] <- lm(formula, data = filtered_data)
}

# Generate output table
stargazer(models, type = "text", title = "TA1b - Balance test (survey Sample only)",
          notes = c("Mean in Control in Women Unreserved 2005", control_mean,
                    "S.D. in Control in Women Unreserved 2005", control_sd),
          out = "~/work/Rajasthan-Voters-Replication/TA1b-replication-survey-sample-only.txt")

