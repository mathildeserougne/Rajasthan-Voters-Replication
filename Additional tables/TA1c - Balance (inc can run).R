## REPLICATION TA1-C : BALANCE SURVEY SAMPLE WHERE INC CAN RUN ################

# Required libraries
# Uncomment packages installation if necessary
# install.packages(c("foreign","dplyr","stargazer","haven"))
library(foreign)
library(dplyr)
library(stargazer)
library(haven)

# Loading the data
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filter the data: survey sample where incumbent can run again
filtered_data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1 & INC05_can_run == 1 & SAMPLE_hhsurvey == 1)

# Frequency table for RES05_gender and INT_treatment
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

# Generating the results table
stargazer(models, type = "text", title = "TA1c - Balance test (survey Sample incumbent can run)",
          notes = c("Mean in Control in Women Unreserved 2005", control_mean,
                    "S.D. in Control in Women Unreserved 2005", control_sd),
          out = "~/work/Rajasthan-Voters-Replication/TA1c-replication-inc-can-run.txt")
