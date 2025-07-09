### Replication tableA1 a  #####################################################

# Necessary libraries
# Uncomment packages installation if needed
# install.packages(c("foreign","dplyr","stargazer","haven"))
library(foreign)
library(dplyr)
library(stargazer)
library(haven)

# Path, Data path, Output path
path <- "~/work"
Data <- file.path(path, "Electoral data cleaned.dta")
output_file_path <- file.path("~/work/Rajasthan-Voters-Replication", "TA1a-replication.txt")

# Output directory
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Macros
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

outregvar0 <- c("RES05_gender_control", "X_anytr_nogenderres05", "X_anytr_genderres05")

# Function for regressions and tables generation
perform_regressions <- function(data, table_name, output_file_path) {
  data <- data %>%
    filter(RES10_gender == 0 & GP_tag == 1)
  
  summary_stats <- data %>%
    group_by(INT_treatment) %>%
    summarise(mean = mean(RES10_obc, na.rm = TRUE),
              sd = sd(RES10_obc, na.rm = TRUE))
  
  control_mean <- summary_stats$mean[summary_stats$INT_treatment == 0]
  control_sd <- summary_stats$sd[summary_stats$INT_treatment == 0]
  
  models <- list()
  i <- 1
  dep_vars <- c("GP_nbvillages", "GP_population", "GP_sc", "GP_st", "GP_lit", "GP_cult", "GP_aglb",
                "CENSUS_VD2011_drnk_wat_f", "CENSUS_VD2011_app_pr", "CENSUS_VD2011_power_dom",
                "ELEC10_electorate_total", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES05_obc", "RES05_sc", "RES05_st", "RES10_obc", "RES10_sc", "RES10_st", "INC05_can_run")
  
  for (dep_var in dep_vars) {
    formula <- as.formula(paste(dep_var, "~", paste(outregvar0, collapse = " + ")))
    models[[i]] <- lm(formula, data = data)
    i <- i + 1
  }
  
  # stargazer output file
  stargazer(models, type = "text", out = output_file_path,
            title = table_name,
            notes = c("Mean in Control in Women Unreserved 2005", control_mean,
                      "S.D. in Control in Women Unreserved 2005", control_sd))
}

# data and regression execution
data <- read_dta(Data)
perform_regressions(data, "TA1a - Balance test (whole sample)", output_file_path)
