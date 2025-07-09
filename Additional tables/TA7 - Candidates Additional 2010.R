### REPLICATION TA7 - CANDIDATES ADDITIONAL 2010 ##############################

# Uncomment packages installation if necessary
# install.packages(c("dplyr","stargazer","lmtest","lme4","haven"))
# Libraries
library(dplyr)
library(stargazer)
library(lmtest)
library(lme4)
library(haven)

# Paths (adapt if necessary)
data_path <- "~/work/Electoral data cleaned.dta"
results_path <- "~/work/Rajasthan-Voters-Replication/TA7- Candidates - Additional - 2010.txt"



# Data
data <- read_dta(data_path)

# Filter data
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1)

# Macros
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Regressions and stock the output
regression_results <- list()
i <- 1

dep_vars <- c("CHAL_prop_new", "CHAL_voteshare_new", "CHAL_prop_loweduc",
              "CHAL_voteshare_loweduc", "CHAL_prop_exsarpanch",
              "CHAL_voteshare_exsarpanch", "CHAL_prop_exwardpanch",
              "CHAL_voteshare_exwardpanch")

# open file to write down the results
sink(file = results_path)

for (dep_var in dep_vars) {
  # control mean
  control_mean1 <- mean(data %>% filter(INT_treatment == 0 & RES05_gender == 0) %>% pull(!!sym(dep_var)), na.rm = TRUE)
  
  # do the regression
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols, "district"), collapse = " + ")))
  model <- lm(formula, data = data)
  
  # model summary
  cat("Regression for", dep_var, "\n")
  print(summary(model))
  cat("Control Mean:", control_mean1, "\n\n")
  
  # stock results
  regression_results[[i]] <- list(model = model, control_mean1 = control_mean1)
  i <- i + 1
}

# close file
sink()



