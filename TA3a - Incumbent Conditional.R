### TA3 REPLICATION - summary statistics where 2010 inc could run ###########
# replicates the table of 'incumbent conditional', not the part with performance

# Libraries
# Uncomment the packages before if necessary
# install.packages(c("dplyr","readr","lmtest","broom","writexl","stargazer"))
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(writexl)
library(stargazer)


# Macros
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages", "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st", "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")
outregvar2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05")

# Loading the data
path <- "~/work"
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filter the data
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == TRUE & INC05_can_run == 1)

# Create new variables
data <- data %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    INC05_voteshare_cond = ifelse(INC05_running == 1, INC05_voteshare, NA),
    INC05_won_cond = ifelse(INC05_running == 1, INC05_won, NA)
  )

# Dependent variables
incum_dep_vars1 <- c("INC05_won", "INCSPOUSE05_won", "INCOTHER05_won", "INC05_voteshare_cond", "INC05_won_cond")

# Regressions, stock the models
models <- list()
for (dep_var in incum_dep_vars1) {
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvar2, gpcontrols, "district"), collapse = " + ")))
  models[[dep_var]] <- lm(formula, data = data)
}

# Export results in xlsx
#results <- lapply(models, tidy)
#write_xlsx(results, file.path(Results, "TA3a- Incumbent - Conditional.xlsx"))

# Output table in stargazer
stargazer(models, type = "text", out = file.path("~/work/Rajasthan-Voters-Replication", "TA3a- Incumbent - Conditional.txt"))
