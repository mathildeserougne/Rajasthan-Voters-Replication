### Replication TA3 - incumbent conditional and performance ?
### second part of the code for table TA3


# Libraries
# Install packages if necessary
# install.packages(c("dplyr","readr","lmtest","broom","writexl","stargazer"))
library(dplyr)
library(readr)
library(lmtest)
library(broom)
library(writexl)
library(stargazer)

# Change path if necessary
path <- "~/work"


# Output directory to stock the results
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}


# Macros (controls, variables)
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

outregvarindex2 <- c("INT_treatment", "RES05_gender", "X_anytr_genderres05",
                     "TEMP_index", "TEMP_X_res_index", "TEMP_X_anytr_index", "TEMP_X_anytr_res_index")

# Loading the data
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filter data (inc can run)
data <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == TRUE & INC05_can_run == 1)

# Create new variables
data <- data %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    INC05_voteshare_cond = ifelse(INC05_running == 1, INC05_voteshare, NA),
    INC05_won_cond = ifelse(INC05_running == 1, INC05_won, NA),
    TEMP_index = index_empl_pre_svysample,
    TEMP_X_res_index = RES05_gender * index_empl_pre_svysample,
    TEMP_X_anytr_index = INT_treatment * index_empl_pre_svysample,
    TEMP_X_anytr_res_index = INT_treatment * RES05_gender * index_empl_pre_svysample
  )

# Dependent variables
incum_dep_vars1 <- c("INC05_won", "INCSPOUSE05_won", "INCOTHER05_won", "INC05_voteshare_cond", "INC05_won_cond")

# Regressions
models <- list()
for (dep_var in incum_dep_vars1) {
  formula <- as.formula(paste(dep_var, "~", paste(c(outregvarindex2, gpcontrols, "district"), collapse = " + ")))
  models[[dep_var]] <- lm(formula, data = data)
}

# Exportating in xlsx
#results <- lapply(models, tidy)
#write_xlsx(results, file.path(Results, "TA3b - Incumbent - Performance - Conditional.xlsx"))

# Output table with stargazer
stargazer(models, type = "text", out = file.path("~/work/Rajasthan-Voters-Replication/TA3b- Incumbent - Performance - Conditional.txt"))
