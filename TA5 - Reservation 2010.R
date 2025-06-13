### REPLICATION TA5: TA5 - Reservation - 2010 ###############################


## PAS ENCORE



# Libraries
# Packages if necessary
# install.packages(c("dplyr","readr","stargazer"))
library(dplyr)
library(readr)
library(stargazer)

# paths
path <- "~/work"


# data
data <- read_dta(file.path(path, "Electoral data cleaned.dta"))

# Filter
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == TRUE)

# new variable
data <- data %>%
  mutate(RES_reserved_gender_once = ifelse(RES05_gender + RES00_gender == 1, 1, 0))

# controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc",
                "RES05_st", "RES00_obc", "RES00_sc", "RES00_st")

# dependent variables
with_incumbent_dep_vars <- c("ELEC10_prop_female", "ELEC10_voteshare_female", "ELEC10_won_female")
without_incumbent_dep_vars <- c("CHAL00_prop_female", "CHAL00_voteshare_female", "CHAL00_won_female")
dep_vars <- c(with_incumbent_dep_vars, without_incumbent_dep_vars)

# list to stock models
models <- list()

# regressions
i <- 1
for (dep_var in dep_vars) {
  # control mean
  control_mean <- data %>%
    filter(RES05_gender == 0 & RES00_gender == 0) %>%
    summarise(mean = mean(.data[[dep_var]], na.rm = TRUE)) %>%
    pull(mean)
  
  # reg 1
  formula <- as.formula(paste(dep_var, "~ RES00_gender + RES05_gender +", paste(gpcontrols, collapse = " + ")))
  model1 <- lm(formula, data = data)
  models[[paste0("Model_", i)]] <- model1
  i <- i + 1
  
  # reg 2
  formula <- as.formula(paste(dep_var, "~ RES_reserved_gender_once +", paste(gpcontrols, collapse = " + ")))
  model2 <- lm(formula, data = data)
  models[[paste0("Model_", i)]] <- model2
  i <- i + 1
}

# Utiliser stargazer pour afficher les résultats dans un fichier texte
stargazer(models, type = "text", out = file.path(path, "TA5 - Reservation - 2010.txt"),
          add.lines = list(c("District FE", "Yes"),
                           c("GP controls", "Yes"),
                           c("Mean in GP not WR in 2005 nor 2000", control_mean)))
