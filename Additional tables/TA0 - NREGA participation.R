## REPLICATION: TA0 - PARTICIPATION TO NREGA PROGR ############################


# Packages : uncomment if necessary to install them
#install.packages(c("dplyr","haven","stargazer"))

# Libraries
library(dplyr)
library(haven)
library(stargazer)

# Loading the data. Adapt path if necessary.
data <- read_dta("~/work/Household survey data cleaned.dta")

# Cleaning the data for the right variables
data_clean <- data %>%
  filter(!is.na(ID_gp_no)) %>%
  mutate(
    D_NREGA_work_gen = ifelse(H_caste == 1, D_NREGA_work, NA),
    D_NREGA_work_nongen = ifelse(H_caste != 1, D_NREGA_work, NA),
    LFP_m = as.integer((q1a6 > 2 & q1a6 < 8) | (q1a11 > 2 & q1a11 < 8)),
    LFP_f = as.integer((q2a6 > 2 & q2a6 < 8) | (q2a11 > 2 & q2a11 < 8))
  ) %>%
  mutate(
    D_NREGA_work_ind = rowMeans(select(., D_NREGA_work_f, D_NREGA_work_m), na.rm = TRUE),
    LFP_ind = rowMeans(select(., LFP_m, LFP_f), na.rm = TRUE)
  )



# Summary statistics
summary_stats <- data_clean %>%
  summarise(
    mean_D_NREGA_work = mean(D_NREGA_work, na.rm = TRUE),
    mean_D_NREGA_work_gen = mean(D_NREGA_work_gen, na.rm = TRUE),
    mean_D_NREGA_work_nongen = mean(D_NREGA_work_nongen, na.rm = TRUE),
    mean_D_NREGA_work_ind = mean(D_NREGA_work_ind, na.rm = TRUE),
    mean_D_NREGA_work_m = mean(D_NREGA_work_m, na.rm = TRUE),
    mean_D_NREGA_work_f = mean(D_NREGA_work_f, na.rm = TRUE),
    mean_LFP_ind = mean(LFP_ind, na.rm = TRUE),
    mean_LFP_m = mean(LFP_m, na.rm = TRUE),
    mean_LFP_f = mean(LFP_f, na.rm = TRUE)
  )

# Making a dataframe out of the summary stats
summary_stats_df <- data.frame(Variable = names(summary_stats), Mean = as.numeric(summary_stats))


# Output table TA0 (adapt output path if necessary)
stargazer(summary_stats_df, type = "text", title = "Statistics descriptives",
          summary = FALSE, out = "TA0_NREGA_Participation.txt")



