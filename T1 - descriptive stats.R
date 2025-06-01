############################################################################
#### T1 - DESCRIPTIVE STATISTICS ###########################################
############################################################################

# Packages and libraries

library(dplyr)
install.packages("openxlsx")
library(openxlsx)
install.packages("haven")
library(haven)

# Importing the set
data <- read_dta("~/work/Electoral data cleaned.dta")
