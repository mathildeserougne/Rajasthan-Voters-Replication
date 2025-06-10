## REPLICATION TA2 - TURNOUT ###################################################
################################################################################


# Charger les bibliothèques nécessaires
install.packages(c("dplyr","haven","broom","lmtest","sandwich","writexl"))
library(dplyr)
library(haven)
library(broom)
library(lmtest)
library(sandwich)
library(writexl)
install.packages("stargazer")
library(stargazer)

# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")

colnames(data)
head(data)


# ne fonctionne pas, absolument nul.






