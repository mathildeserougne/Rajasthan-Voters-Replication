# pursuing on the first try

install.packages(c("haven","tidyverse","stargazer","knitr","broom","fixest","modelsummary"))
library(haven)
install.packages("gt")
install.packages("webshot2")
install.packages("car")

# Libraries
library(tidyverse)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(car)

install.packages("estimatr")    
library(estimatr)

install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)


# source pour stocker les fonctions codées ailleurs
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")



electoral_data <- read_dta("Electoral data cleaned.dta")


data <- electoral_data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )




# BRAS

arms <- c("gender", "water")
# deux bras: gender et water
data <- data %>%
  mutate(
    # value in INT_treatment_code, 1 if value matches, or else 0)
    water = ifelse(INT_treatment_code %in% c(3,4), 2, ifelse(INT_treatment_code %in% c(1,2),1,0)),
    gender = ifelse(INT_treatment_code %in% c(2,4),2, ifelse(INT_treatment_code %in% c(1,3),1,0))
  )



# EFFETS FIXES

data <- data %>%
  mutate(
    # Indicatrice pour DHOLPUR
    district_DHOLPUR = ifelse(district == "DHOLPUR", 1, 0),
    
    # Indicatrice pour KARAULI
    district_KARAULI = ifelse(district == "KARAULI", 1, 0)
  )

fes <- c("district_DHOLPUR","district_KARAULI")







data <- as.data.frame(data %>%
                        select(
                          "INC05_running", "gender","water","RES05_gender","district_DHOLPUR","district_KARAULI"
                        ))

y <- "INC05_running"






# LANCER TVA


estimation_function_name <- "pval_OSE"
#estimation_function_name <- "pval_MSE"


# essai 1 # 
cutoff <- 1


result <- do_TVA(
  data       = data,
  arms       = c("gender", "water"),
  y          = "INC05_running",
  fes        = c("district_DHOLPUR", "district_KARAULI"),
  estim_func = "pval_OSE",       
  cutoff     = NULL               # laisse TVA proposer un cutoff
)



# débug: 
table(data$INC05_running, useNA = "ifany")
with(data, table(gender, water))
# on a bien des valeurs dans tous les bras (comme attendu) et de la variation
# donc on contourne autrement



## essai 2 ##

## on contourne ça en mettant un cutoff manuel?
cutoff_val <- 0.05   # ou 0.1, 0.01… à tester

result <- do_TVA(
  data       = data,
  arms       = c("gender", "water"),
  y          = "INC05_running",
  fes        = c("district_DHOLPUR", "district_KARAULI"),
  estim_func = "pval_OSE",
  cutoff     = cutoff_val
)

# vérifier
str(result)
result$pools_summary

# with this cutoff, pas identifiable. pas assez de variation due aux différences de traitement.
# p values toutes trop hautes donc pas de support retenu.

# on peut néanmoins augmenter le cutoff.


## en fait le problème c'est qu'il nous faudrait des cellules où un bras est activé seul.
# j'ai essayé d'une autre manière

