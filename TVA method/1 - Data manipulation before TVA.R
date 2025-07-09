## Data manipulation before TVA ################################################
################################################################################

# first attempt on the method

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


## HOMOGENEISER LE PROJET R POUR NE PAS AVOIR A CHANGER LES PATHS
# source pour stocker les fonctions codées ailleurs
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")

electoral_data <- read_dta("Electoral data cleaned.dta")

colnames(electoral_data)


# goal = a dataframe containing all your observations with columns for the outcome,
#the dosage in each arm, DONC NOUS GENDER / GENERAL et ROADS / WATER
#the fixed effects
#and the possible weights.


# INT_treatment_code
# 1 et 2 c'est roads
# 3 and 4 c'est water

# et variable d'outcome



# faire la sample restriction comme dans table 1
# Filtering the data
data <- electoral_data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won
  )


## EXPLICATIONS DU 20 JUIN ##############################################

# nous on aura un bras sur gender et un bras sur infrastructures


# créer un fixed effect du district au cas où
# deux variables à créer: parce que trois noms de district
unique(data$district)
# weight: y en a pas



# à ce stade on split juste gender reserved and not gender reserved


#############################################################################

## YOUR DATA

# Out data must include: 
# - observed outcomes
# - dosage in each treatment arm
# - fixed effects
# - weight

# Arguments of the functions: 
# - data: the dataframe with all observations
# - required columns: results, dosage per arm, fixed effects, weights
# - arms: vector with arms columns' names

# each arm = a type of intervention
# value = dosage of intervention for each observation
# fes = vector with columns of fixed effects in the dataframe. equals c() if no fixed effects.
# w = weights column. NULL per default.



# IN OUR CASE ##############


# we look at the column INT_treatment_code. 
# 1 and 2 are for roads: 1 for general and roads, 2 for gender and roads
# 3 and 4 are for water: 3 for general and water, 4 for gender and water

arms <- c("gender", "water")



# deux bras: gender et water
data <- data %>%
  mutate(
    # value in INT_treatment_code, 1 if value matches, or else 0)
    water = ifelse(INT_treatment_code %in% c(3,4), 2, ifelse(INT_treatment_code %in% c(1,2),1,0)),
    gender = ifelse(INT_treatment_code %in% c(2,4),2, ifelse(INT_treatment_code %in% c(1,3),1,0))
  )





## non ##
# by precaution, fixed effect for the district level: 
# on a juste converti le nom des districts en facteurs et dans des régressions ça pourra capture les effets fixes
data <- data %>%
  mutate(fes = factor(district))
# ça, ça va pas
# avoir deux variables : 1 pourfixed de l'un et 1 pour fixed effect de l'autre

fes <- c("fes")
## ##

# correction de la création des effets fixes
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

y <- c("INC05_running")


# weights NULL by default
# we can just omit it


################################################################################

## OTHER ARGUMENTS mentioned in the Git

# do we want to pool arms that are not the same
# set the boolean to TRUE or FALSE
# compare_to_zero

# then, what type of estimation function
# all of them are in the git
# so we have to write a string equal to what we want in the end

# the cutoff algorithm
# they suggest to keep it at NULL for now (because automatically suggested cutoff)


estimation_function_name <- "pval_OSE"
#estimation_function_name <- "pval_MSE"
cutoff <- NULL
result = do_TVA(data, arms, fes, y, cutoff, w=NULL, estimation_function_name)


# essai
temp <- data %>%
  filter(RES05_gender == 0)
fes <- NULL
result = do_TVA(temp, arms, fes, y, cutoff, w=NULL, estimation_function_name)


#unique(data$)


### NOTES 27 JUIN ###


# bien être sûre du format des fixed effects

# essayer avec et sans fixed effects
# essayer systématiquement et comparer les DEUX types d'estimation
# essayer des cutoff: NULL, 0, 1, être sûr que ça marche pour aucun en gros (voir si c un souci)


# LOOPS qu'on utilisera si ça fonctionne
# essayer différents outcomes
# faire une loop sur les différents outcomes de la table 1

# faire une loop sur les valeurs de RES05_gender







# regarder le support

