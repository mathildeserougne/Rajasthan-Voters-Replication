## DATA MANIP CODE - NEW ARMS

# comme on a des soucis sur la définition d'un support j'ai regardé en détail
# le souci c'est de réussir à isoler de la variation avec les activations de certaines caractéristiques des policies
# pbm: on avait codé avec une combinaison qui ne permettait pas d'illustrer ça

# donc ici, tentative avec d'autres bras
# gender et water sont encore les bras
# mais les valeurs sont spécifiées autrement



### THIS FORMATTING OF THE DATA WORKS.
### RUNNING THE TVA FUNCTION WORKS.



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

library(dplyr)


# Source to stock functions coded on Simon Yaspo's git (cloned needs to be cloned in your environment)
source("TVA/R/pooling.R")
source("TVA/R/support_estimation.R")
source("TVA/R/threshold_functions.R")
source("TVA/R/winners_curse.R")



# FORMATTING THE DATA #########################################################

electoral_data <- read_dta("Electoral data cleaned.dta")


data <- electoral_data %>%                                   
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    #gender_arm
    gender_arm = ifelse(INT_treatment_code %in% c(2, 4), 1, 0),
    
    #theme_arm (water, roads, nothing)
    theme_arm = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # routes
      INT_treatment_code %in% c(3, 4) ~ 2,   # eau
      TRUE                              ~ 0   # contrôle
    ),
    
    # as integer (just to be sure)
    gender_arm = as.integer(gender_arm),
    theme_arm  = as.integer(theme_arm),
    
    # district fixed effects
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI  = as.integer(district == "KARAULI")
  ) %>%
  select(INC05_running, gender_arm, theme_arm,
         district_DHOLPUR, district_KARAULI, RES05_gender, INC05_voteshare,
         INCSPOUSE05_running, INCSPOUSE05_voteshare,
         INCOTHER05_running, INCOTHER05_voteshare)

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")

data       = as.data.frame(data)


# check before running the tva function (how many cells? how many empty?)
with(data, table(gender_arm, theme_arm))
# six cells / one empty / 5 non empty situations (control + 4 combos)


## RUN TVA ####################################################################


# on y = the participation of the 2005 incumbent

result <- do_TVA(
  data       = data,   
  arms       = arms,                  
  y          = "INC05_running",
  fes        = fes,                   
  estim_func = "pval_MSE",
  cutoff     = NULL
)



# Aperçu
result$marginal_support
result$pools_summary




# LECTURE DES INTITULES DES POOLS
# p_0_0 = pure control
# p_1_1 = angle genre, illustration routes
# p_1_2 = angle genre, illustration eau
# p_0_1 = pas genre, illustration routes
# p_0_2 = pas genre, illustration eau


# ensuite la sortie va dire quel pool correspond à quelle policy unique



## WHAT THE RESULTS SUGGEST
# avoir 'gender' activé change significativement l'outcome
# ce n'est pas le cas du changement de thème (roads/water)
# pool créé: les cas où gender est activé, avec des thèmes infrastructures diffts


result$pooled_ols        # coefficient et se de chaque pool vs contrôle omis
result$winners_effect    # inutile puisque un seul pool

# CCL
# effet jugé intéressant par tva: qd pas d'axe gender.






# Running the function this time on another outcome (voteshare for the incumbent)


data <- electoral_data %>% 
  filter(RES10_gender == 0, SAMPLE_hhsurvey == 1,
         GP_tag == 1, INC05_can_run == 1) %>%
  mutate(
    # arms
    gender_arm = as.integer(INT_treatment_code %in% c(2, 4)),
    theme_arm  = case_when(
      INT_treatment_code %in% c(1, 2) ~ 1,   # routes
      INT_treatment_code %in% c(3, 4) ~ 2,   # eau
      TRUE                            ~ 0),
    # fe
    district_DHOLPUR = as.integer(district == "DHOLPUR"),
    district_KARAULI  = as.integer(district == "KARAULI")
  ) %>%
  select(INC05_voteshare, gender_arm, theme_arm,
         district_DHOLPUR, district_KARAULI)

arms <- c("gender_arm", "theme_arm")
fes  <- c("district_DHOLPUR", "district_KARAULI")


## Lancer TVA 
result_vs <- do_TVA(
  data       = as.data.frame(data),
  arms       = arms,
  y          = "INC05_voteshare",   
  fes        = fes,
  estim_func = "pval_OSE",       
  cutoff     = 0.20                
)

result_vs$marginal_support   
result_vs$pools_summary     
result_vs$pooled_ols         

# cette fois, les variations des deux bras ont un effet significatif (vu dans 01 et 12)
# pools faits: genre et routes ; eau ou routes sans genre ; contrôle + genre et eau
# le genre sans changer de thème ne change pas le résultat. donc poolés.
# le district n'est pas un facteur significatif

# présenter les résultats sur les infrastructures, sans argument de genre, pénalise le sortant.
# discussion sur les réalisations du mandat semblent compter dans le comportement électoral.
# genre+routes pénalise aussi, mais ça représente moins de sites donc moins puissant comme résultat.







## MEETING 07/07 ##


## ATTEMPT ON RES05_gender ##
# loop, changing the status of 2005 gender reservation

for (RES05_gender_loop in c(0,1)){
  data_loop <- data%>%filter(RES05_gender==RES05_gender_loop)
  result <- do_TVA(
    data       = data_loop,   
    arms       = arms,                  
    y          = "INC05_running",
    fes        = fes,                   
    estim_func = "pval_MSE",
    cutoff     = 0.1
  )
  print(result$pools_summary)
  print(result$pooled_ols)
}

#semble bloquer à la deuxième itération


## ATTEMPT ON y ##
# loop on all of the first table's outcomes
for(outcome in c("INC05_running", "INC05_voteshare",
                 "INCSPOUSE05_running", "INCSPOUSE05_voteshare",
                 "INCOTHER05_running", "INCOTHER05_voteshare")){
  result <- do_TVA(
    data       = data,   
    arms       = arms,                  
    y          = outcome,
    fes        = fes,                   
    estim_func = "pval_OSE",
    cutoff     = 0.1
  )
  print(result$pools_summary)
  print(result$pooled_ols)
}

# essayer de runner l'algorithme pour TOUS LES OUTCOMES qu'on a
# pas dans els tables avec performance
# table 1, 3, 4, 5
# le souci c'est qu'il se bloque dès qu'il trouve pas, serait logique de faire une loop si on arrive à forcer son arrêt
# sinon manuellement et on run pour chaque outcome

# OUTPUT
# pour chaque table, pour chaque colonne, au threshold x il a trouvé tel pool valide
# le faire sur tout l'échantillon et ensuite spliter selon la RES05_gender ==0 ou 1

# un peu comme trois panel par table
# whole sample, RES05_gender ==0, RES05_gender == 1

# quand pas de pooling, pas de pooling



# attemplt at looping on the tolerance threshold cutoff
for(cutoff in seq(from=0.05, to=0.1, by=0.05)){
  #  stuff, such as
  result <- do_TVA(
    data       = as.data.frame(data),   
    arms       = arms,                  
    y          = "INC05_running",
    fes        = fes,                   
    estim_func = "pval_MSE",
    cutoff     = cutoff
  )
  #result$marginal_support
  print(result$pools_summary)
  print(result$pooled_ols)
}

