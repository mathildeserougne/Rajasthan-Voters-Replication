## BEGINNING OF THE DOUBLE LASSO ##


library(haven)
library(stringr)
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

# data
electoral_data <- read_dta("~/Electoral data cleaned.dta")
colnames(electoral_data)

# we want a list of potential controls
# based on the names of the variables, potential controls are: 

# candidate demographics
cand_vars <- names(electoral_data) %>% 
  str_subset("^CAND_") %>%                                
  str_subset("_votes|_voteshare|_winner|_won|_turnout|     # but not what happens after treatment
             _nbcands|_nbvotes|_herfindahl|_gps_|         
             _symbol_rank_|_rank_|_running|_decided_late|
             _votebank", negate = TRUE)

# political history
inc_vars <- names(electoral_data) %>% 
  str_subset("^INC(05|00)")                # INC05_* and INC00_*

# village characteristics
gp_vars <- names(electoral_data) %>% 
  str_subset("^GP_")

# past reservations
res_vars <- names(electoral_data) %>% 
  str_subset("^RES(05|00|10)")

# final list of potential controls
pool_controls <- unique(c(cand_vars, inc_vars, gp_vars, res_vars))

# Petite inspection
length(pool_controls)          
print(pool_controls)




# Now that we have this, we can test the controls for our studied outcomes in tables





