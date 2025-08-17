## this time actually testing from all potential controls ##

options(max.print = 10000)   
print(colnames(data))

data <- read_dta("~/work/Electoral data cleaned.dta")

## ---------------- PARAMÈTRES ----------------
df <- data                   # ton data‑frame
keep2009 <- TRUE             # mets FALSE pour exclure aussi 2009
## --------------------------------------------

id_geo    <- "_code$|_name$|gps_|_lat|_lon|^district$|^ps$|^gp$|^BlockID$"
treat_int <- "^INT_|treatment|intervention"

outcome10 <- paste(
  "ELEC10_",          # tous les résultats électoraux 2010
  "CHAL_",            # challengers 2010
  "INC[0-9]{2}",      # gagnants sortants
  "winner|votes|voteshare|_won|_running|_nbcands|_herfindahl",
  sep = "|"
)

postsuffix <- "_post$|_dl$|_he$"     # marquages « post »
years_post <- "_201[0-9]"            # années 2010 à 2019 (précédées d’un _)
years2009  <- if (!keep2009) "_2009" else NULL  # à ajouter seulement si demandé

## >>> NE PAS LAISSER DE CHAÎNES VIDES ! <<<
exclude_rgx <- paste(
  Filter(Negate(is.null),
         c(id_geo, treat_int, outcome10, postsuffix, years_post, years2009)),
  collapse = "|"
)

controls_candidates <- colnames(df)[!grepl(exclude_rgx, colnames(df), ignore.case = TRUE)]

cat(length(controls_candidates), "variables conservées\n")
writeLines(controls_candidates, "controls_candidates.txt")                    


# vérification de la tête du truc

print(controls_candidates)



# checking which ones are non numeric
non_num <- names(which(!sapply(df[, controls_candidates], is.numeric)))
length(non_num)          # combien ?
head(non_num, 20)       
# those ones: 
# [1] "CAND_caste"      "CAND_jati"       "CAND_caste_sec"  "CAND_village"   [5] "RES10_caste_sec"

