### REPLICATION TA5: TA5 - Reservation - 2010 ###############################


## PAS ENCORE
# NUL



## PREMIERE VERSION DONNE PRESQUE LA BONNE CHOSE MAIS PAS EXACTEMENT JE SAIS PAS QUOI DIRE

# Charger les packages nécessaires
library(haven)
library(dplyr)
library(texreg)

# Importer les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données selon les critères spécifiés
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1) %>%
  mutate(RES_reserved_gender_once = ifelse(RES05_gender + RES00_gender == 1, 1, 0))

# Définir les variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc",
                "RES05_st", "RES00_obc", "RES00_sc", "RES00_st")

with_incumbent_dep_vars <- c("ELEC10_prop_female", "ELEC10_voteshare_female", "ELEC10_won_female")
without_incumbent_dep_vars <- c("CHAL00_prop_female", "CHAL00_voteshare_female", "CHAL00_won_female")

# Créer une liste pour stocker les résultats
results <- list()

# Exécuter les régressions
for (dep_var in c(with_incumbent_dep_vars, without_incumbent_dep_vars)) {
  # Calculer la moyenne de contrôle
  control_data <- data %>% filter(RES05_gender == 0 & RES00_gender == 0)
  control_mean <- mean(control_data[[dep_var]], na.rm = TRUE)
  
  # Modèle avec les variables de contrôle
  model1 <- try(lm(reformulate(c("RES00_gender", "RES05_gender", gpcontrols), dep_var), data = data))
  model2 <- try(lm(reformulate(c("RES_reserved_gender_once", gpcontrols), dep_var), data = data))
  
  # Vérifiez si les modèles ont été estimés correctement
  if (class(model1) != "try-error" && class(model2) != "try-error") {
    results[[dep_var]] <- list(model1 = model1, model2 = model2, control_mean = control_mean)
  } else {
    message("Erreur dans l'estimation du modèle pour ", dep_var)
  }
}

# Utiliser texreg pour générer un tableau de résultats
models_to_display <- list()

# Ajouter les modèles à la liste
for (dep_var in names(results)) {
  # Vérifiez que les éléments sont des modèles avant de les ajouter
  if ("lm" %in% class(results[[dep_var]]$model1)) {
    models_to_display <- c(models_to_display, list(results[[dep_var]]$model1))
  }
  if ("lm" %in% class(results[[dep_var]]$model2)) {
    models_to_display <- c(models_to_display, list(results[[dep_var]]$model2))
  }
}

# Vérifiez que la liste n'est pas vide
if (length(models_to_display) == 0) {
  stop("La liste models_to_display est vide. Vérifiez les résultats des modèles.")
} else {
  # Générer le tableau
  screenreg(models_to_display)
}


################################################################################


## DEUXIEME VERSION - ne donne qu'un tableau dégueu 


# Charger les packages nécessaires
library(haven)
library(dplyr)

# Importer les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données selon les critères spécifiés
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1) %>%
  mutate(RES_reserved_gender_once = ifelse(RES05_gender + RES00_gender == 1, 1, 0))

# Définir les variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc",
                "RES05_st", "RES00_obc", "RES00_sc", "RES00_st")

with_incumbent_dep_vars <- c("ELEC10_prop_female", "ELEC10_voteshare_female", "ELEC10_won_female")
without_incumbent_dep_vars <- c("CHAL00_prop_female", "CHAL00_voteshare_female", "CHAL00_won_female")

# Créer un fichier texte pour écrire les résultats
file_path <- "~/work/ta5-results.txt"
file_conn <- file(file_path, "w")

# Exécuter les régressions et écrire les résultats dans un fichier texte
for (dep_var in c(with_incumbent_dep_vars, without_incumbent_dep_vars)) {
  # Calculer la moyenne de contrôle
  control_data <- data %>% filter(RES05_gender == 0 & RES00_gender == 0)
  control_mean <- mean(control_data[[dep_var]], na.rm = TRUE)
  
  # Modèle avec les variables de contrôle
  model1 <- lm(reformulate(c("RES00_gender", "RES05_gender", gpcontrols), dep_var), data = data)
  model2 <- lm(reformulate(c("RES_reserved_gender_once", gpcontrols), dep_var), data = data)
  
  # Écrire les résultats dans le fichier texte
  writeLines(c(paste("Variable dépendante:", dep_var), paste("Moyenne de contrôle:", control_mean), ""), file_conn)
  
  writeLines(c("Modèle 1:", capture.output(summary(model1))), file_conn)
  writeLines("", file_conn)
  
  writeLines(c("Modèle 2:", capture.output(summary(model2))), file_conn)
  writeLines("", file_conn)
}

# Fermer la connexion au fichier
close(file_conn)


################################################################################

## TROISIEME VERSION


# Charger les packages nécessaires
library(haven)
library(dplyr)
library(broom)

# Importer les données
data <- read_dta("~/work/Electoral data cleaned.dta")

# Filtrer les données selon les critères spécifiés
data <- data %>%
  filter(RES10_gender == 0 & GP_tag == 1) %>%
  mutate(RES_reserved_gender_once = ifelse(RES05_gender + RES00_gender == 1, 1, 0))

# Définir les variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc",
                "RES05_st", "RES00_obc", "RES00_sc", "RES00_st")

with_incumbent_dep_vars <- c("ELEC10_prop_female", "ELEC10_voteshare_female", "ELEC10_won_female")
without_incumbent_dep_vars <- c("CHAL00_prop_female", "CHAL00_voteshare_female", "CHAL00_won_female")

# Créer un fichier texte pour écrire les résultats
file_path <- "~/work/TA5 - results.txt"
file_conn <- file(file_path, "w")

# Écrire les en-têtes et les descriptions
writeLines("RES00_gender: Women reservation status in 2000", file_conn)
writeLines("RES05_gender: Women reservation status in 2005", file_conn)
writeLines("RES_reserved_gender_ever: Women reservation in 2000 OR 2005", file_conn)
writeLines(paste(rep("-", 80), collapse = ""), file_conn)

# Exécuter les régressions et écrire les résultats dans un fichier texte
for (dep_var in c(with_incumbent_dep_vars, without_incumbent_dep_vars)) {
  # Calculer la moyenne de contrôle
  control_data <- data %>% filter(RES05_gender == 0 & RES00_gender == 0)
  control_mean <- mean(control_data[[dep_var]], na.rm = TRUE)
  
  # Modèle avec les variables de contrôle
  model1 <- lm(reformulate(c("RES00_gender", "RES05_gender", gpcontrols), dep_var), data = data)
  model2 <- lm(reformulate(c("RES_reserved_gender_once", gpcontrols), dep_var), data = data)
  
  # Écrire les résultats pour chaque modèle
  for (model in list(model1, model2)) {
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    
    # Extraire les coefficients pour les variables d'intérêt
    res00_gender <- tidy_model %>% filter(term == "RES00_gender") %>% pull(estimate)
    res05_gender <- tidy_model %>% filter(term == "RES05_gender") %>% pull(estimate)
    res_reserved_gender_ever <- tidy_model %>% filter(term == "RES_reserved_gender_once") %>% pull(estimate)
    
    # Écrire les résultats formatés
    writeLines(paste("Variable dépendante:", dep_var), file_conn)
    writeLines(paste("RES00_gender:", round(res00_gender, 4)), file_conn)
    writeLines(paste("RES05_gender:", round(res05_gender, 4)), file_conn)
    writeLines(paste("RES_reserved_gender_ever:", round(res_reserved_gender_ever, 4)), file_conn)
    writeLines(paste("Observations:", glance_model$nobs), file_conn)
    writeLines(paste("R-squared:", round(glance_model$r.squared, 4)), file_conn)
    writeLines("District FE: Yes", file_conn)
    writeLines("GP controls: Yes", file_conn)
    writeLines(paste("Mean in GP not WR in 2005 nor 2000:", round(control_mean, 4)), file_conn)
    writeLines(paste(rep("-", 80), collapse = ""), file_conn)
  }
}

# Fermer la connexion au fichier
close(file_conn)















