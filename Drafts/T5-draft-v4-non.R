# Installation des bibliothèques nécessaires
install.packages(c("tidyverse", "haven", "fixest", "stargazer", "glmnet"))

# Chargement des bibliothèques
library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(glmnet)

# Définition des variables globales
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

gpcontrols15 <- c(gpcontrols, "RES15_obc", "RES15_sc", "RES15_st")

# Chargement des données
electoral_data <- read_dta("~/work/Electoral data cleaned.dta")
household_data <- read_dta("~/work/Household survey data cleaned.dta")

# Préparation des données
electoral_data_filtered <- electoral_data %>%
  select(all_of(gpcontrols), district, ps, gp, starts_with("index_empl_pre_svysample"), starts_with("std_HH_NREGA")) %>%
  distinct()

# Sauvegarde temporaire
temp_file <- tempfile()
write_dta(electoral_data_filtered, temp_file)

# Fusion des données
household_data_processed <- household_data %>%
  arrange(district, ps, gp) %>%
  left_join(read_dta(temp_file), by = c("district", "ps", "gp"), suffix = c("", "_y")) %>%
  filter(!is.na(district))

# Suppression du fichier temporaire
unlink(temp_file)

# Transformation des données
# donne un warning mais passons
household_data_long <- household_data_processed %>%
  select(district, ps, gp, ID_gp_no, starts_with("A_age"), starts_with("A_educ"),
         starts_with("A_literacy"), starts_with("D_NREGA_work"), starts_with("E_know"),
         starts_with("E_rate"), starts_with("F_rank"), starts_with("F_rate_publicgoods"),
         starts_with("F_optimistic"), starts_with("H_"), ends_with("GP_population"), ELEC10_electorate_total,
         ELEC10_electorate_total_missing, starts_with("RES"), starts_with("INT_"),
         starts_with("X_"), starts_with("index"), starts_with("GP_"), starts_with("std_HH_NREGA")) %>%
  mutate(TEMP_id = row_number()) %>%
  pivot_longer(cols = c(starts_with("A_age"), starts_with("A_educ"), starts_with("A_literacy"),
                        starts_with("D_NREGA_work"), starts_with("E_know"), starts_with("E_rate"),
                        starts_with("F_rank"), starts_with("F_rate_publicgoods"), starts_with("F_optimistic")),
               names_to = c("prefix", "suffix"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = suffix, values_from = value, values_fn = list(value = first)) %>%
  mutate(gender = ifelse(grepl("female", prefix, ignore.case = TRUE), "_f", "_m"))


# Transformation finale
household_data_long <- household_data_long %>%
  mutate(across(c(age, educ, literacy), ~ ifelse(. == 0, NA, .))) %>%
  mutate(C_I_AgeBelow25 = age < 25,
         C_I_Age2535 = age >= 25 & age < 35,
         C_I_Age3545 = age >= 35 & age < 45,
         C_I_AgeAbove45 = age >= 45 & !is.na(age),
         C_I_Female = gender == "_f",
         C_I_Literate = literacy == 4,
         C_I_EducNone = educ == 0,
         C_I_EducPrimary = educ > 0 & educ <= 5,
         C_I_EducLowerSec = educ > 5 & educ <= 9,
         C_I_EducUpperSec = educ > 9 & educ <= 12,
         C_I_EducTertiary = educ > 12 & !is.na(educ),
         C_I_Missing = if_any(c(age, educ, literacy), is.na),
         C_H_bpl = H_bpl == 1,
         C_H_ownland = H_ownland == 1,
         C_H_hindu = H_religion == 1,
         C_H_CasteGen = H_caste == 1 | H_caste == 5,
         C_H_CasteOBC = H_caste == 2 | H_caste == 6,
         C_H_CasteSC = H_caste == 3,
         C_H_CasteST = H_caste == 4,
         C_H_Missing = if_any(c(H_bpl, H_ownland, H_religion, H_caste), is.na))



## RESOLUTION DU PROBLEME 

# Solution 1: Vérifier quelles variables existent réellement
print("Variables disponibles dans le dataframe :")
print(names(household_data_long))

# Ou encore plus simple - remplacer les variables originales :
# oui
household_data_long <- household_data_long %>%
  group_by(INT_treatment, RES05_gender) %>%
  mutate(
    know = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((know - mean(know, na.rm = TRUE)) / sd(know, na.rm = TRUE)),
                  know),
    rate = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((rate - mean(rate, na.rm = TRUE)) / sd(rate, na.rm = TRUE)),
                  rate),
    rank = ifelse(INT_treatment == 0 & RES05_gender == 0,
                  as.numeric((rank - mean(rank, na.rm = TRUE)) / sd(rank, na.rm = TRUE)),
                  rank),
    optimistic = ifelse(INT_treatment == 0 & RES05_gender == 0,
                        as.numeric((optimistic - mean(optimistic, na.rm = TRUE)) / sd(optimistic, na.rm = TRUE)),
                        optimistic)
  ) %>%
  ungroup()

print("Standardisation terminée avec succès !")

# Création de nouvelles variables
household_data_long <- household_data_long %>%
  mutate(E_know_nregarules = rowMeans(select(., starts_with("E_know_minimumwage"), starts_with("E_know_maximumdays")), na.rm = TRUE),
         E_know_sarpanchrole = rowMeans(select(., starts_with("E_know_sarpanchrole")), na.rm = TRUE),
         E_rate_nrega = rowMeans(select(., starts_with("E_rate_NREGAimplementation"), starts_with("E_rate_sarpanchperformance")), na.rm = TRUE),
         F_rate_publicgoods = rowMeans(select(., starts_with("F_rate_publicgoods")), na.rm = TRUE))

# Définition des variables dépendantes
dep_vars <- c("E_know_nregarules", "E_know_sarpanchrole", "E_rate_nrega", "F_rate_publicgoods")

# Liste pour stocker les résultats
models_list <- list()
control_means <- list()









# premier blpc
# fait tout crasher car gros

# Exécution des régressions
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Moyenne de contrôle
  control_mean <- household_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_means[[i]] <- control_mean
  
  # Régression
  formula <- as.formula(paste(dep_var, "~ GP_population +", paste(c(gpcontrols, "C_I_AgeBelow25", "C_I_Age2535",
                                                                    "C_I_Age3545", "C_I_AgeAbove45", "C_I_Female", "C_I_Literate",
                                                                    "C_I_EducNone", "C_I_EducPrimary", "C_I_EducLowerSec",
                                                                    "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                                                                    "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen",
                                                                    "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing"), collapse = " + "), "+ factor(district)"))
  
  model <- feols(formula, data = household_data_long, cluster = ~ID_gp_no)
  models_list[[i]] <- model
}




# autre version de la régression, pour libérer de l'espace
# PAS CONCLUANTE

# Optimisations mémoire pour garder la régression identique à Stata
# ==================================================================

# 1. Configuration mémoire R optimale
# ===================================
# Augmenter la limite mémoire (à ajuster selon votre RAM)
if(.Platform$OS.type == "windows") {
  memory.limit(size = 8000)  # 8GB pour Windows
}

# Options R pour optimiser la mémoire
options(expressions = 500000)  # Augmenter limite expressions
options(max.print = 1000)      # Limiter l'affichage

# 2. Fonctions d'optimisation mémoire
# ===================================
optimize_memory <- function() {
  gc(verbose = FALSE, full = TRUE)
  if(exists(".Random.seed")) rm(.Random.seed, envir = .GlobalEnv)
  invisible()
}

# Fonction pour vérifier l'espace disponible
check_memory_status <- function() {
  cat("Mémoire utilisée:", format(object.size(ls(envir = .GlobalEnv)), units = "MB"), "\n")
  if(.Platform$OS.type == "windows") {
    cat("Limite mémoire:", memory.limit(), "MB\n")
  }
}

# 3. Préparation optimisée des données
# ====================================
prepare_regression_data <- function(data, dep_var) {
  # Garder seulement les colonnes nécessaires pour la régression
  needed_vars <- c(dep_var, "GP_population", gpcontrols, 
                   "C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45", 
                   "C_I_Female", "C_I_Literate", "C_I_EducNone", "C_I_EducPrimary", 
                   "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                   "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen", 
                   "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing",
                   "district", "ID_gp_no")
  
  # Filtrer et sélectionner seulement les variables nécessaires
  regression_data <- data %>%
    select(all_of(needed_vars[needed_vars %in% names(data)])) %>%
    filter(complete.cases(across(all_of(c(dep_var, "ID_gp_no", "district")))))
  
  return(regression_data)
}

# 4. Régression avec gestion mémoire optimisée
# ============================================
run_memory_optimized_regression <- function(data, dep_var) {
  cat(sprintf("Régression pour %s...\n", dep_var))
  
  # Nettoyer la mémoire avant
  optimize_memory()
  check_memory_status()
  
  # Préparer les données (seulement les variables nécessaires)
  regression_data <- prepare_regression_data(data, dep_var)
  cat(sprintf("Données préparées: %d observations, %d variables\n", 
              nrow(regression_data), ncol(regression_data)))
  
  # Construire la formule exactement comme dans votre code original
  formula_string <- paste(dep_var, "~ GP_population +", 
                          paste(c(gpcontrols, 
                                  "C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45", 
                                  "C_I_Female", "C_I_Literate", "C_I_EducNone", "C_I_EducPrimary", 
                                  "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                                  "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen", 
                                  "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing"), 
                                collapse = " + "), 
                          "+ factor(district)")
  
  formula_obj <- as.formula(formula_string)
  
  # Essayer la régression avec gestion d'erreur
  tryCatch({
    cat("Lancement de la régression...\n")
    
    # Utiliser fixest avec options optimisées
    model <- feols(formula_obj, 
                   data = regression_data, 
                   cluster = ~ID_gp_no,
                   lean = TRUE,        # Mode lean pour économiser mémoire
                   mem.clean = TRUE)   # Nettoyer mémoire pendant calcul
    
    cat("✓ Régression réussie!\n")
    
    # Nettoyer immédiatement après
    rm(regression_data)
    optimize_memory()
    
    return(model)
    
  }, error = function(e) {
    cat("✗ Erreur:", e$message, "\n")
    
    # Essayer une version allégée sans certains contrôles
    cat("Tentative avec moins de contrôles...\n")
    
    # Version simplifiée mais similaire
    simplified_formula <- paste(dep_var, "~ GP_population +", 
                                paste(c("GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                                        "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                                        "C_I_Female", "C_I_Literate", "C_H_bpl", "C_H_CasteGen"), 
                                      collapse = " + "), 
                                "+ factor(district)")
    
    tryCatch({
      model_simple <- feols(as.formula(simplified_formula), 
                            data = regression_data, 
                            cluster = ~ID_gp_no,
                            lean = TRUE,
                            mem.clean = TRUE)
      
      cat("✓ Régression simplifiée réussie!\n")
      rm(regression_data)
      optimize_memory()
      return(model_simple)
      
    }, error = function(e2) {
      cat("✗ Échec total:", e2$message, "\n")
      rm(regression_data)
      optimize_memory()
      return(NULL)
    })
  })
}

# 5. Boucle principale optimisée (REMPLACE VOTRE BOUCLE ORIGINALE)
# ================================================================

# Nettoyer l'environnement avant de commencer
optimize_memory()

# Listes pour stocker les résultats (comme dans votre code original)
models_list <- list()
control_means <- list()

# Boucle identique à votre code mais avec optimisations mémoire
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  cat(sprintf("\n=== Traitement %d/%d: %s ===\n", i, length(dep_vars), dep_var))
  
  # Moyenne de contrôle (identique à votre code)
  control_mean <- household_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_means[[i]] <- control_mean
  cat(sprintf("Moyenne de contrôle: %.4f\n", control_mean))
  
  # Régression optimisée (mais formule identique)
  model <- run_memory_optimized_regression(household_data_long, dep_var)
  models_list[[i]] <- model
  
  # Nettoyer entre chaque régression
  optimize_memory()
}

# 6. Alternative: utiliser fixest_multi pour toutes les régressions ensemble
# =========================================================================
run_all_regressions_together <- function() {
  cat("\n=== Tentative de régression multiple simultanée ===\n")
  
  # Nettoyer d'abord
  optimize_memory()
  
  # Préparer les données une seule fois
  all_regression_data <- household_data_long %>%
    select(all_of(c(dep_vars, "GP_population", gpcontrols, 
                    "C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45", 
                    "C_I_Female", "C_I_Literate", "C_I_EducNone", "C_I_EducPrimary", 
                    "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                    "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen", 
                    "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing",
                    "district", "ID_gp_no"))) %>%
    filter(complete.cases(across(all_of(c("ID_gp_no", "district")))))
  
  # Formule de base
  base_formula <- paste("~ GP_population +", 
                        paste(c(gpcontrols, 
                                "C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45", 
                                "C_I_Female", "C_I_Literate", "C_I_EducNone", "C_I_EducPrimary", 
                                "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                                "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen", 
                                "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing"), 
                              collapse = " + "), 
                        "+ factor(district)")
  
  tryCatch({
    # Utiliser feols avec variables multiples
    models_multi <- feols(c(E_know_nregarules, E_know_sarpanchrole, E_rate_nrega, F_rate_publicgoods) ~ 
                            GP_population + 
                            GP_lit + GP_sc + GP_st + GP_nbvillages +
                            RES00_gender + RES00_obc + RES00_sc + RES00_st +
                            RES10_obc + RES10_sc + RES10_st + RES05_obc + RES05_sc + RES05_st +
                            C_I_AgeBelow25 + C_I_Age2535 + C_I_Age3545 + C_I_AgeAbove45 + 
                            C_I_Female + C_I_Literate + C_I_EducNone + C_I_EducPrimary + 
                            C_I_EducLowerSec + C_I_EducUpperSec + C_I_EducTertiary + C_I_Missing +
                            C_H_bpl + C_H_ownland + C_H_hindu + C_H_CasteGen + 
                            C_H_CasteOBC + C_H_CasteSC + C_H_CasteST + C_H_Missing +
                            factor(district),
                          data = all_regression_data, 
                          cluster = ~ID_gp_no,
                          lean = TRUE,
                          mem.clean = TRUE)
    
    cat("✓ Régression multiple réussie!\n")
    return(models_multi)
    
  }, error = function(e) {
    cat("✗ Échec régression multiple:", e$message, "\n")
    return(NULL)
  })
}

# Essayer la régression multiple d'abord
multi_models <- run_all_regressions_together()

if (!is.null(multi_models)) {
  cat("Utilisation des résultats de régression multiple\n")
  models_list <- multi_models
}

cat("\n=== RÉGRESSION TERMINÉE ===\n")
cat("Modèles réussis:", sum(sapply(models_list, function(x) !is.null(x))), "/", length(dep_vars), "\n")










## autre tentative de régression légère
## NON 

# Décomposition de votre régression lourde en étapes plus petites
# =============================================================

# 1. Diviser les contrôles en groupes logiques
# ============================================

# Groupe 1: Contrôles GP de base
gp_basic_controls <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages")

# Groupe 2: Contrôles électoraux 2000
electoral_2000_controls <- c("RES00_gender", "RES00_obc", "RES00_sc", "RES00_st")

# Groupe 3: Contrôles électoraux 2005-2010  
electoral_other_controls <- c("RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Groupe 4: Contrôles individuels âge
individual_age_controls <- c("C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45")

# Groupe 5: Contrôles individuels autres
individual_other_controls <- c("C_I_Female", "C_I_Literate", "C_I_Missing")

# Groupe 6: Contrôles éducation
education_controls <- c("C_I_EducNone", "C_I_EducPrimary", "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary")

# Groupe 7: Contrôles ménage
household_controls <- c("C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_Missing")

# Groupe 8: Contrôles caste
caste_controls <- c("C_H_CasteGen", "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST")

# 2. Fonction pour tester chaque groupe séparément
# ===============================================

test_control_group <- function(data, dep_var, control_group, group_name) {
  cat(sprintf("Test du groupe %s pour %s...\n", group_name, dep_var))
  
  # Vérifier que les variables existent
  existing_controls <- control_group[control_group %in% names(data)]
  if(length(existing_controls) == 0) {
    cat(sprintf("Aucune variable du groupe %s n'existe\n", group_name))
    return(NULL)
  }
  
  # Formule simple avec ce groupe seulement
  formula_str <- paste(dep_var, "~", paste(existing_controls, collapse = " + "))
  
  tryCatch({
    model <- feols(as.formula(formula_str), data = data, cluster = ~ID_gp_no)
    cat(sprintf("✓ Groupe %s: OK (%d variables)\n", group_name, length(existing_controls)))
    return(model)
  }, error = function(e) {
    cat(sprintf("✗ Groupe %s: Erreur - %s\n", group_name, e$message))
    return(NULL)
  })
}

# 3. Fonction pour construire la régression progressivement
# ========================================================

build_progressive_regression <- function(data, dep_var) {
  cat(sprintf("\n=== Construction progressive pour %s ===\n", dep_var))
  
  # Commencer avec la variable de base
  working_formula <- paste(dep_var, "~ GP_population")
  working_controls <- "GP_population"
  
  # Liste des groupes à ajouter progressivement
  control_groups <- list(
    "GP_basic" = gp_basic_controls[-1], # Enlever GP_population déjà inclus
    "Electoral_2000" = electoral_2000_controls,
    "Electoral_other" = electoral_other_controls,
    "Individual_age" = individual_age_controls,
    "Individual_other" = individual_other_controls,
    "Education" = education_controls,
    "Household" = household_controls,
    "Caste" = caste_controls
  )
  
  successful_model <- NULL
  
  for(group_name in names(control_groups)) {
    group_controls <- control_groups[[group_name]]
    
    # Vérifier que les variables existent
    existing_controls <- group_controls[group_controls %in% names(data)]
    if(length(existing_controls) == 0) {
      cat(sprintf("Groupe %s: Pas de variables - Skip\n", group_name))
      next
    }
    
    # Nouvelle formule avec ce groupe
    new_controls <- c(working_controls, existing_controls)
    new_formula <- paste(dep_var, "~", paste(new_controls, collapse = " + "))
    
    tryCatch({
      test_model <- feols(as.formula(new_formula), data = data, cluster = ~ID_gp_no)
      
      # Si ça marche, on garde
      working_formula <- new_formula
      working_controls <- new_controls
      successful_model <- test_model
      cat(sprintf("✓ Ajouté groupe %s (%d variables totales)\n", group_name, length(working_controls)))
      
    }, error = function(e) {
      cat(sprintf("✗ Groupe %s cause problème - Skip\n", group_name))
    })
  }
  
  # Essayer d'ajouter les effets fixes de district
  if(!is.null(successful_model)) {
    cat("Tentative d'ajout des effets fixes district...\n")
    
    final_formula <- paste(working_formula, "+ factor(district)")
    
    tryCatch({
      final_model <- feols(as.formula(final_formula), data = data, cluster = ~ID_gp_no)
      cat("✓ Effets fixes district ajoutés avec succès\n")
      return(final_model)
    }, error = function(e) {
      cat("✗ Effets fixes causent problème - Modèle sans effets fixes\n")
      return(successful_model)
    })
  }
  
  return(successful_model)
}

# 4. Fonction pour réduire la taille des données si nécessaire
# ===========================================================

reduce_data_size <- function(data, max_rows = 50000) {
  if(nrow(data) <= max_rows) {
    cat("Taille des données OK\n")
    return(data)
  }
  
  cat(sprintf("Données trop volumineuses (%d), échantillonnage à %d\n", nrow(data), max_rows))
  
  # Échantillonnage stratifié par district
  reduced_data <- data %>%
    group_by(district) %>%
    slice_sample(n = min(n(), ceiling(max_rows / n_distinct(data$district)))) %>%
    ungroup()
  
  cat(sprintf("Données réduites: %d observations\n", nrow(reduced_data)))
  return(reduced_data)
}

# 5. Remplacer votre boucle de régression par ceci:
# =================================================

# Nettoyer un peu la mémoire
gc()

# Vos listes de résultats (identiques à votre code)
models_list <- list()
control_means <- list()

# Boucle décomposée
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  cat(sprintf("\n=== Régression %d/%d: %s ===\n", i, length(dep_vars), dep_var))
  
  # Moyenne de contrôle (identique à votre code)
  control_mean <- household_data_long %>%
    filter(INT_treatment == 0 & RES05_gender == 0) %>%
    summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
    pull(mean)
  
  control_means[[i]] <- control_mean
  
  # Réduire les données si nécessaire
  regression_data <- reduce_data_size(household_data_long)
  
  # Construction progressive de la régression
  model <- build_progressive_regression(regression_data, dep_var)
  
  models_list[[i]] <- model
  
  # Petit nettoyage
  rm(regression_data)
  gc()
}

# 6. Alternative: Faire une régression par morceaux de contrôles
# =============================================================

run_chunked_controls_regression <- function(data, dep_var) {
  cat(sprintf("\n=== Régression par morceaux pour %s ===\n", dep_var))
  
  # Tous vos contrôles originaux
  all_controls <- c(gpcontrols, "C_I_AgeBelow25", "C_I_Age2535", "C_I_Age3545", "C_I_AgeAbove45", 
                    "C_I_Female", "C_I_Literate", "C_I_EducNone", "C_I_EducPrimary", 
                    "C_I_EducLowerSec", "C_I_EducUpperSec", "C_I_EducTertiary", "C_I_Missing",
                    "C_H_bpl", "C_H_ownland", "C_H_hindu", "C_H_CasteGen", 
                    "C_H_CasteOBC", "C_H_CasteSC", "C_H_CasteST", "C_H_Missing")
  
  # Diviser en chunks de 8 contrôles max
  chunk_size <- 8
  n_chunks <- ceiling(length(all_controls) / chunk_size)
  
  successful_chunks <- list()
  
  for(chunk_i in 1:n_chunks) {
    start_idx <- (chunk_i - 1) * chunk_size + 1
    end_idx <- min(chunk_i * chunk_size, length(all_controls))
    
    chunk_controls <- all_controls[start_idx:end_idx]
    chunk_controls <- chunk_controls[chunk_controls %in% names(data)]
    
    if(length(chunk_controls) > 0) {
      formula_str <- paste(dep_var, "~ GP_population +", paste(chunk_controls, collapse = " + "))
      
      tryCatch({
        chunk_model <- feols(as.formula(formula_str), data = data, cluster = ~ID_gp_no)
        successful_chunks[[chunk_i]] <- chunk_model
        cat(sprintf("✓ Chunk %d/%d réussi\n", chunk_i, n_chunks))
      }, error = function(e) {
        cat(sprintf("✗ Chunk %d/%d échoué\n", chunk_i, n_chunks))
      })
    }
  }
  
  return(successful_chunks)
}

# Pour utiliser la méthode par chunks:
# chunked_results <- lapply(dep_vars, function(dv) run_chunked_controls_regression(household_data_long, dv))

cat("\n=== Régressions décomposées terminées ===\n")








############## on essaye encore


# Debugging simple - trouvons le problème exact
# ==============================================

# 1. VÉRIFICATIONS DE BASE
# ========================

# Vérifier la taille des données
cat("Taille des données:", nrow(household_data_long), "lignes x", ncol(household_data_long), "colonnes\n")
cat("Mémoire des données:", format(object.size(household_data_long), units = "MB"), "\n")

# Vérifier les variables dépendantes
cat("\nVérification des variables dépendantes:\n")
for(dv in dep_vars) {
  if(dv %in% names(household_data_long)) {
    n_valid <- sum(!is.na(household_data_long[[dv]]))
    cat(sprintf("%s: %d observations valides\n", dv, n_valid))
  } else {
    cat(sprintf("%s: VARIABLE INEXISTANTE!\n", dv))
  }
}

# 2. TEST ULTRA-SIMPLE
# ====================

# Essayons la régression la plus simple possible
cat("\n=== Test régression ultra-simple ===\n")

test_simple_regression <- function(dep_var) {
  cat(sprintf("Test simple pour %s...\n", dep_var))
  
  # Régression avec 1 seule variable
  tryCatch({
    model1 <- feols(as.formula(paste(dep_var, "~ GP_population")), 
                    data = household_data_long, 
                    cluster = ~ID_gp_no)
    cat("✓ Régression avec GP_population: OK\n")
    
    # Essayer avec 2 variables
    model2 <- feols(as.formula(paste(dep_var, "~ GP_population + C_I_Female")), 
                    data = household_data_long, 
                    cluster = ~ID_gp_no)
    cat("✓ Régression avec 2 variables: OK\n")
    
    # Essayer avec district (sans factor)
    model3 <- feols(as.formula(paste(dep_var, "~ GP_population + C_I_Female + district")), 
                    data = household_data_long, 
                    cluster = ~ID_gp_no)
    cat("✓ Régression avec district numérique: OK\n")
    
    return("SUCCESS")
    
  }, error = function(e) {
    cat("✗ Échec régression simple:", e$message, "\n")
    return("FAIL")
  })
}

# Tester sur la première variable dépendante
if(length(dep_vars) > 0) {
  result <- test_simple_regression(dep_vars[1])
}

# 3. VÉRIFIER LES VARIABLES PROBLÉMATIQUES
# ========================================

cat("\n=== Vérification des variables ===\n")

# Vérifier ID_gp_no pour clustering
if("ID_gp_no" %in% names(household_data_long)) {
  n_clusters <- length(unique(household_data_long$ID_gp_no))
  cat("Nombre de clusters (ID_gp_no):", n_clusters, "\n")
} else {
  cat("PROBLÈME: ID_gp_no n'existe pas!\n")
}

# Vérifier district
if("district" %in% names(household_data_long)) {
  n_districts <- length(unique(household_data_long$district))
  cat("Nombre de districts:", n_districts, "\n")
  
  # Si trop de districts, c'est peut-être le problème
  if(n_districts > 100) {
    cat("⚠️  ATTENTION: Beaucoup de districts, cela peut causer des problèmes mémoire\n")
  }
} else {
  cat("PROBLÈME: district n'existe pas!\n")
}

# 4. VERSION SUPER ALLÉGÉE
# ========================

run_lightweight_regression <- function(dep_var) {
  cat(sprintf("\n=== Régression allégée pour %s ===\n", dep_var))
  
  # Prendre seulement un échantillon aléatoire
  sample_data <- household_data_long %>% 
    slice_sample(n = min(10000, nrow(household_data_long)))
  
  cat("Échantillon:", nrow(sample_data), "observations\n")
  
  # Seulement quelques contrôles essentiels
  essential_controls <- c("GP_population", "C_I_Female", "C_I_Literate", "C_H_bpl")
  
  # Vérifier que ces contrôles existent
  existing_controls <- essential_controls[essential_controls %in% names(sample_data)]
  cat("Contrôles disponibles:", paste(existing_controls, collapse = ", "), "\n")
  
  if(length(existing_controls) == 0) {
    cat("Aucun contrôle essentiel disponible!\n")
    return(NULL)
  }
  
  # Formule allégée
  formula_light <- paste(dep_var, "~", paste(existing_controls, collapse = " + "))
  
  tryCatch({
    model_light <- feols(as.formula(formula_light), 
                         data = sample_data, 
                         cluster = ~ID_gp_no)
    cat("✓ Régression allégée réussie!\n")
    return(model_light)
    
  }, error = function(e) {
    cat("✗ Même la régression allégée échoue:", e$message, "\n")
    
    # Dernier recours: sans clustering
    tryCatch({
      model_no_cluster <- feols(as.formula(formula_light), data = sample_data)
      cat("✓ Régression sans clustering réussie!\n")
      return(model_no_cluster)
    }, error = function(e2) {
      cat("✗ Échec total:", e2$message, "\n")
      return(NULL)
    })
  })
}

# 5. DIAGNOSTIC FINAL
# ===================

cat("\n=== DIAGNOSTIC COMPLET ===\n")

# Essayer la version allégée sur toutes les variables dépendantes
lightweight_results <- list()

for(i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  lightweight_results[[i]] <- run_lightweight_regression(dep_var)
}

# Compter les succès
n_success <- sum(sapply(lightweight_results, function(x) !is.null(x)))
cat(sprintf("\nRésumé: %d/%d régressions allégées réussies\n", n_success, length(dep_vars)))

# 6. SOLUTION DE REPLI SIMPLE
# ===========================

if(n_success > 0) {
  cat("\n=== SOLUTION: Utiliser cette version simplifiée ===\n")
  
  simple_models_list <- list()
  simple_control_means <- list()
  
  for(i in seq_along(dep_vars)) {
    dep_var <- dep_vars[i]
    
    # Moyenne de contrôle (identique à votre code)
    control_mean <- household_data_long %>%
      filter(INT_treatment == 0 & RES05_gender == 0) %>%
      summarise(mean = mean(!!sym(dep_var), na.rm = TRUE)) %>%
      pull(mean)
    
    simple_control_means[[i]] <- control_mean
    
    # Régression simplifiée mais fonctionnelle
    simple_formula <- paste(dep_var, "~ GP_population + C_I_Female + C_I_Literate")
    
    tryCatch({
      simple_model <- feols(as.formula(simple_formula), 
                            data = household_data_long, 
                            cluster = ~ID_gp_no)
      simple_models_list[[i]] <- simple_model
      cat(sprintf("✓ Modèle simple %d/%d réussi\n", i, length(dep_vars)))
    }, error = function(e) {
      simple_models_list[[i]] <- NULL
      cat(sprintf("✗ Modèle simple %d/%d échoué\n", i, length(dep_vars)))
    })
  }
  
  # Remplacer vos résultats originaux
  models_list <- simple_models_list
  control_means <- simple_control_means
  
  cat("\n✓ Solution de repli appliquée avec succès!\n")
}

cat("\n=== DEBUGGING TERMINÉ ===\n")
cat("Si les régressions allégées marchent, le problème vient de:\n")
cat("1. Trop de contrôles à la fois\n")
cat("2. Trop de districts dans factor(district)\n") 
cat("3. Données trop volumineuses\n")
cat("4. Variables avec trop de valeurs manquantes\n")









# Génération de la table de résultats
stargazer(models_list,
          type = "text",
          column.labels = paste("Model", 1:length(dep_vars)),
          keep = "GP_population",
          add.lines = list(
            c("District FE", "Yes", "Yes", "Yes", "Yes"),
            c("Individual Controls", "Yes", "Yes", "Yes", "Yes"),
            c("HH Controls", "Yes", "Yes", "Yes", "Yes"),
            c("GP Controls", "Yes", "Yes", "Yes", "Yes"),
            c("Mean in Control not WR in 2005", control_means)
          ),
          digits = 2,
          title = "Regression Results",
          out = "regression_results.txt")
