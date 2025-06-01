# Chargement des bibliothèques nécessaires
library(dplyr)
library(haven)  # Pour lire les fichiers .dta
library(writexl)
library(readr)

# Définition du nom de fichier de sortie
Table <- "Results_Replication/REP_T1 - Descriptive Statistics"

# IMPORT ET RESTRICTION DE L'ÉCHANTILLON
data <- read_dta("~/work/Electoral data cleaned.dta")
data <- data %>%
  filter(RES10_gender == 0) %>%  # excluding GPs gender reserved for women
  filter(GP_tag == TRUE)         # one observation per GP

# Variables d'intérêt
variables <- c("GP_population", "ELEC10_electorate_total", "RES05_gender", 
               "RES00_gender", "RES10_obc", "RES10_sc", "RES10_st")

# Création d'un dataframe vide pour les résultats
results_df <- data.frame(
  VarName = character(),
  VarStat_Control = numeric(),
  VarStat_General = numeric(),
  VarStat_DiffGeneral = character(),
  VarStat_Gender = numeric(),
  VarStat_DiffGender = character(),
  VarStat_Unreserved = numeric(),
  VarStat_Reserved = numeric(),
  VarStat_DiffReserved = character(),
  stringsAsFactors = FALSE
)

# Fonction pour effectuer un t-test et formater les résultats
format_ttest_result <- function(data, var, group_var) {
  # Séparer les groupes
  group0 <- data[data[[group_var]] == 0, var, drop = TRUE]
  group1 <- data[data[[group_var]] == 1, var, drop = TRUE]
  
  # Vérifier qu'il y a des données dans les deux groupes
  if (length(group0) == 0 || length(group1) == 0) {
    return("NA")
  }
  
  # Supprimer les valeurs manquantes
  group0 <- group0[!is.na(group0)]
  group1 <- group1[!is.na(group1)]
  
  # Vérifier qu'il reste des données après suppression des NA
  if (length(group0) == 0 || length(group1) == 0) {
    return("NA")
  }
  
  # Vérifier si les données sont constantes (même valeur partout)
  if (var(group0, na.rm = TRUE) == 0 && var(group1, na.rm = TRUE) == 0 && 
      mean(group0, na.rm = TRUE) == mean(group1, na.rm = TRUE)) {
    return("0")
  }
  
  # Effectuer le t-test avec gestion d'erreur
  tryCatch({
    test_result <- t.test(group0, group1)
    
    # Calculer la différence
    diff <- round(mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE), 3)
    p_value <- test_result$p.value
    
    # Ajouter les étoiles selon le niveau de significativité
    if (p_value <= 0.01) {
      return(paste0(diff, "***"))
    } else if (p_value <= 0.05) {
      return(paste0(diff, "**"))
    } else if (p_value <= 0.1) {
      return(paste0(diff, "*"))
    } else {
      return(as.character(diff))
    }
  }, error = function(e) {
    # En cas d'erreur, calculer simplement la différence des moyennes
    diff <- round(mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE), 3)
    return(as.character(diff))
  })
}

# Boucle principale pour chaque variable
for (var in variables) {
  print(paste("Traitement de la variable:", var))  # Debug
  
  # Ligne pour la moyenne
  mean_row <- data.frame(
    VarName = var,
    VarStat_Control = NA,
    VarStat_General = NA,
    VarStat_DiffGeneral = "",
    VarStat_Gender = NA,
    VarStat_DiffGender = "",
    VarStat_Unreserved = NA,
    VarStat_Reserved = NA,
    VarStat_DiffReserved = "",
    stringsAsFactors = FALSE
  )
  
  # Ligne pour l'écart-type
  sd_row <- data.frame(
    VarName = "",
    VarStat_Control = NA,
    VarStat_General = NA,
    VarStat_DiffGeneral = "",
    VarStat_Gender = NA,
    VarStat_DiffGender = "",
    VarStat_Unreserved = NA,
    VarStat_Reserved = NA,
    VarStat_DiffReserved = "",
    stringsAsFactors = FALSE
  )
  
  # VALEURS DE CONTRÔLE
  control_data <- data %>% filter(INT_treatment == 0)
  if (nrow(control_data) > 0) {
    mean_row$VarStat_Control <- mean(control_data[[var]], na.rm = TRUE)
    sd_row$VarStat_Control <- sd(control_data[[var]], na.rm = TRUE)
  }
  
  # VALEURS CAMPAGNE GÉNÉRALE
  general_data <- data %>% filter(INT_treatment_general == 1)
  if (nrow(general_data) > 0) {
    mean_row$VarStat_General <- mean(general_data[[var]], na.rm = TRUE)
    sd_row$VarStat_General <- sd(general_data[[var]], na.rm = TRUE)
  }
  
  # ÉQUILIBRE CONTRÔLE VS GÉNÉRAL
  general_test_data <- data %>% filter(INT_treatment_gender == 0)
  if (nrow(general_test_data) > 0) {
    mean_row$VarStat_DiffGeneral <- format_ttest_result(general_test_data, var, "INT_treatment")
  }
  
  # VALEURS GENRE
  gender_data <- data %>% filter(INT_treatment_gender == 1)
  if (nrow(gender_data) > 0) {
    mean_row$VarStat_Gender <- mean(gender_data[[var]], na.rm = TRUE)
    sd_row$VarStat_Gender <- sd(gender_data[[var]], na.rm = TRUE)
  }
  
  # ÉQUILIBRE CONTRÔLE VS GENRE
  gender_test_data <- data %>% filter(INT_treatment_general == 0)
  if (nrow(gender_test_data) > 0) {
    mean_row$VarStat_DiffGender <- format_ttest_result(gender_test_data, var, "INT_treatment")
  }
  
  # VALEURS SANS RÉSERVATION FEMMES 2005
  unreserved_data <- data %>% filter(RES05_gender == 0)
  if (nrow(unreserved_data) > 0) {
    mean_row$VarStat_Unreserved <- mean(unreserved_data[[var]], na.rm = TRUE)
    sd_row$VarStat_Unreserved <- sd(unreserved_data[[var]], na.rm = TRUE)
  }
  
  # VALEURS AVEC RÉSERVATION FEMMES 2005
  reserved_data <- data %>% filter(RES05_gender == 1)
  if (nrow(reserved_data) > 0) {
    mean_row$VarStat_Reserved <- mean(reserved_data[[var]], na.rm = TRUE)
    sd_row$VarStat_Reserved <- sd(reserved_data[[var]], na.rm = TRUE)
  }
  
  # ÉQUILIBRE SANS RÉSERVATION VS AVEC RÉSERVATION
  mean_row$VarStat_DiffReserved <- format_ttest_result(data, var, "RES05_gender")
  
  # Ajouter les lignes aux résultats
  results_df <- rbind(results_df, mean_row, sd_row)
}

# COMPTAGE DES OBSERVATIONS
obs_row <- data.frame(
  VarName = "Number of Panchayats",
  VarStat_Control = sum(data$INT_treatment == 0, na.rm = TRUE),
  VarStat_General = sum(data$INT_treatment_general == 1, na.rm = TRUE),
  VarStat_DiffGeneral = "",
  VarStat_Gender = sum(data$INT_treatment_gender == 1, na.rm = TRUE),
  VarStat_DiffGender = "",
  VarStat_Unreserved = sum(data$RES05_gender == 0, na.rm = TRUE),
  VarStat_Reserved = sum(data$RES05_gender == 1, na.rm = TRUE),
  VarStat_DiffReserved = "",
  stringsAsFactors = FALSE
)

results_df <- rbind(results_df, obs_row)

# EXPORT
write_xlsx(results_df, paste0(Table, ".xlsx"))
write_csv(results_df, paste0(Table, ".csv"))

# Affichage des résultats
print("Aperçu des résultats:")
print(results_df)
cat("\nNombre total de lignes dans les résultats:", nrow(results_df), "\n")
cat("Variables traitées:", length(variables), "\n")

# Vérification des dimensions des données
cat("Dimensions des données filtrées:", nrow(data), "lignes,", ncol(data), "colonnes\n")
cat("Colonnes disponibles:", paste(names(data)[1:10], collapse = ", "), "...\n")