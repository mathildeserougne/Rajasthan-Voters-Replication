## REPLICATION: TA6 - GENDER TREATMENT 2010 ###################################


### NE PAS REGARDER LOOK AWAY

# Libraries
# Uncomment following packages installation if necessary
# install.packages(c("stargazer","dplyr","lmtest","haven"))
library(stargazer)
library(dplyr)
library(lmtest)
library(haven)


# Charger les données
data <- read_dta("~/work/Electoral data cleaned.dta")




# Filtrer les données
data <- data %>%
  filter(RES10_gender == 0, GP_tag == 1)

# Définir les variables dépendantes
dep_vars <- c("ELEC10_nbcands", "CHAL_nbchal", "CHAL_prop_female",
              "CHAL_voteshare_female", "CHAL_prop_nongen", "CHAL_voteshare_nongen")

# Définir les variables de contrôle
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Initialiser une liste pour stocker les résultats
results <- list()
control_means <- numeric(length(dep_vars))
p_values_test1 <- numeric(length(dep_vars))
p_values_test2 <- numeric(length(dep_vars))

# Exécuter les régressions pour chaque variable dépendante
for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  
  # Calculer les moyennes des contrôles
  control_means[i] <- mean(data[data$INT_treatment == 0 & data$RES05_gender == 0, dep_var], na.rm = TRUE)
  
  # Exécuter la régression avec les contrôles
  model <- lm(as.formula(paste(dep_var, "~ INT_treatment_gender + INT_treatment_general + RES05_gender + X_generaltr_genderres05 + X_gendertr_genderres05 + district +", paste(gpcontrols, collapse = " + "))), data = data)
  
  # Exécuter les tests d'hypothèses
  test1 <- waldtest(model, hypothesis.matrix = c(0, 1, 0, 0, rep(0, length(gpcontrols) + 1)))
  test2 <- waldtest(model, hypothesis.matrix = c(0, 1, 1, 0, rep(0, length(gpcontrols) + 1)))
  
  # Stocker les valeurs de p
  p_values_test1[i] <- ifelse(length(test1$p.value) > 0, test1$p.value, NA)
  p_values_test2[i] <- ifelse(length(test2$p.value) > 0, test2$p.value, NA)
  
  # Stocker les résultats
  results[[dep_var]] <- model
}

# Générer le tableau avec stargazer
stargazer(results, type = "text", out = "~/work/TA6 - Gender Treatment - 2010.txt",
          title = "Regression Results", dep.var.labels = dep_vars,
          column.labels = paste("Model", 1:length(dep_vars)),
          add.lines = list(
            c("Mean in Control not WR in 2005", control_means),
            c("Test Gender=General in WR=0", p_values_test1),
            c("Test Gender=General in WR=1", p_values_test2)
          ))





