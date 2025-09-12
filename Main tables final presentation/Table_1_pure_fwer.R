## TABLE 1 - FINAL 
## pure effect, separate outcome, fwer adjusted p values included.

library(haven)
library(tidyverse)
library(fixest)
library(modelsummary)
library(broom)


# DATA
data <- read_dta("~/work/Electoral data cleaned.dta")
data_filtered <- data %>%
  filter(RES10_gender == 0 & SAMPLE_hhsurvey == 1 & GP_tag == 1 & INC05_can_run == 1) %>%
  mutate(
    FAMnotINC05_running = INCorFAM05_running - INC05_running,
    FAMnotINC05_voteshare = INCorFAM05_voteshare - INC05_voteshare,
    FAMnotINC05_won = INCorFAM05_won - INC05_won,
    RES05_gender = as.factor(RES05_gender)
  )

# Controls
gpcontrols <- c("GP_population", "GP_lit", "GP_sc", "GP_st", "GP_nbvillages",
                "RES00_gender", "RES00_obc", "RES00_sc", "RES00_st",
                "RES10_obc", "RES10_sc", "RES10_st", "RES05_obc", "RES05_sc", "RES05_st")

# Dpdt variables
running_dep_vars <- c("INC05_running", "INCSPOUSE05_running", "INCOTHER05_running")
voteshare_dep_vars <- c("INC05_voteshare", "INCSPOUSE05_voteshare", "INCOTHER05_voteshare")

# Subsamples
data_whole <- data_filtered
data_RES05_gender_0 <- data_filtered %>% filter(RES05_gender == 0)
data_RES05_gender_1 <- data_filtered %>% filter(RES05_gender == 1)


# Simple formula (no interaction)
create_formula <- function(dep_var) {
  base_controls <- paste(gpcontrols, collapse = " + ")
  as.formula(paste(dep_var, "~ INT_treatment +", base_controls, "+ factor(district)"))
}

# Extract p-values
get_pvalues <- function(model, var_names) {
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients
  pvals <- coef_table[var_names, "Pr(>|t|)"]
  return(as.numeric(pvals))
}

# Ajustment (FWER)
calculate_fwer <- function(models, var_names) {
  all_pvalues <- unlist(lapply(models, function(model) {
    if (!is.null(model)) get_pvalues(model, var_names) else numeric(0)
  }))
  p.adjust(all_pvalues, method = "holm")
}

# Extract results
get_model_results <- function(model, var_names, var_labels, p_adjusted, offset = 0) {
  if (is.null(model)) return(NULL)
  summary_model <- summary(model)
  coef_table <- summary_model$coefficients[var_names, , drop = FALSE]
  if (nrow(coef_table) == 0) return(NULL)
  data.frame(
    Variable = var_labels[rownames(coef_table)],
    Estimate = coef_table[, "Estimate"],
    P.value = coef_table[, "Pr(>|t|)"],
    FWER = p_adjusted[(1:nrow(coef_table)) + offset]
  )
}


# Model estimation and results extraction
var_names <- c("INT_treatment") 
var_labels <- c("INT_treatment" = "Treatment")

# Running 
running_models_whole <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_whole))
running_models_gender0 <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_0))
running_models_gender1 <- lapply(running_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_1))

# Vote Share 
voteshare_models_whole <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_whole))
voteshare_models_gender0 <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_0))
voteshare_models_gender1 <- lapply(voteshare_dep_vars, function(dep_var) lm(create_formula(dep_var), data = data_RES05_gender_1))

# Compute adjusted p-values
pvalues_running_whole <- get_pvalues(running_models_whole[[1]], var_names)
adjusted_pvalues_running_whole <- calculate_fwer(running_models_whole, var_names)
pvalues_running_gender0 <- get_pvalues(running_models_gender0[[1]], var_names)
adjusted_pvalues_running_gender0 <- calculate_fwer(running_models_gender0, var_names)
pvalues_running_gender1 <- get_pvalues(running_models_gender1[[1]], var_names)
adjusted_pvalues_running_gender1 <- calculate_fwer(running_models_gender1, var_names)

pvalues_voteshare_whole <- get_pvalues(voteshare_models_whole[[1]], var_names)
adjusted_pvalues_voteshare_whole <- calculate_fwer(voteshare_models_whole, var_names)
pvalues_voteshare_gender0 <- get_pvalues(voteshare_models_gender0[[1]], var_names)
adjusted_pvalues_voteshare_gender0 <- calculate_fwer(voteshare_models_gender0, var_names)
pvalues_voteshare_gender1 <- get_pvalues(voteshare_models_gender1[[1]], var_names)
adjusted_pvalues_voteshare_gender1 <- calculate_fwer(voteshare_models_gender1, var_names)

# Results
running_results_whole <- lapply(1:3, function(i) get_model_results(running_models_whole[[i]], var_names, var_labels, adjusted_pvalues_running_whole, (i-1)))
running_results_gender0 <- lapply(1:3, function(i) get_model_results(running_models_gender0[[i]], var_names, var_labels, adjusted_pvalues_running_gender0, (i-1)))
running_results_gender1 <- lapply(1:3, function(i) get_model_results(running_models_gender1[[i]], var_names, var_labels, adjusted_pvalues_running_gender1, (i-1)))

voteshare_results_whole <- lapply(1:3, function(i) get_model_results(voteshare_models_whole[[i]], var_names, var_labels, adjusted_pvalues_voteshare_whole, (i-1)))
voteshare_results_gender0 <- lapply(1:3, function(i) get_model_results(voteshare_models_gender0[[i]], var_names, var_labels, adjusted_pvalues_voteshare_gender0, (i-1)))
voteshare_results_gender1 <- lapply(1:3, function(i) get_model_results(voteshare_models_gender1[[i]], var_names, var_labels, adjusted_pvalues_voteshare_gender1, (i-1)))

# Labels
running_dep_var_labels <- c("Incumbent Runs", "Spouse Runs", "Other Family Member Runs")
voteshare_dep_var_labels <- c("Incumbent Vote Share", "Spouse Vote Share", "Other Family Member Vote Share")


# Display results
print_results <- function(results_list, dep_var_labels, sample_name, panel_name) {
  cat("\n\n======================================================================\n")
  cat(panel_name, " (", sample_name, ")\n")
  cat("======================================================================\n")
  for (i in 1:3) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      cat("\n--- ", dep_var_labels[i], " ---\n")
      cat(sprintf("%-25s %-15s %-15s %-15s\n", "Variable", "Estimate", "p-value", "FWER-adj p"))
      cat("----------------------------------------------------------------------\n")
      cat(sprintf("%-25s %-15.4f %-15s %-15s\n",
                  result$Variable, result$Estimate,
                  format.pval(result$P.value, digits = 3),
                  format.pval(result$FWER, digits = 3)))
    }
  }
}

# Displaying:
print_results(running_results_whole, running_dep_var_labels, "Whole Sample", "Treatment Effect on Running")
print_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Running")
print_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Running")

print_results(voteshare_results_whole, voteshare_dep_var_labels, "Whole Sample", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Vote Share")




# OUTPUT  TXT #


sink("~/work/Table_1_final_fwer.txt", append = FALSE, split = TRUE)

cat("======================================================================\n")
cat("FWER - Table 1: Treatment Effect on Running\n")
cat("======================================================================\n\n")

print_results(running_results_whole, running_dep_var_labels, "Whole Sample", "Treatment Effect on Running")
print_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Running")
print_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Running")


cat("\n\n======================================================================\n")
cat("FWER - Table 1: Treatment Effect on Vote Share\n")
cat("======================================================================\n\n")

print_results(voteshare_results_whole, voteshare_dep_var_labels, "Whole Sample", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0", "Treatment Effect on Vote Share")
print_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1", "Treatment Effect on Vote Share")


sink()



## TENTATIVE D'AUTRE CHOSE


# display results per subsample:
print_structured_results <- function(results_list, dep_var_labels, sample_name) {
  cat("\n\n")
  cat(sample_name, ":\n")
  results <- do.call(rbind, lapply(1:3, function(i) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      return(data.frame(Outcome = dep_var_labels[i], Estimate = result$Estimate, P.value = result$P.value, FWER = result$FWER))
    }
  }))
  print(results, row.names = FALSE)
}

# "Running"
cat("======================================================================\n")
cat("STRUCTURED TABLE 1: Treatment Effect on Running\n")
cat("======================================================================\n")
print_structured_results(running_results_whole, running_dep_var_labels, "WHOLE SAMPLE")
print_structured_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0")
print_structured_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1")

# "Vote Share"
cat("\n\n======================================================================\n")
cat("STRUCTURED TABLE 1: Treatment Effect on Vote Share\n")
cat("======================================================================\n")
print_structured_results(voteshare_results_whole, voteshare_dep_var_labels, "WHOLE SAMPLE")
print_structured_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0")
print_structured_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1")



# separated txt

sink("~/work/1-structured_running_results.txt")
cat("======================================================================\n")
cat("STRUCTURED TABLE 1: Treatment Effect on Running\n")
cat("======================================================================\n")
print_structured_results(running_results_whole, running_dep_var_labels, "WHOLE SAMPLE")
print_structured_results(running_results_gender0, running_dep_var_labels, "RES05_gender = 0")
print_structured_results(running_results_gender1, running_dep_var_labels, "RES05_gender = 1")
sink()

sink("~/work/1-structured_voteshare_results.txt")
cat("======================================================================\n")
cat("STRUCTURED TABLE 1: Treatment Effect on Vote Share\n")
cat("======================================================================\n")
print_structured_results(voteshare_results_whole, voteshare_dep_var_labels, "WHOLE SAMPLE")
print_structured_results(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0")
print_structured_results(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1")
sink()




# tex!

library(xtable)

# tex from a given panel
generate_latex_panel <- function(results_list, dep_var_labels, sample_name) {
  results <- do.call(rbind, lapply(1:3, function(i) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      return(data.frame(Outcome = dep_var_labels[i], Estimate = result$Estimate, P.value = result$P.value, FWER = result$FWER))
    }
  }))
  
  table_xtable <- xtable(results, caption = paste(sample_name), label = gsub("[^a-zA-Z0-9]", "", sample_name))
  
  align(table_xtable) <- "llrrr"
  print(table_xtable,
        include.rownames = FALSE,
        floating = FALSE,
        hline.after = c(-1, 0, nrow(results)),
        booktabs = TRUE,
        onlycontents = TRUE)
}

# whole tex, with all panels
sink("~/work/Table_1_final.tex")
cat("\\documentclass{article}
\\usepackage{booktabs}
\\usepackage[utf8]{inputenc}
\\begin{document}

\\section*{Table 1: Treatment Effects}

\\subsection*{Treatment Effect on Running}
\\begin{center}
\\textbf{WHOLE SAMPLE}\\\\
")
print(generate_latex_panel(running_results_whole, running_dep_var_labels, "WHOLE SAMPLE"))

cat("\\vspace{0.5cm}\\\\
\\textbf{RES05\\_gender = 0}\\\\
")
print(generate_latex_panel(running_results_gender0, running_dep_var_labels, "RES05_gender = 0"))

cat("\\vspace{0.5cm}\\\\
\\textbf{RES05\\_gender = 1}\\\\
")
print(generate_latex_panel(running_results_gender1, running_dep_var_labels, "RES05_gender = 1"))

cat("\\end{center}

\\subsection*{Treatment Effect on Vote Share}
\\begin{center}
\\textbf{WHOLE SAMPLE}\\\\
")
print(generate_latex_panel(voteshare_results_whole, voteshare_dep_var_labels, "WHOLE SAMPLE"))

cat("\\vspace{0.5cm}\\\\
\\textbf{RES05\\_gender = 0}\\\\
")
print(generate_latex_panel(voteshare_results_gender0, voteshare_dep_var_labels, "RES05_gender = 0"))

cat("\\vspace{0.5cm}\\\\
\\textbf{RES05\\_gender = 1}\\\\
")
print(generate_latex_panel(voteshare_results_gender1, voteshare_dep_var_labels, "RES05_gender = 1"))

cat("\\end{center}
\\end{document}")
sink()


