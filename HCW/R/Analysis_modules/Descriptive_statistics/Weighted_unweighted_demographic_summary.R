#### Logistic Regression vs Demo for PS Score
compute_logistic_results_demo <- function(formula, data) {
  model <- glm(formula, data = data, family = binomial)
  coefficients_summary <- summary(model)$coefficients
  odds_ratios <- exp(coefficients_summary[, "Estimate"])
  conf_int <- exp(confint(model))  
  
  results <- data.frame(
    Variable = rownames(coefficients_summary),
    Coefficient = coefficients_summary[, "Estimate"],
    OR = odds_ratios,
    `Lower 95% CI` = conf_int[, 1],
    `Upper 95% CI` = conf_int[, 2],
    `P-value` = format.pval(coefficients_summary[, "Pr(>|z|)"], digits = 4, eps = 1e-4)  
  )
  
  return(results)
}
t0_formulas <- list(
  dropout_all =dropout_all~ Age+Gender+neuroticsm_score
)

demo_logistic_results <- lapply(t0_formulas, compute_logistic_results_demo, data = attrition_dataset.imp)

names(demo_logistic_results) <- names(t0_formulas)
demo_logistic_results

###Unweighted Demographics data summary
demo_unweighted<-attrition_dataset.imp %>% 
  select(participantidentifier, Age, Gender, Cadre, Marital,neuroticsm_score,Children,efe_score,Hoursworked,Discrimination,Majorerror,Experience,SLE) %>%
  distinct()

unweighted_demo_descriptives <- CreateTableOne(
  vars = c('Age','Gender','Cadre','Marital','neuroticsm_score','efe_score','Hoursworked','Discrimination','Majorerror','Experience','SLE','Children'),
  data = demo_unweighted,
  factorVars = c('Gender','Cadre','Marital','Discrimination','Majorerror','SLE')
  #includeNA = TRUE
)
demo_unweighted_table<-print(unweighted_demo_descriptives, showAllLevels = TRUE)

####Weighted data Demographics Summary
demo_weighted <- ps_data %>% 
  select(participantidentifier, Age, Gender, Cadre, Marital,Children,neuroticsm_score,efe_score,Hoursworked,Discrimination,Majorerror,Experience,SLE,new_weight) %>%
  distinct()

demo_svy <- svydesign(ids = ~1, data = demo_weighted, weights = ~new_weight)

numeric_vars <- names(demo_weighted)[sapply(demo_weighted, is.numeric)]
categorical_vars <- names(demo_weighted)[sapply(demo_weighted, is.factor) | sapply(demo_weighted, is.character)]
categorical_vars <- categorical_vars[categorical_vars != "participantidentifier"]

total_weighted_n <- sum(weights(demo_svy))
cat("Total N (weighted):", round(total_weighted_n, 2), "\n")

cat("\nWeighted Summary: Numeric Variables\n")
for (var in numeric_vars) {
  if (var != "new_weight") {  # Skip the weight column
    cat("\n", var, "\n")
    mean_result <- svymean(as.formula(paste0("~", var)), demo_svy, na.rm = TRUE)
    mean_val <- coef(mean_result)
    se_val <- SE(mean_result)
    sd_val <- se_val * sqrt(total_weighted_n)
    cat("Mean:", round(mean_val, 2), "\n")
    cat("Standard Deviation (approx):", round(sd_val, 2), "\n")
  }
}

# Weighted summary: Categorical variables
cat("\nWeighted Summary: Categorical Variables\n")
for (var in categorical_vars) {
  cat("\n", var, "\n")
  
  weighted_tab <- svytable(as.formula(paste0("~", var)), design = demo_svy)
  df_tab <- as.data.frame(weighted_tab)
  df_tab$Percentage <- round(100 * df_tab$Freq / total_weighted_n, 2)
  colnames(df_tab) <- c("Category", "Weighted_Count", "Weighted_Percentage")
  
  print(df_tab)
}
