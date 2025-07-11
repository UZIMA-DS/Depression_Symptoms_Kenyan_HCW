### Anova analysis for PHQ_Score ~ Cadre
cadre_data_list <- list(
  t0 = t0_Cadre_data,
  t1 = t1_Cadre_data,
  t2 = t2_Cadre_data,
  t3 = t3_Cadre_data,
  t4 = t4_Cadre_data
) %>%
  map(~ .x %>%
        rename(phq_score = matches("phq_score"))  # rename any column that includes 'phq_score'
  )

run_weighted_anova <- function(data, label) {
  design <- svydesign(ids = ~1, data = data, weights = ~new_weight)
  model <- svyglm(phq_score ~ Cadre, design = design)
  cat("\nWeighted ANOVA Result for", label, ":\n")
  print(summary(model))
}
walk2(cadre_data_list, names(cadre_data_list), run_weighted_anova)
extract_anova_summary <- function(data, label) {
  design <- svydesign(ids = ~1, data = data, weights = ~new_weight)
  model <- svyglm(phq_score ~ Cadre, design = design)
  model_summary <- summary(model)
  tibble(
    Timepoint = label,
    F_statistic = round(model_summary$coefficients[2, "t value"]^2, 3),  # approximate
    p_value = round(model_summary$coefficients[2, "Pr(>|t|)"], 4)
  )
}
anova_results <- imap_dfr(cadre_data_list, extract_anova_summary)
print(anova_results)