###T tests to evaluate significance of PHQ score ~ Gender
gender_data_list <- list(
  t0 = t0_gender_data,
  t1 = t1_gender_data,
  t2 = t2_gender_data,
  t3 = t3_gender_data,
  t4 = t4_gender_data
)
phq_vars <- c(
  t0 = "t0_phq.phq_score",
  t1 = "t1_phq.phq_score",
  t2 = "t2_phq.phq_score",
  t3 = "t3_phq.phq_score",
  t4 = "t4_phq.phq_score"
)

run_weighted_ttest <- function(data, varname, label) {
  design <- svydesign(ids = ~1, data = data, weights = ~new_weight)
  formula <- as.formula(paste(varname, "~ Gender"))
  result <- svyttest(formula, design = design)
  
  tibble(
    Timepoint = label,
    t_statistic = round(result$statistic, 3),
    df = round(result$parameter, 1),
    p_value = round(result$p.value, 4)
  )
}

weighted_ttest_results <- imap_dfr(phq_vars, function(var, label) {
  run_weighted_ttest(gender_data_list[[label]], var, label)
})

print(weighted_ttest_results)