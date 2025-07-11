#### Chi-Square Tests for PHQ Category ~ Gender
gender_weighted_chisq <- function(data, label) {
  design <- svydesign(ids = ~1, data = data, weights = ~new_weight)
  result <- svychisq(~Gender + phq_category, design = design)
  
  tibble(
    Timepoint = label,
    statistic = round(result$statistic, 3),
    df_num = result$parameter[1],
    df_den = result$parameter[2],
    p_value = round(result$p.value, 4)
  )
}
gender_chisq_results <- imap_dfr(gender_data_list, gender_weighted_chisq)
print("Weighted Chi-Square Test Results by Timepoint:")
print(gender_chisq_results)