### Chi-Square test for PHQ_Category ~ Cadre
cadre_weighted_chisq <- function(data, label) {
  design <- svydesign(ids = ~1, data = data, weights = ~new_weight)
  result <- svychisq(~Cadre + phq_category, design = design)
  
  tibble(
    Timepoint = label,
    statistic = round(result$statistic, 3),
    df_num = result$parameter[1],
    df_den = result$parameter[2],
    p_value = round(result$p.value, 4)
  )
}
chisq_results <- imap_dfr(cadre_data_list, cadre_weighted_chisq)
print("Weighted Chi-Square Test Results by Timepoint:")
print(chisq_results)

### Cadre ~ Hours Duty Summaries
t0_Cadre__hours_summary_stats <- t0_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(Hoursworked, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(Hoursworked, weights = new_weight, na.rm = TRUE))
  )
print(t0_Cadre__hours_summary_stats)


t1_Cadre__hours_summary_stats <- t1_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(Hoursworked, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(Hoursworked, weights = new_weight, na.rm = TRUE))
  )
print(t1_Cadre__hours_summary_stats)


t2_Cadre__hours_summary_stats <- t2_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(Hoursworked, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(Hoursworked, weights = new_weight, na.rm = TRUE))
  )
print(t2_Cadre__hours_summary_stats)


t3_Cadre__hours_summary_stats <- t3_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(Hoursworked, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(Hoursworked, weights = new_weight, na.rm = TRUE))
  )
print(t3_Cadre__hours_summary_stats)

t4_Cadre__hours_summary_stats <- t4_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(Hoursworked, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(Hoursworked, weights = new_weight, na.rm = TRUE))
  )
print(t4_Cadre__hours_summary_stats)
