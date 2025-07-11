
####Demo Vars Correlation
##Pearson Correlation
pearson_corr_data_phqavg.imp<- dropout_phq_correlation_test_data %>%
  select(participantidentifier,t0_phq.phq_score,t1_phq.phq_score,t2_phq.phq_score,t3_phq.phq_score,t4_phq.phq_score)%>%
  rowwise() %>%
  mutate(phq_avg = mean(c_across(starts_with("t")), na.rm = TRUE)) %>%
  ungroup()

pearson_corr_data_demo_phqavg.imp <-ps_data%>% inner_join(pearson_corr_data_phqavg.imp, by='participantidentifier')%>%
  select(participantidentifier,Age,Cadre,Gender,Marital,Children,Experience,neuroticsm_score,efe_score,phq_avg,new_weight) 

dependent_cols_data.imp <- pearson_corr_data_demo_phqavg.imp %>%
  select(Age, neuroticsm_score, efe_score, Gender, Cadre, Children, Marital,Experience) %>% mutate(across(everything(), as.numeric))
weights=pearson_corr_data_demo_phqavg.imp$new_weight

pearson_phq_avg_scores.imp<- pearson_corr_data_demo_phqavg.imp %>%select(phq_avg)

pairs <- expand.grid(var = names(dependent_cols_data.imp), phq_var = names(pearson_phq_avg_scores.imp))
compute_cor <- function(var, phq_col) {
  x <- dependent_cols_data.imp[[var]]
  y <- pearson_phq_avg_scores.imp[[phq_col]]
  
  if (all(is.na(x)) || all(is.na(y)) || all(is.na(weights))) {
    cat("Skipping due to all NA in:", var, "\n")
    return(tibble(
      variable = var,
      phq_var = phq_col,
      test_type = "Pearson Correlation",
      statistic = NA,
      p_value = NA
    ))
  }
  
  test_result <- tryCatch({
    wtd.cor(x, y, weight = weights, bootse = TRUE)
  }, error = function(e) {
    cat("Error in", var, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(test_result)) {
    return(tibble(
      variable = var,
      phq_var = phq_col,
      test_type = "Pearson Correlation",
      statistic = NA,
      p_value = NA
    ))
  }
  
  statistic_value <- test_result[1, "correlation"]
  p_value_value <- test_result[1, "p.value"]
  
  formatted_p_value <- ifelse(p_value_value < 0.001, "<0.001", format(round(p_value_value, 4), nsmall = 4))
  
  tibble(
    variable = var,
    phq_var = phq_col,
    test_type = "Pearson Correlation",
    statistic = round(statistic_value, 4),
    p_value = as.character(formatted_p_value)
  )
}
cor_results <- map2_dfr(pairs$var, pairs$phq_var, compute_cor)
print(cor_results)


###PHQ_Proportion Lukoye Comment---If anyone has a phqtot>10 as 1 else 0
phq_all_prevalence<-ps_data%>% inner_join(pearson_corr_data_phqavg.imp, by='participantidentifier')%>%
  select(participantidentifier,t0_phq.phq_score.x,t1_phq.phq_score,t2_phq.phq_score,t3_phq.phq_score,t4_phq.phq_score,new_weight) %>%
  rename(t0_phq.phq_score=t0_phq.phq_score.x) %>%
  mutate(any_phq10_plus = if_any(
    .cols = starts_with("t"), 
    .fns = ~ !is.na(.) & . >= 10
  ) * 1)


phq_all_prevalence <- phq_all_prevalence %>%
  mutate(
    phq_all_cat_above10 = if_any(starts_with("t"), ~ .x >= 10 & !is.na(.x)) * 1,
    phq_all_cat_above15 = if_any(starts_with("t"), ~ .x >= 15 & !is.na(.x)) * 1,
    phq_all_cat_above20 = if_any(starts_with("t"), ~ .x >= 20 & !is.na(.x)) * 1
  )

phq_all_prevalence_above10_data <- phq_all_prevalence %>%
  select(participantidentifier, phq_all_cat_above10, phq_all_cat_above10, new_weight)

phq_all_prevalence_above15_data <- phq_all_prevalence %>%
  select(participantidentifier, phq_all_cat_above15, phq_all_cat_above15, new_weight)

phq_all_prevalence_above20_data <- phq_all_prevalence %>%
  select(participantidentifier, phq_all_cat_above20, phq_all_cat_above20, new_weight)





phq_all_above10_prevalence <- phq_all_prevalence_above10_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Percent_PHQ_Category = wtd.mean(phq_all_cat_above10 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above10 = sum(new_weight[phq_all_cat_above10 == 1], na.rm = TRUE)
  )
print(phq_all_above10_prevalence)

phq_all_above15_prevalence <- phq_all_prevalence_above15_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Percent_PHQ_Category = wtd.mean(phq_all_cat_above15 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above15 = sum(new_weight[phq_all_cat_above15 == 1], na.rm = TRUE)
  )
print(phq_all_above15_prevalence)

phq_all_above20_prevalence <- phq_all_prevalence_above20_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Percent_PHQ_Category = wtd.mean(phq_all_cat_above20 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above20 = sum(new_weight[phq_all_cat_above20 == 1], na.rm = TRUE)
  )
print(phq_all_above20_prevalence)