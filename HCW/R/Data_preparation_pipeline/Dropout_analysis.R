####Dropout correlation analysis
rename_with_prefix <- function(df, prefix) {
  df %>%
    rename_with(~ paste0(prefix, ".", .), -participantidentifier)
}

t0_phq <-baseline_imputed_data %>% select(participantidentifier,phq_score,phq_category) %>%rename_with_prefix("t0_phq")
t1_phq<-q1_imputed_data%>% select(participantidentifier,phq_score,phq_category) %>%  rename_with_prefix("t1_phq")
t2_phq<-q2_imputed_data%>% select(participantidentifier,phq_score,phq_category) %>%  rename_with_prefix("t2_phq")
t3_phq<-q3_imputed_data%>% select(participantidentifier,phq_score,phq_category) %>%  rename_with_prefix("t3_phq")
t4_phq<-q4_imputed_data%>% select(participantidentifier,phq_score,phq_category) %>%  rename_with_prefix("t4_phq")

correlation_data_list <-list(t0_phq,t1_phq,t2_phq,t3_phq,t4_phq)
dropout_phq_correlation_test_data <-reduce(correlation_data_list,full_join, by ='participantidentifier')

dropout_corr_df.imp <- dropout_phq_correlation_test_data %>%
  mutate(
    dropout_t1 = ifelse(is.na(t1_phq.phq_score) & !is.na(t0_phq.phq_score), 1, 0),
    dropout_t2 = ifelse(is.na(t2_phq.phq_score) & !is.na(t1_phq.phq_score), 1, 0),
    dropout_t3 = ifelse(is.na(t3_phq.phq_score) & !is.na(t2_phq.phq_score), 1, 0),
    dropout_t4 = ifelse(is.na(t4_phq.phq_score) & !is.na(t3_phq.phq_score), 1, 0),
    
    dropout_t1 = ifelse(is.na(t1_phq.phq_score) & is.na(t0_phq.phq_score), 0, dropout_t1),
    dropout_t2 = ifelse(is.na(t2_phq.phq_score) & is.na(t1_phq.phq_score), 0, dropout_t2),
    dropout_t3 = ifelse(is.na(t3_phq.phq_score) & is.na(t2_phq.phq_score), 0, dropout_t3),
    dropout_t4 = ifelse(is.na(t4_phq.phq_score) & is.na(t3_phq.phq_score), 0, dropout_t4),
    
    dropout_all = ifelse(rowSums(!is.na(select(., t1_phq.phq_score, t2_phq.phq_score, t3_phq.phq_score, t4_phq.phq_score))) > 0, 1, 0)
  )

dropout_summary <- dropout_corr_df.imp %>%
  summarise(across(starts_with("dropout"), mean, na.rm = TRUE))

compute_logistic_results <- function(formula, data) {
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
  t1_dropout = dropout_t1 ~ t0_phq.phq_score,
  t2_dropout = dropout_t2 ~ t0_phq.phq_score,
  t3_dropout = dropout_t3 ~ t0_phq.phq_score,
  t4_dropout = dropout_t4 ~ t0_phq.phq_score,
  dropout_all =dropout_all~ t0_phq.phq_score
)

logistic_results <- lapply(t0_formulas, compute_logistic_results, data = dropout_corr_df.imp)

names(logistic_results) <- names(t0_formulas)
logistic_results