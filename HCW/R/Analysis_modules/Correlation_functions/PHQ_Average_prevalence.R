###Phq AVg prevalence
phq_avg_prevalence_data<-ps_data%>% inner_join(pearson_corr_data_phqavg.imp, by='participantidentifier')%>%
  select(participantidentifier,phq_avg,new_weight)


phq_avg_prevalence_data$phq_avg_cat_above10 <- ifelse(is.na(phq_avg_prevalence_data$phq_avg), NA,ifelse(phq_avg_prevalence_data$phq_avg >= 10, 1, 0))
phq_avg_prevalence_data$phq_avg_cat_above15 <- ifelse(is.na(phq_avg_prevalence_data$phq_avg), NA,ifelse(phq_avg_prevalence_data$phq_avg >= 15, 1, 0))
phq_avg_prevalence_data$phq_avg_cat_above20 <- ifelse(is.na(phq_avg_prevalence_data$phq_avg), NA,ifelse(phq_avg_prevalence_data$phq_avg >= 20, 1, 0))


phq_avg_prevalence_data_above10_data<-phq_avg_prevalence_data %>% select(participantidentifier,phq_avg,phq_avg_cat_above10,new_weight)
phq_avg_prevalence_data_above15_data<-phq_avg_prevalence_data %>% select(participantidentifier,phq_avg,phq_avg_cat_above15,new_weight)
phq_avg_prevalence_data_above20_data<-phq_avg_prevalence_data %>% select(participantidentifier,phq_avg,phq_avg_cat_above20,new_weight)



phq_avg_above10_prevalence <- phq_avg_prevalence_data_above10_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(phq_avg, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(phq_avg, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(phq_avg_cat_above10 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above10 = sum(new_weight[phq_avg_cat_above10 == 1], na.rm = TRUE)
  )
print(phq_avg_above10_prevalence)

phq_avg_above15_prevalence <- phq_avg_prevalence_data_above15_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(phq_avg, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(phq_avg, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(phq_avg_cat_above15 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above15 = sum(new_weight[phq_avg_cat_above15 == 1], na.rm = TRUE)
  )
print(phq_avg_above15_prevalence)

phq_avg_above20_prevalence <- phq_avg_prevalence_data_above20_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(phq_avg, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(phq_avg, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(phq_avg_cat_above20 == 1, weights = new_weight, na.rm = TRUE) * 100,
    n_above20 = sum(new_weight[phq_avg_cat_above20 == 1], na.rm = TRUE)
  )
print(phq_avg_above20_prevalence)


