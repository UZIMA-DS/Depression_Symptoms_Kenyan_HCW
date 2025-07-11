###PHQ Point prevalence
t0_phq_prevalence_data<-merge(t0_phq ,demo_weighted, by='participantidentifier', all.x = TRUE)%>%
  select(-Age,-Marital,-Children,-Gender,-efe_score,-Cadre,-neuroticsm_score) %>%
  mutate(across(contains("phq_category"), as.factor))

t0_phq_prevalence <- t0_phq_prevalence_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t0_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t0_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(t0_phq.phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t0_phq_prevalence)

t1_phq_prevalence_data<-merge(t1_phq ,demo_weighted, by='participantidentifier', all.x = TRUE)%>%
  select(-Age,-Marital,-Children,-Gender,-efe_score,-Cadre,-neuroticsm_score) %>%
  mutate(across(contains("phq_category"), as.factor))

t1_phq_prevalence <- t1_phq_prevalence_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t1_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t1_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(t1_phq.phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t1_phq_prevalence)

t2_phq_prevalence_data<-merge(t2_phq ,demo_weighted, by='participantidentifier', all.x = TRUE)%>%
  select(-Age,-Marital,-Children,-Gender,-efe_score,-Cadre,-neuroticsm_score) %>%
  mutate(across(contains("phq_category"), as.factor))

t2_phq_prevalence <- t2_phq_prevalence_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t2_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t2_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(t2_phq.phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t2_phq_prevalence)

t3_phq_prevalence_data<-merge(t3_phq ,demo_weighted, by='participantidentifier', all.x = TRUE)%>%
  select(-Age,-Marital,-Children,-Gender,-efe_score,-Cadre,-neuroticsm_score) %>%
  mutate(across(contains("phq_category"), as.factor))

t3_phq_prevalence <- t3_phq_prevalence_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t3_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t3_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(t3_phq.phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t3_phq_prevalence)

t4_phq_prevalence_data<-merge(t4_phq ,demo_weighted, by='participantidentifier', all.x = TRUE)%>%
  select(-Age,-Marital,-Children,-Gender,-efe_score,-Cadre,-neuroticsm_score) %>%
  mutate(across(contains("phq_category"), as.factor))

t4_phq_prevalence <- t4_phq_prevalence_data %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t4_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t4_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category = wtd.mean(t4_phq.phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t4_phq_prevalence)
