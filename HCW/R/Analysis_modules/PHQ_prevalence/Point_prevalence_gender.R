
##By Gender
t0_gender_data <-ps_data %>% select(participantidentifier,Gender,t0_phq.phq_score,phq_category,new_weight)

t1_gender_data <-t1_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Gender,t1_phq.phq_score,phq_category,new_weight)

t2_gender_data <-t2_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Gender,t2_phq.phq_score,phq_category,new_weight)

t3_gender_data <-t3_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Gender,t3_phq.phq_score,phq_category,new_weight)

t4_gender_data <-t4_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Gender,t4_phq.phq_score,phq_category,new_weight)

t0_gender_summary_stats <- t0_gender_data %>%
  group_by(Gender) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t0_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t0_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t0_gender_summary_stats)


t1_gender_summary_stats <- t1_gender_data %>%
  group_by(Gender) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t1_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t1_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t1_gender_summary_stats)


t2_gender_summary_stats <- t2_gender_data %>%
  group_by(Gender) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t2_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t2_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t2_gender_summary_stats)


t3_gender_summary_stats <- t3_gender_data %>%
  group_by(Gender) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t3_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t3_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t3_gender_summary_stats)

t4_gender_summary_stats <- t4_gender_data %>%
  group_by(Gender) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t4_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t4_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t4_gender_summary_stats)

