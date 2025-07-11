## By Cadre

t0_Cadre_data <-ps_data %>% select(participantidentifier,Cadre,t0_phq.phq_score,phq_category,Hoursworked,new_weight)

t1_Cadre_data <-t1_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Cadre,t1_phq.phq_score,phq_category,Hoursworked,new_weight)

t2_Cadre_data <-t2_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Cadre,t2_phq.phq_score,phq_category,Hoursworked,new_weight)

t3_Cadre_data <-t3_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Cadre,t3_phq.phq_score,phq_category,Hoursworked,new_weight)

t4_Cadre_data <-t4_phq %>% inner_join(ps_data,by ='participantidentifier')%>%
  select(participantidentifier,Cadre,t4_phq.phq_score,phq_category,Hoursworked,new_weight)

### Cadre PHQ Summaries
t0_Cadre_summary_stats <- t0_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t0_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t0_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t0_Cadre_summary_stats)


t1_Cadre_summary_stats <- t1_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t1_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t1_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t1_Cadre_summary_stats)


t2_Cadre_summary_stats <- t2_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t2_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t2_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t2_Cadre_summary_stats)


t3_Cadre_summary_stats <- t3_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t3_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t3_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t3_Cadre_summary_stats)

t4_Cadre_summary_stats <- t4_Cadre_data %>%
  group_by(Cadre) %>%
  summarise(
    N = sum(new_weight, na.rm = TRUE),
    Mean_PHQ = wtd.mean(t4_phq.phq_score, weights = new_weight, na.rm = TRUE),
    STDDEV_PHQ = sqrt(wtd.var(t4_phq.phq_score, weights = new_weight, na.rm = TRUE)),
    Percent_PHQ_Category_1 = wtd.mean(phq_category == 1, weights = new_weight, na.rm = TRUE) * 100
  )

print(t4_Cadre_summary_stats)