
#### STEP-WISE REGRESSION MODEL
stepwise_reg_data.imp<-pearson_corr_data_demo_phqavg.imp %>% 
  rowwise() %>%
  mutate(phq_category = ifelse(is.na(phq_avg), NA, 
                               ifelse(phq_avg >= 10, 1, 0))) %>% 
  ungroup()

stepwise_reg_data_clean.imp<-stepwise_reg_data.imp %>% dplyr::select(participantidentifier,Age,Gender,Marital,neuroticsm_score,Experience,efe_score,new_weight,phq_avg) %>%
  na.omit()

age_initial_reg_model.imp<-lm(phq_avg ~Age+ Gender+Marital+neuroticsm_score+efe_score+Experience,weights = new_weight, data = stepwise_reg_data_clean.imp)
#initial_reg_model.imp<-lm(phq_avg ~Gender+Marital+neuroticsm_score+efe_score,weights = weights,  data = stepwise_reg_data_clean.imp)
summary(age_initial_reg_model.imp)
#summary(initial_reg_model.imp)

stepwise_reg_model.imp<-step(age_initial_reg_model.imp, direction = "both")
summary(stepwise_reg_model.imp)