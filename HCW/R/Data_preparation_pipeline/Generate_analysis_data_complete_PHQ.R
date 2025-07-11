## Generate analysis data of complete PHQ Per time point
baseline_analysis_data <- process_scores(baseline_data_1, baseline_vars_list)%>% filter(!is.na(phq_score))
baseline_analysis_data<-baseline_analysis_data %>% left_join(baseline_sle_analysis,by='participantidentifier')
baseline_analysis_data$SLE <- as.factor(baseline_analysis_data$SLE)

quarter1_analysis_data <- process_scores(quarter1_data_1, quarter1_psychometric_vars_list)%>% filter(!is.na(phq_score))
quarter1_analysis_data<-quarter1_analysis_data %>% left_join(q1_sle_analysis,by='participantidentifier')
quarter1_analysis_data$SLE <- as.factor(quarter1_analysis_data$SLE)

quarter2_analysis_data <- process_scores(quarter2_data_1, quarter2_psychometric_vars_list)%>% filter(!is.na(phq_score))
quarter2_analysis_data<-quarter2_analysis_data %>% left_join(q2_sle_analysis,by='participantidentifier')
quarter2_analysis_data$SLE <- as.factor(quarter2_analysis_data$SLE)

quarter3_analysis_data <- process_scores(quarter3_data_1, quarter3_psychometric_vars_list)%>% filter(!is.na(phq_score))
quarter3_analysis_data<-quarter3_analysis_data %>% left_join(q3_sle_analysis,by='participantidentifier')
quarter3_analysis_data$SLE <- as.factor(quarter3_analysis_data$SLE)

quarter4_analysis_data <- process_scores(quarter4_data_1, quarter4_psychometric_vars_list)%>% filter(!is.na(phq_score))
quarter4_analysis_data<-quarter4_analysis_data %>% left_join(q4_sle_analysis,by='participantidentifier')
quarter4_analysis_data$SLE <- as.factor(quarter4_analysis_data$SLE)
