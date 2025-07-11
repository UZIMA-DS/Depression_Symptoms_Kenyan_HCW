##2. Data Imputation
baseline_data_imputation_method <- make.method(baseline_analysis_data)
q1_data_imputation<-make.method(quarter1_analysis_data)
q2_data_imputation<-make.method(quarter2_analysis_data)
q3_data_imputation<-make.method(quarter3_analysis_data)
q4_data_imputation<-make.method(quarter4_analysis_data)


baseline_imputed_data_results <- mice(baseline_analysis_data, method = baseline_data_imputation_method, m = 1, seed = 6082023)
baseline_imputed_data<-complete(baseline_imputed_data_results, 1)

baseline_imputed_data$Discrimination[baseline_imputed_data$Discrimination == 2] <- 0

baseline_imputed_data$Majorerror[baseline_imputed_data$Majorerror == 2] <- 0
baseline_imputed_data$Treatment[baseline_imputed_data$Treatment == 2] <- 0
baseline_imputed_data$participantidentifier<-factor(baseline_imputed_data$participantidentifier)
baseline_imputed_data$Cadre[baseline_imputed_data$Cadre>2 & baseline_imputed_data$Cadre<=13]<-3
baseline_imputed_data$Marital[baseline_imputed_data$Marital==1 | baseline_imputed_data$Marital==5]<-0
baseline_imputed_data$Marital[baseline_imputed_data$Marital==2 | baseline_imputed_data$Marital==3| baseline_imputed_data$Marital==4]<-1


baseline_imputed_data$Gender=factor(baseline_imputed_data$Gender,levels = c(0,1),labels = c("Male","Female"))

baseline_imputed_data$Cadre=factor(baseline_imputed_data$Cadre,levels = c(1,2,3),
                                   labels = c("Doctor","Nurse","Other cadre"))
baseline_imputed_data$Marital=factor(baseline_imputed_data$Marital,levels = c(0,1), labels = c("Not in a committed relationship","In a committed Relationship"))


baseline_imputed_data$Discrimination <- factor(baseline_imputed_data$Discrimination, levels = c(0, 1),labels = c("No", "Yes"))
baseline_imputed_data$Majorerror <- factor(baseline_imputed_data$Majorerror, levels = c(0, 1),labels = c("No", "Yes"))
baseline_imputed_data$Treatment <- factor(baseline_imputed_data$Treatment, levels = c(0, 1),labels = c("No", "Yes"))

q1_imputed_data_results <- mice(quarter1_analysis_data, method = q1_data_imputation, m = 1, seed = 6082023)
q1_imputed_data<-complete(q1_imputed_data_results, 1)
q1_imputed_data$Discrimination_1[q1_imputed_data$Discrimination_1 == 2] <- 0
q1_imputed_data$Majorerror_1[q1_imputed_data$Majorerror_1 == 2] <- 0
q1_imputed_data$Treatment_1[q1_imputed_data$Treatment_1 == 2] <- 0
q1_imputed_data$participantidentifier<-factor(q1_imputed_data$participantidentifier)
q1_imputed_data$Discrimination_1 <- factor(q1_imputed_data$Discrimination_1, levels = c(0, 1),labels = c("No", "Yes"))
q1_imputed_data$Majorerror_1 <- factor(q1_imputed_data$Majorerror_1, levels = c(0, 1),labels = c("No", "Yes"))
q1_imputed_data$Treatment_1 <- factor(q1_imputed_data$Treatment_1, levels = c(0, 1),labels = c("No", "Yes"))

q2_imputed_data_results <- mice(quarter2_analysis_data, method = q2_data_imputation, m = 1, seed = 6082023)
q2_imputed_data<-complete(q2_imputed_data_results, 1)
q2_imputed_data$Discrimination_2[q2_imputed_data$Discrimination_2 == 2] <- 0
q2_imputed_data$Majorerror_2[q2_imputed_data$Majorerror_2 == 2] <- 0
q2_imputed_data$Treatment_2[q2_imputed_data$Treatment_2 == 2] <- 0
q2_imputed_data$participantidentifier<-factor(q2_imputed_data$participantidentifier)
q2_imputed_data$Discrimination_2 <- factor(q2_imputed_data$Discrimination_2, levels = c(0, 1),labels = c("No", "Yes"))
q2_imputed_data$Majorerror_2 <- factor(q2_imputed_data$Majorerror_2, levels = c(0, 1),labels = c("No", "Yes"))
q2_imputed_data$Treatment_2 <- factor(q2_imputed_data$Treatment_2, levels = c(0, 1),labels = c("No", "Yes"))

q3_imputed_data_results <- mice(quarter3_analysis_data, method = q3_data_imputation, m = 1, seed = 6082023)
q3_imputed_data<-complete(q3_imputed_data_results, 1)
q3_imputed_data$Discrimination_3[q3_imputed_data$Discrimination_3 == 2] <- 0
q3_imputed_data$Majorerror_3[q3_imputed_data$Majorerror_3 == 2] <- 0
q3_imputed_data$Treatment_3[q3_imputed_data$Treatment_3 == 2] <- 0
q3_imputed_data$participantidentifier<-factor(q3_imputed_data$participantidentifier)
q3_imputed_data$Discrimination_3 <- factor(q3_imputed_data$Discrimination_3, levels = c(0, 1),labels = c("No", "Yes"))
q3_imputed_data$Majorerror_3 <- factor(q3_imputed_data$Majorerror_3, levels = c(0, 1),labels = c("No", "Yes"))
q3_imputed_data$Treatment_3 <- factor(q3_imputed_data$Treatment_3, levels = c(0, 1),labels = c("No", "Yes"))


q4_imputed_data_results <- mice(quarter4_analysis_data, method = q4_data_imputation, m = 1, seed = 6082023)
q4_imputed_data<-complete(q4_imputed_data_results, 1)
q4_imputed_data$Discrimination_4[q4_imputed_data$Discrimination_4 == 2] <- 0
q4_imputed_data$Majorerror_4[q4_imputed_data$Majorerror_4 == 2] <- 0
q4_imputed_data$Treatment_4[q4_imputed_data$Treatment_4 == 2] <- 0
q4_imputed_data$participantidentifier<-factor(q4_imputed_data$participantidentifier)
q4_imputed_data$Discrimination_4 <- factor(q4_imputed_data$Discrimination_4, levels = c(0, 1),labels = c("No", "Yes"))
q4_imputed_data$Majorerror_4 <- factor(q4_imputed_data$Majorerror_4, levels = c(0, 1),labels = c("No", "Yes"))
q4_imputed_data$Treatment_4 <- factor(q4_imputed_data$Treatment_4, levels = c(0, 1),labels = c("No", "Yes"))

