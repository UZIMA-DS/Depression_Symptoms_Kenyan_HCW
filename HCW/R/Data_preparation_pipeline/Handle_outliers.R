
###1. Remove outliers
clean_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  data_cleaned <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  return(data_cleaned)
}

baseline_analysis_data <- clean_outliers(baseline_analysis_data, "Hoursduty")
baseline_analysis_data <- baseline_analysis_data %>% mutate(Hoursworked = ifelse(Hoursworked > 168, NA, Hoursworked))
baseline_analysis_data$Experience[baseline_analysis_data$Experience == 1010] <- NA

baseline_analysis_data$hours_normalized<-scale(baseline_analysis_data$Hoursworked,scale = TRUE,center = TRUE)[,1]
baseline_analysis_data$Gender[baseline_analysis_data$Gender==1]<-0
baseline_analysis_data$Gender[baseline_analysis_data$Gender==2]<-1
baseline_analysis_data$Gender[baseline_analysis_data$Gender==4]<-NA


baseline_analysis_data<-baseline_analysis_data %>% select(participantidentifier,Age,Gender,Marital,Children,Discrimination,Hoursworked,
                                                          hours_normalized,efe_score,neuroticsm_score,phq_score,phq_category,gad_score,
                                                          gad_category,Treatment,Majorerror,Experience,Cadre,SLE)

q1_hdd_data_cleaned <- clean_outliers(quarter1_analysis_data, "Hoursduty_1")
quarter1_analysis_data <- quarter1_analysis_data %>% mutate(Hoursworked = ifelse(Hoursworked_1 > 168, NA, Hoursworked_1))
quarter1_analysis_data$hours_normalized<-scale(quarter1_analysis_data$Hoursworked_1,scale = TRUE,center = TRUE)[,1]

quarter1_analysis_data<-quarter1_analysis_data %>% select(participantidentifier,phq_score,phq_category,SLE,gad_score,gad_category,hours_normalized,
                                                          Hoursworked_1,Discrimination_1,Majorerror_1,Treatment_1,)

q2_hdd_data_cleaned <- clean_outliers(quarter2_analysis_data, "Hoursduty_2")
quarter2_analysis_data <- quarter2_analysis_data %>% mutate(Hoursworked = ifelse(Hoursworked_2 > 168, NA, Hoursworked_2))
quarter2_analysis_data$hours_normalized<-scale(quarter2_analysis_data$Hoursworked_2,scale = TRUE,center = TRUE)[,1]


quarter2_analysis_data<-quarter2_analysis_data %>% select(participantidentifier,phq_score,phq_category,SLE,gad_score,gad_category,hours_normalized,
                                                          Hoursworked_2,Discrimination_2,Majorerror_2,Treatment_2)

q3_hdd_data_cleaned <- clean_outliers(quarter3_analysis_data, "Hoursduty_3")
quarter3_analysis_data <- quarter3_analysis_data %>% mutate(Hoursworked = ifelse(Hoursworked_3 > 168, NA, Hoursworked_3))
quarter3_analysis_data$hours_normalized<-scale(quarter3_analysis_data$Hoursworked_3,scale = TRUE,center = TRUE)[,1]


q4_hdd_data_cleaned <- clean_outliers(quarter4_analysis_data, "Hoursduty_4")
quarter4_analysis_data <- quarter4_analysis_data %>% mutate(Hoursworked = ifelse(Hoursworked_4 > 168, NA, Hoursworked_4))
quarter4_analysis_data$hours_normalized<-scale(quarter4_analysis_data$Hoursworked_4,scale = TRUE,center = TRUE)[,1]
