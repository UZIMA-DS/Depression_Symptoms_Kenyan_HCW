# Convert relevant columns to numeric
baseline_varlists<-colnames(baseline_complete)
q1_varlist<-colnames(quarter1_data)
q2_varlist<-colnames(quarter2_data)
q3_varlist<-colnames(quarter3_data)
q4_varlist<-colnames(quarter4_data)

##Function to convert the columns to numeric data type
convert_numeric <- function(data, vars) {
  data %>% select(all_of(vars)) %>% mutate(across(-participantidentifier, ~ as.numeric(.)))
  
}

baseline_data_1 <- convert_numeric(baseline_complete, baseline_varlists)
quarter1_data_1<-convert_numeric(quarter1_data,q1_varlist)
quarter2_data_1<-convert_numeric(quarter2_data,q2_varlist)
quarter3_data_1<-convert_numeric(quarter3_data,q3_varlist)
quarter4_data_1<-convert_numeric(quarter4_data,q4_varlist)
