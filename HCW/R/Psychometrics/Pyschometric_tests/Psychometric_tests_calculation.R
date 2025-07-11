#2.  Function to calculate the Psychometric tests 
calculate_psychometric_assessment_score <- function(data, vars_list) {
  #data <- list(participantidentifier = data$participantidentifier)
  
  # Calculate EFE score
  if (!is.null(vars_list$efe_vars) && all(vars_list$efe_vars %in% colnames(data)) && 
      !is.null(vars_list$efe_positive_vars) && all(vars_list$efe_positive_vars %in% colnames(data))) {
    data$efe_score <- ifelse(
      rowSums(is.na(data[vars_list$efe_vars])) <= 3 & 
        rowSums(is.na(data[vars_list$efe_positive_vars])) <= 1,
      (10 * rowMeans(data[vars_list$efe_vars], na.rm = TRUE) - 10) + 
        (18 - 3 * rowMeans(data[vars_list$efe_positive_vars], na.rm = TRUE)),
      NA
    )
  }
  
  # Calculate Neuroticism score
  if (!is.null(vars_list$neuroticsm_vars1) && all(vars_list$neuroticsm_vars1 %in% colnames(data)) && 
      !is.null(vars_list$neuroticsm_vars2) && all(vars_list$neuroticsm_vars2 %in% colnames(data))) {
    data$neuroticsm_score <- ifelse(
      rowSums(is.na(data[vars_list$neuroticsm_vars1])) <= 2 & 
        rowSums(is.na(data[vars_list$neuroticsm_vars2])) <= 2,
      (24 - 6 * rowMeans(data[vars_list$neuroticsm_vars1], na.rm = TRUE)) + 
        (8 * rowMeans(data[vars_list$neuroticsm_vars2], na.rm = TRUE)),
      NA
    )
  }
  
  # Calculate PHQ-9 score
  if (!is.null(vars_list$phq_vars) && all(vars_list$phq_vars %in% colnames(data))) {
    data$phq_score <- ifelse(
      rowSums(is.na(data[vars_list$phq_vars])) <= 3,
      9 * rowMeans(data[vars_list$phq_vars], na.rm = TRUE),
      ifelse(
        rowSums(is.na(data[vars_list$phq_vars])) == 0,
        rowSums(data[vars_list$phq_vars]),
        NA
      )
    )
    data$phq_category <- ifelse(is.na(data$phq_score), NA, 
                                ifelse(data$phq_score >= 10, 1, 0))
  }
  
  # Calculate GAD-7 score
  if (!is.null(vars_list$gad_vars) && all(vars_list$gad_vars %in% colnames(data))) {
    data$gad_score <- ifelse(
      rowSums(is.na(data[vars_list$gad_vars])) <= 2,
      7 * rowMeans(data[vars_list$gad_vars], na.rm = TRUE),
      NA
    )
    data$gad_category <- ifelse(is.na(data$gad_score), NA, 
                                ifelse(data$gad_score >= 10, 1, 0))
  }
  
  # Calculate PTSD Score and PTSD Category
  if (!is.null(vars_list$ptsd_event_vars) && all(vars_list$ptsd_event_vars %in% colnames(data)) &&
      !is.null(vars_list$ptsd_symptom_vars) && all(vars_list$ptsd_symptom_vars %in% colnames(data))) {
    
    ptsd_score <- ifelse(
      rowSums(data[vars_list$ptsd_event_vars] == 1, na.rm = TRUE) > 0,
      rowSums(data[vars_list$ptsd_symptom_vars], na.rm = TRUE), 
      NA
    )
    
    data$ptsd_score <- ptsd_score
    
    data$ptsd_category <- ifelse(is.na(ptsd_score), NA,
                                 ifelse(ptsd_score >= 31, 1, 0))
  }
  
  result_data <- as.data.frame(data)
  return(data)
}

##Helper function to help calculate the psychometric scores and remove the single columns for each psychometric category
process_scores <- function(data, vars_list, score_function = calculate_psychometric_assessment_score) {
  scores <- score_function(data, vars_list)
  vars_to_remove <- unlist(vars_list)
  analysis_data <- scores[, !colnames(scores) %in% vars_to_remove]
  return(analysis_data)
}
