
# Process Quarter Data
quarter_wide <- lapply(quarter_data, function(qdata) {
  qdata %>%
    distinct(participantidentifier, resultidentifier, .keep_all = TRUE) %>%    # Remove duplicates based on participantidentifier and resultidentifier
    pivot_wider(names_from = resultidentifier, values_from = answers)
})
#Extract the quartely_data
quarter1_data <- quarter_wide[[1]] 
quarter2_data <- quarter_wide[[2]] 
quarter3_data <- quarter_wide[[3]]  
quarter4_data <- quarter_wide[[4]]