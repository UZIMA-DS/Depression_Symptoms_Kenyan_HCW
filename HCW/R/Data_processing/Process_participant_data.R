# Process Participant Data
participant_dob_enrollment <- dob_data %>% 
  inner_join(enrollment_data, by = 'participantidentifier') %>% 
  mutate(Age = floor(as.numeric(difftime(enrollmentdate, answers, units = 'weeks')) / 52.25))

# Pivot Baseline Data
baseline_wide <- baseline_data %>% 
  select(participantidentifier, resultidentifier, answers) %>% 
  pivot_wider(names_from = resultidentifier, values_from = answers)

# Merge with Age Data
baseline_complete <- baseline_wide %>%
  left_join(participant_dob_enrollment %>% select(participantidentifier, Age), by = 'participantidentifier')
