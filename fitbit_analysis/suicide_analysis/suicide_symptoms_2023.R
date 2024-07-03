### In this section, data is imported from the depression score of q1 and q2 
# done in 2023

################################################################# SUICIDE
suicide_data <- sleep_2023_answers_viz %>%
  filter(str_detect(resultidentifier, "suic"))
class(suicide_data$answers)

count <- rep(1, times=247)
suicide_data <- cbind(suicide_data,count)
suicide_data %>%
  ggplot(aes(answers, count))+
  geom_col()

suicide_scores_table <- suicide_data %>%
  group_by(answers) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(suicide_scores_table)

list_247Val <- suicide_data$participantidentifier
################################################## get those who selected 1 and 2

suicide_ideation <- suicide_data %>%
  filter(answers %in% c("1","2")) %>%
  select(participantidentifier)

suicide_ideation_list <- as.list(suicide_ideation$participantidentifier)

## get the sleep pattern of the 22 people
suicide_filtered <- sleep_q2 %>% filter(participantidentifier %in% suicide_ideation_list)

suicide_filtered <- suicide_filtered %>% arrange(start_date)

######################################################## get those who selected 0

suicide_no_ideation <- suicide_data %>%
  filter(answers == "0") %>%
  select(participantidentifier)

######### get a random of 22 participants who didn't report suicide symptoms
suicide_no_ideation22 <- suicide_no_ideation %>%
  sample_n(22)

suicide_ideation22_list <- as.list(suicide_no_ideation22$participantidentifier)

## get them in a table
suicide22_filtered <- sleep_q2 %>% filter(participantidentifier %in% suicide_ideation22_list)

suicide22_filtered <- suicide22_filtered %>% arrange(start_date)