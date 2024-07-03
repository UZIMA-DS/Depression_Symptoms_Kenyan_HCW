############################################################ DEPRESSION Q1 n Q2

####### This analysis focuses on Depression symptoms between July to Dec 2023
# They are based on q1 and q2 survey results

sleep_2023_viz <- sleep_2023_filtered %>%
  group_by(resultidentifier) %>%
  mutate(count=n()) %>%
  ungroup()

sleep_2023_answers_viz <- sleep_2023_viz %>% arrange(participantidentifier)


## DATA VIZ
# display
sleep_2023_answers_viz %>%
  ggplot(aes(resultidentifier,fill=answers))+
  geom_bar(position="fill", col="black")

sleep_2023_answers_viz$answers <- as.numeric(sleep_2023_answers_viz$answers)
# converted to numeric
#############################################################################
# run this before next one to create a duplicate
sleep_variables <-  sleep_2023_answers_viz

#############################################################################
columns_5 <- c('participantidentifier','answers','resultidentifier')
sleep_2023_answers_viz <- sleep_2023_answers_viz[, columns_5, drop=FALSE]
sleep_2023_answers_viz <- sleep_2023_answers_viz %>% distinct()

is.null(sleep_2023_answers_viz$answers)

# change data frame to wide
sleep_2023_answers_wide <- sleep_2023_answers_viz %>%
  pivot_wider(
    id_cols = participantidentifier,
    names_from = resultidentifier,
    values_from = answers
  )
############################################################################
#### 247 participants

# add a total column
sleep_2023_answers_widetotal <- sleep_2023_answers_wide %>%
  rowwise() %>% 
  mutate(total=sum(c_across(concentr:failure)))

# Classify depression scores
sleep_depression_scores <- sleep_2023_answers_widetotal %>%
  mutate(score = case_when(
    total <= 4 ~ "No depression",
    total <= 9 ~ "Mild depression",
    total <= 14 ~ "Moderate depression",
    total <= 19 ~ "Moderately Severe depression",
    TRUE ~ "Severe depression"
  ))

sleep_depression_scores %>%
  ggplot(aes(score))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

count <- rep(1, times=247)
sleep_depression_scores <- cbind(sleep_depression_scores,count)

sleep_depression_scores_table <- sleep_depression_scores %>%
  group_by(score) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(sleep_depression_scores_table)

sleep_depression_table <- tableGrob(sleep_depression_scores_table)
png("Q1 n 2 depression 2023.png", 600, 400)
grid.draw(sleep_depression_table)
dev.off()

