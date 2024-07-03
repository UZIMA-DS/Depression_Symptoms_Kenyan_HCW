## Run this file after running q1 setup336

########################################## 

# Classify depression scores
depression_scores <- q1_answers_widetotal %>%
  mutate(score = case_when(
    total <= 4 ~ "No depression",
    total <= 9 ~ "Mild depression",
    total <= 14 ~ "Moderate depression",
    total <= 19 ~ "Moderately Severe depression",
    TRUE ~ "Severe depression"
  ))

depression_scores %>%
  ggplot(aes(score))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

count <- rep(1, times=336)
depression_scores <- cbind(depression_scores,count)

depression_scores_table <- depression_scores %>%
  group_by(score) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(depression_scores_table)

#### save
depression_scores_table <- tableGrob(depression_scores_table)
png("q1_depression336.png", 600, 400)
grid.draw(depression_scores_table)
dev.off()
 

###############################################################################
# ANXIETY
### This code section contains q1 survey based on Anxiety scoring
# then filtered with participants reported in quarter 1



## get GAD 7
q1_dictionary <- clean_names(q1_dictionary)
gad_dictionary <- q1_dictionary[q1_dictionary$step_identifier == 'GAD7', ]
gad_version_5 <- gad_dictionary[gad_dictionary$survey_version == '5', ]
rownames(gad_version_5) <- NULL

# rename resultidentifier column
# survey_version_5
gad_version_5 <- gad_version_5 %>%
  rename(resultidentifier=result_identifier)

# merge data
gad_merged <- inner_join(gad_version_5, q1_all_participants, by="resultidentifier")
columns_4 <- c('participantidentifier','answers','resultidentifier','StartDate')
gad_merged_cleaned <- gad_merged[, columns_4, drop=FALSE]



## DATA VIZ
# clean
gad_answers_viz <- gad_merged_cleaned %>%
  group_by(resultidentifier) %>%
  mutate(count=n()) %>%
  ungroup()

gad_answers_viz <- gad_answers_viz %>% arrange(participantidentifier)
# display
gad_answers_viz %>%
  ggplot(aes(resultidentifier,fill=answers))+
  geom_bar(position="fill", col="black")

gad_answers_viz$answers <- as.numeric(gad_answers_viz$answers)
# converted to numeric

# change data frame to wide
gad_wide <- gad_answers_viz %>%
  pivot_wider(
    id_cols = participantidentifier,
    names_from = resultidentifier,
    values_from = answers
  )
# add a total column
gad_answers_widetotal <- gad_wide %>%
  rowwise() %>% 
  mutate(total=sum(c_across(norelax0_1:worrying0_1)))


# Classify Anxiety Scores
gad_scores <- gad_answers_widetotal %>%
  mutate(score = case_when(
    total <= 4 ~ "No anxiety",
    total <= 9 ~ "Mild",
    total <= 14 ~ "Moderate",
    TRUE ~ "Severe anxiety"
  ))

gad_scores %>%
  ggplot(aes(score))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

count <- rep(1, times=336)
gad_scores <- cbind(gad_scores,count)

gad_scores_table <- gad_scores %>%
  group_by(score) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(gad_scores_table)

Anxiety_scores_table <- tableGrob(gad_scores_table)
png("q1_anxiety336.png", 600, 400)
grid.draw(Anxiety_scores_table)
dev.off()






################################################################################
###
# Substance Used
## get PHQ
q1_dictionary <- clean_names(q1_dictionary)
Substancesused_dictionary <- q1_dictionary[q1_dictionary$step_identifier == 'Substancesused', ]
Substancesused_v5 <- Substancesused_dictionary[Substancesused_dictionary$survey_version == '5', ]
rownames(Substancesused_dictionary) <- NULL

# rename resultidentifier column
# survey_version_5
Substancesused_v5 <- Substancesused_v5 %>%
  rename(resultidentifier=result_identifier)

# merge data
Substancesused_v5_merged <- inner_join(Substancesused_v5, q1_all_participants, by="resultidentifier")
columns_4 <- c('participantidentifier','answers','resultidentifier','StartDate')
Substancesused_v5_merged_cleaned <- Substancesused_v5_merged[, columns_4, drop=FALSE]


## DATA VIZ SUBSTANCE
# clean
substance_answers_viz <- Substancesused_v5_merged_cleaned %>%
  group_by(resultidentifier) %>%
  mutate(count=n()) %>%
  ungroup()

substance_answers_viz <- substance_answers_viz %>% arrange(participantidentifier)

class(substance_answers_viz$answers)
# character
substance_answers_viz$answers <- as.numeric(substance_answers_viz$answers)
# converted to numeric

substance_answers_viz %>%
  ggplot(aes(resultidentifier,answers, fill = answers))+
  geom_bar(stat="identity")

substance_answers_viz <- substance_answers_viz [,-5]

count <- rep(1, times=3372)
substance_answers_viz <- cbind(substance_answers_viz,count)


##########################
# Alcohol
substance_alcohol <- substance_answers_viz[substance_answers_viz$resultidentifier == 'Alcohol_1',]
substance_alcohol <- substance_alcohol %>% distinct()

alcohol_table <- substance_alcohol %>%
  group_by(answers) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(alcohol_table)

alcohol_scores_table <- tableGrob(alcohol_table)
png("Alcohol 336.png", 600, 400)
grid.draw(alcohol_scores_table)
dev.off()

##########################
# Cannabis
substance_cannabis <- substance_answers_viz[substance_answers_viz$resultidentifier == 'Cannabis_1',]
substance_cannabis <- substance_cannabis %>% distinct()

cannabis_table <- substance_cannabis %>%
  group_by(answers) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(cannabis_table)

cannabis_scores_table <- tableGrob(cannabis_table)
png("Cannabis 336.png", 600, 400)
grid.draw(cannabis_scores_table)
dev.off()

##########################
# Tobacco
substance_tobacco <- substance_answers_viz[substance_answers_viz$resultidentifier == 'Tobacco_1',]
substance_tobacco <- substance_tobacco %>% distinct()

tobacco_table <- substance_tobacco %>%
  group_by(answers) %>%
  summarise(count_a = n()) %>%
  mutate(percentage = count_a / sum(count) * 100)

print(tobacco_table)

tobacco_scores_table <- tableGrob(tobacco_table)
png("Tobacco 336.png", 600, 400)
grid.draw(tobacco_scores_table)
dev.off()


