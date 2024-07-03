################################################################# sleep in 2023
## This data comes from the sleep_analysis 01


sleep_2023 <- sleep_cleaning %>%
  filter(start_date >= "2023-07-01" & start_date <= "2023-12-31")
# clean
sleep_2023 <- sleep_2023 %>%
  rename(participantidentifier=participant_identifier)

##### Then, combine sleep and quarter two values
sleep_q2 <- inner_join(q2_merged_cleaned, sleep_2023, by='participantidentifier')
# sleep q2 is now a 'master table'

# Now, Merge participants in Filtered list to sleep q2 data
# this assumption is based on

sleep_2023_filtered <- sleep_q2 %>% filter(participantidentifier %in% filtered) ## down to 247

################################################################################ 

sleep_2023_filtered %>%
  ggplot(aes(start_date,minutes_asleep))+
  geom_line()+
  labs(title="Minutes asleep",
       x='',
       y='Time in Minutes')+
  theme_bw()

#### extract only average
avg_sleep_2023 <- sleep_2023_filtered %>%
  group_by(start_date) %>%
  summarise(avg_sleep = mean(minutes_asleep))


avg_sleep_2023 %>%
  ggplot(aes(start_date,avg_sleep))+
  geom_line()+
  labs(title="Average of Minutes asleep",
       subtitle = 'For Participants in Q1 and Q2 2023',
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep_2023$avg_sleep)
avg_sleep_summary <- get_summary_stats(avg_sleep_2023)


avg_summary_table <- tableGrob(avg_sleep_summary)
png("MinutesAsleep_Q1n2_2023.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()

