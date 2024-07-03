################################################################# SUICIDE VIZ
# Those who didn't have suicide symptoms selected at random

## Data Viz
suicide22_filtered %>%
  ggplot(aes(start_date,minutes_asleep, group = participantidentifier))+
  geom_line()+
  labs(title="Minutes asleep",
       x='',
       y='Time in Minutes')+
  theme_bw()

##### plot with highlights
suicide22_filtered %>%
  ggplot(aes(start_date, minutes_asleep, colour = participantidentifier))+
  geom_point()+
  geom_line()+
  scale_color_viridis(discrete=TRUE)+
  gghighlight(max(minutes_asleep)> 300,
              unhighlighted_params = list(linewidth = 0.3,
                                          linetype = "dotted"),
              use_direct_label = FALSE)+
  theme(legend.position = "none")



## extract only average
suicide_sleep <-suicide22_filtered %>%
  group_by(start_date) %>%
  summarise(avg_sleep_val = mean(minutes_asleep))

suicide_sleep %>%
  ggplot(aes(start_date,avg_sleep_val))+
  geom_line()+
  labs(title="Average sleep",
       subtitle = "Participants reported No_symptoms",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(suicide_sleep$avg_sleep_val)
suicide_sleep_summary <- get_summary_stats(suicide_sleep)


suicide_sleep_summary <- tableGrob(suicide_sleep_summary)
png("N_MinutesAsleep.png", 600, 400)
grid.draw(suicide_sleep_summary)
dev.off()




######################################################## sleep level light 

## get sleep level light variable

sleep_light <- suicide22_filtered %>%
  select(participantidentifier, start_date, sleep_level_light)

### Drop null values
sleep_light <- sleep_light %>%
  filter(!is.na(sleep_level_light))

## extract only average
avg_sleep_light <- sleep_light %>%
  group_by(start_date) %>%
  summarise(avg_light = mean(sleep_level_light))


avg_sleep_light %>%
  ggplot(aes(start_date,avg_light))+
  geom_line()+
  labs(title="Average of Sleep Level Light",
       subtitle = "Participants reported No_symptoms",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep_light$avg_light)
avg_sleeplight_summary <- get_summary_stats(avg_sleep_light)

avg_summary_table <- tableGrob(avg_sleeplight_summary)
png("N_Sleeplight.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()


##################################################################### efficiency


sleep_efficiency <- suicide22_filtered %>%
  select(participantidentifier, start_date, efficiency)

### Drop null values
sleep_light <- sleep_light %>%
  filter(!is.na(sleep_level_light))

## extract only average
avg_sleep_efficiency <- sleep_efficiency %>%
  group_by(start_date) %>%
  summarise(avg_efficiency = mean(efficiency
  ))
avg_sleep_efficiency %>%
  ggplot(aes(start_date,avg_efficiency))+
  geom_line()+
  labs(title="Average of Sleep Efficiency",
       subtitle = "Participants reported No_symptoms",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep_efficiency$avg_efficiency)
avg_sleepefficiency_summary <- get_summary_stats(avg_sleep_efficiency)

avg_summary_table <- tableGrob(avg_sleepefficiency_summary)
png("N_Efficiency.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()

##################################################################### REM

sleep_rem <- suicide22_filtered %>%
  select(participantidentifier, start_date, sleep_level_rem)

### Drop null values
sleep_rem <- sleep_rem %>%
  filter(!is.na(sleep_level_rem))

## extract only average
avg_sleep_rem <- sleep_rem %>%
  group_by(start_date) %>%
  summarise(avg_rem = mean(sleep_level_rem
  ))

avg_sleep_rem %>%
  ggplot(aes(start_date,avg_rem))+
  geom_line()+
  labs(title="Average of Sleep REM",
       subtitle = "Participants reported No_symptoms",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep_rem$avg_rem)
avg_sleepeREM_summary <- get_summary_stats(avg_sleep_rem)

avg_summary_table <- tableGrob(avg_sleepeREM_summary)
png("N_REM_summary2023.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()



######################################################## sleep level DEEP


## get sleep level light variable

sleep_deep <- suicide22_filtered %>%
  select(participantidentifier, start_date, sleep_level_deep)

### Drop null values
sleep_deep <- sleep_deep %>%
  filter(!is.na(sleep_level_deep))

## extract only average
avg_sleep_deep <- sleep_deep %>%
  group_by(start_date) %>%
  summarise(avg_deep = mean(sleep_level_deep))


avg_sleep_deep %>%
  ggplot(aes(start_date,avg_deep))+
  geom_line()+
  labs(title="Average of Sleep Level Deep",
       subtitle = "Participants reported No_symptoms",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep_deep$avg_deep)
avg_sleepdeep_summary <- get_summary_stats(avg_sleep_deep)


avg_summary_table <- tableGrob(avg_sleepdeep_summary)
png("N_leveldeep_summary.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()
