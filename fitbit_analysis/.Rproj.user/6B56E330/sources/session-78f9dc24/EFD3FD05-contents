############################################################# sleep log analysis

sleep_log <- dbReadTable(con, "FactFitBitSleepLog")
dim(sleep_log)
# -68974 23      as per 29th/June

## check column names and clean
names(sleep_log)
sleep_log <- clean_names(sleep_log)


# First sleep analysis, the following variables were slelected.
# - Efficiency
# - Minutes asleep
# - Sleep level light
# - Sleep level deep
# - Sleep level rem


columns_sleep  <- c('participant_identifier','start_date','efficiency','minutes_asleep',
              'sleep_level_light','sleep_level_deep','sleep_level_rem')
sleep_minutes <- sleep_log[, columns_sleep, drop =FALSE]

#################################################################################

######## All data points minutes asleep
#### saved
sleep_minutes %>%
  ggplot(aes(start_date,minutes_asleep))+
  geom_line()+
  labs(title="Minutes asleep",
       x='',
       y='Time in Minutes')+
  theme_bw()

############ clean data
# - drop duplicates
sleep_cleaning <- sleep_minutes %>% distinct()

sleep_cleaning$start_date <- as.Date(sleep_cleaning$start_date)

sleep_cleaning <- sleep_cleaning %>% arrange(start_date)

# plot with simple moving average
# sleep minutes asleep
sleep_cleaning %>%
  ggplot(aes(start_date,minutes_asleep))+
  geom_line()+
  geom_ma(ma_fun = SMA, n = 30)+
  labs(title="Moving average of Minutes asleep",
       subtitle='(window n=30)',
       x='',
       y='Time in Minutes')+
  theme_bw()

######## extract only average
avg_sleep<-sleep_cleaning %>%
  group_by(start_date) %>%
  summarise(avg_sleep = mean(minutes_asleep))

#### saved
avg_sleep %>%
  ggplot(aes(start_date,avg_sleep))+
  geom_line()+
  labs(title="Average of Minutes asleep",
       x='',
       y='Time in Minutes')+
  theme_bw()

summary(avg_sleep$avg_sleep)
avg_sleep_summary <- get_summary_stats(avg_sleep)

#### saved
avg_summary_table <- tableGrob(avg_sleep_summary)
png("avg_MinutesAsleep.png", 600, 400)
grid.draw(avg_summary_table)
dev.off()


