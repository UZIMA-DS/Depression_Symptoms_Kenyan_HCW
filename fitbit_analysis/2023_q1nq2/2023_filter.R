###### Analysis on 2023 data

# Note: There's no time marked for end and start of either Quarter 1 or 2
# This script combines Q1 (336 participants) and Q2 (325 participants)
# and filters them to get, 287 participants.

# This participants are then filtered for the period July to December 2023

q2_2023 <- q2_merged_cleaned %>%
  filter(StartDate >= "2023-07-01" & StartDate <= "2023-12-31")

q1_2023 <- q1_all_participants_merged %>%
  filter(StartDate >= "2023-07-01" & StartDate <= "2023-12-31")

# q2 survey participants
q2_2023_list <- length(unique(q2_2023$participantidentifier))
## 140 participants

# q1 survey participants
q1_2023_list <- length(unique(q1_2023$participantidentifier))
## 275 participants