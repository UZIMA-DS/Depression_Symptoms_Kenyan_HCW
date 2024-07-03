# In this script we will merge Quarter 1 and 2 participants.
# This will help us know which participants have done quarter 1 and 2 surveys

# Quarter 1 data
# - q1_answers_widetotal

# Quarter 2 data
# - q2_answers_widetotal

# Merge q1 with q2 participant's list based on the Participant intensifier's list
Merged_answers_widetotal <- q1_answers_widetotal %>% filter(participantidentifier %in% q2_participants_list)

# takes the number to 287
# Now we save it to a list
filtered <- Merged_answers_widetotal$participantidentifier