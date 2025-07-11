#### Baseline psychometric Assesment Vars
baseline_ptsd_event_vars <- c('Disaster', 'Accident', 'Robbed', 'Beatfamily', 'Beatnotfamily', 'Familybeat', 
                              'Commbeat', 'Touch', 'Pressure', 'Dying', 'Attacked', 'Someoneattack', 
                              'Stressfulmed', 'War', 'Otherstressful')
baseline_ptsd_symptom_vars <- c('Upset', 'Dreams', 'Event', 'Emotion', 'Reactions', 'Remember', 'Reminder', 
                                'Notremember', 'Negativechange', 'Thinking', 'Negativeemotion', 'Distant', 
                                'Notpositive', 'Irritable', 'Riskybehav', 'Alert', 'Jumpy', 'Concentration', 'Sleep')
baseline_efe_vars <- c('insulted', 'pushed', 'drinkers', 'violenhouse', 'quarrpar', 
                       'quarryoupar', 'quarrparsib', 'quarryousib', 'chaotic', 'neglected')
baseline_efe_positive_vars <- c('loved', 'hugged', 'wellhouse')
baseline_phq_vars <- c('interest0', 'down0', 'asleep0', 'tired0', 'appetite0', 
                       'failure0', 'concentr0', 'activity0', 'suic0')
baseline_gad_vars <- c('nervous0', 'worrying0', 'muchworry0', 'norelax0', 'restless0', 
                       'irritable0', 'afraid0')
baseline_neuroticsm_vars1 <- c('anxious', 'emotions', 'worrier', 'noblue', 'nodepr', 'comfort')
baseline_neuroticsm_vars2 <- c('wrong', 'thoughts', 'giveup', 'helpless', 'overeat', 'hide', 'pieces', 'inferior')

baseline_vars_list <- list(
  efe_vars = baseline_efe_vars,
  efe_positive_vars = baseline_efe_positive_vars,
  phq_vars = baseline_phq_vars,
  gad_vars = baseline_gad_vars,
  neuroticsm_vars1 = baseline_neuroticsm_vars1,
  neuroticsm_vars2 = baseline_neuroticsm_vars2,
  ptsd_event_vars=baseline_ptsd_event_vars,
  ptsd_symptom_vars=baseline_ptsd_symptom_vars
)
