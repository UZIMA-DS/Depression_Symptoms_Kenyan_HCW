
#### GEE Model
gee_t0_data.imp<-baseline_imputed_data %>% 
  select(participantidentifier,Discrimination,Majorerror,Treatment,Hoursworked,SLE,phq_score) %>% mutate(time = 0)

gee_t1_data.imp<-q1_imputed_data %>% 
  select(participantidentifier,Discrimination_1,Majorerror_1,Treatment_1,Hoursworked_1,SLE,phq_score)%>%
  rename_with(~ str_remove(.x, "_1")) %>%
  mutate(time = 1)

gee_t2_data.imp<-q2_imputed_data %>% 
  select(participantidentifier,Discrimination_2,Majorerror_2,Treatment_2,Hoursworked_2,SLE,phq_score)%>%
  rename_with(~ str_remove(.x, "_2")) %>%
  mutate(time = 2)

gee_t3_data.imp<-q3_imputed_data %>% 
  select(participantidentifier,Discrimination_3,Majorerror_3,Treatment_3,Hoursworked_3,SLE,phq_score)%>%
  rename_with(~ str_remove(.x, "_3")) %>%
  mutate(time = 3)

gee_t4_data.imp<-q4_imputed_data %>%
  select(participantidentifier,Discrimination_4,Majorerror_4,Treatment_4,Hoursworked_4,SLE,phq_score)%>%
  rename_with(~ str_remove(.x, "_4")) %>%
  mutate(time = 4)

gee_t0_t4_data.imp<- bind_rows(gee_t0_data.imp, gee_t1_data.imp, gee_t2_data.imp, gee_t3_data.imp, gee_t4_data.imp) %>%
  arrange(participantidentifier, time)

gee_sle_full_data.imp<-gee_t0_t4_data.imp %>% left_join(sle_imputed_data,by=c('participantidentifier','time'))

attrition_weights_df<-ps_data[,c("participantidentifier","Gender","Cadre","Age","Marital","Children","Experience","efe_score","neuroticsm_score","dropout_all","attrweight" ,"new_weight" )]
gee_data_weighted.imp<-merge(gee_t0_t4_data.imp ,attrition_weights_df, by='participantidentifier', all.x = TRUE)

###GEE Data preprocessing

gee_data_weighted.imp$participantidentifier<-factor(gee_data_weighted.imp$participantidentifier)

gee_data_weighted.imp$Hours_normalized<-scale(gee_data_weighted.imp$Hoursworked,scale = TRUE,center = TRUE)[,1]
gee_data_weighted.imp$Experience_normalized<-scale(gee_data_weighted.imp$Experience,scale = TRUE,center = TRUE)[,1]
gee_data_weighted.imp$Cadre <- relevel(gee_data_weighted.imp$Cadre, ref = "Other cadre")

###GEE MODEL
gee_model_weighted <- geeglm(
  phq_score ~ Discrimination +Gender+ Majorerror  + SLE+ Hours_normalized +Cadre+Experience_normalized+
    Age + neuroticsm_score + efe_score + Marital ,
  id = participantidentifier,  
  data = gee_data_weighted.imp,
  family = gaussian,  
  corstr = "independence",
  weights = new_weight
  
)
summary(gee_model_weighted)
sjPlot::tab_model(gee_model_weighted, show.obs=F, show.ngroups = F)