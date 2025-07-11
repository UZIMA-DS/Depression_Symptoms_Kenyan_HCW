#### Attrition Weighting
attrition_dataset.imp<-dropout_corr_df.imp %>% inner_join(baseline_imputed_data, by='participantidentifier')%>%
  select('participantidentifier','t0_phq.phq_score','phq_category','Gender','Cadre','Age','Marital','Children','efe_score','Hoursworked',
         'neuroticsm_score','Discrimination','Majorerror','Experience','SLE','dropout_all')

ps_data=as.data.frame(attrition_dataset.imp)
pscore <- ps(dropout_all ~ Age+neuroticsm_score,data = ps_data,estimand = "ATE",verbose = F)
bal.table(pscore)
ps_data$attrweight <- get.weights(pscore,stop.method = 'es.mean')
summary(ps_data$attrweight)

quantile(ps_data$attrweight, .99) 
ps_data$attrweight.trim=ifelse(ps_data$attrweight>quantile(ps_data$attrweight, .99),quantile(ps_data$attrweight, .99),ps_data$attrweight) # trimming: exclude extreme values
ps_data$new_weight <- ps_data$attrweight * (length(ps_data$participantidentifier) / sum(ps_data$attrweight, na.rm = TRUE))
