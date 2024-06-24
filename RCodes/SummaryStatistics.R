rm(list=ls())
library("readxl")
library(tidyverse)
library(gtsummary)
library(skimr)
library(officer)
library(reshape)
library(lcsm)
library(data.table)
library(reshape2)
library(flextable)
setwd("Your working directory here")
excel_sheets("SurveyResultsPivoted.xlsx")
baseline=read_excel("SurveyResultsPivoted.xlsx",
                    sheet = "Sheet1")
skim(baseline)
names(baseline)
###############################################
theme_gtsummary_journal(journal = "jama")
###PWD:cghr_ICF2
#> Setting theme `JAMA`
theme_gtsummary_compact()
#> Setting theme `Compact`
sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)
###################################
baseline$Sex=factor(baseline$Sex,levels = c(1,2),
                    labels = c("Male","Female"))
baseline$Gender=factor(baseline$Gender,levels = c(1,2),
                       labels = c("Male","Female"))
baseline$Sexorientation=factor(baseline$Sexorientation,levels = c(1:5),
                               labels = c("Heterosexual (sexually attracted to people of the opposite sex)", 
                                          "Gay / Lesbian (man who has a romantic and/or sexual orientation toward men / woman who has a romantic and/or sexual orientation toward women)", 
                                          "Bisexual (sexual attraction to both males and females)", 
                                          "Other",
                                          "Prefer not to say"))
baseline$Marital=factor(baseline$Marital,levels =c(1:5) ,
                        labels = c("Not in a committed relationship",
                                   "In a committed relationship", 
                                   "Engaged", 
                                   "Married",
                                   "Separated/ Divorced/ Widowed"))
baseline$Cadre=factor(baseline$Cadre,levels = c(1:13),
                      labels = c("Doctor", 
                                 "Nurse", 
                                 "Psychologist/ counsellor",
                                 "Occupational therapist", 
                                 "Medical student",
                                 "Pharmacist",
                                 "Physiotherapist", 
                                 "Dietician/ Nutritionist",
                                 "Social worker",
                                 "Medical equipment technician",
                                 "Laboratory technician",
                                 "Other",
                                 "Prefer not to say"))
baseline$Education=factor(baseline$Education,levels = c(1:6),
                          labels = c("Diploma", 
                                     "Bachelor's degree", 
                                     "Master's degree", 
                                     "PhD", 
                                     "Other", 
                                     "Prefer not to say"))
baseline$Residence=factor(baseline$Residence,levels = c(1:5),
                          labels = c("Rural",
                                     "Suburban", 
                                     "Urban", 
                                     "Other", 
                                     "Prefer not to say"))
baseline$Religiousactivity=factor(baseline$Religiousactivity,levels = c(1:5),
                                  labels = c("Rarely or never", 
                                             "A few times a month", 
                                             "Once a week", 
                                             "Two or more times a week", 
                                             "More than once a day"))
baseline$Discrimination=factor(baseline$Discrimination,levels = c(1,2),
                               labels = c("Yes","No"))
####Demographic
baseline|>
  dplyr::select(Sex,
                Gender,   
                Countryregion,
                Sexorientation,
                Marital,
                Children,      
                Cadre,Othercadre,
                Experience,
                Education,
                Residence,
                Religiousactivity,
                Discrimination)|>
  tbl_summary(label=c(Countryregion~"Country or region of birth",
                      Sexorientation~"sexual orientation",
                      Marital~"current marital status",
                      Children~"Number of children",      
                      Othercadre~"Other cadre",
                      Experience~"Years of experience",
                      Education~"Highest level of education",
                      Religiousactivity~"Time spent in religious activities",
                      Discrimination~"Been personally discriminated"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("demographic.docx"), pr_section = sect_properties)


##############################Neuroticism###################################
yesno=c("Strongly agree", 
        "Agree",
        "Neutral", 
        "Disagree", 
        "Strongly disagree")
baseline$anxious=factor(baseline$anxious,levels = c(4,3,2,1,0),
                        labels = yesno)
baseline$emotions=factor(baseline$emotions,levels = c(4,3,2,1,0),
                         labels = yesno)
baseline$worrier=factor(baseline$worrier,levels = c(4,3,2,1,0),
                        labels = yesno)
baseline$wrong=factor(baseline$wrong,levels = c(4,3,2,1,0),
                      labels = yesno)
baseline$thoughts=factor(baseline$thoughts,levels = c(4,3,2,1,0),
                         labels = yesno)
baseline$noblue=factor(baseline$noblue,levels = c(4,3,2,1,0),
                       labels = yesno)
baseline$giveup=factor(baseline$giveup,levels = c(4,3,2,1,0),
                       labels = yesno)
baseline$nodepr=factor(baseline$nodepr,levels = c(4,3,2,1,0),
                       labels = yesno)
baseline$helpless=factor(baseline$helpless,levels = c(4,3,2,1,0),
                         labels = yesno)
baseline$overeat=factor(baseline$overeat,levels = c(4,3,2,1,0),
                        labels = yesno)
baseline$hide=factor(baseline$hide,levels = c(4,3,2,1,0),
                     labels = yesno)
baseline$pieces=factor(baseline$pieces,levels = c(4,3,2,1,0),
                       labels = yesno)
baseline$inferior=factor(baseline$inferior,levels = c(4,3,2,1,0),
                         labels = yesno)
baseline$comfort=factor(baseline$comfort,levels = c(4,3,2,1,0),
                        labels = yesno)
baseline|>
  dplyr::select(anxious,           
                emotions,
                worrier,          
                wrong,
                thoughts,         
                noblue,
                giveup,            
                nodepr,
                helpless,          
                overeat,
                hide,              
                pieces,
                inferior,         
                comfort)|>
  tbl_summary(label = c(anxious~"I rarely feel anxious or nervous",           
                        emotions~"I rarely experience strong emotions",
                        worrier~"I am not a worrier",          
                        wrong~"I often worry about things that might go wrong",
                        thoughts~"Frightening thoughts sometimes come into my head",         
                        noblue~"I rarely feel lonely or blue",
                        giveup~"Too often, when things go wrong, I get discouraged and feel like giving up",            
                        nodepr~"I am seldom sad or depressed",
                        helpless~"I often feel helpless and want someone else to solve my problems",          
                        overeat~"When I am having my favorite foods, I tend to eat too much",
                        hide~"At times I have been so ashamed that I just wanted to hide",              
                        pieces~"When I'm under a great deal of stress, sometimes I feel like I'm going to pieces",
                        inferior~"I often feel inferior to others",         
                        comfort~"I feel comfortable in the presence of my bosses or other authorities"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("Neuroticism.docx"), pr_section = sect_properties)

############################Early Family Environment###################################
often=c("Not at all","2","3","4","5","Very often")
baseline$loved=factor(baseline$loved,levels = c(1:6),
                      labels = often)
baseline$insulted=factor(baseline$insulted,levels = c(1:6),
                         labels = often)
baseline$hugged=factor(baseline$hugged,levels = c(1:6),
                       labels = often)
baseline$pushed=factor(baseline$pushed,levels = c(1:6),
                       labels = often)
baseline$drinkers=factor(baseline$drinkers,levels = c(1:6),
                         labels = often)
baseline$wellhouse=factor(baseline$wellhouse,levels = c(1:6),
                          labels = often)
baseline$violenhouse=factor(baseline$violenhouse,levels = c(1:6),
                            labels = often)
baseline$quarrpar=factor(baseline$quarrpar,levels = c(1:6),
                         labels = often)
baseline$quarryoupar=factor(baseline$quarryoupar,levels = c(1:6),
                            labels = often)
baseline$quarrparsib=factor(baseline$quarrparsib,levels = c(1:6),
                            labels = often)
baseline$quarryousib=factor(baseline$quarryousib,levels = c(1:6),
                            labels = often)
baseline$chaotic=factor(baseline$chaotic,levels = c(1:6),
                        labels = often)
baseline$neglected=factor(baseline$neglected,levels = c(1:6),
                          labels = often)

baseline|>
  dplyr::select(loved,            
                insulted,
                hugged,
                pushed,           
                drinkers,
                wellhouse,
                violenhouse,       
                quarrpar,
                quarryoupar,
                quarrparsib,      
                quarryousib,
                chaotic,
                neglected)|>
  tbl_summary(label = c(loved~"How often did a parent or other adult in the household make you feel that you were loved, supported and cared for?",            
                        insulted~"How often did a parent or other adult in the household swear at you, insult you, put you down or act in a way that made you feel threatened?",
                        hugged~"How often did a parent or other adult in the household express physical affection for you, such as hugging or other physical gestures of warmth and affection?",
                        pushed~"How often did a parent or other adult in the household push, slap or shove you?",           
                        drinkers~"In your childhood, did you live with anyone who was a problem drinker or alcoholic or who used illicit drugs?",
                        wellhouse~"Would you say that the household you grew up in was well-organized and well-managed?",
                        violenhouse~"How often would you say that a parent or other adult in the household behaved violently toward a family member or visitor in your home?",       
                        quarrpar~"How often would you say that there was quarreling, arguing or shouting between your parents?",
                        quarryoupar~"How often would you say there was quarreling, arguing, or shouting between a parent and you?",
                        quarrparsib~"How often would you say there was quarreling, arguing, or shouting between a parent and one of your siblings?",      
                        quarryousib~"How often would you say there was quarreling, arguing, or shouting between your sibling(s) and you?",
                        chaotic~"Would you say the household you grew up in was chaotic and disorganized?",
                        neglected~"How often would you say you were neglected while you were growing up, that is, left on your own to fend for yourself?"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("erly_family.docx"), pr_section = sect_properties)

##########################################################################
phq9label=c("Not at all", 
            "Less than half the days", 
            "More than half the days", 
            "Nearly everyday")
baseline$down0=factor(baseline$down0,levels = c(0,1,2,3),labels =phq9label)
baseline$interest0=factor(baseline$interest0,levels = c(0,1,2,3),labels =phq9label)
baseline$asleep0=factor(baseline$asleep0,levels = c(0,1,2,3),labels =phq9label)
baseline$tired0=factor(baseline$tired0,levels = c(0,1,2,3),labels =phq9label)
baseline$appetite0=factor(baseline$appetite0,levels = c(0,1,2,3),labels =phq9label)
baseline$failure0=factor(baseline$failure0,levels = c(0,1,2,3),labels =phq9label)
baseline$concentr0=factor(baseline$concentr0,levels = c(0,1,2,3),labels =phq9label)
baseline$activity0=factor(baseline$activity0,levels = c(0,1,2,3),labels =phq9label)
baseline$suic0=factor(baseline$suic0,levels = c(0,1,2,3),labels =phq9label)
baseline$nervous0=factor(baseline$nervous0,levels = c(0,1,2,3),labels =phq9label)
baseline$worrying0=factor(baseline$worrying0,levels = c(0,1,2,3),labels =phq9label)
baseline$muchworry0=factor(baseline$muchworry0,levels = c(0,1,2,3),labels =phq9label)
baseline$norelax0=factor(baseline$norelax0,levels = c(0,1,2,3),labels =phq9label)
baseline$restless0=factor(baseline$restless0,levels = c(0,1,2,3),labels =phq9label)
baseline$irritable0=factor(baseline$irritable0,levels = c(0,1,2,3),labels =phq9label)
baseline$afraid0=factor(baseline$afraid0,levels = c(0,1,2,3),labels =phq9label)
baseline$Dead=factor(baseline$Dead,levels = c(0,1,2,3),labels =phq9label)
###############################################################
baseline|>
  dplyr::select(down0,interest0,         
                asleep0,tired0,            
                appetite0,failure0,          
                concentr0,activity0,         
                suic0,nervous0,          
                worrying0,muchworry0,        
                norelax0,restless0,         
                irritable0,afraid0,           
                Dead)|>
  tbl_summary(label = c(down0~"Feeling down, depressed or hopeless",
                        interest0~"Little interest or pleasure in doing things",         
                        asleep0~"Trouble falling asleep, staying asleep or sleeping too much",
                        tired0~"Feeling tired or having little energy",            
                        appetite0~"Poor appetite or overeating",
                        failure0~"Feeling badly about yourself, or that you are a failure, or that you have let yourself or your family down",          
                        concentr0~"Trouble concentrating on things like schoolwork, reading, or watching TV?",
                        activity0~"Moving or speaking so slowly that other people could have noticed.  Or the opposite-being so fidgety or restless that you have been moving around a lot more than usual",         
                        suic0~"Thoughts that you would be better off dead or hurting yourself in some way",
                        nervous0~"Feeling anxious, nervous, or on edge",          
                        worrying0~"Not being able to stop or control worrying",
                        muchworry0~"Worrying too much about different things",        
                        norelax0~"Trouble relaxing",
                        restless0~"Being so restless that it's hard to sit still",         
                        irritable0~"Becoming easily annoyed or irritable",
                        afraid0~"Feeling afraid as if something awful might happen",           
                        Dead~"How often have you been bothered by thoughts that you would be better off dead, or of hurting yourself in some way during the past 2 weeks?"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("phq9.docx"), pr_section = sect_properties)

#################################################################
workhrslabel=c("Strongly disagree",
               "Disagree",
               "Neutral",
               "Agree",
               "Strongly agree")
baseline$Masks=factor(baseline$Masks,levels = c(1:5),labels = workhrslabel)
baseline$Sanitizer=factor(baseline$Sanitizer,levels = c(1:5),labels = workhrslabel)
baseline$Handwashing=factor(baseline$Handwashing,levels = c(1:5),labels = workhrslabel)
baseline$Thermometer=factor(baseline$Thermometer,levels = c(1:5),labels = workhrslabel)
baseline$Gown=factor(baseline$Gown,levels = c(1:5),labels = workhrslabel)
baseline$Gloves=factor(baseline$Gloves,levels = c(1:5),labels = workhrslabel)
baseline$Eyeprotection=factor(baseline$Eyeprotection,levels = c(1:5),labels = workhrslabel)
baseline$Resources=factor(baseline$Resources,levels = c(1:5),labels = workhrslabel)
baseline$Workload=factor(baseline$Workload,levels = c(1:5),labels = workhrslabel)
baseline$Demands=factor(baseline$Demands,levels = c(1:5),labels = workhrslabel)
baseline$Rest=factor(baseline$Rest,levels = c(1:5),labels = workhrslabel)
baseline$Workload1=factor(baseline$Workload1,levels = c(1:5),labels = workhrslabel)
baseline$Selfcare=factor(baseline$Selfcare,levels = c(1:5),labels = workhrslabel)
baseline$Teamwork=factor(baseline$Teamwork,levels = c(1:5),labels = workhrslabel)
baseline$Discuss=factor(baseline$Discuss,levels = c(1:5),labels = workhrslabel)
baseline$Casemanage=factor(baseline$Casemanage,levels = c(1:5),labels = workhrslabel)
baseline$Infectionprev=factor(baseline$Infectionprev,levels = c(1:5),labels = workhrslabel)
################################################
baseline|>
  dplyr::select(Hoursduty,        
                Hoursworked,      
                Masks,       
                Sanitizer,        
                Handwashing,       
                Thermometer,       
                Gown,       
                Gloves,          
                Eyeprotection,     
                Resources,        
                Workload,         
                Demands,          
                Rest,           
                Workload1,        
                Selfcare,         
                Teamwork,          
                Discuss,          
                Casemanage,       
                Infectionprev)|>
  tbl_summary(label=c(Hoursduty~"In the last two weeks, how many days have you been on duty?",        
                      Hoursworked~"How many hours have you worked in the PAST WEEK?",      
                      Handwashing~"Handwashing facilities",       
                      Thermometer~"Infrared Thermometer",       
                      Gown~"Waterproof gown",       
                      Eyeprotection~"Eye Protection (Googles or Face Shield)",     
                      Resources~"You have the equipment and resources needed to do your job well and safely during this time of COVID 19",        
                      Workload~"Your workload during the COVID-19 pandemic is manageable and the expectations are reasonable",         
                      Demands~"You are able to reasonably balance the demands of work and personal life during the COVID-19 pandemic",          
                      Rest~"You have adequate time for rest and recovery from your work during this COVID 19 pandemic",           
                      Workload1~"Your workload is similar to the one you experienced before the COVID 19 pandemic",        
                      Selfcare~"Your workplace promotes self-care and well-being techniques",         
                      Teamwork~"There is good teamwork in your place of work during this COVID 19 pandemic",          
                      Discuss~"You are able to discuss concerns with your manager",          
                      Casemanage~"You have been trained in COVID-19 case management",       
                      Infectionprev~"You have been trained on infection prevention and control"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("workhours.docx"), pr_section = sect_properties)

##########################################################################
neverlabels=c("Never",
              "Once in a while",
              "Half the time",
              "Almost always")
yesnolabels=c("No","Yes")
baseline$Upset=factor(baseline$Upset,levels = c(0,1,2,3),labels = neverlabels)
baseline$Dreams=factor(baseline$Dreams,levels = c(0,1,2,3),labels = neverlabels)
baseline$Event=factor(baseline$Event,levels = c(0,1,2,3),labels = neverlabels)
baseline$Emotion=factor(baseline$Emotion,levels = c(0,1,2,3),labels = neverlabels)
baseline$Reactions=factor(baseline$Reactions,levels = c(0,1,2,3),labels = neverlabels)
baseline$Remember=factor(baseline$Remember,levels = c(0,1,2,3),labels = neverlabels)
baseline$Reminder=factor(baseline$Reminder,levels = c(0,1,2,3),labels = neverlabels)
baseline$Notremember=factor(baseline$Notremember,levels = c(0,1,2,3),labels = neverlabels)
baseline$Negativechange=factor(baseline$Negativechange,levels = c(0,1,2,3),labels = neverlabels)
baseline$Thinking=factor(baseline$Thinking,levels = c(0,1,2,3),labels = neverlabels)
baseline$Negativeemotion=factor(baseline$Negativeemotion,levels = c(0,1,2,3),labels = neverlabels)
baseline$Distant=factor(baseline$Distant,levels = c(0,1,2,3),labels = neverlabels)
baseline$Notpositive=factor(baseline$Notpositive,levels = c(0,1,2,3),labels = neverlabels)
baseline$Irritable=factor(baseline$Irritable,levels = c(0,1,2,3),labels = neverlabels)
baseline$Riskybehav=factor(baseline$Riskybehav,levels = c(0,1,2,3),labels = neverlabels)
baseline$Alert=factor(baseline$Alert,levels = c(0,1,2,3),labels = neverlabels)
baseline$Jumpy=factor(baseline$Jumpy,levels = c(0,1,2,3),labels = neverlabels)
baseline$Concentration=factor(baseline$Concentration,levels = c(0,1,2,3),labels = neverlabels)
baseline$Sleep=factor(baseline$Sleep,levels = c(0,1,2,3),labels = neverlabels)
baseline$Disaster=factor(baseline$Disaster,levels = c(0,1),labels = yesnolabels)
baseline$Accident=factor(baseline$Accident,levels = c(0,1),labels = yesnolabels)
baseline$Robbed=factor(baseline$Robbed,levels = c(0,1),labels = yesnolabels)
baseline$Beatfamily=factor(baseline$Beatfamily,levels = c(0,1),labels = yesnolabels)
baseline$Beatnotfamily=factor(baseline$Beatnotfamily,levels = c(0,1),labels = yesnolabels)
baseline$Familybeat=factor(baseline$Familybeat,levels = c(0,1),labels = yesnolabels)
baseline$Commbeat=factor(baseline$Commbeat,levels = c(0,1),labels = yesnolabels)
baseline$Touch=factor(baseline$Touch,levels = c(0,1),labels = yesnolabels)
baseline$Pressure=factor(baseline$Pressure,levels = c(0,1),labels = yesnolabels)
baseline$Dying=factor(baseline$Dying,levels = c(0,1),labels = yesnolabels)
baseline$Attacked=factor(baseline$Attacked,levels = c(0,1),labels = yesnolabels)
baseline$Someoneattack=factor(baseline$Someoneattack,levels = c(0,1),labels = yesnolabels)
baseline$Stressfulmed=factor(baseline$Stressfulmed,levels = c(0,1),labels = yesnolabels)
baseline$War=factor(baseline$War,levels = c(0,1),labels = yesnolabels)
baseline$Majorerror=factor(baseline$Majorerror,levels = c(1,2),labels = c("Yes","No"))
##########################################################
baseline|>
  dplyr::select(Majorerror,Errortype,       
                Disaster,Accident,       
                Robbed,Beatfamily,        
                Beatnotfamily,Familybeat,       
                Commbeat,Touch,   
                Pressure,Dying,             
                Attacked,Someoneattack,   
                Stressfulmed,War,         
                Otherstressful,Otherevent,       
                Upset,Dreams,
                Event,Emotion,           
                Reactions,Remember,       
                Reminder,Notremember,       
                Negativechange,Thinking,          
                Negativeemotion,Distant,           
                Notpositive,Irritable,         
                Riskybehav,Alert,           
                Jumpy,Concentration,    
                Sleep)|>
  tbl_summary(label = c(Majorerror~"Are you concerned you have made any major medical errors in the LAST 3 MONTHS?",
                        Errortype~"Please indicate the type of medical error that occurred (select all that apply)",       
                        Disaster~"Serious natural disaster like a flood, tornado, hurricane, earthquake, or fire",
                        Accident~"Serious accident or injury like a car/bike crash, dog bite, sports injury",       
                        Robbed~"Robbed by threat, force or weapon",
                        Beatfamily~"Slapped, punched, or beat up in your family",        
                        Beatnotfamily~"Slapped, punched, or beat up by someone not in your family",
                        Familybeat~"Seeing someone in your family get slapped, punched or beat up",       
                        Commbeat~"Seeing someone in the community get slapped, punched or beat up",
                        Touch~"Someone older touching your private parts when they shouldnot",   
                        Pressure~"Someone forcing or pressuring sex, or when you couldnot say no",
                        Dying~"Someone close to you dying suddenly or violently",             
                        Attacked~"Attacked, stabbed, shot at or hurt badly",
                        Someoneattack~"Seeing someone attacked, stabbed, shot at, hurt badly or killed",   
                        Stressfulmed~"Stressful or scary medical procedure",
                        War~"Being around war",         
                        Otherstressful~"Other stressful or scary events",
                        Otherevent~"Specify other stressful events",       
                        Upset~"Upsetting memories about a stressful event that pop into your head unplanned or when you are reminded",
                        Dreams~"Bad dreams related to a stressful event that feels like it is happening in the dream",
                        Event~"Acting or feeling as if the stressful event is happening right now",
                        Emotion~"Feeling very emotionally upset when you are reminded of a stressful event",           
                        Reactions~"Strong physical reactions when reminded of a stressful event (sweating, heart beating fast)",
                        Remember~"Trying not to remember, talk about or have feelings about a stressful event",       
                        Reminder~"Avoiding activities, people, places or things that remind you of a stressful event",
                        Notremember~"Not being able to remember an important part of a stressful event",       
                        Negativechange~"Negative changes in how you think about yourself, others or the world after a stressful event",
                        Thinking~"Thinking a stressful event happened because you or someone else did something wrong or did not do enough to stop it",          
                        Negativeemotion~"Having a very negative emotional state (afraid, angry, guilty, ashamed)",
                        Distant~"Feeling distant or cut off from people around you",           
                        Notpositive~"Not being able to have positive feelings (being happy, having loving feelings)",
                        Irritable~"Feeling irritable or having angry outbursts without a good reason and taking it out on other people or things",         
                        Riskybehav~"Risky behavior or behavior that could hurt you",
                        Alert~"Being overly alert or on guard",           
                        Jumpy~"Being jumpy or easily startled",
                        Concentration~"Problems with concentration",    
                        Sleep~"Trouble falling or staying asleep"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("medical_error.docx"), pr_section = sect_properties)

#############################################substance abuse########################################
substancelabel=c("No","Yes")
baseline$Tobacco=factor(baseline$Tobacco,levels = c(0,3),labels = substancelabel)
baseline$Alcohol=factor(baseline$Alcohol,levels = c(0,3),labels = substancelabel)
baseline$Cannabis=factor(baseline$Cannabis,levels = c(0,3),labels = substancelabel)
baseline$Cocaine=factor(baseline$Cocaine,levels = c(0,3),labels = substancelabel)
baseline$Amphetamine=factor(baseline$Amphetamine,levels = c(0,3),labels = substancelabel)
baseline$Inhalants=factor(baseline$Inhalants,levels = c(0,3),labels = substancelabel)
baseline$Sedative=factor(baseline$Sedative,levels = c(0,3),labels = substancelabel)
baseline$Hallucinogens=factor(baseline$Hallucinogens,levels = c(0,3),labels = substancelabel)
baseline$Opioids=factor(baseline$Opioids,levels = c(0,3),labels = substancelabel)
productslabel=c("Never", 
                "Once or twice", 
                "Monthly", 
                "Weekly", 
                "Daily or almost daily")
baseline$Tobacco2=factor(baseline$Tobacco2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Alcohol2=factor(baseline$Alcohol2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cannabis2=factor(baseline$Cannabis2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Amphetamine2=factor(baseline$Amphetamine2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Sedatives2=factor(baseline$Sedatives2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Opioids2=factor(baseline$Opioids2,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Alcohol3=factor(baseline$Alcohol3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cannabis3=factor(baseline$Cannabis3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cocaine3=factor(baseline$Cocaine3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Amphetamine3=factor(baseline$Amphetamine3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Inhalants3=factor(baseline$Inhalants3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Sedatives3=factor(baseline$Sedatives3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Hallucinogens3=factor(baseline$Hallucinogens3,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Tobacco4=factor(baseline$Tobacco4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Alcohol4=factor(baseline$Alcohol4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cannabis4=factor(baseline$Cannabis4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cocaine4=factor(baseline$Cocaine4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Amphetamine4=factor(baseline$Amphetamine4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Inhalants4=factor(baseline$Inhalants4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Sedatives4=factor(baseline$Sedatives4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Hallucinogens4=factor(baseline$Hallucinogens4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Opioids4=factor(baseline$Opioids4,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Tobacco5=factor(baseline$Tobacco5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Alcohol5=factor(baseline$Alcohol5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cannabis5=factor(baseline$Cannabis5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cocaine5=factor(baseline$Cocaine5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Amphetamine5=factor(baseline$Amphetamine5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Inhalants5=factor(baseline$Inhalants5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Sedatives5=factor(baseline$Sedatives5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Hallucinogens5=factor(baseline$Hallucinogens5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Opioids5=factor(baseline$Opioids5,levels = c(0,2,3,4,6),labels =productslabel)
baseline$Cannabis5=factor(baseline$Cannabis5,levels = c(0,2,3,4,6),labels =productslabel)
labelssb=c("Never","Yes, but not in the past 3 months","Yes, in the past 3 months")
baseline$Tobacco6=factor(baseline$Tobacco6,levels =c(0,3,6),labels = labelssb)
baseline$Alcohol6=factor(baseline$Alcohol6,levels =c(0,3,6),labels = labelssb)
baseline$Cannabis6=factor(baseline$Cannabis6,levels =c(0,3,6),labels = labelssb)
baseline$Cocaine6=factor(baseline$Cocaine6,levels =c(0,3,6),labels = labelssb)
baseline$Amphetamine6=factor(baseline$Amphetamine6,levels =c(0,3,6),labels = labelssb)
baseline$Inhalants6=factor(baseline$Inhalants6,levels =c(0,3,6),labels = labelssb)
baseline$Sedatives6=factor(baseline$Sedatives6,levels =c(0,3,6),labels = labelssb)
baseline$Hallucinogens6=factor(baseline$Hallucinogens6,levels =c(0,3,6),labels = labelssb)
baseline$Opioids6=factor(baseline$Opioids6,levels =c(0,3,6),labels = labelssb)
baseline$Cannabis7=factor(baseline$Cannabis7,levels =c(0,3,6),labels = labelssb)
baseline$Cocaine7=factor(baseline$Cocaine7,levels =c(0,3,6),labels = labelssb)
baseline$Amphetamine7=factor(baseline$Amphetamine7,levels =c(0,3,6),labels = labelssb)
baseline$Inhalants7=factor(baseline$Inhalants7,levels =c(0,3,6),labels = labelssb)
baseline$Sedatives7=factor(baseline$Sedatives7,levels =c(0,3,6),labels = labelssb)
baseline$Opioids7=factor(baseline$Opioids7,levels =c(0,3,6),labels = labelssb)  
baseline$Alcohol7=factor(baseline$Alcohol7,levels =c(0,3,6),labels = labelssb)
baseline$Substabceuseinject=factor(baseline$Substabceuseinject,levels = c(0,2,3),
                                   labels = c("No, Never","Yes, In the past 3 months",
                                              "Yes, But not in the past 3 months"))
#####################################################
baseline|>
  dplyr::select(Tobacco,           
                Alcohol,
                Cannabis,          
                Cocaine,Amphetamine,       
                Inhalants,Sedative,          
                Hallucinogens,Opioids,           
                Other,Tobacco2,          
                Alcohol2,Cannabis2,         
                Amphetamine2,Sedatives2,        
                Opioids2,Tobacco3,          
                Alcohol3,Cannabis3,         
                Cocaine3,Amphetamine3,      
                Inhalants3,Sedatives3,        
                Hallucinogens3,Tobacco4,          
                Alcohol4,Cannabis4,         
                Cocaine4,Amphetamine4,      
                Inhalants4,Sedatives4,      
                Hallucinogens4,Opioids4,        
                Tobacco5,Alcohol5,        
                Cannabis5,Cocaine5,         
                Amphetamine5,Inhalants5,      
                Sedatives5,Hallucinogens5,   
                Opioids5,Tobacco6,        
                Alcohol6,Cannabis6,       
                Cocaine6,Amphetamine6,     
                Inhalants6,Sedatives6,        
                Hallucinogens6,Opioids6,         
                Tobacco7,Alcohol7,        
                Cannabis7,Cocaine7,         
                Amphetamine7,Inhalants7,        
                Sedatives7,Opioids7,         
                Substabceuseinject)|>
  tbl_summary(label = c(Tobacco~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",           
                        Alcohol~"Alcoholic beverages (beer, wine, spirits, etc.)",
                        Cannabis~"Cannabis (marijuana, pot, grass, hash, etc.)",          
                        Cocaine~"Cocaine (coke, crack, etc.)",
                        Amphetamine~"Amphetamine-type stimulants (speed, diet pills, ecstasy, etc.)",       
                        Inhalants~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",
                        Sedative~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",          
                        Hallucinogens~"Hallucinogens (LSD, acid, mushrooms, PCP, Special K, etc.)",
                        Opioids~"Opioids (heroin, morphine, methadone, codeine, etc.)",           
                        Other~"Other substance",
                        Tobacco2~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",          
                        Alcohol2~"Alcoholic beverages (beer, wine, spirits, etc.)",
                        Cannabis2~"Cannabis (marijuana, pot, grass, hash, etc.)",         
                        Amphetamine2~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",
                        Sedatives2~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",        
                        Opioids2~"Opioids (heroin, morphine, methadone, codeine, etc.)",
                        Tobacco3~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",          
                        Alcohol3~"Alcoholic beverages (beer, wine, spirits, etc.)",
                        Cannabis3~"Cannabis (marijuana, pot, grass, hash, etc.)",         
                        Cocaine3~"Cocaine (coke, crack, etc.)",
                        Amphetamine3~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",      
                        Inhalants3~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",
                        Sedatives3~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",        
                        Hallucinogens3~"Hallucinogens (LSD, acid, mushrooms, PCP, Special K, etc.)",
                        Tobacco4~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",          
                        Alcohol4~"Alcoholic beverages (beer, wine, spirits, etc.)",
                        Cannabis4~"Cannabis (marijuana, pot, grass, hash, etc.)",         
                        Cocaine4~"Cocaine (coke, crack, etc.)",
                        Amphetamine4~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",      
                        Inhalants4~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",
                        Sedatives4~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",      
                        Hallucinogens4~"Hallucinogens (LSD, acid, mushrooms, PCP, Special K, etc.)",
                        Opioids4~"Opioids (heroin, morphine, methadone, codeine, etc.)",        
                        Tobacco5~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",
                        Alcohol5~"Alcoholic beverages (beer, wine, spirits, etc.)",        
                        Cannabis5~"Cannabis (marijuana, pot, grass, hash, etc.)",
                        Cocaine5~"Cocaine (coke, crack, etc.)",         
                        Amphetamine5~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",
                        Inhalants5~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",      
                        Sedatives5~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",
                        Hallucinogens5~"Hallucinogens (LSD, acid, mushrooms, PCP, Special K, etc.)",   
                        Opioids5~"Opioids (heroin, morphine, methadone, codeine, etc.)",
                        Tobacco6~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",        
                        Alcohol6~"Alcoholic beverages (beer, wine, spirits, etc.)",
                        Cannabis6~"Cannabis (marijuana, pot, grass, hash, etc.)",       
                        Cocaine6~"Cocaine (coke, crack, etc.)",
                        Amphetamine6~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",     
                        Inhalants6~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",
                        Sedatives6~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",        
                        Hallucinogens6~"Hallucinogens (LSD, acid, mushrooms, PCP, Special K, etc.)",
                        Opioids6~"Opioids (heroin, morphine, methadone, codeine, etc.)",         
                        Tobacco7~"Tobacco products (cigarettes, chewing tobacco, cigars, etc.)",
                        Alcohol7~"Alcoholic beverages (beer, wine, spirits, etc.)",        
                        Cannabis7~"Cannabis (marijuana, pot, grass, hash, etc.)",
                        Cocaine7~"Cocaine (coke, crack, etc.)",         
                        Amphetamine7~"Amphetamine type stimulants (speed, diet pills, ecstasy, etc.)",
                        Inhalants7~"Inhalants (nitrous, glue, petrol, paint thinner, etc.)",        
                        Sedatives7~"Sedatives or Sleeping Pills (Valium, Serepax, Rohypnol, etc.)",
                        Opioids7~"Opioids (heroin, morphine, methadone, codeine, etc.)",         
                        Substabceuseinject~"Have you ever used any drug by injection? (Non-medical use only) (Select the number that best indicates your answer)"),
              digits = list(all_continuous() ~ 1,
                            all_categorical()~c(0,1)))|>
  bold_labels()|>
  as_flex_table()|>
  flextable::save_as_docx(demo,path = ("substanceuse.docx"), pr_section = sect_properties)

