library("tidyverse")

#Results from Julia (emailed)
results<-read.csv("results.csv", header= TRUE)

#From Coronavirus Infection Among Children in North Central Florida - Production
redcap_production<-read.csv("CoronavirusInfection_DATA_2020-07-28_1211.csv", header= TRUE)

#From scheduling project 
scheduling<-read.csv("PKYongeCOVID19Schedu_DATA_2020-07-28_1455.csv", header= TRUE)


redcap_production1<- redcap_production %>% 
  mutate(igg_antibodies = as.character(igg_antibodies)) %>%
  mutate(igm_antibodies = as.character(igm_antibodies)) %>% 
  mutate(research_encounter_id = as.character(research_encounter_id)) %>% 
  select(record_id= ï..record_id, research_encounter_id, redcap_event_name, ce_family_id, test_date_and_time)%>% 
  drop_na(research_encounter_id, ce_family_id)


results1<- results %>% 
  filter(!is.na(record_id)) %>% 
  mutate(igg_antibodies = as.character(igg_antibodies)) %>% 
  mutate(research_encounter_id = as.character(record_id)) %>% 
  mutate(igg_antibodies = case_when(
    str_detect(str_to_lower(igg_antibodies), "pos") ~ "1",
    str_detect(str_to_lower(igg_antibodies), "neg") ~ "0",
    str_detect(str_to_lower(igg_antibodies), "indeterminate") ~ "2",
    TRUE ~ igg_antibodies
  )) %>% 
  select(research_encounter_id, igg_antibodies)
  

scheduling1<- scheduling %>% 
  mutate(igg_antibodies = as.character(igg_antibodies)) %>%
  mutate(igm_antibodies = as.character(igm_antibodies)) %>% 
  mutate(research_encounter_id = as.character(research_encounter_id)) %>%
  select(record_id= ï..record_id, research_encounter_id, test_date_and_time, redcap_event_name, ce_family_id)


##PKY project 

results_pky<- join(results1, scheduling1, by= "research_encounter_id")

pky_results<-results_pky %>% 
  filter(!is.na(test_date_and_time))

write.csv(pky_results, "pky_results.csv")



### Time .2 

library("plyr")

results_dataset<- dplyr::left_join(redcap_production1, results1, by= "research_encounter_id")

timetwo_results<-results_dataset %>% 
  filter(!research_encounter_id =="") %>% 
  filter(!is.na(igg_antibodies)) 

scheduling2<- scheduling %>%
  group_by(ce_family_id) %>% 
  mutate(test_date_and_time = lead(test_date_and_time)) %>% 
select(test_date_and_time, ce_family_id) 
  
results_schedule<- merge(timetwo_results, scheduling2, by= "ce_family_id" )

colnames(results_schedule)<- c("ce_family_id", "record_id", "research_encounter_id",  "redcap_event_name", "remove", "igg_antibodies",  "test_date_and_time")


results_schedule<- results_schedule %>% 
  select("record_id", "research_encounter_id", "redcap_event_name", "ce_family_id", "igg_antibodies",  "test_date_and_time")


results_schedule<- results_schedule[!duplicated(results_schedule$"research_encounter_id"), ] 

write.csv(results_schedule, "results_schedule.csv")

results_test<- results_schedule %>% 
  select(research_encounter_id)

results_total<- results %>% 
  select(research_encounter_id= record_id)


library("arsenal")

summary(comparedf(redcap_production, results_total, by= "research_encounter_id"))

summary(comparedf(pky_results, results_total, by= "research_encounter_id"))


write.csv(results_schedule, "results_schedule.csv")
