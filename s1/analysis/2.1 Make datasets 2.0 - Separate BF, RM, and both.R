clean.l = read_rds("Data/clean.l.rds")

# Custom Functions
pull_nominees = function(id,tim,var){
  if(is.na(id)){return(NA)}
  data = clean.l %>% filter(StudyID == id, time == tim)
  if(nrow(data) == 0){return(NA)}
  data %>% pull({{var}}) %>% return()
}

# Those who nominated you
pull_nominators = function(nominator,nominee,time,variable_to_pull,nomination_criteria){
  if(is.na(nominee)){return(NA)}
  data = clean.l %>% filter(across(nomination_criteria, ~. == nominee)) %>% filter(time == time) 
  if(nrow(data) == 0){return(NA)}
  data %>% pull({{variable_to_pull}}) %>% return()
}


nominations = clean.l %>% 
    select(StudyID,time, school,matches("pn"),coregpa) %>% 
    rename(pn_scw1 = pn_scw,
           pn_grit1 = pn_grit, 
           pn_sci1 = pn_sci,
           pn_grat1 = pn_grat,
           pn_aot1 = pn_aot
    ) %>% 
    pivot_longer(pn_bf1:pn_aot2) %>% 
    mutate(n = str_sub(name,-1),
           name  = str_sub(name,1,-2)) %>% 
    filter(!is.na(value)) %>% 
  rename(nominee = value)

nominations = nominations %>% 
  #Exclude self noms (N = 1203)
  filter(StudyID != nominee) %>% 
  #Exclude double noms (Keep only one  N = 11)
  group_by(StudyID,time,name,nominee) %>% 
  slice(1)

nominations =
nominations %>% filter(name %in% c("pn_bf","pn_scw")) %>%
  group_by(StudyID,time,school, coregpa, nominee) %>%
  mutate(n = n()) %>%
  arrange(nominee) %>%
  arrange(-n) %>% 
  # rename pn variable people who were nominated for both
  mutate(name = ifelse(n == 2, "pn_both",name)) %>% 
  group_by(StudyID,time,name,nominee) %>% 
  slice(1)

nominations = 
  nominations %>% 
  mutate(nominee_gpa = pmap_dbl(list(nominee, time,"avg_scw_tr"),pull_nominees))

nominations = 
  nominations %>% 
  arrange(StudyID)

nominations=nominations %>% 
  rename(nominator = StudyID, nominator_gpa = coregpa) %>% 
  select(time,school,nominator,nominator_gpa,name, nominee, nominee_gpa, n )
  

# NomineeGPA
kadeem_q_data = nominee_gpa %>% 
  select(m) %>% 
  spread(name,m) %>% 
  rename(StudyID = nominator) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  group_by(school) %>% 
  mutate_at(vars(pn_bf:pn_scw, female, ell:avg_aot_tr),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(pn_bf:pn_scw, female, ell:avg_aot_tr),function(x){x %>% scale %>% as.numeric})

kadeem_q_data %>% 
  filter(school == "p2" | school == "p1") %>% 
  # filter(time %in% year) %>% 
  mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
  group_by(StudyID,eth) %>% 
  summarise_if(is.numeric,mean,na.rm=T) %>% 
  lfe::felm(scw_s ~ pn_scw+ pn_bf + pn_both + coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2,data=.) %>% 
  stargazer::stargazer(type = "latex", star.cutoffs = c(.05,.01,.001))
