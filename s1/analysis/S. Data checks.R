bf_coregpa = read_rds("Data/Peer Quality Datasets/bf_coregpa.rds")

bf_coregpa %>% group_by(StudyID,time) %>% 
  summarise(near = mean(near)) %>% 
  left_join(peer_gpas %>% filter(name == "pn_bf1"|name == "pn_bf2") %>% 
              group_by(StudyID,time) %>% 
              summarise(pg = mean(peergpa,na.rm = T))
  ) %>% ungroup %>% 
  # select(3,4) %>% 
  filter(is.na(pg)& !is.na(near)) %>% 
  left_join(clean.l)

clean.l %>% filter((StudyID == 1202 & time == "T3") | (StudyID == 1330 & time == "T3"))
analysis_data %>% left_join(
  bf_coregpa %>% select(StudyID, school, time, near) %>% group_by(school) %>% mutate(near = scale(near)) %>% ungroup() %>% mutate(near = scale(near))
                                                                                     ) %>% 
  select(StudyID, school, time,pn_bf,near) %>% 
  select(4,5) %>% 
  Ben::HARcor()

  
analysis_data %>% 
  filter(school == "p2" | school == "p1") %>% 
  # filter(time %in% year) %>% 
  mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
  group_by(StudyID,eth) %>% 
  summarise_if(is.numeric,mean,na.rm=T) %>% 
  lfe::felm(scw_s ~ pn_scw + coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2,data=.) %>% 
  stargazer::stargazer(type = "text")
  
read_rds("Data/Peer Quality Datasets/scwpeer_coregpa.rds") %>% 
  rowwise() %>% 
  mutate(near = mean(c(peer1,peer2),na.rm = T)) %>% 
  ungroup() %>% 
  filter(school == "p2" | school == "p1") %>% 
  # filter(time %in% year) %>% 
  mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
  group_by(StudyID,eth) %>% 
  summarise_if(is.numeric,mean) %>% 
  lfe::felm(scw_s ~ near + coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2,data=.) %>% 
  stargazer::stargazer(type = "text")


m1 = analysis_data %>% 
  pivot_longer(scw_s:aot_s,names_to = "outcome",values_to = "outcome_value") %>% 
  pivot_longer(pn_grat:pn_scw,names_to = "peer_type",values_to = "pn_rolemodel") %>% 
  mutate(outcome = str_remove_all(outcome,"_s"),
         peer_type = str_remove_all(peer_type, "pn_")) %>% 
  filter(outcome == peer_type) %>% 
  group_by(outcome) %>% 
  select(-matches("aot"),-matches("avg_")) %>% 
  group_by_at(vars(StudyID:frpl,school,outcome,peer_type)) %>% 
  summarise_all(mean,na.rm=T) %>% 
  select(-time) %>% 
  group_by(outcome) %>% 
  nest() %>% 
  mutate(felm1 = map(data,function(x){lfe::felm(outcome_value ~ pn_rolemodel  + coregpa+school+ female + eth + ell + sped + frpl|0|0|school,data=x)})) %>% 
  mutate(felm2 = map(data,function(x){lfe::felm(outcome_value ~ pn_bf  + coregpa+school+ female + eth + ell + sped + frpl|0|0|school,data=x)})) %>% 
  mutate(felm3 = map(data,function(x){lfe::felm(outcome_value ~ pn_rolemodel + pn_bf  + coregpa+school+ female + eth + ell + sped + frpl|0|0|school,data=x)})) %>% 
  pivot_longer(felm1:felm3)

m1 %>% 
  .$value %>% 
  stargazer::stargazer(type = "text")
