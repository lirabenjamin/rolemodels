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
  nominations %>% 
  mutate(nominee_gpa = pmap_dbl(list(nominee, time,"coregpa"),pull_nominees),
         nominee_tr_scw = pmap_dbl(list(nominee, time,"avg_scw_tr"),pull_nominees))

nominations = 
  nominations %>% 
  arrange(StudyID)

nominations=nominations %>% 
  rename(nominator = StudyID, nominator_gpa = coregpa) %>% 
  select(time,school,nominator,nominator_gpa,name, nominee, nominee_gpa,nominee_tr_scw, n )
  
#what is theh average pa of people who I nominated
nominee_gpa = nominations %>% 
  select(-school,-nominator_gpa,-nominee,-n) %>% 
  pivot_longer(nominee_gpa:nominee_tr_scw, names_to = "proxy", values_to = "proxy_value") %>% 
  group_by(nominator,name,time,proxy) %>% 
  nest() %>% 
  mutate(data = map(data,pull,proxy_value)) %>% 
  mutate(m = map_dbl(data,mean,na.rm=T),
         sd = map_dbl(data,sd,na.rm = T),
         n = map_dbl(data,~sum(!is.na(.))))

#what is the average gpa of those who nominated me
nominator_gpa = nominations %>% 
  select(-school,-nominee_gpa,-nominator,-n) %>% 
  pivot_longer(nominee_gpa:nominee_tr_scw, names_to = "proxy", values_to = "proxy_value") %>% 
  group_by(nominee,name,time) %>% 
  nest() %>% 
  mutate(data = map(data,pull,proxy_value)) %>% 
  mutate(m = map_dbl(data,mean,na.rm=T),
         sd = map_dbl(data,sd,na.rm = T),
         n = map_dbl(data,~sum(!is.na(.))))

nominations = nominee_gpa %>% rename(StudyID = nominator) %>% mutate(kind = "Nominee") %>% 
  # bind_rows(nominator_gpa %>%rename(StudyID = nominee) %>% mutate(kind = "Nominator")) %>%
  pivot_wider(names_from = kind, values_from = c(data,m,sd,n))
  # mutate(data_Both = map2(data_Nominee,data_Nominator,c),
  #        m_Both = map_dbl(data_Both,mean,na.rm=T),
  #        sd_Both = map_dbl(data_Both,sd,na.rm = T),
  #        n_Both = map_dbl(data_Both,~sum(!is.na(.)))) %>% 
  # arrange(-sd_Both)

write_rds(nominations, file = "Data/Peer Quality Datasets/nominations.rds")


rawdata = nominations %>% 
  select(-sd_Nominee, -n_Nominee,-data_Nominee) %>% 
  filter(name == "pn_bf" | name == "pn_scw") %>% 
  pivot_wider(names_from = c(name,proxy), values_from = m_Nominee) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select("StudyID","school","female","eth","ell","sped","frpl","time","age","scw_s","coregpa" ,"avg_scw_tr"  ,pn_bf_nominee_gpa:pn_scw_nominee_tr_scw)

write_rds(rawdata, file = "Data/Peer Quality Datasets/rawdata.rds")


# Means
nominations %>% 
  select(m_Both) %>% 
  spread(name,m_Both) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_both_M.rds")

nominations %>% 
  select(m_Nominator) %>% 
  spread(name,m_Nominator) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominator_M.rds")

nominations %>% 
  select(m_Nominee) %>% 
  spread(name,m_Nominee) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominee_M.rds")

#SDs
nominations %>% 
  select(sd_Both) %>% 
  spread(name,sd_Both) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_both_sd.rds")

nominations %>% 
  select(sd_Nominator) %>% 
  spread(name,sd_Nominator) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominator_sd.rds")

nominations %>% 
  select(sd_Nominee) %>% 
  spread(name,sd_Nominee) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominee_sd.rds")

#Ns
nominations %>% 
  select(n_Both) %>% 
  spread(name,n_Both) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_both_n.rds")

nominations %>% 
  select(n_Nominator) %>% 
  spread(name,n_Nominator) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric})%>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominator_n.rds")

nominations %>% 
  select(n_Nominee) %>% 
  spread(name,n_Nominee) %>% 
  right_join(clean.l%>% select(-matches("pn_"))) %>% 
  arrange(StudyID) %>% 
  select(colorder) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric})%>% 
  write_rds("Data/Peer Quality Datasets/analysis_data_nominee_n.rds")
