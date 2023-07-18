#Load
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


#What is the T1 coregpa of person 1415
pull_nominees(1415,"T1","coregpa") 
#What is the mean T1 coregpa of those who nominated 1415 for x
pull_nominators(1415,1329,"T1","coregpa","pn_bf1") 




if(rerun){
  nominator_gpas = clean.l %>% 
    pivot_longer(pn_bf1:pn_aot2) %>% 
    mutate(nominatorgpa = pmap(list(value, StudyID,time,"coregpa",name),pull_nominators))
  write_rds(nominator_gpas,"Data/Peer Quality Datasets/nominator_gpas.rds")
  }

if(rerun){
  peer_gpas = clean.l %>% 
    pivot_longer(pn_bf1:pn_aot2) %>% 
    mutate(peergpa = pmap_dbl(list(value,time,"coregpa"),pull_nnominees)) %>% 
    unnest(peergpa)
  write_rds(peer_gpas,"Data/Peer Quality Datasets/peer_gpas.rds")
}

peer_gpas = read_rds("Data/Peer Quality Datasets/peer_gpas.rds")
analysis_data = peer_gpas %>% 
  mutate(name = str_remove(name, "[12]")) %>% 
  group_by_at(vars(StudyID:avg_aot_tr,name)) %>% 
  summarise(peergpa = mean(peergpa,na.rm= T)) %>% 
  pivot_wider(names_from = name,values_from = peergpa) %>% 
  group_by(school) %>% 
  mutate_at(vars(age:pn_scw),function(x){x %>% scale %>% as.numeric}) %>% 
  ungroup() %>% 
  mutate_at(vars(female,ell,sped,frpl,age:pn_scw),function(x){x %>% scale %>% as.numeric})
write_rds(analysis_data,"Data/Peer Quality Datasets/analysis_data.rds")

bf = read_rds("Data/Peer Quality Datasets/bf_coregpa.rds")
analysis_data %>% select(StudyID,time,pn_bf) %>% 
  left_join(bf %>% select(StudyID,time,near)) %>% 
  select(3,4) %>% 
  Ben::HARcor()

##### SCRAPS ####


mutate_peer_quality = function(data,p1,p2,time,var){
  data %>% 
    mutate(peer1 = pmap(list({{p1}},time,vars({{var}})),pull_var),
           peer2 = pmap(list({{p2}},time,vars({{var}})),pull_var)) %>% 
    pivot_longer(cols = c(peer1,peer2),names_to = "peern",values_to = "peer") %>% 
    unnest(peer) %>% 
    spread(peern,peer) %>% 
    # filter(!(is.na(peer1) & is.na(peer2))) %>%
    rowwise() %>% 
    mutate(near = mean(c(peer1,peer2),na.rm=T)) %>%  
    ungroup # %>% 
    # group_by(school) %>%  
    # mutate(StudyID = as.character(StudyID)) %>% 
    # mutate_if(is.numeric,scale) %>% ungroup() %>% 
    # mutate_if(is.numeric,scale) %>% 
    # mutate(StudyID = as.numeric(StudyID))
}

# Stop here to make several datasets:
# X measures of peer quality (coregpa, teach_nomination) * X peer definitions (bf, grit_nom, sci_nom, scw_nom, popular kids)
if(rerun){
  bf_coregpa = clean.l %>%
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa)
  # 
  # bf_teachnom = clean.l %>%
  #   mutate_peer_quality(pn_bf1,pn_bf2,time,avg_scw_tr)
  # 
  # bf_scw = clean.l %>%
  #   mutate_peer_quality(pn_bf1,pn_bf2,time,scw_s)
  # 
  # Core GPA of peers nominated for target character strength
  gritpeer_coregpa = clean.l %>% filter(school == "p1",time == "T1") %>% 
    mutate_peer_quality(pn_grit,pn_grit2,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)
  
  scwpeer_coregpa = clean.l %>%
    mutate_peer_quality(pn_scw,pn_scw2,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)
  
  scipeer_coregpa = clean.l %>%
    mutate_peer_quality(pn_sci,pn_sci,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)
  
  aotpeer_coregpa = clean.l %>%
    mutate_peer_quality(pn_aot,pn_aot2,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)
  
  gratpeer_coregpa = clean.l %>%
    mutate_peer_quality(pn_grat,pn_grat2,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)
  
  psppeer_coregpa = clean.l %>%
    mutate_peer_quality(pn_psp,pn_psp2,time,coregpa) %>% 
    rename(rolemodel = near) %>% 
    mutate_peer_quality(pn_bf1,pn_bf2,time,coregpa) %>% 
    rename(friend = near)  
  
  # BF dfs
  # write_rds(bf_coregpa,file = "Data/Peer Quality Datasets/bf_coregpa.rds")
  # write_rds(bf_teachnom,file = "Data/Peer Quality Datasets/bf_teachnom.rds")
  # write_rds(bf_scw,file = "Data/Peer Quality Datasets/bf_scw.rds")
  
  # Exemplar dfs
  write_rds(scwpeer_coregpa,file = "Data/Peer Quality Datasets/scwpeer_coregpa.rds")
  write_rds(scipeer_coregpa,file = "Data/Peer Quality Datasets/scipeer_coregpa.rds")
  write_rds(gritpeer_coregpa,file = "Data/Peer Quality Datasets/gritpeer_coregpa.rds")
  write_rds(aotpeer_coregpa,file = "Data/Peer Quality Datasets/aotpeer_coregpa.rds")
  write_rds(gratpeer_coregpa,file = "Data/Peer Quality Datasets/gratpeer_coregpa.rds")
  write_rds(psppeer_coregpa,file = "Data/Peer Quality Datasets/psppeer_coregpa.rds")
}

