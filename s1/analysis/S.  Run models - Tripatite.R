

bf_coregpa = read_rds("Data/Peer Quality Datasets/bf_coregpa.rds")
bf_teachnom = read_rds("Data/Peer Quality Datasets/bf_teachnom.rds")
bf_scw = read_rds("Data/Peer Quality Datasets/bf_scw.rds")
gritpeer_coregpa = read_rds("Data/Peer Quality Datasets/gritpeer_coregpa.rds")
scwpeer_coregpa = read_rds("Data/Peer Quality Datasets/scwpeer_coregpa.rds")
scipeer_coregpa = read_rds("Data/Peer Quality Datasets/scipeer_coregpa.rds")
aotpeer_coregpa = read_rds("Data/Peer Quality Datasets/aotpeer_coregpa.rds")
gratpeer_coregpa= read_rds("Data/Peer Quality Datasets/gratpeer_coregpa.rds")
psppeer_coregpa = read_rds("Data/Peer Quality Datasets/psppeer_coregpa.rds")

scwpeer_coregpa %>% select(StudyID,scw = near) %>% 
  left_join(gratpeer_coregpa%>% select(StudyID,grat = near)) %>% 
  left_join(gritpeer_coregpa%>% select(StudyID,grit = near)) %>% 
  left_join(scipeer_coregpa%>% select(StudyID,sci = near)) %>% 
  left_join(psppeer_coregpa%>% select(StudyID,psp = near)) %>% 
  left_join(aotpeer_coregpa%>% select(StudyID,aot = near)) %>% 
  select(-StudyID) %>% 
  Ben::HARcor()

# Is there enough within person variance in the outcome
icc = function(data,var,group){
  vcor  = data %>% 
    lme4::lmer(paste(var,glue::glue("~ 1 + (1 | {group})")),data =.) %>% 
    lme4::VarCorr() %>% 
    as_tibble() %>% 
    pull(vcov)
  return(vcor[1]/(vcor[1]+ vcor[2]))
  
}
bengazer = function(...){stargazer::stargazer(...,type = "text",style = "asr",ci = T,initial.zero = F,report = "vc*s",
                                              table.layout = "-dc-t-sa-n")
}

tibble(group = rep(1:3, each=10),value = rnorm(30)) %>% icc("value","group") # 0 means that all variation is within groups, there are no differences between groups over and above within group var
tibble(group = rep(1:3, each=10),value = group+rnorm(30,sd = .1)) %>% icc("value","group") # 1 means that all variation is between groups, knowing your score tells me what group youre in. 

icc(clean.l,"grit_s","StudyID")
icc(clean.l,"scw_s","StudyID")
icc(clean.l,"sci_s","StudyID")
icc(clean.l,"sccomb_s","StudyID")
icc(clean.l,"coregpa","StudyID")
icc(bf_coregpa,"near","StudyID")
icc(bf_scw,"near","StudyID")


# RUN models
year1 = c("T1","T2")
year2 = c("T3","T4")

# Main specs
mainspecs = function(data,outcome,...){
  a = data %>%
    filter(school == "p2" | school == "p1") %>%
    ungroup() %>%
    mutate(StudyID = as.character(StudyID)) %>%
    lfe::felm(scw_s ~ near + coregpa|StudyID|0|StudyID,data=.)
   
  b = data %>% 
    filter(school == "p2" | school == "p1") %>% 
    ungroup() %>% 
    lfe::felm(scw_s ~ near + coregpa + school|0|0|StudyID,data=.)
  
  bd = data %>% 
    filter(school == "p2" | school == "p1") %>% 
    ungroup() %>% 
    lfe::felm(scw_s ~ near + coregpa+school+ female + eth + ell + sped + frpl|0|0|StudyID,data=.)
  
  c = data %>% 
    filter(school == "p2" | school == "p1") %>% 
    # filter(time %in% year) %>% 
    mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
    group_by(StudyID) %>% 
    summarise_all(mean) %>% 
    lfe::felm(scw_s ~ near + coregpa+schoolp2|0|0|schoolp2,data=.)
  cd = data %>% 
    filter(school == "p2" | school == "p1") %>% 
    # filter(time %in% year) %>% 
    mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
    group_by(StudyID,eth) %>% 
    summarise_if(is.numeric,mean) %>% 
    lfe::felm(scw_s ~ near+ coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2,data=.)
  return(list(a,b,bd,c,cd))
}

report_models = function(models){return(bengazer(models,no.space=T,column.labels = c("Within-person","No fixed effects","No fixed effects","Average","Average")))}

# Exemplars
exemplars = list(
  scwpeer_coregpa  %>%                                                   mainspecs(),
  gratpeer_coregpa %>%  select(-scw_s) %>% rename(scw_s = grat_s)         %>%  mainspecs(),
  gritpeer_coregpa %>% select(-scw_s) %>% rename(scw_s = grit_s)    %>%  mainspecs(),
  scipeer_coregpa  %>%  select(-scw_s) %>% rename(scw_s = sci_s)    %>%  mainspecs(),
  scwpeer_coregpa  %>%  select(-scw_s) %>% rename(scw_s = sccomb_s) %>% mainspecs(),
  aotpeer_coregpa  %>%  select(-scw_s) %>% rename(scw_s = aot_s)    %>%  mainspecs(),
  psppeer_coregpa  %>%  select(-scw_s) %>% rename(scw_s = purpose_s)       %>%  mainspecs(),
  psppeer_coregpa  %>%  select(-scw_s) %>% rename(scw_s = purpose_s_nofam) %>%  mainspecs()
)

# Friends
friends = list(
  bf_coregpa %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = grat_s) %>%  mainspecs(),
  bf_coregpa  %>% select(-scw_s) %>% rename(scw_s = grit_s) %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = sci_s) %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = sccomb_s) %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = aot_s) %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = purpose_s) %>%  mainspecs(),
  bf_coregpa %>%  select(-scw_s) %>% rename(scw_s = purpose_s_nofam) %>%  mainspecs()
)

models = expand_grid(peer_type = c("Exemplars","Friends"),
            outcome = c("Academic\nSelf-Control","Gratitude","Grit","Interpersonal\nSelf-Control","Self-Control\nCombined","Actively\nOpen-Minded\nThinking","Prosocial\nPurpose","Prosocial\nPurpose\n(Ex. Family)"),
            model = c('Within-Person.Demographics','No Fixed Effects.Unadjusted','No Fixed Effects.Demographics','Average.Unadjusted','Average.Demographics')) %>% 
  mutate(result = NA)

for (i in 1:8){
  for(j in 1:5){
    models$result[((i-1)*5+j)] = exemplars[[i]][j]
  }
}
for (i in 1:8){
  for(j in 1:5){
    models$result[((i-1)*5+j+40)] = friends[[i]][j]
  }
}

write_rds(models,file = "Output/Raw/models.rds")

