library(broom)
clean.l = read_rds("Data/clean.l.rds")
analysis_data = read_rds("Data/Peer Quality Datasets/analysis_data_nominee_M.rds")
analysis_data = read_rds("Data/Peer Quality Datasets/analysis_data_both_M.rds")

analysis_data %>% 
  select(matches("pn")) %>% 
  Ben::HARcor()

# bf_coregpa = read_rds("Data/Peer Quality Datasets/bf_coregpa.rds")
# bf_teachnom = read_rds("Data/Peer Quality Datasets/bf_teachnom.rds")
# bf_scw = read_rds("Data/Peer Quality Datasets/bf_scw.rds")
# Correlations among peer variables
t.cors_peers = analysis_data %>% 
  select(matches("pn_"),-pn_aot) %>% 
  Ben::HARcor() %>% 
  mutate(var = str_remove_all(var,"pn_")) %>% 
  mutate(var = str_replace(var,"scw","Academic Self-Control"),
         var = str_replace(var,"grit","Grit"),
         var = str_replace(var,"bf","Close Friend"),
         var = str_replace(var,"grat","Gratitude"),
         var = str_replace(var,"sci","Interpersonal Self-Control"),
         var = str_replace(var,"aot","Actively Open-Minded Thinking"),
         var = str_replace(var,"scw","Academic Self-Control")) %>% 
  rename(Variable = `var`) %>% 
  gt::gt() 
t.cors_peers
gt::gtsave(t.cors_peers,filename = "Output/Supplement/cors_peers.tex")
gt::gtsave(t.cors_peers,filename = "Output/Supplement/cors_peers.html")


# Is there enough within person variance in the outcome
icc = function(var,group){
  ids = analysis_data %>% 
    group_by(StudyID) %>% 
    count() %>% 
    filter(n>=k) %>% 
    pull(StudyID)
  data = analysis_data %>% filter(StudyID %in% ids)
  vcor  = analysis_data %>% 
    lme4::lmer(paste(var,glue::glue("~ 1 + (1 | {group})")),data =.) %>% 
    lme4::VarCorr() %>% 
    as_tibble() %>% 
    pull(vcov)
  return(vcor[1]/(vcor[1]+ vcor[2]))
  
}



# 0 means that all variation is within groups, there are no differences between groups over and above within group var
# 1 means that all variation is between groups, knowing your score tells me what group you're in. 
# These are equivalent to ICCs fr individual ratings, not for the average
iccs_mlm = tibble(
  variable = c(outcomes,"pn_bf","pn_scw","pn_grit","pn_grat","pn_sci"),
  group = "StudyID",
  Variable = c(outcomes, outcomes, "friend"),
  peer = c(rep("Self-Report",4),rep("Peer-GPA",5))
) %>% 
  mutate(icc = pmap_dbl(list(variable,group),icc))

p = iccs_mlm %>% 
  rename_outs(Variable,single_line = F) %>% 
  ggplot(aes(name,icc,fill = peer))+
  geom_col(position = "dodge")+
  geom_text(aes(label = numformat(icc,3),y = .025),position = position_dodge(width = .9),color= "white")+
  scale_fill_brewer(palette = "Set1")+
  theme_ang()+
  labs(x = "Variable",fill = "Type")
if(bw){p= p+scale_color_manual(values = gray_errorbars)+scale_fill_manual(values = grayscale)}
p
ggsave2("Scraps/MLM_iccs",6,4)

# RUN models
year1 = c("T1","T2")
year2 = c("T3","T4")

scwpeer_coregpa = scwpeer_coregpa %>% rename(rolemodel = near) %>% full_join(bf_coregpa %>% select(StudyID,school,time,friend = near))
gritpeer_coregpa = gritpeer_coregpa %>% rename(rolemodel = near) %>% full_join(bf_coregpa %>% select(StudyID,school,time,friend = near))
gratpeer_coregpa = gratpeer_coregpa %>% rename(rolemodel = near) %>% full_join(bf_coregpa %>% select(StudyID,school,time,friend = near))
scipeer_coregpa = scipeer_coregpa %>% rename(rolemodel = near) %>% full_join(bf_coregpa %>% rename(friend = near))
aotpeer_coregpa = aotpeer_coregpa %>% rename(rolemodel = near) %>% full_join(bf_coregpa %>% select(StudyID,school,time,friend = near))

# Main specs
mainspecs = function(outcome,time=0, peer_type = "models",model = 1, ...){
  
data = analysis_data

if(outcome == "scw_s"){data = data %>% rename(rolemodel  = pn_scw)}
if(outcome == "avt_scw_tr"){data = data %>% rename(rolemodel  = pn_scw)}
if(outcome == "grit_s"){data = data %>% rename(rolemodel  = pn_grit)}
if(outcome == "grat_s"){data = data %>% rename(rolemodel  = pn_grat)}
if(outcome == "sci_s"){data = data %>% rename(rolemodel  = pn_sci)}
if(outcome == "aot_s"){data = data %>% rename(rolemodel  = pn_aot)}
  
  ids = data %>% 
    group_by(StudyID) %>% 
    count() %>% 
    filter(n>=k) %>% 
    pull(StudyID)
  
  if(time == 1){data = data %>% filter(time == "T1")}
  if(time == 2){data = data %>% filter(time == "T2")}
  if(time == 3){data = data %>% filter(time == "T3")}
  if(time == 4){data = data %>% filter(time == "T4")}
  
  preds = case_when(peer_type == "both" ~ "rolemodel + pn_bf",
                    peer_type == "models" ~ "rolemodel",
                    peer_type == "friends" ~ "pn_bf")
  
  f1 = formula(glue::glue("{outcome} ~ {preds} + coregpa|StudyID|0|StudyID"))
  f2 = formula(glue::glue("{outcome} ~ {preds} + coregpa + school|0|0|StudyID"))
  f3 = formula(glue::glue("{outcome} ~ {preds} + coregpa+school+ female + eth + ell + sped + frpl|0|0|StudyID"))
  f4 = formula(glue::glue("{outcome} ~ {preds} + coregpa+schoolp2|0|0|schoolp2"))
  f5 = formula(glue::glue("{outcome} ~ {preds} + coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2"))
  
  
  
  if(model == 1){
    a = data %>%
      filter(school == "p2" | school == "p1") %>%
      filter(StudyID %in% ids) %>% 
      ungroup() %>%
      mutate(StudyID = as.character(StudyID)) %>%
      lfe::felm(f1,data=.)
  }
  
  if(model == 2){
    a = data %>% 
      filter(school == "p2" | school == "p1") %>% 
      ungroup() %>% 
      lfe::felm(f2,data=.)
  }
   
  if(model == 3){
    a = data %>% 
    filter(school == "p2" | school == "p1") %>% 
    ungroup() %>% 
    lfe::felm(f3,data=.)
  }
  
  if(model == 4){
    a = data %>% 
      filter(school == "p2" | school == "p1") %>% 
      # filter(time %in% year) %>% 
      mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
      group_by(StudyID) %>% 
      summarise_all(mean,na.rm=T) %>% 
     lfe::felm(f4 ,data=.)
    
  }
  
  if(model == 5){
    a = data %>% 
      filter(school == "p2" | school == "p1") %>% 
      # filter(time %in% year) %>% 
      mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
      group_by(StudyID,eth) %>% 
      summarise_if(is.numeric,mean,na.rm=T) %>% 
      lfe::felm(f5,data=.)
  }
  
  
  return(a)
}
bengazer = function(...){stargazer::stargazer(...,type = "text",style = "asr",ci = T,initial.zero = F,report = "vc*s",
                                              table.layout = "-dc-t-sa-n")
}




big_models = expand_grid(time = 0:4, outcomes = "scw_s", peer_type = c("models","friends","both"),model = c(1:5)) %>% 
  filter(!(time == 1 & outcomes == "sci_s" & peer_type %in%  c("models","both"))) %>% 
  filter(!(time != 0 & model == 1))

if(!eachtime){big_models = big_models %>% filter(time == 0)}

big_models = big_models %>% 
  mutate(lm = pmap(list(outcomes,time,peer_type,model),mainspecs))

big_models
write_rds(big_models,"Output/Raw/big_models_both.rds")


report_models = function(models){return(bengazer(models,no.space=T,column.labels = c("Within-person","No fixed effects","No fixed effects","Average","Average")))}
