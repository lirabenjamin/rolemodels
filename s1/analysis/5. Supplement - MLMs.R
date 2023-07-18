data = read_rds("Data/Peer Quality Datasets/analysis_data.rds")

run_mlm = function(control = TRUE, outcome = scw_s, peer = pn_scw, model = c("together","friend","model")){
  datatest = data %>% 
    group_by(StudyID) %>% 
    rename(pn_model = {{peer}}) %>% 
    select(StudyID, school, female, eth, ell, sped, frpl, time, age,coregpa,{{outcome}},pn_model,pn_bf) %>% 
    mutate(coregpa_m = mean(coregpa,na.rm=T),
           model_m = mean(pn_model,na.rm=T),
           friend_m = mean(pn_bf,na.rm=T)) %>% 
    rowwise() %>% 
    mutate(coregpa_d = coregpa - coregpa_m,
           model_d = pn_model - model_m,
           friend_d = pn_bf-friend_m)
  
    
    if(control == TRUE & model == "model"){f = glue::glue("{outcome} ~  model_m + model_d + coregpa_m+coregpa_d +school+ female + eth + ell + sped + frpl+(1|StudyID)")}
    if(control == TRUE & model == "friend"){f= glue::glue("{outcome} ~  friend_m + friend_d +coregpa_m+coregpa_d +school+ female + eth + ell + sped + frpl +(1|StudyID)")}
    if(control == TRUE & model == "together"){f = glue::glue("{outcome} ~  model_m + model_d + friend_m + friend_d +coregpa_m+coregpa_d +school+ female + eth + ell + sped + frpl+ (1|StudyID)")}
    
    if(control == FALSE & model == "model"){f = glue::glue("{outcome} ~  model_m + model_d+coregpa_m+coregpa_d  +school+ (1|StudyID)")}
    if(control == FALSE & model == "friend"){f = glue::glue("{outcome} ~ friend_m + friend_d+coregpa_m+coregpa_d +school + (1|StudyID)")}
    if(control == FALSE & model == "together"){f = glue::glue("{outcome} ~  model_m + model_d + friend_m + friend_d +coregpa_m+coregpa_d +school+ (1|StudyID)")}
return(lme4::lmer(formula(f),datatest))
  }


specs = expand_grid(outcome = outcomes,control = c(TRUE,FALSE), model = c("together","friend","model")) %>% 
  mutate(peer = paste0("pn_",str_remove(outcome,pattern = "_s")), 
         mlm = pmap(list(control,outcome, peer,model),run_mlm)) %>% 
  rename_outs(outcome)

specs.p = specs %>% 
  mutate(mlm = map(mlm,broom.mixed::tidy,conf.int = T)) %>% 
  unnest(mlm)

specs.p %>% 
  filter(term %in% c('coregpa_m' ,
                     'coregpa_d',
                     'model_m'  ,
                     'model_d'  ,
                     'friend_m' ,
                     'friend_d' )) %>% 
  
  filter(model != "together", control) %>% 
  ggplot(aes(term,estimate,ymin = conf.low,ymax = conf.high))+
  geom_point()+
  geom_errorbar(width = .3)+
  geom_hline(yintercept = 0)+
  facet_grid(model~name,scales = "free_y")+
  coord_flip()

yellow= "#ffbf01"
blue = "#4472c3"

specs.p %>% 
  filter(term %in% c('model_m'  ,
                     'model_d'  ,
                     'friend_m' ,
                     'friend_d' )) %>% 
  mutate(term = case_when(term  == 'model_m'  ~ "Role Model GPA (Mean)",
                          term  == 'model_d'  ~"Role Model GPA (Deviation)",
                          term  == 'friend_m' ~"Friend GPA (Mean)",
                          term  == 'friend_d' ~ "Friend GPA (Deviation)")) %>% 
  mutate(term = fct_inorder(term)) %>% 
  mutate(color = ifelse(str_detect(term,"Role"),yellow,blue),
         lty = ifelse(str_detect(term,"Mean"),1,5)) %>% 
  filter(model != "together", control) %>% 
  ggplot(aes(term,estimate,ymin = conf.low,ymax = conf.high, color = I(color), lty = I(lty)))+
  geom_point()+
  geom_errorbar(width = .3)+
  geom_hline(yintercept = 0)+
  facet_grid(~name,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5),
        axis.title.y  = element_text(angle = 0,hjust = .5,vjust = .5))+
  theme_ang()+
  labs(x = NULL, y = expr(beta))
ggsave("Output/Supplement/MLMs.pdf",width = 7,height = 4)

covlabels = c("Role Models' Core GPA (Mean)",
              "Role Models' Core GPA (Residual)",
              "Friends' Core GPA (Mean)",
              "Friends' Core GPA (Residual)",
              "Own Core GPA (Mean)",
              "Own Core GPA (Residual)",
              "School",
              "Female",
              "American Indian",
              "Asian",
              "Caucasian",
              "Hispanic",
              "Multi-Racial",
              "English Language Learner",
              "Special Education Student",
              "Eligibile for Free or Reduced Priced Meals")

make_table = function(models_df,filename,format = c("tex","html"),...){stargazer::stargazer(models_df$mlm,
                                                                                            type = "text",
                                                                                            dep.var.caption = models_df$name[1] %>% as.character,
                                                                                            out = glue::glue("Output/Supplement/{filename}.{format}"),
                                                                                            covariate.labels = covlabels,
                                                                                            initial.zero = F,
                                                                                            ci = F,
                                                                                            no.space = T,
                                                                                            digits.extra = 0,
                                                                                            digits = 3,
                                                                                            table.layout = "-l-#-t-a-s-",
                                                                                            star.cutoffs = c(0.05, 0.01, 0.001),
                                                                                            align=TRUE,
                                                                                            notes.align = "l",
                                                                                            ...
)}

specs %>% 
  filter(outcome == outcomes[[1]]) %>% 
  make_table(filename = "MLM_asc",
             add.lines = list(c("Role Models and Friends Modelled Simultanieously",rep(c("Yes","No","No"),2)),
                              c("Controls for Demographics?",rep("Yes",3), rep("No",3))))

specs %>% 
  filter(outcome == outcomes[[2]]) %>% 
  make_table(filename = "MLM_grit",
             add.lines = list(c("Role Models and Friends Modelled Simultanieously",rep(c("Yes","No","No"),2)),
                              c("Controls for Demographics?",rep("Yes",3), rep("No",3))))

specs %>% 
  filter(outcome == outcomes[[3]]) %>% 
  make_table(filename = "MLM_grat",
             add.lines = list(c("Role Models and Friends Modelled Simultanieously",rep(c("Yes","No","No"),2)),
                              c("Controls for Demographics?",rep("Yes",3), rep("No",3))))

specs %>% 
  filter(outcome == outcomes[[4]]) %>% 
  make_table(filename = "MLM_sci",
             add.lines = list(c("Role Models and Friends Modelled Simultanieously",rep(c("Yes","No","No"),2)),
                              c("Controls for Demographics?",rep("Yes",3), rep("No",3))))



