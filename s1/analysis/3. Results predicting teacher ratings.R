outcome = "avg_scw_tr"
preds = "pn_scw + pn_bf"

f1 = formula(glue::glue("{outcome} ~ {preds} + coregpa|StudyID|0|StudyID"))
f2 = formula(glue::glue("{outcome} ~ {preds} + coregpa + school|0|0|StudyID"))
f3 = formula(glue::glue("{outcome} ~ {preds} + coregpa+school+ female + eth + ell + sped + frpl|0|0|StudyID"))
f4 = formula(glue::glue("{outcome} ~ {preds} + coregpa+schoolp2|0|0|schoolp2"))
f5 = formula(glue::glue("{outcome} ~ {preds} + coregpa+schoolp2+ female + eth + ell + sped + frpl|0|0|schoolp2"))


analysis_data %>% 
  filter(school == "p2" | school == "p1") %>% 
  # filter(time %in% year) %>% 
  mutate(schoolp2 = ifelse(school == "p2",1,0)) %>% 
  group_by(StudyID,eth) %>% 
  summarise_if(is.numeric,mean,na.rm=T) %>% 
  lfe::felm(f5,data=.) %>% 
  stargazer::stargazer(type = "latex", star.cutoffs = c(.05,.01,.001),caption = "Preddicting teacher ratings")
