# Factor analyzing measures to test a tripartite model
library(gt)
item.l = grit_items %>% 
  left_join(scw_items) %>% 
  left_join(sci_items) %>% 
  left_join(aot_items) %>% 
  left_join(gratitude_items) %>% 
  left_join(grit_items) %>% 
  left_join(psp_items_multigoal)

library(broom);library(gt)
clean.l = read_rds("Data/clean.l.rds")

# EFA ####
#Mean
clean.l %>% select(StudyID,school,time,grit_s:purpose_s,-sccomb_s) %>% 
  select(-purpose_s) %>% 
  group_by(StudyID) %>% 
  summarise_all(mean) %>% 
  select(grit_s:grat_s) %>% 
  psych::fa(nfactors = 2)  %>% Ben::gt_fatable()

#Overall
clean.l %>% select(StudyID,school,time,grit_s:purpose_s,-sccomb_s) %>% 
  select(grit_s:purpose_s) %>% 
  psych::fa(nfactors = 3) %>% 
  Ben::gt_fatable()


# CFA ####
# Wide
clean.w = clean.l %>% select(StudyID,time,grit_s:purpose_s,-sccomb_s) %>% 
  pivot_wider(names_from = time,values_from = grit_s:purpose_s)

model = c(paste("intra =~ ",clean.w %>% select(2:9) %>% colnames() %>% paste(collapse = "+")),
paste("inter=~ ",clean.w %>% select(10:13, 18:25) %>% colnames() %>% paste(collapse = "+")),
paste("intel =~",clean.w %>% select(14:17) %>% colnames() %>% paste(collapse = "+")))

fit = cfa(model,clean.w,missing = "fiml") 
fit %>% summary(fit.measures = T,std = T)


# Long
model = "
intra =~ grit_s + scw_s
inter =~ sci_s + grat_s + purpose_s
intel =~ aot_s"
fit = cfa(model,clean.l,missing = "fiml") 
fit %>% summary(fit.measures = T,std = T)
inspect(fit,"cor.lv")

# Item level
library(lavaan.survey)
model = "
intra =~ grit1 + grit2 + grit3 + grit4 + grit5 + scw1 + scw2 + scw3 + scw4 + scw5
inter =~ sci1 + sci2 + sci3 + sci4 + sci5 + pp1 + pp2 + pp3 + pp4 + pp5 + pp6 + grat1 + grat2+ grat3 + grat4 + grat5
intel =~ aot1 + aot2 + aot3 + aot4 + aot5 + aot5 + aot6 + aot7 + aot8"

fit = cfa(model,item.l,estimator = "WLSMV") 
clustered_STS <- svydesign(ids = ~studyid , data = item.l) 
fit.path <- lavaan.survey(fit, survey.design = clustered_STS)

fit.path %>% summary(fit.measures = T,std = T)

#Time points as separate varaibles
clean.l %>% select(StudyID,school,time,grit_s:purpose_s,-sccomb_s) %>% 
  pivot_wider(names_from = time,values_from = c(grit_s:purpose_s)) %>% 
  select(grit_s_T1:purpose_s_T4) %>% 
  psych::fa(nfactors = 3) %>% 
  psych::print.psych(cut = .4,sort= T)


