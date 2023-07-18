library(broom)
clean.l = read_rds("Data/clean.l.rds")

p.gpa_lm = clean.l %>% 
  group_by(StudyID,eth, school) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate_if(is.numeric,scale) %>% 
  nest() %>% 
  mutate(
    Adjusted = map(data,function(x){lm(coregpa ~ scw_s + grit_s + grat_s + sci_s+aot_s+ school +female + eth + ell + sped + frpl,data = x)}),
    Unadjusted = map(data,function(x){lm(coregpa ~ scw_s + grit_s + grat_s + sci_s+aot_s,data = x)})
  ) %>% 
  select(-data) %>% 
  pivot_longer(Adjusted:Unadjusted)%>% 
  mutate(value = map(value,tidy,conf.int=T)) %>% 
  unnest(value) %>% 
  filter(term %in%  outcomes) %>% 
  mutate(Model = name) %>% 
  rename_outs(term,single_line = F) %>% 
  mutate(term = name) %>% 
  ggplot(aes(term,estimate,fill=Model,col = Model))+
  geom_col(position = position_dodge(width = .9),color = NA)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = .3,position = position_dodge(width = .9),show.legend = F)+
  scale_fill_brewer(palette = "Set1")+
  geom_text(aes(label = Ben::numformat(estimate),y = estimate+sign(estimate)*.0075), position = position_dodge(width = .9),size = 2.5, hjust = 1.125,show.legend = F)+
  scale_color_manual(values = c("#FF4D4F","#6AB1EB"))+
  geom_hline(yintercept = 0,size = .1)+
  theme(legend.position = c(.9,.8 ))+
  labs(x = "Term",y = "Beta of Charater Trait on GPA",fill = "")
p.gpa_lm

clean.l %>% 
  group_by(StudyID,eth, school) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate_if(is.numeric,scale) %>% 
  nest() %>% 
  mutate(
    Adjusted = map(data,function(x){lm(coregpa ~ scw_s + grit_s + grat_s + sci_s+aot_s+ school +female + eth + ell + sped + frpl,data = x)}),
    Unadjusted = map(data,function(x){lm(coregpa ~ scw_s + grit_s + grat_s + sci_s+aot_s,data = x)})
  ) %>% 
  select(-data) %>% 
  pivot_longer(Adjusted:Unadjusted) %>% 
  mutate(value = map(value,tidy,conf.int=T)) %>% 
  unnest(value) %>% 
  filter(term %in% outcomes, name == "Adjusted") %>% 
  mutate(estimate = numformat(estimate))

ggsave2("Main Manuscript/Explanations_lmGPA",width = 6,height = 4)

ggpubr::ggarrange(p.gpa_lm,p.teacher_ratings_iccs,nrow = 1,heights = c(3,3),align = "hv",labels = "AUTO",font.label =list(face="plain"))
ggsave2("Main Manuscript/Explanations",width = 10,height = 4)

clean.l %>% 
  select(StudyID,time,matches("pn")) %>% 
  pivot_longer(c(3:8,11:13)) %>% 
  mutate(g1 = pn_grat==value,
         g2 = pn_grat2 == value) %>% 
  rowwise() %>% 
  mutate(g = mean(c(g1,g2),na.rm=T)) %>% 
  group_by(StudyID,time,name) %>% 
  summarise(prop_repeated = mean(g,na.rm=T)) %>% 
  ggplot(aes(prop_repeated,fill = name))+
  geom_histogram()
  
clean.l %>% 
  group_by(StudyID) %>% 
  summarise_all(mean) %>% 
  # summarise_all(scale) %>% 
  select(outcomes,coregpa) %>% 
  Ben::HARcor() %>% 
  gt::gt() %>% 
  gt::gtsave(filename = "Output/Main Manuscript/corsongpa.html")
