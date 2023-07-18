load("Data/teacher_ratings.rda")
clean.l = read_rds("Data/clean.l.rds")

icc_teachers = function(data,ci_level){data %>% select(3:6) %>% as.matrix %>% psych::ICC(alpha = ci_level) %>% `$`(results)}

teacher_ratings_aot %>% select(3:6) %>% as.matrix %>% psych::ICC(alpha = .01) %>% `$`(results)

teacher_ratings_iccs = tibble(variable = c(
  "Academic\nSelf-Control",
  "Grit",
  "Gratitude",
  "Interpersonal\nSelf-Control",
  "Actively\nOpen-Minded\nThinking"),
  data = list(
    teacher_ratings_scw,
    teacher_ratings_grit,
    teacher_ratings_grat,
    teacher_ratings_sci,
    teacher_ratings_aot
  )) %>%
  mutate(icc05 = map(data, icc_teachers,.05),
         icc01 = map(data, icc_teachers,.01),
         icc001 = map(data, icc_teachers,.001),
         variable = fct_inorder(variable)) %>% 
  pivot_longer(icc05:icc001) %>% 
  unnest(value) %>% 
  filter(type == "ICC3k") %>% select(variable,name,ICC,p,lo = `lower bound`, hi = `upper bound`)

t.teacher_ratings_iccs = teacher_ratings_iccs %>% 
  filter(name == "icc05") %>% 
  rename(`Personality Trait` = variable) %>% 
  mutate(p = ifelse(p < .001, "<.001"),
         ICC = Ben::numformat(ICC,3),
         `[95% CI]` = glue::glue("[{Ben::numformat(lo,3)}-{Ben::numformat(hi,3)}]")) %>% 
  select(1,3,4,7) %>% 
  gt::gt() %>% 
  gt::cols_align("left",1)
t.teacher_ratings_iccs
gt::gtsave(t.teacher_ratings_iccs,filename = "Output/Main Manuscript/t.teacher_ratings_iccs.tex")
gt::gtsave(t.teacher_ratings_iccs,filename = "Output/Main Manuscript/t.teacher_ratings_iccs.html")


p.teacher_ratings_iccs = teacher_ratings_iccs %>% 
  ggplot(aes(variable,ICC))+
  # geom_col(fill = "white",col = "black")+
  # geom_text(aes(label = Ben::numformat(ICC,3),y = .05))+
  # geom_text(aes(label = Ben::numformat(ICC,3)),color = "white",y = .1)+
  geom_errorbar(aes(ymin = lo, ymax = hi),width= .3)+
  # Ben::theme_ang()+
  facet_grid(~name) +
  labs(y = "Intraclass Correlation Coefficient (ICC3k)",
       x = "Personality Trait")
  # annotate("segment", x = 1, xend = 3, y = .9, yend = .9)+
  # annotate("text", x = 2, y = .92, label = "***")
p.teacher_ratings_iccs
ggsave2("Scraps/ICCs_significance", width = 7, height = 4)
a = .1
p.teacher_ratings_iccs = teacher_ratings_iccs %>% filter(name == "icc05") %>% 
  ggplot(aes(variable,ICC))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(ICC,3),y = .05),color='white')+
  # geom_text(aes(label = Ben::numformat(ICC,3)),color = "black",y = .1)+
  geom_errorbar(aes(ymin = lo, ymax = hi),width= .3)+
  Ben::theme_ang()+
  labs(y = "Intraclass Correlation Coefficient (ICC3k)",
       x = "Personality Trait")+
  # annotate("segment", x = 1+a, xend = 2-a, y = .9, yend = .9)+annotate("text", x = 1.5, y = .92, label = "ns")+
  # annotate("segment", x = 1+a, xend = 3-a, y = .95, yend = .95)+annotate("text", x = 2,   y = .97, label = "***")+
  # annotate("segment", x = 1+a, xend = 4-a, y = 1, yend = 1)+annotate("text", x = 2.5, y = 1.02, label = "***")+
  # annotate("segment", x = 1+a, xend = 5-a, y = 1.05, yend = 1.05)+annotate("text", x = 3,   y = 1.07, label = "***")+
  # annotate("segment", x = 2+a, xend = 3-a, y = .9, yend = .9)+annotate("text", x = 2.5, y = .92, label = "***")+
  # annotate("segment", x = 2+a, xend = 4-a, y = 1.1, yend = 1.1)+annotate("text", x = 3,   y = 1.12, label = "*")+
  # annotate("segment", x = 2+a, xend = 5-a, y = 1.15, yend = 1.15)+annotate("text", x = 3.5, y = 1.17, label = "***")+
  # annotate("segment", x = 3+a, xend = 4-a, y = .9, yend = .9)+annotate("text", x = 3.5, y = .92, label = "***")+
  # annotate("segment", x = 3+a, xend = 5-a, y = .95, yend = .95)+annotate("text", x = 4,   y = .97, label = "ns")+
  # annotate("segment", x = 4+a, xend = 5-a, y = .9, yend = .9)+annotate("text", x = 4.5, y = .92, label = "***")+
  scale_y_continuous(breaks = (0:5)/5)
p.teacher_ratings_iccs
ggsave2("Main Manuscript/ICCs", width = 8, height = 4)

 # Teacher rating to self report validity
cors = clean.l %>% 
  select(StudyID,time,ends_with("_s"),starts_with("avg")) %>% 
  group_by(StudyID) %>% 
  summarise_all(mean,na.rm= T) %>% 
  select(scw_s:avg_aot_tr) %>% 
  corrr::correlate() %>% 
  corrr::stretch() %>% 
  filter(!str_detect(x,"avg"),str_detect(y,"avg")) %>%
  mutate_at(vars(x),str_remove_all,"_s") %>% 
  mutate_at(vars(y),str_remove_all,"avg_") %>% 
  mutate_at(vars(y),str_remove_all,"_tr") %>% 
  mutate_at(vars(x,y), factor,levels = c("scw","grit","grat","sci",'aot','purpose'),labels = c(
    "Academic\nSelf-Control",
    "Grit",
    "Gratitude",
    "Interpersonal\nSelf-Control",
    "Actively\nOpen-Minded\nThinking",
    "Prosocial\nPurpose"))

# Significance of correlations for results
clean.l %>% 
  select(StudyID,time,ends_with("_s"),starts_with("avg")) %>% 
  group_by(StudyID) %>% 
  summarise_all(mean,na.rm= T) %>% 
  select(scw_s:avg_aot_tr) %>% 
  as.matrix() %>% 
  Hmisc::rcorr() %>%
  `[[`('P') %>% corrr::as_cordf()

a = cors %>% filter(x == y) %>% 
  ggplot(aes(x,r))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(r,3),y = .05*sign(r)),color= 'white')+
  labs(title = "Correlation with Teacher Reports",x= "Self-Reports")+
  Ben::theme_ang()
b = cors %>% 
  mutate(col = x == y,size = x == y) %>% 
  ggplot(aes(x,y,fill = r))+
  geom_tile(show.legend = F, aes(col = col, size= size))+
  scale_fill_viridis_c()+
  scale_color_manual(values = c("white", "black"))+
  scale_size_manual(values = c(.25,.5))+
  geom_text(aes(label = Ben::numformat(r,3)))+
  labs(y = "Teacher Ratings",x= "Self-Reports")+
  Ben::theme_ang()

ggpubr::ggarrange(a,b,ncol = 1,heights = c(3,3),align = "hv",labels = "AUTO",font.label =list(face="plain"))
ggsave2("Scraps/Explanations",width = 7,height = 5)


# Scraps

# Correlations among teachers
clean.l %>% 
  select(starts_with("avg")) %>%
  select(-avg_aot_tr) %>% 
  rename("Academic Self-Control" = avg_scw_tr,"Grit" = avg_grit_tr,"Gratitude" =avg_grat_tr,"Interpersonal Self-Cntrol"=avg_sci_tr) %>% 
  corrr::correlate() %>% 
  corrr::stretch() %>% 
  filter(!is.na(r)) %>% 
  ggplot(aes(x,y,fill = r))+
  geom_tile()+
  scale_fill_viridis_c()+
  geom_text(aes(label = Ben::numformat(r)))+
  Ben::theme_ang()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))
ggsave2("Scraps/Teacher ratings intercors",width = 7,height = 5)
