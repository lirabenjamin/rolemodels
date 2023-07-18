load("Data/teacher_ratings.rda")

icc_teachers = function(data,ci_level){data %>% select(3:6) %>% as.matrix %>% psych::ICC(alpha = ci_level) %>% `$`(results)}

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
  mutate(icc = map(data, icc_teachers),variable = fct_inorder(variable)) %>% unnest(icc) %>% 
  filter(type == "ICC3k") %>% select(variable,ICC,p,lo = `lower bound`, hi = `upper bound`)

#Finding null distribution
teacher_ratings_scw %>% select(3:6) %>% as.matrix %>% psych::ICC() %>% `$`(results)

random_ratings = 
right_join(teacher_ratings_scw,teacher_ratings_grit) %>% 
left_join(teacher_ratings_grat) %>% 
left_join(teacher_ratings_sci) %>% 
left_join(teacher_ratings_aot) %>% 
select(3:27) %>% 
select(-matches("avg")) %>% 
select(sample(1:20,4))

#Scramble %>% 
null_iccs = numeric(length = 2*10^4)
set.seed(2345343)
for (j in 1:length(null_iccs)){
  tryCatch({
    null_iccs[j] = (random_ratings %>% 
      sample(replace = T) %>% 
      as.matrix() %>% 
      psych::ICC()%>% `$`(results) %>% pull(ICC))[6]
  }, 
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
   }
 
null_iccs = null_iccs %>% enframe() %>% 
  filter(value != 0) %>% 
  slice_sample(n  = 19694) %>% 
  mutate(name = rep(c('a','b'),19694/2),
         id = rep(1:(19694/2),each = 2)) %>% 
  spread(name,value) %>% 
  mutate(d = a-b)

write_rds(null_iccs, "Output/Scraps/null_iccs.rds")

quantiles = null_iccs %>% summarise(x = quantile(d, c(.001/2, .01/2, .05/2, 1 - (.05/2), 1- (.01/2), 1 - (.001/2)))) %>% unlist()


null_iccs$d %>% hist()
abs(null_iccs$d) %>% quantile(c(.999, .99, .95))
abs(null_iccs$d) %>% hist()
abline(v = abs(null_iccs$d) %>% quantile(c(.999, .99, .95)), col="red", lwd=3, lty=2)


teacher_ratings_iccs %>% select(1,2) %>% rename(variable2 = variable, ICC2 = ICC) %>% 
  expand_grid(teacher_ratings_iccs %>% select(1,2)) %>% 
  filter(variable2 != variable) %>% 
  mutate(dif = ICC2 - ICC) %>% 
  expand_grid(quantiles = abs(null_iccs$d) %>% quantile(c(.999, .99, .95))) %>% 
  mutate(sig = abs(dif) > quantiles) %>% 
  filter(sig)

p.teacher_ratings_iccs = teacher_ratings_iccs %>% 
  ggplot(aes(variable,ICC))+
  geom_col(fill = "white",col = "black")+
  geom_text(aes(label = Ben::numformat(ICC,3),y = .05))+
  # geom_text(aes(label = Ben::numformat(ICC,3)),color = "white",y = .1)+
  geom_errorbar(aes(ymin = lo, ymax = hi),width= .3)+
  Ben::theme_ang()+
  labs(y = "Intraclass Correlation Coefficient (ICC3k)",
       x = "Personality Trait")+
  annotate("segment", x = 1, xend = 3, y = .9, yend = .9)+
  annotate("text", x = 2, y = .92, label = "***")
p.teacher_ratings_iccs
ggsave2("Main Manuscript/ICCs", width = 7, height = 4)

 # Teacher rating to self report validity
cors = clean.l %>% 
  select(ends_with("_s"),starts_with("avg")) %>% 
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

a = cors %>% filter(x == y) %>% 
  ggplot(aes(x,r))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(r,2),y = .05*sign(r)),color= 'white')+
  labs(title = "Correlation with Teacher Reports",x= "Self-Reports")+
  Ben::theme_ang()
b = cors %>% 
  ggplot(aes(x,y,fill = r))+
  geom_tile(show.legend = F)+
  scale_fill_viridis_c()+
  geom_text(aes(label = Ben::numformat(r,2)))+
  labs(y = "Teacher Ratings",x= "Self-Reports")+
  Ben::theme_ang()

ggpubr::ggarrange(a,b,ncol = 1,heights = c(3,3),align = "hv",labels = "AUTO",font.label =list(face="plain"))
ggsave2("Main Manuscript/Explanations",width = 7,height = 5)
# Scraps

# Correlations among teachers
clean.l %>% 
  select(starts_with("avg")) %>% 
  corrr::correlate() %>% 
  corrr::stretch() %>% 
  filter(!is.na(r)) %>% 
  ggplot(aes(x,y,fill = r))+
  geom_tile()+
  scale_fill_viridis_c()+
  geom_text(aes(label = Ben::numformat(r)))+
  Ben::theme_ang()
ggsave2("Scraps/Teacher ratings intercors",width = 7,height = 5)

