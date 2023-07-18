# Participants
clean = read_rds("Data/clean.rds")
clean.l = read_rds("Data/clean.l.rds")


print("SAMPLE CHARACTERISTICS:")
print("Gender:")
clean %>% count(female) %>% mutate(n = n*100 / (1100),n = Ben::numformat(n,2)) %>% print()
print("Age:")
clean.l %>% select(StudyID,age,time) %>% 
  group_by(time) %>% 
  summarise(mean = mean(age,na.rm =T) %>% numformat(),
            min = min(age,na.rm =T)%>% numformat(),
            max = max(age,na.rm =T)%>% numformat(),
            med = median(age,na.rm =T)%>% numformat()) %>% print()

print("Ethnicity:")
clean.l %>% 
  mutate(eth = fct_lump_n(eth,4)) %>% 
  count(eth) %>% 
  arrange(n) %>% 
  mutate(n = (n/nrow(clean.l)*100) %>% numformat) %>% print()

print("Ethnicity:")
clean.l %>% 
  count(eth) %>% 
  arrange(n) %>% 
  mutate(n = (n/nrow(clean.l)*100) %>% numformat)

print("Free or reduced priced meals:")
clean.l %>% 
  count(frpl) %>% 
  arrange(n) %>% 
  mutate(n = (n/nrow(clean.l)*100) %>% numformat)


# Alphas
cdap = haven::read_sav("Data/DEIDENTIFIED_TO_SHARE_T1-T4 CDAP Dataset 4.6.18.sav")

find_items = function(variables){
  cdap %>% filter(PublicMS1 == 1 | PublicMS2 ==1) %>%
    select(StudyID,{{variables}}) %>% 
    rename_with(tolower) %>% 
    pivot_longer(cols = 2:ncol(.)) %>% 
    separate(name, c("variable","time")) %>% 
    pivot_wider(names_from = variable,values_from = value)
  
}
six_alphas = function(item_data){
  overall = item_data %>% select(-studyid) %>% select_if(is.numeric) %>% psych::alpha(check.keys = T) 
  print(overall)
  overall = overall %>% `$`(total) %>% `$`(raw_alpha) %>% enframe(name = "time",value = "alpha") %>% mutate(time = "overall")
  mean = item_data %>% group_by(studyid) %>% summarise_all(function(x){mean(x,na.rm=T)}) %>% select(-studyid) %>% select_if(is.numeric) %>% psych::alpha(check.keys = T)%>% `$`(total) %>% `$`(raw_alpha) %>% enframe(name = "time",value = "alpha") %>% mutate(time = "mean")
  times = item_data %>% select(-studyid) %>% group_by(time) %>% nest() %>% mutate(alpha_obj = map(data,psych::alpha,check.keys = T),
                                                                                  alpha = map(alpha_obj,"total"),
                                                                                  alpha = map_dbl(alpha,"raw_alpha")) %>% select(-data)
  return(bind_rows(overall,mean,times))
}

grit_items = find_items(c(GRIT1_T1:GRIT5_T1,Grit1_T2:Grit5_T2,Grit1_T3:Grit5_T3,Grit1_T4:Grit5_T4))
scw_items =  find_items(c(SCW1_T1:SCW5_T1,SCW1_T2:SCW5_T2,SCW1_T3:SCW5_T3,SCW1_T4:SCW5_T4))
sci_items = find_items(c(SCI1_T1:SCI5_T1,SCI1_T2:SCI5_T2,SCI1_T3:SCI5_T3,SCI1_T4:SCI5_T4))
gratitude_items = find_items(c(GRAT1_T1:GRAT5_T1,Grat1_T2:Grat5_T2,Grat1_T3:Grat5_T3,Grat1_T4:Grat5_T4))
aot_items = find_items(c(AOT1_T1:AOT8_T1,AOT1_T2:AOT8_T2,AOT1_T3:AOT8_T3,AOT1_T4:AOT8_T4)) 
# psp_items = find_items(c(PP1a_T1:PP6e_T1,PP1a_T2:PP6e_T2,PP1a_T3:PP6e_T3,PP1a_T4:PP6e_T4))

# Scoring and alphaing PSP
# psp_items_multigoal = psp_items %>% 
#   pivot_longer(pp1a:pp6e) %>% 
#   filter(!is.na(value)) %>% 
#   separate(name,c("item","goal"),3) %>% 
#   pivot_wider(names_from = item,values_from = value) %>% 
#   rowwise() %>% 
#   mutate(total = pp1+pp2+pp3+pp4+pp5+pp6) %>% 
#   group_by(studyid,time) %>% 
#   filter(total == max(total)) %>% 
#   select(studyid,time, pp1:pp6) %>% 
#   ungroup()

alphas = enframe(list(
  scw_items,
  grit_items,
  gratitude_items,
  sci_items
  # ,
  # aot_items
  #,
  # psp_items,psp_items_multigoal,
  # psp_items %>% select(1,2,(0:5)*5+3),
  # psp_items %>% select(1,2,(0:5)*5+4),
  # psp_items %>% select(1,2,(0:5)*5+5),
  # psp_items %>% select(1,2,(0:5)*5+6),
  # psp_items %>% select(1,2,(0:5)*5+7)
),
                 name = NULL,value = "data") %>% 
  mutate(variable = c("Academic\nSelf-Control",
                      "Grit",
                      "Gratitude",
                      "Interpersonal\nSelf-Control"
                      # ,
                      # "Actively\nOpen-Minded\nThinking"#,
                      # "Prosocial\nPurpose - All items",
                      # "Prosocial\nPurpose - Goal used for scoring",
                      # "Prosocial\nPurpose Goal 1 ",
                      # "Prosocial\nPurpose Goal 2",
                      # "Prosocial\nPurpose Goal 3",
                      # "Prosocial\nPurpose Goal 4",
                      # "Prosocial\nPurpose Goal 5"
                      )) %>% 
  mutate(alphas = map(data,six_alphas)) %>% 
  unnest(alphas)

alphas %>% 
  filter(time != "overall") %>% 
  filter(time != "mean") %>% 
  group_by(variable) %>% 
  summarise(a = mean(alpha) %>% numformat())

t.alphas = alphas %>% 
  mutate(variable = str_replace_all(variable,"\n"," ")) %>% 
  mutate(variable = fct_inorder(variable),alpha = Ben::numformat(alpha,3)) %>%  select(variable,time,alpha) %>% spread(time,alpha) %>% 
  gt::gt()
t.alphas
gt::gtsave(t.alphas,filename = "Output/Supplement/Alphas.tex")
gt::gtsave(t.alphas,filename = "Output/Supplement/Alphas.html")

alphas %>%
  mutate(variable = str_replace_all(variable,"\n"," ")) %>% 
  mutate(variable = fct_inorder(variable)) %>% 
  ggplot(aes(time,alpha))+
  geom_col()+
  facet_wrap(~variable)+
  geom_text(aes(label = Ben::numformat(alpha)),y = .1, col = "white")+
  labs(y = "Cronbach's Alpha")
ggsave2(filename = "Supplement/Alphas",width = 7,height = 5)

# Test retest reliability
clean.l %>% 
  select(StudyID,outcomes,time) %>% 
  pivot_longer(outcomes) %>% 
  pivot_wider(names_from = time,values_from = value) %>% 
  group_by(name) %>% 
  select(-StudyID) %>% 
  nest() %>% 
  mutate(cors = map(data,corrr::correlate),
         cors = map(cors,corrr::stretch)) %>% 
  unnest(cors) %>% 
  group_by(name) %>% 
  slice(2,7,12) %>% 
  filter(r == min(r) | r == max(r)) %>% 
  mutate(n = c(1,2)) %>% 
  select(name, n,r) %>% 
  mutate(r = numformat(r)) %>% 
  spread(n,r)
