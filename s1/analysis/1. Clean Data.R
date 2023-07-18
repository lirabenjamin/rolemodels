#Cleaning data
if (reclean_data){
  cdap = haven::read_sav("s1/data/DEIDENTIFIED_TO_SHARE_T1-T4 CDAP Dataset 4.6.18.sav")

clean = cdap %>% 
  select(
    StudyID,PublicMS1:CharterMS6,
    female = Female,eth = EthnicityName,ell = ELL,sped = SPED,frpl = FRLunch,fass = FinancialAssistance,
    matches("Grit_S_"),
    matches("SCW_S_"),matches("SCI_S_"),matches("SCcomb_S_"),
    matches("DelayD"),
    GGrid_Total_Score_Self_Scored,
    GGrid_Total_Score_Rater_Scored,
    DT_ZAvgNumCorr_PerBlock,
    DT_ZAvg_Prop_TimeOnTask,
    matches("GRIT_SS_T"),matches("SCW_SS_T"),
    matches("PN"),matches("Popularity"),
    matches("GPA"),matches("Tardies"),matches("Abs")
    ) %>% 
  select(-matches("AOT"),-matches("Grat"),-matches("PSP"))

AOT = cdap %>% 
  select(StudyID,matches("AOT")) %>% 
  select(StudyID,matches("AOT_S"),matches("PN_AOT"),-matches("_T_"),
         -matches("Score"))

Grat = 
  cdap %>% 
  select(StudyID,matches("Grat")) %>% 
  select(StudyID,matches("GRAT_S"),matches("PN_GRAT"),-matches("_T_"),
         -matches("Score"))

PSP = cdap %>% 
  select(StudyID,matches("Purpose"),matches("PN_PSP")) %>% 
  select(StudyID,matches("Purpose_S"),matches("PN_PSP"),-matches("_T_"),
         -matches("Score"))

clean = left_join(clean,AOT) %>% left_join(Grat) %>% left_join(PSP)

clean = clean %>% mutate(school = case_when(PublicMS1 == 1 ~ "p1",
                                    PublicMS2 == 1 ~ "p2",
                                    CharterMS1 == 1 ~ "c1",
                                    CharterMS2 == 1 ~ "c2",
                                    CharterMS3 == 1 ~ "c3",
                                    CharterMS4 == 1 ~ "c4",
                                    CharterMS5 == 1 ~ "c5",
                                    CharterMS6 == 1 ~ "c6")) %>% 
  select(StudyID,school, everything()) %>% select(-c(PublicMS1:CharterMS6))

clean = clean %>% select(-matches("log"))
gpa = clean %>% select(matches("gpa"),-matches("Z")) %>% select(-1,-2,-3) %>% select(order(colnames(.))) %>% colnames()
dropgpa = setdiff(colnames(clean)[colnames(clean) %>% str_detect(pattern = "GPA")],gpa)

clean = clean %>% select(-all_of(dropgpa))
clean = left_join(clean,cdap %>% select(StudyID,T1AGE,T2AGE,T3AGE,T4AGE))
rm(dropgpa)

# Get teacher ratings
teacher_ratings_grit = cdap %>% 
  select(StudyID,matches("grit_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","_SCI","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_grit_tr = mean(c(grit_eng_t,grit_math_t,grit_sci_t,grit_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_grit_tr)) %>% 
  ungroup()

teacher_ratings_scw = cdap %>% 
  select(StudyID,matches("SCW_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","SCI","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_scw_tr = mean(c(scw_eng_t,scw_math_t,scw_sci_t,scw_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_scw_tr)) %>% 
  ungroup()

teacher_ratings_sci = cdap %>% 
  select(StudyID,matches("SCI_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","_SCI_","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_sci_tr = mean(c(sci_eng_t,sci_math_t,sci_sci_t,sci_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_sci_tr)) %>% 
  ungroup() %>% select(-matches(c("grit","scw","aot","psp","grat")))

teacher_ratings_grat = cdap %>% 
  select(StudyID,matches("grat_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","_SCI_","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_grat_tr = mean(c(grat_eng_t,grat_math_t,grat_sci_t,grat_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_grat_tr)) %>% 
  ungroup()

teacher_ratings_aot = cdap %>% 
  select(StudyID,matches("AOT_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","_SCI_","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_aot_tr = mean(c(aot_eng_t,aot_math_t,aot_sci_t,aot_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_aot_tr)) %>% 
  ungroup()

teacher_ratings_psp = cdap %>% 
  select(StudyID,matches("psp_")) %>% select(StudyID,matches("_T_")) %>% select(StudyID,matches(c("Eng","Math","_SCI_","SS"))) %>% select(-matches("Z"))%>% select(-matches("by")) %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time)) %>% 
  rowwise() %>% 
  mutate(avg_psp_tr = mean(c(psp_eng_t,psp_math_t,psp_sci_t,psp_ss_t),na.rm = T)) %>% 
  filter(!is.na(avg_psp_tr)) %>% 
  ungroup()

save(
  teacher_ratings_scw,
  teacher_ratings_grat,
  teacher_ratings_grit,
  teacher_ratings_sci,
  teacher_ratings_aot,
  teacher_ratings_psp,file = "s1/data/teacher_ratings.rda")

clean.l = clean %>% 
  select(StudyID,school,female:frpl,age_T1 = T1AGE, age_T2 = T2AGE, age_T3 = T3AGE,age_T4 = T4AGE,
         matches("SCW_S_"),
         matches("Grit_S_"),
         #matches("SCcomb_S_"),
         matches("Grat_S"),
         matches("SCI_S_"),
         matches("AOT_S_"),
         #matches("Purpose_S_"),
         matches("CoreGPA"),-matches("Z"),matches("PN_BF"),
         matches("PN_SC"),
         matches("PN_Grit"),
         matches("PN_Grat"),
         matches("PN_AOT")
         #matches("PN_PSP")
         ) %>% 
  rename(PN_SCW_T1 = PN_SC1_T1,
         PN_SCW2_T1 = PN_SC2_T1,
         PN_SCW_T2 = PN_SCW_T2,
         PN_SCI_T2 = PN_SCI_T2,
         PN_SCW_T3 = PN_SCW_T3,
         PN_SCI_T3 = PN_SCI_T3,
         PN_SCW_T4 = PN_SCW_T4,
         PN_SCI_T4 = PN_SCI_T4,
         PN_Grit_T1 = PN_GRIT1_T1,
         PN_AOT_T1 = PN_AOT1_T1,
         PN_GRAT_T1 = PN_GRAT1_T1,
         # PN_PSP_T1 = PN_PSP1_T1
         ) %>% 
  pivot_longer(cols = 8:ncol(.)) %>% 
  mutate(name = str_replace_all(name,"_T1",".T1"),
         name = str_replace_all(name,"_T2",".T2"),
         name = str_replace_all(name,"_T3",".T3"),
         name = str_replace_all(name,"_T4",".T4")) %>% 
  separate(name,c("name","time"),"\\.") %>% 
  mutate(name = tolower(name)) %>% 
  pivot_wider(values_from = value,names_from = name,id_cols = c(StudyID:time))

clean.l = clean.l %>% select(1:19,pn_grit,pn_grit2, pn_grat, pn_grat2, pn_sci,pn_aot,pn_aot2)

clean.l = clean.l %>% 
  left_join(teacher_ratings_scw %>% select(StudyID,time,avg_scw_tr),by = c("StudyID","time")) %>% 
  left_join(teacher_ratings_grit %>% select(StudyID,time,avg_grit_tr),by = c("StudyID","time")) %>% 
  left_join(teacher_ratings_grat %>% select(StudyID,time,avg_grat_tr),by = c("StudyID","time")) %>% 
  left_join(teacher_ratings_sci %>% select(StudyID,time,avg_sci_tr),by = c("StudyID","time")) %>% 
  left_join(teacher_ratings_aot %>% select(StudyID,time,avg_aot_tr),by = c("StudyID","time"))
  # left_join(teacher_ratings_psp %>% select(StudyID,time,avg_psp_tr),by = c("StudyID","time"))


              
# Find Task measures to use as ground truth
cdap %>% 
  select(grit_grid_rater.T2 = "GGrid_Total_Score_Rater_Scored",
         grit_grid_self.T2 = "GGrid_Total_Score_Self_Scored",
         adt_correct.T2 = "DT_ZAvgNumCorr_PerBlock",
         adt_timeontask.T2 = "DT_ZAvg_Prop_TimeOnTask",
         "AOTbelief1_T2ann"          ,                       
         "AOTPre_T2"                  ,                       "AOTDesc_T2"     ,                                  
         "AOTSelf_T2"                  ,                      "TotalAOTArg"   ,
         )

cdap %>% select(PublicMS1,PublicMS2,PublicHS,
  "CTSch_Complaints_T3",
  "CTSch_Thanks_T3",
  "CTOut_Complaints_T3",
  "CTOut_Thanks_T3",
  "TCSch_Thanks_T3",
  "TCSch_Complaints_T3",
  "TCOut_Thanks_T3",
  "TCOut_Complaints_T3",
  "CTOverallGratRatio_T3" ,
  "TC_OverallGratRatio_T3",
  "CT_Sch_GratRatio_T3",
  "TCSch_GratRatio_T3",
  "CTOut_GratRatio_T3",
  "TC_Out_GratRatio_T3"
) %>% 
  mutate(nas = rowSums(is.na(.))) %>% 
  arrange(desc(nas)) %>% 
  filter(nas != 14) %>% 
  rowwise %>% 
  mutate(sc = sum(PublicMS1,PublicMS2,PublicHS)) %>% 
  filter(sc != 0)

# Schools to keep
clean.l = clean.l %>% filter(school == "p1" | school == "p2")

# Fix data errors
clean.l = clean.l %>% mutate(age = ifelse(age > 100, NA,age))

clean.l %>% count(coregpa<coregpa_cut) %>% mutate(n = (n/nrow(clean.l)*100) %>% numformat())
clean.l = clean.l %>% mutate(coregpa = ifelse(coregpa < coregpa_cut, NA,coregpa))
# clean.l = clean.l %>% mutate(purpose_s = ifelse(purpose_s == 1, NA,purpose_s))
# clean.l = clean.l %>% mutate(purpose_s_nofam = ifelse(purpose_s_nofam == 1, NA,purpose_s_nofam))
# 
# clean.l[clean.l$StudyID == 2395 & clean.l$time == "T3",]$purpose_s = 1
# clean.l[clean.l$StudyID == 2395 & clean.l$time == "T3",]$purpose_s_nofam = 1
# clean.l[clean.l$StudyID == 2317 & clean.l$time == "T4",]$purpose_s = 1
# clean.l[clean.l$StudyID == 2317 & clean.l$time == "T4",]$purpose_s_nofam = 1

# Lump together infrequent ethnicities
clean.l = clean.l %>% mutate(eth = fct_lump_n(eth,4))

clean = clean.l %>% pivot_wider(names_from = time,
                        values_from = c(age:ncol(.)))


write_rds(clean, "s1/data/clean.rds")
write_rds(clean.l, "s1/data/clean.l.rds")
}

# Data checks
# Histosgoorams
clean.l %>% select_if(is.numeric) %>% select(-c(pn_bf1:pn_aot2,StudyID)) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~name,scales = "free")
# ggsave2(filename = "s1/data_checks/histograms",7,7)

clean.l %>% select_if(is.numeric) %>% select(-c(pn_bf1:pn_aot2,StudyID)) %>% 
  corrr::correlate() %>% 
  corrr::stretch() %>% 
  filter(!is.na(r)) %>% 
  ggplot(aes(x,y,fill = r))+
  geom_tile()+
  geom_text(aes(label = abs(r) %>% Ben::numformat(2) %>% str_remove("\\.")))+
  scale_fill_gradient2()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
# ggsave2(filename = "s1/data_checks/corplot",6,5)

clean.l %>% select_if(is.character) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot(aes(value))+
  geom_bar()+
  coord_flip()+
  facet_grid(rows = vars(name), scales = "free_y", switch = "y", space = "free_y") +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    strip.text.y = element_text(angle = 270, face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
  )
# ggsave2(filename = "s1/data_checks/factor variables",6,4)

#Intraperosnal change
clean.l %>% 
  select(StudyID,time,outcomes) %>% 
  pivot_longer(outcomes) %>% 
  rename_outs(name,single_line = F) %>% 
  ggplot(aes(time,value,group = StudyID,col = name))+
  geom_line(alpha = .025,show.legend = F)+
  geom_line(data = clean.l %>% 
              select(StudyID,time,outcomes) %>% 
              pivot_longer(outcomes) %>% 
              rename_outs(name,single_line = F) %>% 
              group_by(time,name) %>% 
              summarise(value = mean(value,na.rm=T)), aes(group=name) ,show.legend = F,size = 2)+
  facet_wrap(~name,scales = "free",nrow = 1)+
  scale_color_manual(values = c("#c1292e","#d47b1c",'#f1d302','#4fb029','#235789','#8a6a87'))
# ggsave2(filename = "s1/data_checks/Intrapersonal Change",8,4)

 # Missing data
clean.l %>% 
  select(StudyID,time,outcomes) %>% 
  pivot_longer(outcomes) %>% 
  mutate(na = is.na(value)) %>% 
  group_by(name,time) %>% 
  summarise(Missing = sum(na)) %>% 
  rename_outs(name,single_line = F) %>% 
  ggplot(aes(time,Missing,col = name,group = name))+
  geom_line()+
  scale_color_manual(values = c("#c1292e","#d47b1c",'#f1d302','#4fb029','#235789','#8a6a87'))
# ggsave2(filename = "s1/data_checks/Missing Data",6,4)
 
        
