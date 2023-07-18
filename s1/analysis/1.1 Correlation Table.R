library(tidyverse)
# Correlatiaon table
clean.l = read_rds("Data/Peer Quality Datasets/rawdata.rds")

clean.l |> group_by(StudyID) |> 
  summarise_all(mean, na.rm = T) |> 
  select(avg_scw_tr, scw_s) %>% 
  Ben::harcor()

t.cor = clean.l %>% 
  select(school,    female ,eth,       ell  ,sped,  frpl,    age,StudyID,time,coregpa,scw_s,avg_scw_tr, pn_scw_nominee_gpa,pn_scw_nominee_tr_scw, pn_bf_nominee_gpa, pn_bf_nominee_tr_scw ) %>% 
  group_by(StudyID,school,eth) %>% 
  summarise_all(mean,na.rm= T) %>% 
  ungroup() %>% 
  select(-time)

t.cor.p = t.cor %>% 
  pivot_longer(coregpa:pn_bf_nominee_tr_scw) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(lm = map(data,function(x){lm(value ~ school+female+eth+ell+sped+frpl+age,data = x)}),
         aug = map(lm,broom::augment)) %>% 
  unnest(aug) %>% 
  select(.rownames,name,.resid) %>% 
  pivot_wider(names_from = name,values_from = .resid) %>% 
  select(-.rownames) %>% 
  Ben::HARcor()

t.cor.nop = t.cor %>%
  select(coregpa:ncol(t.cor)) %>% 
  Ben::HARcor() %>% 
  rename(Variable = var)

p = ncol(t.cor.nop)-1

t.cor.nop[1:p,2:(p+1)][upper.tri(t.cor.nop[1:p,2:(p+1)])] = (t.cor.p[(1:p),(2:(p+1))] %>% t())[upper.tri(t.cor.p[1:p,2:(p+1)] %>% t())]

t.cor = t.cor.nop %>% 
mutate(cat = c(rep("Self Composites",3),rep("Role Model Composites",2),rep("Close Friend Composites",2)," "," "," ")) %>% 
  group_by(cat) %>% 
  gt::gt()
t.cor
gt::gtsave(t.cor,filename = "Output/Main Manuscript/Descriptives.tex")
gt::gtsave(t.cor,filename = "Output/Main Manuscript/Descriptives.html")

long_cortest = function(data,type = "pearson",remove_first_dummy = T){
  # If there are factor columns, make them dummies
  if(length(map(data, class)[map(data, class) %in% c("factor", "character")]) > 0){
    data = data %>% fastDummies::dummy_cols(remove_selected_columns = T,remove_first_dummy = remove_first_dummy)
  }
  cors = data %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = type)
  cors %>% 
    broom::tidy() %>% 
    return()
}


cordata = clean.l %>% 
  select(StudyID,time,Female = female,age,coregpa,ends_with("_s"),starts_with("avg")) %>% 
  group_by(StudyID,Female) %>% 
  summarise_all(mean,na.rm= T) %>% 
  ungroup() %>% 
  select(-time,-StudyID)

cordata %>% 
  select(outcomes) %>% 
  long_cortest() %>% 
  filter(estimate == max(estimate)|estimate == min(estimate))

cordata %>% 
  long_cortest() %>% 
  filter(column1 == 'coregpa' |column2 == 'coregpa') %>% 
  filter(str_detect(column1,"avg")|str_detect(column2,"avg")) %>% 
  filter(estimate == max(estimate)|estimate == min(estimate))

cordata %>% 
  long_cortest() %>% 
  filter(column1 == 'coregpa' |column2 == 'coregpa') %>% 
  filter(str_detect(column1,"_s")|str_detect(column2,"_s")) %>% 
  filter(estimate == max(estimate)|estimate == min(estimate))
