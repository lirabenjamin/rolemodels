# Network descriptives
library(ggnet)
library(network)
library(tidyverse)
clean.l = read_rds("Data/clean.l.rds")

nominations = clean.l %>% 
  select(StudyID,time, school,matches("pn")) %>% 
  rename(pn_scw1 = pn_scw,
         pn_grit1 = pn_grit, 
         pn_sci1 = pn_sci,
         pn_grat1 = pn_grat,
         pn_aot1 = pn_aot
  ) %>% 
  pivot_longer(pn_bf1:pn_aot2) %>% 
  mutate(n = str_sub(name,-1),
         name  = str_sub(name,1,-2)) %>% 
  filter(!is.na(value))

# Plot
plot.network = function(target_school, target_time, color = F){
  net.el = nominations %>% 
    rename(from = StudyID,to = value,type = name) %>% 
    filter(type == "pn_bf" | type == "pn_scw") %>% 
    filter(time %in% target_time) %>% 
    filter(school  %in%  target_school) %>% 
    select(from,to,type) %>% 
    filter(from != to) %>% 
    group_by(from,to) %>% 
    slice_sample(n = 1) %>% 
    ungroup() 
  net.p = net.el %>% 
    network(x = .,loops =T,multiple = F)
  set.edge.attribute(net.p, "lty", ifelse(net.el$type == "pn_bf", 1,2))
  net.p %>% 
    GGally::ggnet2(size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open",edge.lty = "lty")
  if(color){
    set.edge.attribute(net.p, "col", ifelse(net.el$type == "pn_bf", yellow,blue))
    net.p %>% 
      GGally::ggnet2(size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open",edge.color = "col")
  }
}
set.seed(123)
plot.network("p2","T1", color = T)
ggsave("Output/Main Manuscript/Network.pdf",width =5, height = 5)
ggsave("Output/Main Manuscript/Network_ppt.pdf",width =6, height = 4)

net.el = nominations %>% 
  rename(from = StudyID,to = value,type = name) %>% 
  filter(type == "pn_bf" | type == "pn_scw") %>% 
  filter(time %in% "T1") %>% 
  filter(school  %in%  "p2") %>% 
  select(from,to,type) %>% 
  filter(from != to) %>% 
  group_by(from,to) %>% 
  mutate(n = n()) %>% 
  mutate(type = ifelse( n ==2 ,"both",type)) %>% 
  slice_sample(n = 1) %>% 
  ungroup() 
net.p = net.el %>% 
  network(x = .,loops =T,multiple = F)
  set.edge.attribute(net.p, "col", case_when(net.el$type == "pn_bf" ~ blue,
                                           net.el$type == "both" ~ "red",
                                           net.el$type == "pn_scw" ~ yellow))
net.p %>% 
  ggnet::ggnet2(size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open",edge.color = "col")
ggsave("Output/Main Manuscript/Network_ppt.pdf",width =6, height = 4)

net.el %>% 
  mutate(type = factor(type,levels = c("pn_scw","pn_bf","both"),labels = c("Academic Self-Control Role Model", "Friend","Both"))) %>% 
  ggplot(aes(x = from, y = to,col = type))+
  geom_line()+
  scale_color_manual(values = c(yellow,blue,"red"))
ggsave("Output/Main Manuscript/legendNetwork_ppt.pdf",width =6, height = 4)

plot.network = function(target_school, target_time,peer_type){
  net.el = nominations %>% 
    rename(from = StudyID,to = value,type = name) %>% 
    filter(type == peer_type) %>% 
    filter(time %in% target_time) %>% 
    filter(school  %in%  target_school) %>% 
    select(from,to,type) %>% 
    filter(from != to) %>% 
    group_by(from,to) %>% 
    slice_sample(n = 1) %>% 
    ungroup() 
  net.p = net.el %>% 
    network(x = .,loops =T,multiple = F)
  net.p %>% 
    ggnet::ggnet2(size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open")
}

plot.network("p2","T1","pn_bf")

set.seed(123)
schools = c("p1","p2")
times = c("T1","T2","T3","T4")
peer_types = unique(nominations$name)[c(1,2,3,4,6)]

for (s in schools){
  for (t in times){
    for (pt in peer_types){
      plot.network(s,t,pt)
      ggsave(glue::glue("Output/Network Plots/{s}{t}{pt}_2.pdf"),width = 5,height = 5)
    }
  }
}

expand_grid(schools, times, peer_types) %>% 
  filter(!(times == "T1" & peer_types == "pn_sci")) %>% 
  mutate(plot = pmap(list(schools,times,peer_types),plot.network),
         save = pmap(list(plot,schools,times,peer_types), function(p,s,t,pt){ggsave(plot = p, glue::glue("Output/Network Plots/{s}{t}{pt}_2.pdf"),width = 5,height = 5)}
         ))

plot.network("p2","T1",peer_type = "pn_bf")+ggtitle("sdf")+theme(title = element_text(size = 10,hjust = 2))
ggsave("Output/Main Manuscript/Network.pdf",width =5, height = 5)



# Number of nominations
nominations %>% 
  mutate(name = fct_inorder(name)) %>% 
  select(-n,-StudyID) %>% 
  group_by(time,name) %>%
  summarise(n = n(),prop = n/1100) %>% 
  select(time,prop,name) %>% 
  spread(name,prop) %>% 
  janitor::adorn_totals() %>% 
  as_tibble() %>% 
  mutate_at(2:7, numformat) %>% 
  t() %>% as_tibble(rownames = "Variable") %>% 
  gt::gt() %>% 
  gt::gtsave("Output/Supplement/Number of Nominations.tex")

# Number of reciprocal ties
calculate_reciprocal_ties = function(target, variable , target_time,overall = F){
  nominations_test = nominations
  if(overall){nominations_test = nominations_test %>% mutate(time = "overall")}
  if(overall & target_time != "overall"){print("target time set to overall")}
  if(overall){target_time = "overall"}
  peers_of_target = nominations_test %>% filter(name == variable,StudyID == target, time == target_time) %>% pull(value)
  number_of_matches = nominations_test %>% filter(name == variable,StudyID %in% peers_of_target, time == target_time) %>% mutate(matches = value == target) %>% pull(matches) %>% sum()
  return(number_of_matches)
}

if(rerun){
  reciprocal_ties = nominations %>% select(StudyID,time,name) %>% unique() %>% 
    rowwise() %>% 
    mutate(number_of_matches = calculate_reciprocal_ties(StudyID,name,time))
  reciprocal_ties_overall = nominations %>% select(StudyID,name) %>% unique() %>% 
    rowwise() %>% 
    mutate(number_of_matches = calculate_reciprocal_ties(StudyID,name, "overall",overall= T))
  all_ties = bind_rows(reciprocal_ties,reciprocal_ties_overall %>% mutate(time = "overall")) %>% arrange(name,StudyID)
  write_rds(all_ties,"Data/Network Analysis/all_ties.rds")
}

all_ties= read_rds("Data/Network Analysis/all_ties.rds")

nominations %>% 
  mutate(name = fct_inorder(name)) %>% 
  group_by(StudyID,time,name) %>% 
  count() %>% 
  pivot_wider(names_from = time,values_from = n) %>% 
  mutate(overall = sum(c(T1,T2,T3,T4),na.rm =T))  %>% 
  pivot_longer(T1:overall,names_to = "time",values_to = "n") %>% 
  left_join(all_ties) %>% 
  mutate(name = fct_inorder(name)) %>% 
  mutate(prop = number_of_matches/n) %>% 
  group_by(time,name) %>% 
  summarise(mean = mean(prop,na.rm=T)) %>% 
  pivot_wider(names_from = time,values_from = mean) %>% 
  # filter(time == "overall") %>% 
  ungroup() %>% 
  slice(2,6,4,3,5) %>% 
  mutate_at(2:6, numformat) %>% 
  gt::gt() %>% 
  gt::gtsave("Output/Supplement/Reciprocal Ties.tex")

# Self nominations
nominations %>% 
  mutate(self = StudyID == value) %>% 
  group_by(time,name) %>% 
  summarise(n = n(),self = sum(self)) %>% 
  mutate(self_prop = self/n) %>% 
  select(time,name,self_prop) %>% 
  pivot_wider(names_from = name,values_from = self_prop) %>% 
  select(1,3,6,5,4,7,2) %>% 
  mutate_at(2:7, numformat) %>% 
  gt::gt() %>% 
  gt::gtsave("Output/Supplement/Self nominations.tex")

# Repeated nominations
nominations %>% 
  select(-n) %>% 
  group_by(StudyID,time,name) %>% 
  unique() %>% 
  group_by(time,name) %>% 
  count() %>% 
  rename(unique_n = n) %>% 
  left_join(
    nominations %>% 
      select(-n) %>% 
      group_by(StudyID,time,name) %>% 
      group_by(time,name) %>% 
      count()
  ) %>% 
  mutate(prop = 1-unique_n/n) %>% 
  arrange(-prop)


# Number of overlapping nominations

find_overlapping_nominations = function(target , target_time,overall = F){
  nominations_test = nominations
  if(overall){nominations_test = nominations_test %>% mutate(time = "overall")}
  if(overall & target_time != "overall"){print("target time set to overall")}
  if(overall){target_time = "overall"}
  nominations_test = nominations_test %>% filter(StudyID == target, time == target_time) %>% 
    select(-n) %>% 
    group_by(name) %>% 
    nest(data = value) %>% 
    mutate(data = map(data,unlist))
  overlap = nominations_test %>% 
    select(name,data) %>% 
    expand_grid(nominations_test %>% select(name,data) %>% rename(name2 = name,data2 = data)) %>% 
    filter(name != name2) %>% 
    # filter(paste(name,name2) == paste(name2,name)) %>% 
    mutate(overlap = map2(data,data2,intersect)) %>% 
    select(name1=name,name2,overlap)
  return(overlap)
}


if(rerun){
  overlapping_ties = nominations %>% select(StudyID,time) %>% unique() %>% 
    # slice(1:100) %>% 
    rowwise() %>% 
    mutate(overlapping_ties = list(find_overlapping_nominations(StudyID,time)))
  overlapping_ties %>% unnest(overlapping_ties) %>% unnest(overlap)
  overlapping_ties_overall = nominations %>% select(StudyID) %>% unique() %>% 
    # slice(1:100) %>% 
    rowwise() %>% 
    mutate(overlapping_ties = list(find_overlapping_nominations(StudyID, "overall",overall= T)))
  all_overlapping_ties = bind_rows(overlapping_ties,overlapping_ties_overall %>% mutate(time = "overall")) %>% arrange(StudyID)
  all_overlapping_ties %>% unnest(overlapping_ties) %>% unnest(overlap)
  write_rds(all_overlapping_ties,"Data/Network Analysis/all_overlapping_ties.rds")
}
all_overlapping_ties = read_rds("Data/Network Analysis/all_overlapping_ties.rds")

all_overlapping_ties %>% 
  unnest(overlapping_ties) %>% 
  unnest(overlap) %>% 
  group_by(time) %>% 
  count(overlap) %>% 
  filter(time == "T1")

all_overlapping_ties_count = all_overlapping_ties %>% 
  unnest(overlapping_ties) %>% unnest(overlap) %>% 
  rename(name = name1) %>% 
  group_by(StudyID,time,name,name2) %>% 
  count(name = "overlap_n") %>% 
  ungroup()



overlapping_nominations = nominations %>% 
  select(-n) %>% 
  group_by(time,name,StudyID) %>% 
  count() %>% 
  spread(time,n) %>% 
  # NAs mean that the person nominated 0 peers
  mutate_at(vars(T1:T4),function(x){ifelse(is.na(x),0,x)}) %>% 
  janitor::adorn_totals(name= "overall",where = "col") %>% 
  as_tibble() %>% 
  # Subtract StudyID from totals
  mutate(overall = overall - StudyID) %>% 
  pivot_longer(T1:overall,values_to = "n", names_to = "time") %>% 
  # Remove all comparisons involving sci at time 1
  filter(!(time == "T1" & name == "pn_sci")) %>% 
  expand_grid(name2 = c(unique(nominations$name))) %>% 
  filter(name != name2) %>% 
  # Remove all comparisons involving sci at time 1
  filter(!(time == "T1" & name2 == "pn_sci")) %>% 
  left_join(all_overlapping_ties_count) %>% 
  mutate(overlap_n = ifelse(is.na(overlap_n),0,overlap_n)) %>% 
  mutate(overlap_p = overlap_n/n)


all_overlapping_ties_count %>% 
  filter(time != "overall" ) %>% 
  filter(name != "pn_bf" & name2 != "pn_bf")

overlapping_nominations %>% 
  select(-overlap_n,-n) %>% 
  group_by(name,name2,time) %>% 
  summarise(mean_overlap = mean(overlap_p,na.rm=T)) %>% 
  mutate(time = ifelse(time == 'overall',"Overall",time)) %>% 
  mutate_at(1:2, function(x){factor(x,levels = c("pn_bf","pn_scw","pn_grit","pn_grat","pn_sci","pn_aot"),
                                    labels = c("Friends","Academic Self-Control","Grit","Gratitude","Interpersonal Self-Control","Actively Open-Minded Thinking"))}) %>% 
  ggplot(aes(name,name2,fill=mean_overlap))+
  geom_tile(show.legend = F)+
  facet_wrap(~time,ncol = 2)+
  geom_text(aes(label  = Ben::numformat(mean_overlap)),size= 3)+
  scale_fill_viridis_c()+
  Ben::theme_ang()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = NULL, y = NULL)
ggsave2("Supplement/Overlapping Nominations",6,7)

 # Character traaits of most and leaast popular kids
nas_to_0 = function(x){ifelse(is.na(x),0,x)}
popularity = nominations %>% 
  mutate(name = ordered(fct_inorder(name))) %>% 
  group_by(time,name) %>% 
  count(value) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(name),values_from = n) %>% 
  mutate_at(3:8,nas_to_0)

popularity = popularity %>% rename(StudyID = value) %>% rename_at(3:8, str_replace,"pn","pop")


clean.l %>% 
  left_join(popularity) %>% 
  select(matches("pop_"),outcomes, matches("_tr")) %>% 
  select(-pop_aot,-avg_aot_tr) %>% 
  mutate_at(vars(matches("pop_")),nas_to_0) %>% 
  Ben::HARcor() %>% 
  select(1:6) %>% 
  rename(Variable = var) %>% 
  gt::gt() %>% 
  gt::gtsave("Output/Supplement/Popularity Cors.tex")
  
