clean.l = read_rds("s1/data/clean.l.rds")
dat = clean.l %>% 
  filter(time == "T3",school == "p2") %>% 
  select(1,pn_bf1,pn_bf2) %>% 
  filter(!(is.na(pn_bf1)&is.na(pn_bf2))) %>% 
  pivot_longer(2:3) %>% 
  select(1,3) %>% 
  rename("from" = StudyID,"to"=value)
  
networkD3::simpleNetwork(Data = dat %>% filter(from != to),
                         height="1000px",
                         width = "1000px",
                         Source = "from",
                         Target = "to",
                         charge = -1,linkDistance = 5,zoom = T)

clean.l %>% filter(!(is.na(pn_bf1)&is.na(pn_bf2))) %>% 
  count(time,school ) %>% spread(school, n)

# devtools::install_github("briatte/ggnet")
# devtools::install_github("lirabenjamin/Ben")
# install.packages("networkD3")
install.packages("GGally")

library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(igraph)
library(networkD3)

for(t in c("T1","T2", "T3","T4")){
  for (s in unique(clean.l$school)){
    dat = clean.l %>% 
      filter(time == t,school == s) %>% 
      select(1,pn_bf1,pn_bf2) %>% 
      filter(!(is.na(pn_bf1)&is.na(pn_bf2))) %>% 
      pivot_longer(2:3) %>% 
      select(1,3) %>% 
      rename("from" = StudyID,"to"=value)
    # dat %>% fastDummies::dummy_cols(select_columns = "to",remove_selected_columns = T) %>% group_by(from) %>% summarise_all(sum) %>% select(-from)
    net = network(dat %>% 
                    filter(is.na(dat)%>%rowSums == 0) %>%
                    filter(from != to) %>% 
                    unique(),
                  multiple = F,
                  directed = T)
    GGally::ggnet2(net,size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open")
    ggsave(filename = glue::glue("s1/figures/Network Plots/{t}-{s}.png"))
    ggsave(filename = glue::glue("s1/figures/Network Plots/{t}-{s}.png"))
  }
  }

clean.l %>% 
  mutate(test = case_when((time) == "T1" ~ 1)) %>% 
  select(time,test)

filter_net = function(data,x){
  ids = data %>% slice(x) %>% unlist()
  data %>% filter(to %in% ids| to %in% ids ) %>% return
  
}

dat %>%
  filter(is.na(dat) %>% rowSums == 0) %>%
  filter(to != from) %>% 
  unique()  %>%
  # filter_net(30) %>% 
  # filter(to == 2063 | from == 2063| to == 2364|from == 2364) %>% 
  network(directed = T,loops = T) %>% 
  GGally::ggnet2(arrow.size = 10,arrow.gap = .000025,size = 05)
  