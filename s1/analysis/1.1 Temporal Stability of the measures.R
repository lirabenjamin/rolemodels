# 1.1 Temporal Stability
clean.l = read_rds("Data/clean.l.rds")
temporal_stability = function(var){
  clean.l %>% 
    select(time,StudyID,{{var}}) %>% 
    spread(time,{{var}}) %>% 
    select(-StudyID) %>% 
    as.matrix %>% 
    psych::ICC() %>% 
    `$`(results) %>% as_tibble()
}

c("grit_s","scw_s","grat_s","sci_s","aot_s") %>% map(temporal_stability) %>% 
  enframe() %>% mutate(name = c("grit_s","scw_s","grat_s","sci_s","aot_s")) %>% 
  unnest(value) %>% 
  # We are looking at the average rating of the population of judges
  filter(type == "ICC3k") %>% 
  rename_outs(name) %>% 
  mutate(outs = fct_inorder(name)) %>% 
  ggplot(aes(outs,ICC))+
  geom_col()+
  geom_text(aes(label = Ben::numformat(ICC)),y = .1, color = "white")+
  labs(x = "Outcome")

ggsave2("Data Checks/Temporal Stability")  

clean.l |> colnames()
