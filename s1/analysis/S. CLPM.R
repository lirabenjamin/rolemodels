library(lavaan)
clean = read_rds("Data/Peer Quality Datasets/analysis_data.rds")


test = clean %>% 
  select(StudyID,time,ogpa = coregpa, p = scw_s, pgpa = pn_scw) %>% 
  pivot_wider(names_from = time, values_from = 3:5) %>% 
  select(-1)

# Add equality constraint
model = "
p_T2 ~ p_T1 + pgpa_T1 + ogpa_T1
pgpa_T2 ~ p_T1 + pgpa_T1+ ogpa_T1

p_T3 ~ p_T2 + pgpa_T2+ ogpa_T2
pgpa_T3 ~ p_T2 + pgpa_T2+ ogpa_T2

p_T4 ~ p_T3 + pgpa_T3+ ogpa_T3
pgpa_T4 ~ p_T3 + pgpa_T3+ ogpa_T3"

model = lavaan::sem(model,test,missing = "fiml")
summary(model,standardized = T, fit.measures = T)

parameterestimates(model) %>% 
  as_tibble() %>% 
  separate(lhs, c("lhs", "timel"),"_") %>% 
  separate(rhs, c("rhs", "timer"),"_") %>% 
  filter(lhs != rhs, op == "~") %>% 
  arrange(est) %>% 
  filter(pvalue < .05)

semPlot::semPaths(model,whatLabels = "std",style = "lisrel")




# Yearly ####
test = clean %>% 
  select(StudyID,time,ogpa = coregpa, p = scw_s, pgpa = pn_scw) %>% 
  mutate(year = ifelse(time == "T1" | time == "T2", "T1","T2")) %>% 
  group_by(StudyID,year) %>% 
  summarise_if(is.numeric,mean,na.rm=T) %>% 
  pivot_wider(names_from = year, values_from = 3:5) %>% 
  ungroup() %>% 
  select(-1)

model = "
p_T2 ~ p_T1 + pgpa_T1 + ogpa_T1
pgpa_T2 ~ p_T1 + pgpa_T1+ ogpa_T1"

model = lavaan::sem(model,test,missing = "fiml")
summary(model,standardized = T, fit.measures = T)

parameterestimates(model) %>% 
  as_tibble() %>% 
  separate(lhs, c("lhs", "timel"),"_") %>% 
  separate(rhs, c("rhs", "timer"),"_") %>% 
  filter(lhs != rhs, op == "~") %>% 
  arrange(est) %>% 
  filter(pvalue < .05)

semPlot::semPaths(model,whatLabels = "std",style = "lisrel")


devtools::install_github("alishinski/lavaanPlot")
lavaanPlot::lavaanPlot(model = model, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = T)
