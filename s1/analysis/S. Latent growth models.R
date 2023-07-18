# Latent growth curves
library(lavaan)
data = read_rds("Data/Peer Quality Datasets/analysis_data_nominee_M.rds")

data = data %>% 
  select(StudyID,school, female, eth, ell, sped, frpl, time, scw_s, coregpa, pn_scw, pn_bf) %>% 
  pivot_wider(names_from = time, values_from = scw_s:pn_bf)

m1 <-   '
i_y =~ 1*scw_s_T1 + 1*scw_s_T2 + 1*scw_s_T3 + 1*scw_s_T4
s_y =~ 0*scw_s_T1 + 1*scw_s_T2 + 2*scw_s_T3 + 3*scw_s_T4

i_x1 =~ 1*pn_scw_T1 + 1*pn_scw_T2 + 1*pn_scw_T3 + 1*pn_scw_T4
s_x1 =~ 0*pn_scw_T1 + 1*pn_scw_T2 + 2*pn_scw_T3 + 3*pn_scw_T4

i_x2 =~ 1*pn_bf_T1 + 1*pn_bf_T2 + 1*pn_bf_T3 + 1*pn_bf_T4
s_x2 =~ 0*pn_bf_T1 + 1*pn_bf_T2 + 2*pn_bf_T3 + 3*pn_bf_T4


scw_s_T1 ~ coregpa_T1 +school+ female + eth + ell + sped + frpl
scw_s_T2 ~ coregpa_T2+school+ female + eth + ell + sped + frpl
scw_s_T3 ~ coregpa_T3+school+ female + eth + ell + sped + frpl
scw_s_T4 ~ coregpa_T4+school+ female + eth + ell + sped + frpl
'

# s_y ~ i_x1 + i_x2

fit_m1 <- growth(m1, data=data, missing = "fiml")

summary(fit_m1, fit.measures = T)

parameterestimates(fit_m1, standardized =T) %>% filter(op == "~~") %>% 
  filter(lhs %in% c("s_x1", 's_x2','s_y',"i_x1", 'i_x2','i_y') | rhs %in% c("s_x1", 's_x2','s_y',"i_x1", 'i_x2','i_y')) %>% 
  filter(lhs != rhs) %>% 
  mutate_at(vars(rhs,lhs), ~case_when(. == "s_x1" ~ "slope\nrole model",
                                      . == "s_x2" ~ "slope\nfriend",
                                      . == "i_x1" ~ "intercept\nrole model",
                                      . == "i_x2" ~ "intercept\nfriend",
                                      . == "i_y" ~ "slope\nself-control",
                                      . == "s_y" ~ "intercept\nself-control")) %>% 
  mutate(lhs = fct_inorder(lhs), rhs = fct_inorder(rhs)) %>% 
  ggplot(aes(lhs, rhs, fill = std.all))+
  geom_tile()+
  geom_text(aes(label = glue::glue("{Ben::numformat(std.all)}{Ben::codeps(pvalue)}")), color = "black")+
  scale_fill_gradient2()+
  labs(x = NULL, y = NULL)
# scale_fill_viridis_c(option = "B",begin = 0, end=  .8)

semPlot::semPaths(fit_m1)
