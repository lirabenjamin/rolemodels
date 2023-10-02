# nolint start
#
#
#
#
#
#
#
#
#
#| echo: false
rerun = F
if(rerun){source("p3_data_checklist.R")}
load("output/output.rda")
library(gt)

#
#
#
#
#
#
#
#
#
#| fig-width: 10
#| fig-height: 5
knitr::include_graphics("figures/mean_sc_across_conditions.png")
#
#
#
#
#
#
#
#| fig-width: 15
knitr::include_graphics("output/table1.png", dpi = 100)
#
#
#
#
#
#
#| fig-width: 10
#| fig-height: 5
knitr::include_graphics("figures/standards.png")
#
#
#
#
#
#| output: asis
data.z = data %>% group_by(condition) %>% mutate_if(is.numeric,scale) %>% ungroup()

all = data.z %>% lm(sc_self ~ standard10 + self_minutes, data = .)

m1 = lm(sc_self ~ standard10 + self_minutes, data = data.z %>% filter(condition == "bedtime"))
m2 = lm(sc_self ~ standard10 + self_minutes, data = data.z %>% filter(condition == "healthy eating"))
m3 = lm(sc_self ~ standard10 + self_minutes, data = data.z %>% filter(condition == "saving"))
m4 = lm(sc_self ~ standard10 + self_minutes, data = data.z %>% filter(condition == "work"))
stargazer::stargazer(type = "html", column.labels = c("All","Bedtime","Healthy Eating","Saving","Work"), all, m1,m2,m3,m4)
#
#
#
#
#
#
knitr::include_graphics("figures/paths.png")
#
#
#
#
#
multigroup_output
#
#
#
#
#
knitr::include_graphics("figures/plot_diffs.png")
#
#
#
#
#| warning: false
data %>% 
  group_by(condition) %>% 
  summarise(
    mean_cl_normal(importance_self)
  ) %>% 
  ggplot(aes(x = condition, y = y)) +
  geom_col()+
  geom_jitter(data = data, aes(y = importance_self), alpha = .2, width = .1)+
  geom_errorbar(aes(ymin = ymin, ymax = ymax),width = .2)+
  labs(x = NULL, y = "Reported Importance (0-10)")
#
#
#
#
#
#| output: asis
datai = data |> group_by(condition) |> mutate_if(is.numeric,scale) %>% ungroup()
m1 = lm(sc_self ~ importance_self*exemplar_minutes + self_minutes, datai) 
m2 = lm(standard10 ~ importance_self*exemplar_minutes, datai)
m3 = lm(exemplar_minutes ~ importance_self, datai)
m4 = lm(friend_minutes ~ importance_self, datai)
stargazer::stargazer(m1,m2,m3,m4,type = "html")
#
#
#
#
make_table = function(test_condition){
  datai = data |> filter(condition == test_condition) |> mutate_if(is.numeric,scale)
  m1 = lm(sc_self ~ importance_self*exemplar_minutes + self_minutes, datai) 
  m2 = lm(standard10 ~ importance_self*exemplar_minutes, datai)
  m3 = lm(exemplar_minutes ~ importance_self, datai)
  m4 = lm(friend_minutes ~ importance_self, datai)
  stargazer::stargazer(m1,m2,m3,m4,type = "html")
}
#
#
#
#
#
#| output: asis
make_table("bedtime")
#
#
#
#
#
#
#| output: asis
make_table("healthy eating")
#
#
#
#
#
#| output: asis
make_table("saving")
#
#
#
#
#
#| output: asis
make_table("work")
#
#
#
#
#
#
#
#
table_describe
#
#
#
#
#
table_correlations |> gt_theme()

table_correlations_drinking |> gt_theme() |> tab_header("Work")
table_correlations_exercise |> gt_theme() |> tab_header("Healthy Eating")
table_correlations_social_media |> gt_theme()|> tab_header("Saving")
table_correlations_punctuality |> gt_theme() |> tab_header("Bedtime")
#
#
#
#
#
#| fig-width: 10
knitr::include_graphics("figures/standards.png")
#
#
#
#
#
#| fig-width: 10
# knitr::include_graphics("regressions_self_control.png",dpi = 1000)
#
#
#
#
#
#| fig-width: 20
#| fig-height: 15
plot_histograms
plot_bars
#
#
#
#
#
# plot_sc_friends_exemplars_sqrt
#
#
#
#| fig-width: 10
# plot_main_sqrt
#
#
#
#| fig-width: 6
#| fig-height: 3
# plot_main_bendy
#
#
#
#
#
plot_sc_friends_exemplars
#
#
#
#| fig-width: 10
# plot_main
#
#
#
#
