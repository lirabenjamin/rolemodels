library(gganimate)
library(broom)
big_models = read_rds("Output/Raw/big_models.rds")

big_models.p = big_models %>% 
  rename_outs(outcomes,F) %>% 
  mutate(peer_type = case_when(peer_type == "friends" ~ "Friends",
                               peer_type == "models" ~ "Role Models",
                               peer_type == "both" ~ "Both"),
         model = case_when(model == 1 ~ "Within.Demo",
                           model == 2 ~ "Between.NoDemo",
                           model == 3 ~ "Between.Demo",
                           model == 4 ~ "Average.NoDemo",
                           model == 5 ~ "Average.Demo")) %>% 
  mutate(tidy = map(lm, tidy,conf.int = T)) %>% 
  unnest(tidy) %>% 
  arrange(peer_type) %>% 
  filter(term %in%  c("pn_bf","rolemodel","coregpa")) %>% 
  # filter(model %in% c(5)) %>% 
  select(time:model, term, estimate, conf.low,conf.high,name) %>% 
  mutate(name = fct_inorder(name),
         model2 = model,
         term = case_when(term == "rolemodel" ~ "Role Model GPA",
                          term == "pn_bf" ~ "Friend GPA",
                          term == "coregpa" ~ "Own GPA"),
         term = fct_inorder(term)) %>% 
  separate(model2, c("Type","Controls"),"\\.")


yellow= "#ffbf01"
blue = "#4472c3"


p = big_models.p %>% 
  filter(outcomes == "scw_s") %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Both") %>% 
  mutate(frame = 0, estimate = 0, conf.low = 0, conf.high = 0) %>% 
  bind_rows(big_models.p %>% 
              filter(outcomes == "scw_s") %>% 
              filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Both") %>% mutate(frame = 1)) %>% 
  ggplot(aes(term,estimate,fill == term))+
  geom_col(aes(fill = term),color = NA)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_fill_manual(values = c(yellow,blue,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,y = NULL, title = NULL,fill = NULL)+
  theme_ang()+
  transition_states(frame,wrap = F,state_length = 0) +
  coord_cartesian(ylim = c(-.25,.5))+
  theme(legend.position = "none")

myrenderer <- gifski_renderer(loop=FALSE)
mygif <- animate(p, width=5, height=3, units = "in", res = 300, renderer=myrenderer,fps = 50)  
anim_save(filename="Reports/animation0.gif", mygif)

p = big_models.p %>% 
  filter(outcomes == "scw_s") %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Both") %>% 
  mutate(frame = 0) %>% 
  bind_rows(big_models.p %>% 
              filter(outcomes == "scw_s") %>% 
              filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Both") %>% mutate(frame = 1)) %>% 
  ggplot(aes(term,estimate,fill == term))+
  geom_col(aes(fill = term),color = NA)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_fill_manual(values = c(yellow,blue,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,y = NULL, title = NULL,fill = NULL)+
  theme_ang()+
  transition_states(frame,wrap = F) +
  coord_cartesian(ylim = c(-.25,.5))+
  theme(legend.position = "none")

myrenderer <- gifski_renderer(loop=FALSE)
mygif <- animate(p, width=5, height=3, units = "in", res = 300, renderer=myrenderer)  
anim_save(filename="Reports/animation0.1.gif", mygif)


p = big_models.p %>% 
  filter(time == 0, Type != "Between", Controls== "Demo", peer_type == "Both") %>% 
  ggplot(aes(term,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  transition_states(Type,wrap = F,state_length = 0) +
  labs(x = NULL,title = NULL,fill = NULL, y = NULL)+
  theme_ang()+
  coord_cartesian(ylim = c(-.25,.5))+
  
  theme(legend.position = "none")

myrenderer <- gifski_renderer(loop=FALSE)
mygif <- animate(p, width=5, height=3, units = "in", res = 300, renderer=myrenderer,fps = 50)  
anim_save(filename="Reports/animation1.gif", mygif)


p = big_models.p %>% 
  filter(time == 0, Type != "Between", Controls== "Demo", peer_type == "Both") %>% 
  mutate(Type = ifelse(Type == "Average","Between-Person","Within-Person")) %>% 
  bind_rows(big_models.p %>% 
              filter(time == 0, Type != "Between", Controls== "Demo", peer_type == "Both") %>% 
              mutate(Type = ifelse(Type == "Average","Between-Person","Within-Person"),outcomes= "x")) %>% 
  ggplot(aes(term,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = NULL,fill = NULL, y = NULL)+
  theme_ang()+
  facet_wrap(~Type)+
  transition_states(outcomes,wrap = F,state_length = 0) +
  coord_cartesian(ylim = c(-.25,.5))+
  theme(legend.position = "none")

myrenderer <- gifski_renderer(loop=FALSE)
mygif <- animate(p, width=7, height=3, units = "in", res = 300, renderer=myrenderer,fps = 50)  
anim_save(filename="Reports/animation2.gif", mygif)



big_models.p %>% 
  filter(time == 0, Type != "Between", peer_type == "Both") %>% 
  mutate(Type = ifelse(Type == "Average","Between-Person","Within-Person")) %>% 
  bind_rows(big_models.p %>% 
              filter(time == 0, Type != "Between", Controls== "Demo", peer_type == "Both") %>% 
              mutate(Type = ifelse(Type == "Average","Between-Person","Within-Person"),outcomes= "x")) %>% 
  ggplot(aes(term,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = NULL,fill = NULL, y = NULL)+
  theme_ang()+
  facet_grid(Controls~Type)+
  coord_cartesian(ylim = c(-.25,.5))+
  theme(legend.position = "none")
ggsave(width=7, height=6, units = "in", filename = "Reports/supp2.pdf")  

# 699 Plots


big_models.p %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Role Models") %>% 
  ggplot(aes(name,estimate,fill = term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_fill_manual(values = c(yellow,"gray70"))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = "Beta of Own GPA and Role Model GPA",fill = "",y = NULL)+
  theme_ang()+
  theme(legend.position = "none")
ggsave("Reports/RM&O GPA.png",width = 5, height = 3)

big_models.p %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Role Models", term == "Role Model GPA") %>% 
  ggplot(aes(name,estimate,fill = term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_fill_manual(values = c(yellow))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = "Beta of Own GPA and Role Model GPA",fill = "",y = NULL)+
  theme_ang()+
  coord_cartesian(ylim = c(-.24,.2))+
  theme(legend.position = "none")

ggsave("Reports/RM GPA.png",width = 5, height = 3)

big_models.p %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type != "Both", term != 'Own GPA') %>% 
  ggplot(aes(name,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = "Beta of Role Model GPA and Friend GPA",fill = NULL, y= NULL)+
  theme_ang()+
  coord_cartesian(ylim = c(-.24,.2))+
  theme(legend.position = "none")
ggsave("Reports/RMF GPA.png",width = 5, height = 3)

big_models.p %>% 
  filter(time == 0, Type == "Average", Controls== "Demo", peer_type == "Both") %>% 
  mutate(term = factor(term, levels = c("Own GPA", "Role Model GPA", "Friend GPA"))) %>% 
  ggplot(aes(name,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c("gray70",yellow,blue))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = "Outcome",y = "Beta of Role Model GPA and Friend GPA",fill = "")+
  coord_cartesian(ylim = c(-.24,.2))+
  
  theme_ang()
ggsave("Reports/Legend.png",width = 5, height = 3)



big_models.p %>% 
  filter(time == 0, Type == "Within", Controls== "Demo", peer_type == "Both", term != 'Own GPA') %>% 
  ggplot(aes(name,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  # facet_grid(peer_type!= "Both"~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = "Beta of Role Model GPA and Friend GPA",fill = NULL, y= NULL)+
  theme_ang()+
  coord_cartesian(ylim = c(-.24,.2))+
  
  theme(legend.position = "none")
ggsave("Reports/RMF GPA_within.png",width = 5, height = 3)

big_models.p %>% 
  filter(time == 0, Type != "Between", Controls== "Demo", peer_type == "Both", term != 'Own GPA') %>% 
  mutate(Type = ifelse(Type == "Average","Between","Within")) %>% 
  ggplot(aes(x = Type,estimate, fill= term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  facet_grid(~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = c(yellow,blue))+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = NULL,title = "Beta of Role Model GPA and Friend GPA",fill = NULL, y= NULL)+
  theme_ang()+
  coord_cartesian(ylim = c(-.24,.2))+
  
  theme(legend.position = "none")
ggsave("Reports/All.png",width = 7, height = 3)

rmarkdown::render("Reports/results_plot.Rmd",output_dir = "Reports/")
