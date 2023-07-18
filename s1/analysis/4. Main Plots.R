library(broom)
big_models = read_rds("Output/Raw/big_models_both.rds")

big_models |> glimpse()


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
  filter(term %in%  c("pn_bf","rolemodel","coregpa")) %>% 
  # filter(model %in% c(5)) %>% 
  select(time:model, term, estimate, conf.low,conf.high,name, p.value) %>% 
  mutate(name = fct_inorder(name),
         model2 = model,
         term = case_when(term == "rolemodel" ~ "Role Model GPA",
                          term == "pn_bf" ~ "Friend GPA",
                          term == "coregpa" ~ "Own GPA")) %>% 
  separate(model2, c("Type","Controls"),"\\.")

big_models.p |> filter(
  outcomes == "scw_s",
  peer_type == "Both",
  model == "Average.Demo"
) |>  mutate(p.value = round(p.value, 3))

big_models.p2 = big_models.p %>% filter(time == 0) %>% filter(peer_type != "Both",model == "Average.Demo", term != "Own GPA") %>% mutate(time = list(c(1:4))) %>% unnest(time)

big_models.p |> 
  filter(outcomes == "scw_s",
         time == 0,
         model %in% c("Within.Demo", "Between.Demo")) |> 
  ggplot(aes(term, estimate))+
  geom_col()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
  facet_grid(peer_type~Type)

# Main Man Plot
big_models.p %>% 
  filter(model %in% c("Within.Demo","Average.Demo"),time == 0, peer_type == "Both", term != "Own GPA") %>% 
  # filter(term %in%  c("pn_bf","rolemodel")) %>% 
  ggplot(aes(model,estimate,fill = term))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  facet_grid(~name)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = grayscale)+
  geom_text(aes(label = numformat(estimate),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = "Model Specification",y = "Beta of Peer GPA",fill = "")+
  theme_ang()+
  theme(legend.position = c(.1,.79))

# Choose if simultaneous model by changing != to == both
big_models.p %>% 
  filter(peer_type != "Both",model == "Average.Demo", term != "Own GPA") %>% 
  filter(time != 0) %>% 
  ggplot(aes(time,estimate))+
  # geom_point(position = position_dodge(width = .5))+
  # geom_linerange(aes(ymin = conf.low,ymax = conf.high))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low,ymax = conf.high),alpha = .2)+
  
  geom_line(data = big_models.p2,lty = 2)+
  geom_ribbon(data = big_models.p2, aes(ymin = conf.low,ymax = conf.high),alpha = .2)+
  
  facet_grid(term~name)+
  theme_ang()+
  geom_hline(yintercept = 0)+
  labs(y = "Beta of Peer GPA", x = "Time")
ggsave2("Supplement/Time Subgroup",8.5,6)


big_models.p %>%
  group_by(term,outcomes) %>% 
  arrange(estimate) %>% 
  ggplot(aes(term,estimate,col = paste(model,time,peer_type)))+
  geom_point(position = position_dodge(width = .5),show.legend = F)+
  geom_linerange(aes(ymin = conf.low,ymax = conf.high),position = position_dodge(width = .5),show.legend = F,alpha = .3,size = 1)+
  facet_grid(~outcomes)

plot_multi = function(var,legend){
  
  big_models.p %>% 
    mutate(peer_type= ifelse(peer_type == "Both","Simultaneous","Independent")) %>% 
    mutate(Controls = ifelse(Controls == "Demo","Controlling for Demographics","Unadjusted")) %>% 
    filter(term != "Own GPA") %>% 
    mutate_at(vars(time,model),as.factor) %>% 
    group_by({{var}},outcomes) %>% 
    arrange(estimate) %>% 
    ggplot(aes(name,estimate,col = {{var}}, group= estimate))+
    geom_point(position = position_dodge(width = .6))+
    geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 1,alpha = .2,position = position_dodge(width = .6)) +
    Ben::theme_ang()+
    geom_hline(yintercept = 0)+
    theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5))+
    coord_flip()+
    labs(y = "Beta",x = "Self-Reported Outcome",col = legend)+
    # facet_grid(~term)+
    scale_color_brewer(palette = "Set1")
    # scale_color_manual(values = c(yellow,blue))
}

plot_multi(time,"Time")
ggsave2("Supplement/Time",10,6)

plot_multi(term,"Term")
ggsave2("Supplement/Term",10,6)
plot_multi(peer_type,"Model Specification")
ggsave2("Supplement/Controlling for each other",10,6)
plot_multi(Controls,"Controls")
ggsave2("Supplement/Demographics",10,6)
plot_multi(Type,"Model Specification")
ggsave2("Supplement/ModelSpec",10,6)





models = read_rds("Output/Raw/big_models.rds")
models_plot = models %>% 
  mutate(peer_type = ifelse(peer_type == "Exemplars","Role Models",peer_type)) %>% 
  mutate(tidy = map(lm,tidy,conf.int = T)) %>% 
  unnest(tidy) %>% 
  mutate(outcomes = fct_inorder(outcomes),
         stars = case_when(p.value < .001 ~ "***",
                           p.value < .01 ~ "**",
                           p.value < .05 ~ "*",
                           T ~ ""),
         Significant = ifelse(p.value < .05,"Significant","Not Significant"))

w = .65

#Observable vs not
p = models_plot %>% 
  filter(term == "coregpa"| term == "rolemodelgpa",
         peer_type == "models",
         model == "1") %>% 
  mutate(term = ifelse(term == "near", "Role Model GPA", "Own GPA"),term = fct_inorder(factor(term))) %>% 
  ggplot(aes(outcomes,estimate,fill = term,col= term))+
  # geom_point(size = 2,position = position_dodge(width = .9))+
  # geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4) +
  geom_col(position = position_dodge(width = w), col = "gray40",size = .125,width = w)+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = ci_width,show.legend = F,position = position_dodge(width = w)) +
  geom_text(aes(label = paste0(Ben::numformat(estimate),stars),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = w),
            size = 2.5, 
            
            show.legend = F)+
  # geom_text(aes(label = stars,y = estimate+sign(estimate)*.015), position = position_dodge(width = w),size = 3, hjust = - .75 ,show.legend = F)+
  # facet_grid(peer_type~model)+
  Ben::theme_ang()+
  coord_cartesian(ylim = c(-.3, .5))+
  theme(legend.position = c(.5,.9))+
  scale_color_manual(values = c("#FF4D4F","#6AB1EB"))+
  scale_fill_brewer(palette = "Set1")+
  geom_hline(yintercept = 0,size = .1)+
  # geom_vline(xintercept = 4.5,size = .1)+
  labs(x = "Outcome",y = "Beta of Role Model and Own GPA",fill = "")
if(bw){p= p+scale_color_manual(values = gray_errorbars)+scale_fill_manual(values = grayscale)}
p
ggsave("Output/Main Manuscript/Average_Role Models.pdf",width = main_plot_w,height = main_plot_h)
ggsave("Output/Main Manuscript/Average_Role Models.png",width = main_plot_w,height = main_plot_h)

pred0 =  seq(-3,3,.1)
models_plot %>% 
  filter(model == "Average.Demographics",
         peer_type == "Role Models") %>% 
  select(-result) %>% 
  filter(term %in% c("(Intercept)","near")) %>% 
  mutate(pred = map(outcome,function(x){return(pred0)}),
         outcome = ordered(fct_inorder(outcome))) %>% 
  unnest(pred) %>% 
  mutate(y = ifelse(term == "near", estimate*pred,estimate),
         yl = y + statistic*std.error,
         yh = y - statistic*std.error) %>% 
  group_by(pred,outcome) %>% 
  summarise(y = sum(y),
            yl = sum(yl),
            yh = sum(yh)) %>% 
  ungroup %>% 
  mutate(outcome = ordered(fct_inorder(outcome))) %>% 
  ggplot(aes(pred,y,col = outcome,fill =outcome))+
  geom_line(show.legend = T)+
  geom_ribbon(aes(ymin = yl,ymax = yh),col = NA,alpha = .1,show.legend = T)+
  # geom_text(aes(label = str_replace_all(outcome,"\n"," ")),show.legend = F,data = . %>% filter(pred== 2.5),size= 2,angle = c(-2,-24,-3,-20,0,10),vjust = 2)+
  Ben::theme_ang()+
  labs(x = "z-Scored Peer GPA",
       y = "z-Scored Self-Report Outcome",
       col = "Outcome",
       fill = "Outcome")+
  scale_color_viridis_d(end = .9)+
  scale_fill_viridis_d(end = .9)
ggsave("Output/Scraps/Main_prediction.pdf",width = main_plot_w,height = main_plot_h)
ggsave("Output/Scraps/Main_prediction.png",width = main_plot_w,height = main_plot_h)

#  Role Models vs Friends
models_plot %>% 
  filter(term == "near",
         model == "Average.Demographics",
         outcome == "Grit" | outcome == "Academic\nSelf-Control") %>% 
  select(peer_type,outcome,estimate) %>% 
  spread(peer_type,estimate) %>% 
  mutate(rel = Friends/`Role Models`*100)
  
# Relative dif
p = models_plot %>% 
  filter(term == "coregpa"| term == "near",
         peer_type == "Friends",
         model == "Average.Demographics") %>% 
  mutate(outcome = fct_inorder(outcome)) %>% 
  mutate(term = ifelse(term == "near", "Friend GPA", "Own GPA"),term = fct_inorder(term)) %>% 
  ggplot(aes(outcome,estimate,fill = term,col= term))+
  # geom_point(size = 2,position = position_dodge(width = 0.40))+
  # geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  geom_col(position = position_dodge(width = w),width = w,col = "gray40",size = .125)+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = ci_width,show.legend = F,position = position_dodge(width = w))+
  scale_fill_brewer(palette = "Set1")+
  coord_cartesian(ylim = c(-.3, .5))+
  geom_text(aes(label = paste0(Ben::numformat(estimate),stars),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = w),
            size = 2.5, 
            
            show.legend = F)+
  
  # facet_grid(peer_type~model)+
  Ben::theme_ang()+
  scale_color_manual(values = c("#FF4D4F","#6AB1EB"))+
  # theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5))+
  geom_hline(yintercept = 0,size = .1)+
  # geom_vline(xintercept = 4.5,size = .1)+
  theme(legend.position = c(.9,.2))+
  labs(x = "Outcome",y = "Beta of Role Model and Own GPA",fill = "")
if(bw){p= p+scale_color_manual(values = gray_errorbars)+scale_fill_manual(values = grayscale)}
p
ggsave("Output/Main Manuscript/Average_Friends.pdf",width = main_plot_w,height = main_plot_h)
ggsave("Output/Main Manuscript/Average_Friends.png",width = main_plot_w,height = main_plot_h)

#Average vs. Within
models_plot %>% 
  filter(term == "near",
         peer_type == "Role Models",
         model == "Within-Person.Demographics" |model == "Average.Demographics" ,
         outcome == "Grit" | outcome == "Academic\nSelf-Control") %>% 
  select(model,outcome,estimate) %>% 
  spread(model,estimate) %>% 
  mutate(rel = `Within-Person.Demographics`/Average.Demographics*100)

p = models_plot %>% 
  filter(term == "coregpa"| term == "near",
         peer_type == "Role Models",
         model  %in%  c("Within-Person.Demographics")) %>% 
  mutate(model = ifelse(model == "Average.Demographics", "Between-person","Within-person")) %>% 
  mutate(term = ifelse(term == "near", "Role Model GPA", "Own GPA"),term = fct_inorder(term)) %>% 
  ggplot(aes(outcome,estimate,fill = term,col= term))+
  # geom_point(size = 2,position = position_dodge(width = 0.40))+
  # geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  geom_col(position = position_dodge(width = w),width = w,col = "gray40",size = .125)+
  coord_cartesian(ylim = c(-.3, .5))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = ci_width,show.legend = F,position = position_dodge(width = w))+
  geom_text(aes(label = paste0(Ben::numformat(estimate),stars),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.006), 
            position = position_dodge(width = w),
            size = 2.5, 
            
            show.legend = F)+
  scale_fill_brewer(palette = "Set1")+
  Ben::theme_ang()+
  scale_color_manual(values = c("#FF4D4F","#6AB1EB"))+
  geom_hline(yintercept = 0,size = .1)+
  # geom_vline(xintercept = 4.5,size = .1)+
  theme(legend.position = c(.8,.95))+
  labs(x = "Outcome",y = "Beta of Role Model and Own GPA",fill = "")
if(bw){p= p+scale_color_manual(values = gray_errorbars)+scale_fill_manual(values = grayscale)}
p
ggsave("Output/Main Manuscript/Within_Role Model.pdf",width = main_plot_w,height = main_plot_h)
ggsave("Output/Main Manuscript/Within_Role Model.png",width = main_plot_w,height = main_plot_h)


#Average vs. Within - Friensd
models_plot %>% 
  filter(term == "near",
         peer_type == "Friends",
         model == "Within-Person.Demographics" |model == "Average.Demographics" ,
         outcome == "Grit" | outcome == "Academic\nSelf-Control") %>% 
  select(model,outcome,estimate) %>% 
  spread(model,estimate) %>% 
  mutate(rel = `Within-Person.Demographics`/Average.Demographics*100)

p = models_plot %>% 
  filter(term == "coregpa"| term == "near",
         peer_type == "Friends",
         model  %in%  c("Within-Person.Demographics")) %>% 
  mutate(model = ifelse(model == "Average.Demographics", "Between-person","Within-person")) %>%
  mutate(term = ifelse(term == "near", "Friend GPA", "Own GPA"),term = fct_inorder(term)) %>% 
  ggplot(aes(outcome,estimate,fill = term,col= term))+
  # geom_point(size = 2,position = position_dodge(width = 0.40))+
  # geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  geom_col(position = position_dodge(width = w),width = w,col = "gray40",size = .125)+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = ci_width,show.legend = F,position = position_dodge(width = w))+
  geom_text(aes(label = paste0(Ben::numformat(estimate),stars),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.006), 
            position = position_dodge(width = w),
            size = 2.5, 
            show.legend = F)+
  scale_fill_brewer(palette = "Set1")+
  Ben::theme_ang()+
  coord_cartesian(ylim = c(-.3, .5))+
  scale_color_manual(values = c("#FF4D4F","#6AB1EB"))+
  geom_hline(yintercept = 0,size = .1)+
  # geom_vline(xintercept = 4.5,size = .1)+
  theme(legend.position = c(.8,.95))+
  labs(x = "Outcome",y = "Beta of Role Model and Own GPA",fill = "")
if(bw){p= p+scale_color_manual(values = gray_errorbars)+scale_fill_manual(values = grayscale)}
p
ggsave("Output/Main Manuscript/Within_Friends.pdf",width = main_plot_w,height = main_plot_h)
ggsave("Output/Main Manuscript/Within_Friends.png",width = main_plot_w,height = main_plot_h)


models_plot %>% 
  filter(outcome != "Actively\nOpen-Minded\nThinking") %>% 
  filter(term == "near") %>% 
  filter(model %in% c("Average.Demographics","Within-Person.Demographics")) %>% 
  mutate(model = ifelse(model == "Average.Demographics", "Between","Within")) %>% 
  ggplot(aes(model,estimate,fill = peer_type))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = .9),width = ci_width,show.legend = F,color = gray_errorbars[1])+
  facet_grid(~outcome)+
  scale_color_manual(values = gray_errorbars)+
  scale_fill_manual(values = grayscale)+
  geom_text(aes(label = paste0(stars),
                y = estimate+sign(estimate)*(estimate-conf.low)+sign(estimate)*.02), 
            position = position_dodge(width = .9),
            size = 3, 
            color = gray_errorbars[1],
            show.legend = F)+
  geom_hline(yintercept = 0,size = .1)+
  labs(x = "Model Specification",y = "Beta of Peer GPA",fill = "")+
  theme_ang()+
  theme(legend.position = c(.1,.79))
 ggsave2("Scraps/All",8,3)
   