library(broom)
models = read_rds("Output/Raw/models.rds")
models_plot = models %>% 
  mutate(peer_type = ifelse(peer_type == "Exemplars","Role Models",peer_type)) %>% 
  mutate(tidy = map(result,tidy,conf.int = T)) %>% 
  unnest(tidy) %>% 
  mutate(outcome = fct_inorder(outcome),
         stars = case_when(p.value < .001 ~ "***",
                           p.value < .01 ~ "**",
                           p.value < .05 ~ "*",
                           T ~ ""),
         Significant = ifelse(p.value < .05,"Significant","Not Significant"))

w = .65

#Observable vs not
p = models_plot %>% 
  filter(term == "coregpa"| term == "near",
         peer_type == "Role Models",
         model == "Average.Demographics") %>% 
  mutate(term = ifelse(term == "near", "Role Model GPA", "Own GPA"),term = fct_inorder(term)) %>% 
  ggplot(aes(outcome,estimate,fill = term,col= term))+
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
   