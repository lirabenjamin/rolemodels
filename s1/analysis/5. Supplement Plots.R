models = read_rds("Output/Raw/models.rds")
models_plot = models %>% 
  mutate(tidy = map(result,tidy,conf.int = T)) %>% 
  unnest(tidy) %>% 
  mutate(outcome = fct_inorder(outcome),
         p.value = ifelse(p.value < .05,"Significant","Not Significant"),
         model2 = model) %>% 
  separate(model2, c("Type","Controls"),"\\.")


#Observable vs not
models_plot %>% 
  filter(term == "near",
         peer_type == "Exemplars",
         Type == "Average") %>% 
  ggplot(aes(outcome,estimate,color = Controls))+
  geom_point(size = 2,position = position_dodge(width = 0.40))+
  geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  # facet_grid(peer_type~model)+
  Ben::theme_ang()+
  theme(legend.position = c(.9,.2))+
  scale_color_brewer(palette = "Set1")+
  geom_hline(yintercept = 0,size = .1)+
  geom_vline(xintercept = 4.5,size = .1)+
  labs(x = "Outcome",y = "Beta of Peer GPA",col = "")
ggsave("Output/Supplement/Main.pdf",width = 7,height = 4)
ggsave("Output/Supplement/Main.png",width = 7,height = 4)

pred0 =  seq(-3,3,.1)
models_plot %>% 
  filter(Type == "Average",
         peer_type == "Exemplars") %>% 
  select(-result) %>% 
  filter(term %in% c("(Intercept)","near")) %>% 
  mutate(pred = map(outcome,function(x){return(pred0)}),
         outcome = ordered(fct_inorder(outcome))) %>% 
  unnest(pred) %>% 
  mutate(y = ifelse(term == "near", estimate*pred,estimate),
         yl = y + statistic*std.error,
         yh = y - statistic*std.error) %>% 
  group_by(pred,outcome,Controls) %>% 
  summarise(y = sum(y),
            yl = sum(yl),
            yh = sum(yh)) %>% 
  ungroup %>% 
  mutate(outcome = ordered(fct_inorder(outcome))) %>% 
  ggplot(aes(pred,y,col = Controls,fill =Controls))+
  geom_line(show.legend = F)+
  facet_grid(~outcome)+
  geom_ribbon(aes(ymin = yl,ymax = yh),col = NA,alpha = .1,show.legend = F)+
  geom_text(aes(label = str_replace_all(Controls,"\n"," ")),show.legend = F,data = . %>% filter(pred== 2.5),size= 2,vjust = 2)+
  Ben::theme_ang()+
  labs(x = "z-Scored Peer GPA",
       y = "z-Scored Self-Report Outcome")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")
ggsave("Output/Supplement/Main_prediction2.pdf",width = 7,height = 4)
ggsave("Output/Supplement/Main_prediction2.png",width = 7,height = 4)

#  Exemplars vs Friends
models_plot %>% 
  filter(term == "near",
         Type == "Average") %>% 
  mutate(outcome = fct_inorder(outcome)) %>% 
  ggplot(aes(peer_type,estimate,color = Controls))+
  geom_point(size = 2,position = position_dodge(width = 0.40))+
  geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  facet_wrap(~outcome,scales = "free_x")+
  Ben::theme_ang()+
  # theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5))+
  scale_color_brewer(palette = "Set1")+
  geom_hline(yintercept = 0,size = .1)+
  geom_vline(xintercept = 4.5,size = .1)+
  # theme(legend.position = c(.1,.9))+
  labs(x = "Outcome",y = "Beta",col = "Controls")
ggsave("Output/Supplement/Friends v. Peers.pdf",width = 7,height = 5)
ggsave("Output/Supplement/Friends v. Peers.png",width = 7,height = 5)

#Average vs. Within
models_plot %>% 
  filter(term == "near",
         peer_type == "Exemplars") %>% 
  mutate(Type = str_replace_all(Type," ","\n"),
         Type = str_replace_all(Type,"-","\n")) %>% 
  ggplot(aes(Type,estimate,color = Controls))+
  geom_point(size = 2,position = position_dodge(width = 0.40))+
  geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 5,alpha = .4,position = position_dodge(width = 0.40)) +
  Ben::theme_ang()+
  scale_color_brewer(palette = "Set1")+
  geom_hline(yintercept = 0,size = .1)+
  geom_vline(xintercept = 4.5,size = .1)+
  facet_wrap(~outcome,scales = "free_x")+
  labs(x = "Self-Reported Outcome",y = "Beta",col = "Controls")
ggsave("Output/Supplement/Between v. Within.pdf",width = 8,height = 5)
ggsave("Output/Supplement/Between v. Within.png",width = 8,height = 5)

 # Multiverse
plot_multi = function(var,legend){
  p = models_plot %>%
    filter(outcome != "Actively\nOpen-Minded\nThinking") %>% 
    # mutate(outcome = fct_inorder(outcome)) %>% 
    filter(term == "near") %>% 
    group_by({{var}},outcome) %>% 
    arrange(estimate) %>% 
    ggplot(aes(outcome,estimate,col = {{var}}, group= estimate))+
    geom_point(position = position_dodge(width = .6))+
    geom_linerange(aes(ymin = conf.low,ymax = conf.high),size = 1,alpha = .2,position = position_dodge(width = .6)) +
    Ben::theme_ang()+
    geom_hline(yintercept = 0)+
    theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = .5))+
    coord_flip()+
    labs(y = "Beta",x = "Self-Reported Outcome",col=legend)+
    scale_color_brewer(palette = "Set1")+
    ggtitle(legend)
  ggsave(p, filename = glue::glue("Output/Supplement/Multi_{legend}.png"),width = 7,height = 5)
  ggsave(p, filename = glue::glue("Output/Supplement/Multi_{legend}.pdf"),width = 7,height = 5)
  return(p)
}
plot_multi(peer_type,"Peer Definition")
plot_multi(Type,"Model Type")
plot_multi(Controls,"Controls")
plot_multi(observable,"Observability")

ggpubr::ggarrange(plot_multi(peer_type,"Peer Definition"),
                  plot_multi(Type,"Model Type"),
                  plot_multi(Controls,"Controls"),ncol = 1)
ggsave(filename = "Output/Supplement/Multi.pdf",width = 6,height = 10)
ggsave(filename = "Output/Supplement/Multi.png",width = 6,height = 10)
