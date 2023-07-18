# Make gif for intrapersoal change
clean.l = read_rds("Data/clean.l.rds")
uniqueids = clean.l$StudyID %>% unique
uniqueids = sample(uniqueids,200)
plot_data = clean.l %>% 
  select(StudyID,time,outcomes) %>% 
  pivot_longer(outcomes) %>% 
  rename_outs(name,single_line = F)

for (i in uniqueids){
  plot_data%>% 
    filter(StudyID == i) %>% 
    ggplot(aes(time,value,group = StudyID,col = name))+
    geom_line(show.legend = F)+
    geom_line(data = plot_data,show.legend = F,alpha = .01)+
    geom_line(data = plot_data %>%
                group_by(time,name) %>%
                summarise(value = mean(value,na.rm=T)), aes(group=name) ,show.legend = F,size = 2,alpha = .5)+
    facet_wrap(~name,scales = "free",nrow = 1)+
    Ben::theme_ang()+
    scale_color_manual(values = c("#c1292e","#d47b1c",'#f1d302','#4fb029','#235789','#8a6a87'))
  ggsave(glue::glue("Output/Data Checks/Intra Change Gif/{i}.png"),width = 8,height = 4)
}
