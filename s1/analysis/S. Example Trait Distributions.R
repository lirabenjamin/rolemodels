clean.l = read_rds("Data/clean.l.rds")
clean.l %>% ggplot(aes(sci_s))+
  geom_density(fill = "gray", color = NA)+
  Ben::theme_ang()+
  labs(x = "Trait",y = NULL)
ggsave2("Example Distribution Trait", 5,3)

clean.l %>% 
  filter(!is.na(female)) %>% 
  ggplot(aes(coregpa,fill = as.character(female)))+
  geom_density(color = NA,alpha = .3,show.legend = F)+
  Ben::theme_ang()+
  labs(x = "Trait",y = NULL)+
  scale_fill_brewer(palette = "Set1")
ggsave2("Example Distribution Trait2", 5,3)

