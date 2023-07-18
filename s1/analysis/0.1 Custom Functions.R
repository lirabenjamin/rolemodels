# Custom Functions
# Functions
ggsave2 = function(filename,width, height,...){
  filename = paste0("Output/",filename)
  ggsave(filename = paste0(filename,".pdf"),width = width, height = height)
  ggsave(filename = paste0(filename,".png"),width = width, height = height)
}
rename_outs = function(data,outs,single_line = T){
  data = data %>% 
    mutate(name = case_when({{outs}} == "grit_s" ~ "Grit",
                            {{outs}} == "scw_s" ~ "Academic\nSelf-Control",
                            {{outs}} == "sci_s" ~ "Interpersonal\nSelf-Control",
                            {{outs}} == "grat_s" ~ "Gratitude",
                            {{outs}} == "aot_s" ~ "Actively\nOpen-Minded\nThinking",
                            {{outs}} == "female" ~ "Female",
                            T ~ {{outs}}))
  if(single_line){data = data %>% mutate(name = str_replace_all(name,"\n"," "))}
  data = data %>% mutate(name = fct_inorder(name))
  return(data)
}
numformat = function(val, n = 2){sub("^(-?)0.", "\\1.", sprintf(paste0("%.", n, "f"), val))}
theme_ang = function(){
  theme(legend.position = "bottom", panel.grid = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(size = 0.25))}