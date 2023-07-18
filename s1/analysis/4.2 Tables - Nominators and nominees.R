models = read_rds("Output/Raw/big_models_both.rds")
models = models %>% rename_outs(outcomes) %>% rename(outcome= outcomes)

models

covlabels = c(
  "Role Models' Core GPA",
  "Friends' Core GPA",
  "Own Core GPA",
              "School",
              "Female",
              "American Indian",
              "Asian",
              "Caucasian",
              "Hispanic",
              "Multi-Racial",
              "English Language Learner",
              "Special Education Student",
              "Eligibile for Free or Reduced Priced Meals")


# rename_model = function(dv,peer_type,model){
#   dimnames(model$coefficients)[[2]] = dimnames(model$coefficients)[[2]] %>% str_replace_all("scw_s",dv)
#   if(peer_type == "Exemplars"){
#     dimnames(model$coefficients)[[1]] = dimnames(model$coefficients)[[1]] %>% str_replace("near", "Role Model GPA")
#     names(model$cse) = names(model$cse) %>% str_replace("near","Role Model GPA")
#   }
#   if(peer_type == "Friends"){
#     dimnames(model$coefficients)[[1]] = dimnames(model$coefficients)[[1]] %>% str_replace("near", "Friend GPA")
#     names(model$cse) = names(model$cse) %>% str_replace("near","Friend GPA")
#   }
#   return(model)
# }
# models = models %>% mutate(result = pmap(list(outcome,peer_type,result),rename_model))

make_table = function(models_df,filename,format = c("tex","html"),...){stargazer::stargazer(models_df$lm,
                     type = "text",
                     column.labels   = c("Role Models", "Friends"),
                     column.separate = c(5, 5),
                     dep.var.caption = (str_replace_all(models_df$outcome,"\n"," "))[1],
                     out = glue::glue("Output/Main Manuscript/Both_{filename}.{format}"),
                     covariate.labels = covlabels,
                     initial.zero = F,
                     ci = F,
                     no.space = T,
                     digits.extra = 0,
                     digits = 3,
                     table.layout = "-lc-#-t-a-s-",
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     align=TRUE,
                     notes.align = "l",
                     ...
                     )}

models %>% filter(outcome == unique(models$outcome)[1]) %>% arrange(peer_type,model) %>% 
  make_table(filename = "All ASC",
             add.lines = list(c("Fixed effects for Student",rep(c(rep("Yes",1), rep("No",4)),3)),
                              c("Composites across time?",rep(c(rep("No",3), rep("Yes",2)),3))))

models %>% filter(outcome == unique(models$outcome)[2]) %>% arrange(peer_type,model)  %>% 
  make_table(filename = "All Grit",
             add.lines = list(c("Fixed effects for Student",rep(c(rep("Yes",1), rep("No",4)),3)),
                              c("Composites across time?",rep(c(rep("No",3), rep("Yes",2)),3))))

models %>% filter(outcome == unique(models$outcome)[3]) %>% arrange(peer_type,model)  %>% 
  make_table(filename = "All Grat",
             add.lines = list(c("Fixed effects for Student",rep(c(rep("Yes",1), rep("No",4)),3)),
                              c("Composites across time?",rep(c(rep("No",3), rep("Yes",2)),3))))
models %>% filter(outcome == unique(models$outcome)[4]) %>% arrange(peer_type,model)  %>% 
  make_table(filename = "All SCI",
             add.lines = list(c("Fixed effects for Student",rep(c(rep("Yes",1), rep("No",4)),3)),
                              c("Composites across time?",rep(c(rep("No",3), rep("Yes",2)),3))))

