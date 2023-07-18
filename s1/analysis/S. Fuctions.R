# Function to run CDAP
pull_near = function(ids,sch,tim,var){
  clean.l %>% 
    filter(StudyID  %in%  ids, 
           time == tim,
           school == sch) %>% pull({{var}}) %>% mean(na.rm=T) %>% return()
}
pull_far = function(ids,sch,tim,var){
  clean.l %>% filter(time == "T1",school == sch, !(StudyID  %in%  ids)) %>% pull({{var}}) %>% mean(na.rm=T) %>% return()
}

pull_near(ids = c(1415,1424),sch = "p2",tim = "T1",var = "coregpa")
pull_far(ids = c(1415,1435),sch = "p2",tim = "T1",var = "coregpa")

mutate_near_far = function(data,peer_definition,sch,tim,peer_quality){
  data %>% 
    nest(data = {{peer_definition}}) %>% 
    mutate(data = map(data,function(x){x %>% t() %>% as.integer()})) %>% 
    mutate(near = pmap_dbl(list(data,school,time,{{peer_quality}}),pull_near),
           # far = pmap_dbl(list(data,school,time,{{peer_quality}}),pull_far)
           )
}

runreg = function(data,predictors, fe, clust,outcome){
  data %>% 
    mutate_at(vars({{outcome}}, near,{{predictors}},-eth),function(x){x %>% scale %>% as.numeric()}) %>% 
    lfe::felm(formula = formula(glue::glue("{outcome} ~ {paste(c('near',predictors),collapse = '+')} | {fe} | 0 | {clust}")),data = .)
}

clean.lzz = clean.l %>% group_by(school,time) %>% mutate(coregpa = scale(coregpa) %>% as.numeric()) %>% ungroup() %>% mutate(coregpa = scale(coregpa))

clustidfeid = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa"),"StudyID","StudyID","grit_s")

clustidfeid = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa"),"StudyID","StudyID","grit_s")

clustscfesc = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa"),"school","school","grit_s")

clustsc = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa"),"0","school","grit_s")

lm1 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>%
  mutate_at(vars(grit_s, near,coregpa,-eth),function(x){x %>% scale %>% as.numeric()}) %>% 
  lm(formula = grit_s ~ near+coregpa,data=.)

h = stargazer::stargazer(lm1,clustsc,clustscfesc,clustidfeid, type = "html",column.labels = c("lm","nofe - school","school-school","id-id"),star.cutoffs = c(.1,.05,.01,.001),star.char = c("+",'*','**','***'),note)
h %>% display_html()

clustidfeid2 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa","female","eth","ell","sped","frpl"),"StudyID","StudyID","grit_s")

clustidfeid2 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa","female","eth","ell","sped","frpl"),"StudyID","StudyID","grit_s")

clustscfesc2 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa","female","eth","ell","sped","frpl"),"school","school","grit_s")

clustsc2 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>% 
  runreg(c("coregpa","female","eth","ell","sped","frpl"),"0","school","grit_s")

lm2 = clean.lzz %>% 
  mutate_near_far(c(pn_bf1,pn_bf2),school,tim = time,peer_quality = "coregpa") %>%
  mutate_at(vars(grit_s, near,coregpa),function(x){x %>% scale %>% as.numeric()}) %>% 
  lm(formula = grit_s ~ near+coregpa+female+eth+ell+sped+frpl,data=.)

h = stargazer::stargazer(lm2,clustsc2,clustscfesc2,clustidfeid2, type = "html",column.labels = c("lm","nofe - school","school-school","id-id"),star.cutoffs = c(.1,.05,.01,.001),star.char = c("+",'*','**','***'))
h %>% display_html()

clean.lzz %>% count(school)

clean.l %>% group_by(school,time) %>% summarise(g = mean(grit_s, na.rm=T)) %>% ggplot(aes(school,g,group = time))+geom_line()+labs(y = "grit")+Ben::theme_ang()
clean.l %>% group_by(school,time) %>% summarise(g = mean(coregpa, na.rm=T)) %>% ggplot(aes(school,g,group = time))+geom_line() + labs(y = "gpa")+Ben::theme_ang()

clean %>% filter(school == "c4") %>% select(matches("gpa"))

display_html = function(str){
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  writeLines(str, con = htmlFile)
  # (code to write some content to the file)
  rstudioapi::viewer(htmlFile)
}

bfgpa %>% unnest(bf1gpa) %>% unnest(bf2gpa) %>% filter((!is.na(bf1gpa) & !is.na(bf2gpa))) %>% rowwise() %>% 
  mutate(bfgpa = meann(bf1gpa,bf2gpa,trim=1)) %>%  ungroup %>% mutate_if(is.numeric,scale) %>%
  lm(grit_s ~ coregpa+bfgpa,data=.) %>% stargazer::stargazer(type = "text")