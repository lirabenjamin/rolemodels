# to-do
# - [ ] make sure we have a set of students that remains consistent throught
# - [ ] figure out why we don't get the same resutls.

# Clean data #### 
data = read_rds('s1/data/clean.l.rds') %>% 
  select(-matches("grat"),-matches("sci"),-matches("aot"),-matches("grit"))

data
 
# create networks #### 
library(igraph)
library(tidyverse)

clean <- data
attr <- data %>% pivot_wider(names_from = time, values_from = age:avg_scw_tr) 

from_ids <- clean %>%
  pull(StudyID) %>%
  unique()
to_ids <-
  clean %>%
  select(pn_bf1:pn_scw2) %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value)) %>%
  pull(value) %>%
  unique()

length(from_ids)
length(to_ids)
missing_ids <- to_ids[!to_ids %in% from_ids]
missing_attr <- attr[seq_along(missing_ids), ] %>%
  mutate_all(~NA) %>%
  mutate(StudyID = missing_ids)
attr <- rbind(attr, missing_attr)
colnames(attr)
attr_long <- attr %>%
  pivot_longer(
    cols = contains("_T"), 
    names_to = c(".value", "time"), 
    names_sep = "_T",
    names_transform = list(time = as.integer)
  ) %>%
  mutate(time = paste0("T", time))

edgelist <-
  clean %>%
  # filter(time == "T1", !is.na(scw_s)) %>%
  select(StudyID, time, pn_bf1, pn_bf2, pn_scw, pn_scw2)

edgelist <-
  edgelist %>%
  pivot_longer(pn_bf1:pn_scw2, names_to = "pn", values_to = "value") %>%
  # filter out edges where there is no nomination
  filter(!is.na(value)) %>%
  mutate(
    pn = str_replace(pn, "pn_", ""),
    pn = tm::removeNumbers(pn)
  ) %>%
  select(from = StudyID, to = value, time, pn)

# 323 times people (n = 232) nominated themselves
edgelist %>%
  filter(from == to) %>%
  group_by(from)

# remove self-nominations
edgelist <- edgelist %>% filter(from != to)
attr_long  <- attr_long %>% filter(StudyID %in% edgelist$from | StudyID %in% edgelist$to)

library(igraph);library(ggraph)

make_graph_time_type <- function(edgelist, attr_long, type_filter = "bf", time_filter = "T1") {
  edgelist = edgelist %>% filter(time == time_filter & pn == type_filter)
  attr_long = attr_long %>% filter(time == time_filter) %>% select(-starts_with("pn"))

  g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attr_long)
  # remove isolates
  # g <- delete.vertices(g, which(degree(g) == 0))
  return(g)
}
graph_from_data_frame(edgelist, directed = TRUE, vertices = attr_long)

edgelist$from %>% unique() %>%length()
edgelist$to %>% unique() %>%length()
c(edgelist$to,edgelist$from) %>% unique() %>%length()
attr_long$StudyID %>% unique() %>%length()

graph_from_data_frame(edgelist)

make_graph_time_type(edgelist, attr_long, type_filter  = "bf", time_filter = "T1")

g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attr)

filter_graph <- function(graph = g, type = "bf", time = "T1") {
  time_ids <- which(E(graph)$time == time)
  g_r <- subgraph.edges(g, time_ids, delete.vertices = FALSE)
  if(type == 'both'){return(g_r)}
  type_ids <- which(E(g_r)$pn == type)
  g_r <- subgraph.edges(g_r, type_ids, delete.vertices = FALSE)
  return(g_r)
}

g

write_rds(g, "s1/data/graph.rds")

# network plots ####

a = filter_graph(graph = g, type = "both", time = "T1")
# filter one school
a = delete.vertices(a, which(V(a)$school != "p1"))
# remove isolates
a = delete.vertices(a, which(degree(a) == 0))

membership = a %>% cluster_infomap() %>%membership()
layout = create_layout(a,"stress")
node_data <- data.frame(name = V(a)$name, membership = as.factor(membership))
layout <- left_join(layout, node_data, by = "name")

library(ggraph)
g = delete.vertices(g, which(degree(g) == 0))
ggraph(g, layout = 'kk') + 
  geom_edge_link(aes(
    edge_alpha = 0.2,
    color = E(g)$time,
    linetype = E(g)$pn
  ), show.legend = FALSE) +
  # geom_mark_ellipse(aes(fill = membership, group = membership, x =x, y=y), 
  #                   data = layout,
  #                   expand = unit(0.15, "inches")) +
  geom_node_point(aes(
    size = degree(g), 
    # color = V(a)$school
    # color = degree(a)
    color = V(g)$coregpa_T1

    # color = V(a)$age_T1
    )) +
  # geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  scale_color_viridis_c() +
  theme(legend.position = "none")
ggsave('s1/figures/0000 network.pdf', width = 6, height = 4, units = 'in')

# get attributes of neighbors ####
vertices = ego(a, order = 3, node = 1, mode = "out")[[1]]

get_avg_value <- function(graph, node, delta, mode = "all", attribute = "value") {
    if(is.character(node)) {
    node <- which(V(graph)$name == node)
    if(length(node) == 0) {
      stop("Unknown vertex name: ", node)
    }
  }
  
  if(delta <= 0) {
    print("delta must be greater than 0")
  } else {
    neighbors <- ego(graph, order = delta, nodes = node, mode = mode)[[1]]
    return(mean(vertex_attr(graph, attribute, neighbors), na.rm = TRUE))
  }
}

bf1 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T1")
bf2 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T2")
bf3 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T3")
bf4 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T4")
scw1 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T1")
scw2 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T2")
scw3 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T3")
scw4 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T4")

get_avg_value_time_type = function(node, delta, mode = "all", attribute = "value",time = "T1", type = "bf"){
  if(type == "bf" & time == "T1"){g = bf1}
  if(type == "bf" & time == "T2"){g = bf2}
  if(type == "bf" & time == "T3"){g = bf3}
  if(type == "bf" & time == "T4"){g = bf4}
  if(type == "scw" & time == "T1"){g = scw1}
  if(type == "scw" & time == "T2"){g = scw2}
  if(type == "scw" & time == "T3"){g = scw3}
  if(type == "scw" & time == "T4"){g = scw4}

  return(get_avg_value(g, node, delta, mode = mode, attribute = attribute))
  }

# make datasets ####
make_dataset = function(data,attribute, delta, mode){
attr_long_test = data %>% 
  mutate(
    friend = map2_dbl(StudyID, time, ~get_avg_value_time_type(as.character(.x), delta, time = .y, type = "bf", attribute = attribute, mode =  mode)),
    exemplar = map2_dbl(StudyID, time, ~get_avg_value_time_type(as.character(.x), delta, time = .y, type = "scw", attribute = attribute, mode =  mode))
  )
  return(attr_long_test)
}

collapse_time = function(data){data %>% group_by(StudyID, school, female, eth, ell, sped, frpl) %>% summarize_all(mean, na.rm = TRUE) %>% select(-time) %>% ungroup()}

d = make_dataset(attr_long, "coregpa", 3, "out") %>% collapse_time()
scale_dataset = function(d){d %>% mutate_at(vars(age:exemplar), scale)}

# run regressions ####
library(broom)
run_reg = function(d)
{d %>% 
  scale_dataset() %>%
  lm(scw_s ~ coregpa + friend + exemplar + school + female + eth + ell + sped + frpl + age, data = .)
}

# mode: out is people you nominate, in is people who nominate you, all is both
lms = expand_grid(delta = 1:7, attribute = c("coregpa", "avg_scw_tr"), mode = c("out","in","all")) %>% 
  mutate(
    data = pmap(list(attribute, delta,mode), function(attribute, delta, mode) make_dataset(attr_long, attribute,delta, mode)),
    data = map(data, collapse_time),
    data = map(data, scale_dataset),
    model = map(data, run_reg)
    )

# PARAMS
# together, separate
# controls yes no
# delta 1-7
# attribute coregpa, avg_scw_tr
# model: within person, between person, fixedeffect
# timesubset: T1, T2, T3, T4, all


lms %>%
  mutate(model = map(model, tidy, conf.int = T)) %>%
  unnest(model) %>%
  filter(term %in% c("friend","exemplar")) %>%
  mutate(attribute = factor(attribute, levels = c("coregpa", "avg_scw_tr"), labels = c("Core GPA","Teacher Rated Self-Control"))) %>%
  ggplot(aes(x = delta, y = estimate, color = term, linetype = term)) +
  # add confint
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = term, color = NULL), alpha = 0.1) +
  geom_point() +
  geom_line() +
  # geom_text(aes(label = sprintf("%#.2f", estimate)) %>% sub("^0", "", .), vjust = -1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1", guide="none") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 1:7) +
  scale_linetype(guide = "none") +
  facet_wrap(~attribute, scales = "free_y")+
  labs(x = "Distance from Ego", y = "Regression Coefficient")
ggsave('s1/figures/0000 regression.pdf', width = 6, height = 4, units = 'in')

lms %>% filter(attribute == "avg_scw_tr", delta == 1) %>% 
mutate(model = map(model, glance)) %>%
unnest(model)
