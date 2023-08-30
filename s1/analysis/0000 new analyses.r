# to-do
# - [ ] make sure we have a set of students that remains consistent throught
# - [ ] figure out why we don't get the same resutls.
# - [ ] add how many people there are in each peer group
# - [ ] look at changes of friends or rolemodels?

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



# Clean network inputs
# 323 times people (n = 232) nominated themselves
edgelist %>%
  filter(from == to) %>%
  group_by(from)

# remove self-nominations
edgelist <- edgelist %>% filter(from != to)
attr_long  <- attr_long %>% filter(StudyID %in% edgelist$from | StudyID %in% edgelist$to)

# remove double nominations
edgelist <- edgelist %>% distinct(from, to, time, pn, .keep_all = TRUE)
attr_long  <- attr_long %>% filter(StudyID %in% edgelist$from | StudyID %in% edgelist$to)

library(igraph);library(ggraph)

make_graph_time_type <- function(edgelist, attr_long, type_filter = "bf", time_filter = "T1") {
  if(type_filter == "both") {
    edgelist = edgelist %>% filter(time == time_filter)
    attr_long = attr_long %>% filter(time == time_filter) %>% select(-starts_with("pn"))
    g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attr_long)
    return(g)
  }
  edgelist = edgelist %>% filter(time == time_filter & pn == type_filter)
  attr_long = attr_long %>% filter(time == time_filter) %>% select(-starts_with("pn"))

  g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attr_long)
  
  # remove isolates
  # g <- delete.vertices(g, which(degree(g) == 0))
  return(g)
}

edgelist$from %>% unique() %>%length()
edgelist$to %>% unique() %>%length()
c(edgelist$to,edgelist$from) %>% unique() %>%length()
attr_long$StudyID %>% unique() %>%length()

graph_from_data_frame(edgelist)

bf1 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T1")
bf2 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T2")
bf3 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T3")
bf4 = make_graph_time_type(edgelist, attr_long, type = "bf",time = "T4")
scw1 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T1")
scw2 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T2")
scw3 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T3")
scw4 = make_graph_time_type(edgelist, attr_long, type = "scw",time = "T4")
both1 = make_graph_time_type(edgelist, attr_long, type = "both",time = "T1")
both2 = make_graph_time_type(edgelist, attr_long, type = "both",time = "T2")
both3 = make_graph_time_type(edgelist, attr_long, type = "both",time = "T3")
both4 = make_graph_time_type(edgelist, attr_long, type = "both",time = "T4")

plot(bf1, vertex.size = 3, vertex.label = NA, arrow.size = 0.5, edge.arrow.size = 0.5)

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
a = delete.vertices(a, which(V(a)$school != "p2"))
# remove isolates
a = delete.vertices(a, which(degree(a) == 0))

membership = a %>% cluster_infomap() %>%membership()
layout = create_layout(a,"stress")
node_data <- data.frame(name = V(a)$name, membership = as.factor(membership))
layout <- left_join(layout, node_data, by = "name")

library(ggraph)
g = delete.vertices(g, which(degree(g) == 0))
plotg = filter_graph(graph = g, type = "both", time = "T1")
plotg = delete.vertices(plotg, which(V(plotg)$school != "p2"))
plotg = delete.vertices(plotg, which(degree(plotg) == 0))
highlight = 16
# Get the first order neighbors
neighbors_1 <- neighbors(plotg, highlight)
# Get the second order neighbors (neighbors of neighbors)
neighbors_2 <- unique(unlist(lapply(neighbors_1, function(x) neighbors(plotg, x))))
# Make sure we do not count first order neighbors as second order neighbors
neighbors_2 <- setdiff(neighbors_2, neighbors_1)

# Initialize colors to some default color
V(plotg)$color <- "lightblue"

# Color the highlight node
V(plotg)$color[highlight] <- "red"

# Color the first order neighbors
V(plotg)$color[neighbors_1] <- "darkred"

# Color the second order neighbors
V(plotg)$color[neighbors_2] <- "black"

V(plotg)$names = as.numeric(V(plotg)$name)
layout = layout_with_fr(plotg, niter = 2000, repulserad = 10)
ggraph(plotg, layout = layout) + 
  geom_edge_link(
    arrow = grid::arrow(length = unit(1.5, 'mm')),
    end_cap = circle(1, 'mm'),
    aes(
    edge_alpha = 0.2,
    # color = E(plotg)$time,
    linetype = E(plotg)$pn
  ), show.legend = FALSE) +
  # geom_mark_ellipse(aes(fill = membership, group = membership, x =x, y=y), 
  #                   data = layout,
  #                   expand = unit(0.15, "inches")) +
  geom_node_point(aes(
    size = degree(plotg, mode = "in")/2, 
    # color = V(a)$school
    # color = degree(a)
    color = I(V(plotg)$color)

    # color = V(a)$age_T1
    )) +
  # geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  # scale_color_viridis_d() +
  theme(legend.position = "none")
ggsave('s1/figures/0000 network.pdf', width = 6, height = 4, units = 'in')

# GGAlly
ggallyg = edgelist %>% 
  filter(time == "T1") %>% 
  unique() %>% group_by(to, from, time) %>% mutate(n = n()) %>% 
  mutate(pn = ifelse(n == 2, "both", pn)) %>% select(-n) %>% unique() %>%
  mutate(lty = case_match(pn, 
                          "bf" ~ 1, 
                          "fb" ~ 2, 
                          "both" ~ 3)) %>%
network::network(., multiple = F)
GGally::ggnet2(ggallyg, size = 2,arrow.size = 3,arrow.gap = .005,arrow.type = "open",lty = "lty")

# Network descriptives ####

# Double nominations
# how often did an ego nominate the same person for bf and scw?
edgelist %>% 
  group_by(from, to, time) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(pn = ifelse(n == 2, "both", pn)) %>%
  distinct() %>% 
  group_by(time) %>%
  count(pn) %>%
  pivot_wider(names_from = pn, values_from = n) %>% 
  mutate(double_ratio = both/(bf+scw+both))

# Self, Role models and friends GPA 
attr_long
edgelist %>% 
  left_join(attr_long %>% select(StudyID, time, coregpa), by = c("from" = "StudyID", "time" = "time")) %>%
  rename(coregpa_own = coregpa) %>%
  left_join(attr_long %>% select(StudyID, time, coregpa), by = c("to" = "StudyID", "time" = "time")) %>%
  rename(coregpa_other = coregpa) %>% 
  mutate(ego_has_higher_gpa = coregpa_own > coregpa_other) %>% 
  group_by(time, pn) %>%
  filter(!is.na(ego_has_higher_gpa)) %>%
  count(ego_has_higher_gpa) %>% 
  mutate(
    prop = n/sum(n),
    ci = 1.96 * sqrt(prop*(1-prop)/sum(n))
    ) %>%
  mutate(
    pn = ifelse(pn == "bf", "Close Friends", "Exemplars"),
    ego_has_higher_gpa = ifelse(ego_has_higher_gpa, "Ego has higher GPA", "Ego has lower GPA")
  ) %>%
  ggplot(aes(time, prop, group = paste(ego_has_higher_gpa, pn), color = pn, lty = ego_has_higher_gpa)) +
  geom_line() +
  geom_point()+
  geom_ribbon(aes(ymin=prop-ci, ymax=prop+ci, fill = pn), width=0.2, color = NA, alpha = .3) +  # add error bars for CI
  geom_text(aes(label = Ben::numformat(prop, 2)), vjust = 2, size = 2.5) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(~pn)+
  geom_hline(yintercept = .5, linetype = "dashed")+
  theme(legend.position = c(.2, .2))+
  labs(x = "Time", y = "Proportion of Ties", color = "Tie Type", lty = "")+
  guides(color = 'none', fill = 'none')
ggsave("s1/output/exemplars have higher gpa.pdf", width = 5, height = 3, units = 'in')

# Network correlations
correlate_nets = function(net1, net2, directed = T){
  mode = ifelse(directed, "digraph", "graph" )
  adj1 = get.adjacency(net1, sparse = FALSE)
  adj2 = get.adjacency(net2, sparse = FALSE)
  return(sna::gcor(adj1, adj2, mode = mode))
}
correlate_nets(bf1, scw1)
correlate_nets(bf2, scw2)
correlate_nets(bf3, scw3)
correlate_nets(bf4, scw4)
bf1 %>% length()

# Significance testing
correlate_sig <- function(net1, net2, replications = 10000, seed = 36) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  stack <- array(dim = c(2, length(net1), length(net1)))
  stack[1, , ] <- get.adjacency(net1, sparse = FALSE)
  stack[2, , ] <- get.adjacency(net2, sparse = FALSE)
  cortest <- sna::qaptest(stack, sna::gcor, g1 = 1, g2 = 2, reps = replications) # 10K replications
  return(cortest)
}

p1 = correlate_sig(bf1, scw1, replications = 10)
p1$testval
p1$pvalue
p1$dist %>% quantile(c(.025,.975))

# Reciprocity
out_reciprocity <- edgelist %>% 
  mutate(to_from = map2_chr(to, from, ~paste(sort(c(.x,.y)), collapse = "_"))) %>%
  group_by(time, pn) %>%
  count(to_from) %>% 
  count(n) %>% 
  mutate(prop = nn/sum(nn)) %>% 
  mutate(tie_type = ifelse(n == 1, "directional", "reciprocal"))  %>% 
  select(time, type = pn, n = nn,tie_type, prop)

# plot reciprocity
out_reciprocity %>%
  mutate(
    type = ifelse(type == "bf", "Close Friends", "Exemplars"),
    tie_type = ifelse(tie_type == "directional", "Directional", "Reciprocal")
  ) %>%
  ggplot(aes(time, prop, group = paste(type, tie_type), color = type, lty = tie_type)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label = Ben::numformat(prop, 2)), vjust = 2, size = 2.5) +
  scale_color_brewer(palette = "Set1")+
  guides(color = 'none')+
  facet_wrap(~type)+
  theme(legend.position = c(.85, .5))+
  labs(x = "Time", y = "Proportion of Reciprocal Ties", color = "Tie Type", lty = "Tie Type")
ggsave('s1/figures/0000 reciprocity.pdf', width = 4.5, height = 2.5, units = 'in')

# Centrality correlations
out_centrality_cors = tibble(nets = list(bf1, bf2, bf3, bf4, scw1, scw2, scw3, scw4)) %>%
  mutate(
    time = c(1,2,3,4,1,2,3,4),
    type = c("bf","bf","bf","bf","scw","scw","scw","scw")
  ) %>% 
  mutate(
    eigen = map(nets, function(x) eigen_centrality(x)$vector),
    degree = map(nets, function(x) degree(x, mode = "in", loops = FALSE, normalized = TRUE)),
    closeness = map(nets, function(x) closeness(x, mode = "in")),
    pagerank = map(nets, function(x) page_rank(x, directed = TRUE)$vector),
    betweenness = map(nets, function(x) betweenness(x, directed = TRUE, normalized = TRUE)) 
  ) %>% 
  pivot_longer(eigen:betweenness, names_to = "measure", values_to = "value") %>% 
  select(-nets) %>%
  pivot_wider(names_from = "type", values_from = "value") %>% 
  unnest(bf, scw) %>% 
  group_by(time, measure) %>%
  summarise(
    cor = cor(bf, scw, use = "pairwise.complete.obs"),
    p.value = cor.test(bf, scw, use = "pairwise.complete.obs")$p.value,
    cor.hi = cor.test(bf, scw, use = "pairwise.complete.obs")$conf.int[2],
    cor.lo = cor.test(bf, scw, use = "pairwise.complete.obs")$conf.int[1]
  ) 

plot_centrality_cors = out_centrality_cors %>%
  ggplot(aes(time, cor, color = measure, fill = measure)) +
  geom_ribbon(aes(ymin = cor.lo, ymax = cor.hi), alpha = .3, color = NA) +
  # geom_errorbar(aes(ymin = cor.lo, ymax = cor.hi), width = .1, alpha = .2) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = Ben::numformat(cor)), vjust = 2, size = 2.5) +
  geom_text(aes(label = Ben::numformat(p.value, 3)), vjust = -1, size = 2) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  # remove legend for fill
  guides(fill = "none", color = "none") +
  theme(legend.position = c(.85, .85))+
  facet_wrap(~measure,nrow = 1)+
  labs(x = "Time", y = "Correlation", color = "Centrality Measure")
 ggsave("s1/figures/0000 centrality correlations.pdf", width = 6, height = 4, units = 'in')

# get attributes of neighbors ####
vertices = ego(a, order = 3, node = 1, mode = "out")[[1]]
setdiff(ego(a, 2)[[1]], ego(a,1)[[1]])
vertices[-1]
V(a)$name[vertices]
V(a)[1]

get_avg_value <- function(graph, node, delta, mode = "all", attribute = "value", exclusive = F) {
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
    neighbors  <- neighbors[-1]
    if (exclusive){
      neighbors <- setdiff(neighbors, ego(graph, order = delta-1, nodes = node, mode = mode)[[1]])
    }
    return(mean(vertex_attr(graph, attribute, neighbors), na.rm = TRUE))
  }
}




get_avg_value_time_type = function(node, delta, mode = "all", attribute = "value",time = "T1", type = "bf", exclusive = T){
  if(type == "bf" & time == "T1"){g = bf1}
  if(type == "bf" & time == "T2"){g = bf2}
  if(type == "bf" & time == "T3"){g = bf3}
  if(type == "bf" & time == "T4"){g = bf4}
  if(type == "scw" & time == "T1"){g = scw1}
  if(type == "scw" & time == "T2"){g = scw2}
  if(type == "scw" & time == "T3"){g = scw3}
  if(type == "scw" & time == "T4"){g = scw4}

  return(get_avg_value(g, node, delta, mode = mode, attribute = attribute, exclusive = exclusive))
  }


# make datasets ####
make_dataset = function(data,attribute, delta, mode, exclusive){
attr_long_test = data %>% 
  mutate(
    friend = map2_dbl(StudyID, time, ~get_avg_value_time_type(as.character(.x), delta, time = .y, type = "bf", attribute = attribute, mode =  mode, exclusive = exclusive)),
    exemplar = map2_dbl(StudyID, time, ~get_avg_value_time_type(as.character(.x), delta, time = .y, type = "scw", attribute = attribute, mode =  mode,exclusive = exclusive))
  )
  return(attr_long_test)
}

collapse_time = function(data){data %>% group_by(StudyID, school, female, eth, ell, sped, frpl) %>% summarize_all(mean, na.rm = TRUE) %>% select(-time) %>% ungroup()}

d = make_dataset(attr_long, "coregpa", 3, "out", exclusive = T) %>% collapse_time()
scale_dataset = function(d){d %>% mutate_at(vars(age:exemplar), scale)}

# run regressions ####
library(broom)
run_reg = function(d)
{d %>% 
  scale_dataset() %>%
  lm(scw_s ~ coregpa + friend + exemplar + school + female + eth + ell + sped + frpl + age, data = .)
}

# mode: out is people you nominate, in is people who nominate you, all is both
lms = expand_grid(
  delta = 1:7,
  attribute = c("coregpa", "avg_scw_tr"), 
  mode = c("out", "in", "all"),
  exclusive = c(T,F)
  ) %>% 
  mutate(
    data = pmap(list(attribute, delta,mode,exclusive), function(attribute, delta, mode,exclusive) make_dataset(attr_long, attribute,delta, mode, exclusive)),
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

write_rds(lms, "s1/output/lms.rds")

lms %>%
  filter(exclusive) %>%
  filter(mode != "all") %>% 
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
  facet_grid(attribute~exclusive~mode, scales = "free_y")+
  labs(x = "Distance from Ego", y = "Regression Coefficient")
ggsave('s1/figures/0000 regression.pdf', width = 6, height = 6, units = 'in')

lms %>% filter(attribute == "avg_scw_tr", delta == 1) %>% 
mutate(model = map(model, glance)) %>%
unnest(model)


# spatial auto regression

# Normalize matrix
normalize = function(matrix){return(matrix/rowSums(matrix))}
# Set isolates to zero
isolates_to_zero = function(matrix){matrix[is.na(matrix)] <- 0; return(matrix)}

m1 = both1 %>% get.adjacency() %>% as.matrix() %>% normalize() %>% isolates_to_zero()
m2 = both2 %>% get.adjacency() %>% as.matrix() %>% normalize() %>% isolates_to_zero()
m3 = both3 %>% get.adjacency() %>% as.matrix() %>% normalize() %>% isolates_to_zero()
m4 = both4 %>% get.adjacency() %>% as.matrix() %>% normalize() %>% isolates_to_zero()

# average all matrices
matrix = (m1 + m2 + m3 + m4)/4

library(spdep)
# Create listweights object
test.listwAd<-mat2listw(matrix, style = "W")

# Moran's I for outcome dependence
moran.test(
  attr_long %>% collapse_time() %>% select(scw_s) %>%drop_na() %>%pull(scw_s),
  test.listwAd, zero.policy=TRUE)

gg = make_graph_time_type(edgelist, attr_long, type_filter = "both",  time_filter = "T1")
# drop observations without scw_s
gg = delete.vertices(gg, V(gg)[is.na(V(gg)$scw_s)])

gg %>% 
  get.adjacency() %>% as.matrix() %>% normalize() %>% isolates_to_zero() %>% mat2listw(style = "W") %>% 
  moran.test(
    V(gg)$scw_s,
    .,
  zero.policy=TRUE)

list(lcc  = transitivity(gg, type = "local"), centr = eigen_centrality(gg)$vector)  %>% 
  as.data.frame() %>%as_tibble() %>% mutate(StudyID = V(gg)$name) %>% 
  ggplot(aes(lcc, centr))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm")


## longitudinal
# self control ~ selfcontrol_t-1 + friendgpa_t-1, + rolegpa_t-1 + controls

data = expand_grid(delta = 1, attribute = c("coregpa", "avg_scw_tr"), mode = c("out","in","all")) %>% 
  mutate(
    data = pmap(list(attribute, delta,mode), function(attribute, delta, mode) make_dataset(attr_long, attribute,delta, mode)))

lag = dplyr::lag
data$data[[1]] %>%
  arrange(StudyID) %>%
  mutate(lagsc = lag(scw_s), lagfriend = lag(friend), lagexemplar = lag(exemplar)) %>%
  ungroup() %>% 
  mutate_at(vars(age:lagexemplar), scale) %>%
  lfe::felm(scw_s ~ coregpa + lagfriend + lagexemplar + school + female + eth + ell + sped + frpl + age | 0| 0 | school, data = .) %>%
  # lm() %>%
  stargazer::stargazer(type = "text")
