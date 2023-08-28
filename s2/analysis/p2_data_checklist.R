# Data checklist: https://docs.google.com/document/d/1bdSqD7UkWOwNsbiu_8pQQo5l45s9YIb7Lkww01HY7sQ/edit
# latex table previewer http://www.tlhiv.org/ltxpreview/
# My approach is to work on a script, save the results, and then load them in a Rmarkdown for reporting. 

# Libraries
library(tidyverse)
library(qualtRics)
library(glue)
library(gt)

# Decisions
# drop double or single nominatinos or none
drop_double_nominators = "double"
# drop monotonics: none, loose (can be equal), strict (must be greater)
drop_non_monotonic = "loose"


# Gt theme
gt_theme = function(data, ...){
  data %>%
    tab_options(
    table.font.size = px(12),
    data_row.padding = px(0),
    row_group.padding = px(0),
    column_labels.padding = px(0)
  )
}

# Save your raw data, or better yet, read direclty from qualtrics ----

# IDs available at https://upenn.co1.qualtrics.com/Q/QualtricsIdsSection/IdsSection
raw = qualtRics::fetch_survey("SV_865OvT8hHfRoVXE", force_request = F)

data = raw %>% 
  # Drop people you don't want (previews, pilots, too young, attention checks...)
  filter(DistributionChannel == "anonymous") |> 
  filter(StartDate > lubridate::ymd_hms("2023-08-15 00:00:00")) |>
  filter(StartDate < lubridate::ymd_hms("2023-08-18 00:00:00")) %>%

  # Drop variables you wont need
  select(id = ResponseId,e_sc_self:suggestion_box)

data = data %>% 
  select(-ends_with("NPS_GROUP"),-matches("_DO_")) %>%
  mutate_at(vars(e_sc_self:sm_diff_friends), as.character) %>%
  pivot_longer(e_sc_self:sm_diff_friends) %>% 
  filter(!is.na(value)) %>% 
  mutate(condition = case_when(
    str_starts(name, "e_") ~ "exercise",
    str_starts(name, "d_") ~ "drinking",
    str_starts(name, "p_") ~ "punctuality",
    str_starts(name, "sm_") ~ "social media",
    T ~ "error"
  ),
  name = str_remove(name, "e_|d_|p_|sm_"), 
  name = case_when(
    name == "role_models_1" ~ "rolemodels_1",
    name == "role_models_2" ~ "rolemodels_2",
    T ~ name
  )) %>% 
  pivot_wider(names_from = name, values_from = value) 

colnames(data)

data  <- 
  data %>%
  # Rename variables to useful names
  rename(
    standard10 = standard10_1,
    standard5 = standard5_1,
    standard0 = standard0_1,
    friend1_minutes = `minutes_1`,
    friend2_minutes = `minutes_5`,
    exemplar1_minutes = `minutes_8`,
    exemplar2_minutes = `minutes_9`,
    self_minutes = minutes_6
    ) %>% 
    mutate_at(vars(sc_self, standard10, standard5, standard0, friend1_minutes:diff_rolemodels), as.numeric)
  
# Fix factor variables and other
data = data |> 
  mutate(bl_race = case_when(
    bl_ethnicity == "Yes" ~ "Other",
    bl_race_1 == "White" ~ "White",
    !is.na(bl_race_2) ~ "Other",
    !is.na(bl_race_3) ~ "Other",
    !is.na(bl_race_4) ~ "Other",
    !is.na(bl_race_5) ~ "Other",
    !is.na(bl_race_6) ~ "Other",
    T ~ "Other"
  )) |>  
  mutate(bl_gender = ifelse(bl_gender == "Other", NA, bl_gender)) |> 
  select(-c(bl_race_1:bl_ethnicity)) |> 
  mutate(
    bl_race = fct_inorder(bl_race),
    bl_gender = fct_inorder(bl_gender))
  
# Failed attention checks and other exclusions
# returns true if there is a problem

check_monotonic <- function(high, low, condition, strict=TRUE){
  if(condition == "exercise"){
    # If strict, 10 needs to exercise strictly more than 5
    if(strict){
      return(high > low)
    }
    # If not strict, 10 needs to exercise at least as much as 5
    else{
      return(high >= low)
    }
  } else {
    # If strict, 10 needs to drink strictly less than 5
    if(strict){
      return(high < low)
    }
    # If not strict, 10 needs to drink at most as much as 5
    else{
      return(high <= low)
    }
  }
}

check_non_monotonic  <- function(high, low, condition, strict=TRUE){
  !check_monotonic(high, low, condition, strict)
}

if(drop_non_monotonic == "loose"){
data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_non_monotonic, strict = FALSE),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_non_monotonic,strict = FALSE),
    e101 = pmap_lgl(list(standard10, standard0, condition), check_non_monotonic,strict = FALSE),
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e != 0) %>% 
  count(condition) %>% 
  print()

# non monotonic
data = data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_non_monotonic, strict = FALSE),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_non_monotonic,strict = FALSE),
    e101 = pmap_lgl(list(standard10, standard0, condition), check_non_monotonic,strict = FALSE)
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e == 0) |> 
  select(-e, -e105, -e51, -e101)
}

if(drop_non_monotonic == "strict"){
data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_non_monotonic, strict = TRUE),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_non_monotonic,strict = TRUE),
    e101 = pmap_lgl(list(standard10, standard0, condition), check_non_monotonic,strict = TRUE),
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e != 0) %>% 
  count(condition) %>% 
  print()

# non monotonic
data = data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_non_monotonic, strict = TRUE),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_non_monotonic,strict = TRUE),
    e101 = pmap_lgl(list(standard10, standard0, condition), check__non_monotonic,strict = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e == 0) |> 
  select(-e, -e105, -e51, -e101)
}


# final sample size after exclusions
data %>% 
  count(condition)


# invert scores - if not exercise, then invert (many drinks, many minutes on social media, and many minutes late is bad, many minutes of exercise is good)
# make all values positive. Add 30 to punctuality
data = data %>% 
  pivot_longer(c(
    "standard10":"standard0", 
    "self_minutes", "exemplar1_minutes":"exemplar2_minutes",
    'friend1_minutes':'friend2_minutes'
    )) %>% 
  mutate(value = case_when( 
    condition == "punctuality" ~ value*-1 + 30,
    condition == "drinking" ~ value*-1 + 15,
    condition == "social media" ~ value*-1 + 120,
    condition == "exercise" ~ value
  )) %>% 
  pivot_wider(names_from = name, values_from = value)

data = data %>% 
  select(
    id, condition, starts_with("bl"), sc_self, standard0, standard5, standard10,
    self_minutes, friend1_minutes, friend2_minutes, exemplar1_minutes, exemplar2_minutes, 
    friends_1, friends_2, rolemodels_1, rolemodels_2, 
    starts_with("diff"), suggestion_box) 

# Suggestions
data %>% select(suggestion_box) %>% 
  filter(!is.na(suggestion_box)) %>% 
  gt()
data = data %>% select(-suggestion_box)


# Let's look at names. Double nominations.
repeated = data %>% 
  select(id,friends_1, friends_2, rolemodels_1, rolemodels_2, matches("minutes")) %>% 
  mutate(
    friend1_in_rm = ifelse(friends_1 == rolemodels_1 | friends_1 == rolemodels_2, T, F), 
    friend2_in_rm = ifelse(friends_2 == rolemodels_1 | friends_2 == rolemodels_2, T, F),
    repeated = map2_dbl(friend1_in_rm, friend2_in_rm, ~ sum(c(.x, .y)))
    )

# counts
repeated %>% count(repeated) %>% gt()

# do the numbers match
data %>% 
  mutate(diff = case_when(
    friends_1 == rolemodels_1 ~ friend1_minutes - exemplar1_minutes,
    friends_1 == rolemodels_2 ~ friend1_minutes - exemplar2_minutes,
    friends_2 == rolemodels_1 ~ friend2_minutes - exemplar1_minutes,
    friends_2 == rolemodels_2 ~ friend2_minutes - exemplar2_minutes,
    T ~ NA
  )) %>%
  mutate(diff = ifelse(diff == 0, "same", "different")) %>% 
  count(diff) %>% gt::gt()

# DECISION
if(drop_double_nominators == "double"){
  ids_to_drop = repeated %>% filter(repeated == 2) %>% pull(id)
  data = data %>% filter(!id %in% ids_to_drop)
}
if(drop_double_nominators == "single"){
  ids_to_drop = repeated %>% filter(repeated > 0) %>% pull(id)
  data = data %>% filter(!id %in% ids_to_drop)
}

# drop names now
data = data %>% 
  select(-c(friends_1:rolemodels_2))

# Creating scores for scales (this process does not apply to scoring items where there is a specific correct answer, such as a math problem) ---- 
data = data %>% clnR2::cv_bake(friend_minutes, quos(friend1_minutes, friend2_minutes))
data = data %>% clnR2::cv_bake(exemplar_minutes, quos(exemplar1_minutes, exemplar2_minutes))


# Check correlations between going inside the quos
data %>% 
  select(self_minutes,friend1_minutes, friend2_minutes, exemplar1_minutes, exemplar2_minutes) %>% Ben::harcor() %>% gt()


data %>% 
  select(condition, matches("minutes")) %>% 
  group_by(condition) %>%
  summarise(
    exemplars = cor(exemplar1_minutes, exemplar2_minutes, use = "pairwise.complete.obs"),
    friends = cor(friend1_minutes, friend2_minutes, use = "pairwise.complete.obs"),
    cross11 = cor(exemplar1_minutes, friend1_minutes, use = "pairwise.complete.obs"),
    cross12 = cor(exemplar1_minutes, friend2_minutes, use = "pairwise.complete.obs"),
    cross21 = cor(exemplar2_minutes, friend1_minutes, use = "pairwise.complete.obs"),
    cross22 = cor(exemplar2_minutes, friend2_minutes, use = "pairwise.complete.obs")
  ) %>%
  mutate(avg_cross = (cross11 + cross12 + cross21 + cross22)/4) %>% 
  select(-cross11, -cross12, -cross21, -cross22) %>% 
  gt() %>% 
  fmt_number(columns = vars(exemplars, friends, avg_cross), decimals = 2)  %>%
  gt_theme()

data = data %>% 
  select(
    id, condition, starts_with("bl"), sc_self, standard0, standard5, standard10,
    self_minutes, friend_minutes, exemplar_minutes,
    # friends_1, friends_2, rolemodels_1, rolemodels_2, 
    starts_with("diff")) 

#Histogram for all vars
data %>% 
  select(condition,standard10:standard0, self_minutes:exemplar_minutes) %>%
  pivot_longer(-condition) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(value))+
  geom_histogram(bins = 10)+
  labs(x = NULL, y = NULL)+
  facet_wrap(condition~name, scales = "free",nrow = 4) 
ggsave("s2/p2/figures/histograms.png", width = 10, height = 10)

# Eyeball the data. Do you see any gaping “holes” (missing data)?
missing_col = data %>% is.na %>% colSums()
missing_row = data %>% is.na %>% rowSums()

missing_col;missing_row

# Are there redundant cases? Use the SPSS function to verify there are not.
if(nrow(data %>% unique()) == nrow(data)){
  print("No redundant cases")
} else {
  print("Redundant cases")}

# Is the sample size roughly what you expect?
print(glue("There are: {data %>% nrow()} cases"))

# Run frequency counts for all items. Are any “impossible” values evident (e.g., a response of “9” on a 1 through 8 scale).
# Are missing values properly encoded as missing – not as zeros? If a discreet number is used to signal “missingness” then make sure SPSS knows this.
table_describe = data %>% psych::describe() |> as.data.frame() |> rownames_to_column() |> gt::gt() |> gt::fmt_number() %>% gt_theme()


# Are the variables coded correctly?
# i. 	The duckworth lab convention is to code gender this way: male = 0, female =1 (the variable is often called female so that a 1 signifies an affirmative answer to the question “is this a female?”)
data = data %>% 
  mutate(female = case_when(bl_gender == "Male" ~ 0,
                                          bl_gender == "Female" ~ 1,
                                          T ~ NA),
         white = case_when(
           bl_race == 'White' ~ 1,
           T ~ 0
         ))


# Optionally, if there is a lot of missing data, make a decision rule about whether to create scores for participants who skipped questions (e.g., only score when 80% or more items were answered).

# Explore the distributions of all your variables ----

#Histograms
plot_histograms = data %>% 
  group_by(condition) %>%
  select_if(is.numeric) %>% 
  select(-female, -white, -bl_age) %>%
  pivot_longer(-condition) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(value, fill = condition))+
  geom_histogram(bins = 10)+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(condition~name, scales = "free", nrow = 8) #Histogram for all vars
ggsave("s2/p2/figures/histograms_condition.png", width = 10, height = 10)

plot_minutes = data %>% 
  select(condition, friend_minutes, exemplar_minutes, self_minutes) %>%
  pivot_longer(-condition) %>%
  mutate(name = fct_inorder(name)) %>%
  group_by(condition, name) %>% 
  mutate(value = scale(value)) %>%
  ggplot(aes(value, color = condition))+
  geom_density()+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~name, scales = "free") #Histogram for all vars
ggsave("s2/p2/figures/minutes.png", width = 7, height = 4)

#Bar charts
plot_bars = data %>%
  mutate(
    bl_income = factor(bl_income, ordered = F),
    bl_education = factor(bl_education, ordered = F)) %>%
  select_if(is.factor) |> 
  pivot_longer(everything()) |> 
  ggplot(aes(value))+
  geom_bar() +
  coord_flip()+
  facet_wrap(~name, scales = "free",nrow = 1) #Histogram for all vars
ggsave("s2/p2/figures/bars.png", width = 12, height = 4)

# Transformations
data = data |> 
  mutate(
    friend_minutes_sqrt = sqrt(friend_minutes),
    exemplar_minutes_sqrt = sqrt(exemplar_minutes)
    )

plot_histograms = data %>% 
  group_by(condition) %>%
  select_if(is.numeric) %>% 
  select(-female, -white, -bl_age) %>%
  pivot_longer(-condition) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(value, fill = condition))+
  geom_histogram(bins = 10)+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(condition~name, scales = "free", nrow = 8) #Histogram for all vars

# Explore and then clarify how variables are related to each other ----
# Bivariate correlations

table_correlations = data %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()

table_correlations_exercise = data %>%filter(condition == "exercise") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_social_media = data %>%filter(condition == "social media") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_drinking = data %>%filter(condition == "drinking") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_punctuality = data %>%filter(condition == "punctuality") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()


# Determine which are your most important variables and run scatterplots for each of them. Check for a linear relationship. If the data are not generally linear the bivariate correlations you just completed won’t reveal the true relationship between the variables. From this point on if the variables aren’t linear they should be examined using statistical methods that account for their true shape.

# 1. friends and role model min
library(ggpubr)
plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_minutes, exemplar_minutes))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~condition, scales = "free")+
  # add x = y dashed line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  stat_cor(method = "pearson", label.x.npc = 1, label.y.npc = 0, cor.coef.name = "r", hjust = 1, r.accuracy = .01, p.accuracy = .001, digits = 2)+
  labs(x = "Friend Behavior",y = "Role Model Behavior")
plot_sc_friends_exemplars
ggsave("s2/p2/figures/scatterplot_friends_exemplars.png", width = 7, height = 4)

plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_minutes_sqrt, exemplar_minutes_sqrt))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~condition, scales = "free")+
  stat_cor(method = "pearson", label.x.npc = 1, label.y.npc = 0, cor.coef.name = "r", hjust = 1, r.accuracy = .01, p.accuracy = .001)+
  labs(x = "Friend Minutes of Exercise",y = "Role Model Minutes of Exercise")
plot_sc_friends_exemplars

# effect of friends and role models on self control, behavior, and standards
library(gt)
data %>% 
  group_by(condition) %>% 
  select(id,sc_self, self_minutes, standard10:standard0, friend_minutes, exemplar_minutes) %>% 
  pivot_longer(self_minutes:exemplar_minutes) %>% 
  mutate(value = case_when(
    condition == "exercise" ~ value,
    condition != "exercise" ~ value * -1
  )) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  group_by(condition) %>%
  pivot_longer(friend_minutes:exemplar_minutes, names_to = "pred_name", values_to = "pred_value") %>%
  pivot_longer(sc_self:standard0, names_to = "outcome_name", values_to = "outcome_value") %>% 
  group_by(condition, pred_name, outcome_name) %>%
  summarise(
    r = cor(pred_value, outcome_value, use = "pairwise.complete.obs"),
    p = cor.test(pred_value, outcome_value, use = "pairwise.complete.obs")$p.value
  ) %>% 
  pivot_wider(names_from = pred_name, values_from = c(r,p)) %>% 
  mutate(difference = `r_exemplar_minutes` - `r_friend_minutes`) %>% 
  gt()  %>% 
  fmt_number() %>% 
  data_color(columns = difference, method = "numeric", 
  colors = scales::col_numeric(
      palette = c("red","white", "blue"),
      domain = c(-.80, 0,.80)
    )) %>%
    tab_header("Bivariate correlations between friend and exemplar behavior and self-control, behavior and standards") %>% 
    gt_theme()

# replicating reference bias

# average levels of sc across condition
data %>% group_by(condition) %>% 
  summarise(mean_cl_normal(sc_self)) %>% 
  ggplot(aes(condition, y))+
  geom_col()+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .3)+
  labs(x = NULL, y = 'Mean Self-Control (0-10)')
ggsave("s2/p2/figures/mean_sc_across_conditions.png", width = 5, height = 3)

data %>% 
  group_by(condition) %>%
  nest() %>%
  mutate(
    lm_model = map(data, ~lm(sc_self ~ friend_minutes + exemplar_minutes + self_minutes, data = .x)),    
    lm_beta = map(lm_model, lm.beta::lm.beta),
    tidy = map(lm_beta, broom::tidy)
  ) %>% 
  unnest(tidy) %>% 
  select(condition, term, estimate, std_estimate, std.error, statistic, p.value) %>% 
  gt()  %>% 
  fmt_number() %>%
  tab_header('Replicating Reference Bias') %>% 
  gt_theme()

compute_wald <- function(model) {car::linearHypothesis(model, "friend_minutes = exemplar_minutes") %>% broom::tidy()}
models = data %>% 
  group_by(condition) %>%
  mutate_at(vars(sc_self, friend_minutes, exemplar_minutes, self_minutes), scale) %>%
  nest() %>%
  mutate(
    lm_model = map(data, ~lm(sc_self ~ friend_minutes + exemplar_minutes + self_minutes, data = .x)),    
    tidy = map(lm_model, broom::tidy),
    wald = map(lm_model, compute_wald)
  ) %>% 
  unnest(wald)


models  %>% 
  pull(lm_model) %>% 
  stargazer::stargazer(
    star.cutoffs = c(.05, .01, .001), 
    # omit.stat = c("F", "ser"),
    column.labels = models$condition,
    add.lines = list(
      c("Difference between friend and role model beta",models$estimate %>% Ben::numformat(3)),
      c("p for difference",models$p.value %>% Ben::numformat(3))
    ),
    out = "s2/p2/output/refbias.tex"
    )

data.z = data %>% group_by(condition)  %>% mutate_at(vars(ends_with("minutes")), scale) %>% ungroup %>% 
  mutate(condition = factor(
    condition,
     levels = c('exercise', 'drinking', 'punctuality', 'social media'), 
     labels  = c('exercise', 'drinking', 'punctuality', 'social media')))
# contrasts(data.z$condition) <- contr.sum(levels(data.z$condition))
model = lm(sc_self ~ condition*friend_minutes + condition*exemplar_minutes + condition*self_minutes, data = data.z)
car::linearHypothesis(model,"friend_minutes = exemplar_minutes" )

summary(model)
# Calculate the EMMs
library(emmeans)
emm_friends <- emmeans(model, specs = ~ condition*friend_minutes, at = list(friend_minutes = seq(-3, 3, by = 0.5)))  %>% as.data.frame()
emm_exemplars <- emmeans(model, specs = ~ condition*exemplar_minutes, at = list(exemplar_minutes = seq(-3, 3, by = 0.5)))%>% as.data.frame()

# Plot
ggplot() +
  geom_line(data = emm_friends, aes(x = friend_minutes, y = emmean, color = condition, linetype = "Friends")) +
  geom_ribbon(data = emm_friends, aes(x = friend_minutes, ymin = lower.CL, ymax = upper.CL, fill = condition), alpha = .1)+
  geom_line(data = emm_exemplars, aes(x = exemplar_minutes, y = emmean, color = condition, linetype = "Exemplars")) +
  geom_ribbon(data = emm_exemplars, aes(x = exemplar_minutes, ymin = lower.CL, ymax = upper.CL, fill = condition), alpha = .1)+
  labs(y = "Self Control", x = "Standardized Referent Behavior", color = "Condition", linetype = "Reference Group")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  guides(color = "none", fill = "none")+
  # ggpubr::add_summary()+
  facet_grid(~condition)
ggsave("s2/p2/figures/refbias.png", width = 6, height = 3)

model %>%
  stargazer::stargazer(
    star.cutoffs = c(.05, .01, .001), 
    omit.stat = c("F", "ser"),
    no.space = T,
    out = "s2/p2/output/refbias.tex"
    )


# predicting standards
lm_standards = data %>% 
  group_by(condition) %>%
  mutate_at(vars(friend_minutes, exemplar_minutes, self_minutes, standard0, standard5, standard10), scale)  %>%
  pivot_longer(standard10:standard0) %>% 
  group_by(condition, name) %>%
  nest() %>%
  mutate(
    lm_model = map(data, ~lm(value ~ friend_minutes + exemplar_minutes + self_minutes, data = .x)),    
    lm_beta = map(lm_model, lm.beta::lm.beta),
    tidy = map(lm_beta, broom::tidy, conf.int = T)
  ) %>% 
  unnest(tidy) 


lm_standards  %>% 
  filter(name == "standard10") %>%
  select(condition, term, estimate, std_estimate, std.error, statistic, p.value) %>% 
  gt()  %>% 
  fmt_number()

lm_standards %>% 
  ungroup() %>%
  filter(term != "(Intercept)") %>%
  filter(term != "self_minutes") %>%
  mutate(
    name = parse_number(name),
    term = ifelse(term == "friend_minutes", "Friend", "Exemplar"),
  ) %>%
  ggplot(aes(name, estimate, color = term, group = term))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = c(0, 5, 10), labels = c("a\nlittle", "medium", "a\nlot"))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2, alpha = .2)+ 
  facet_wrap(~condition, nrow = 1)+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Standard", y = "Beta", color = "Referent")
ggsave("s2/p2/figures/standards.png", width = 7, height = 3)
# some lavaan
library(lavaan)

model = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + fb*friend_minutes + eb*exemplar_minutes
standard10 ~ fs*friend_minutes + es*exemplar_minutes
friend_minutes~~exemplar_minutes"
data.z = data %>% group_by(condition)  %>% mutate_if(is.numeric, scale) %>% ungroup 
fit_constrain = sem(model, data = data.z, group = "condition", group.equal = c("regressions"))
fit = sem(model, data = data.z, group = "condition")
fit %>% summary(standardized = T, fit.measures = T)
fit_constrain %>% summary(standardized = T, fit.measures = T)
modindices(fit) %>% arrange(mi)

# compare models
anova(fit, fit_constrain)

# Wald test
test_result <- lavTestWald(fit, constraints = "fs - es == 0")
test_result

test_result <- lavTestWald(fit, constraints = "fb - eb == 0")
test_result


print(test_result)

parameterEstimates(fit, standardized = T) %>% 
  filter(op %in% c("~", "~~")) %>% 
  select(lhs, rhs, est, std.all, pvalue, group) %>% 
  as_tibble() %>%
  mutate(group = case_match(
    group,
    1 ~ "punctuality",
    2~ "social media",
    3~"exercise" ,
    4~"drinking"
  )) %>%
  filter(lhs != rhs) %>%
  pivot_wider(names_from = group, values_from = c(est, std.all, pvalue), names_glue = "{group}_{.value}") %>%
  select(lhs, rhs, starts_with("exercise"), starts_with("punctuality"), starts_with("drinking"), starts_with("social"))  %>%  
  gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number() %>%
  gt_theme()

data %>% 
  select(starts_with("standa")) %>% 
  Ben::harcor()

# anova for difficulty
long_data = data %>% 
  select(id, condition,  starts_with("diff")) %>% 
  pivot_longer(starts_with('diff')) %>%
  as.data.frame()

result = long_data %>%
  ez::ezANOVA(value, wid = id, within = name, detailed = T, between = condition)
result

library(emmeans)
model <- lme4::lmer(value ~ name * condition + (1|id), data = long_data)
posthoc <- emmeans(model, pairwise ~ name * condition, adjust = "tukey")

contrasts = posthoc$contrasts %>% 
  as_tibble() %>% separate(contrast, c("l","r"), " - ") %>%
  separate(l, c("name", "condition"), " ") %>%
  separate(r, c("name2", "condition2"), " ") %>%
  filter(condition == condition2) %>%
  filter(name != name2) %>% 
  filter(name2 != "diff_self") %>% 
  mutate(
    condition = ifelse(condition == "social","social media", condition),
    label = glue("difference = {round(estimate, 2)}\np = {round(p.value, 3)}")
    )


print(posthoc)

posthoc$emmeans %>% 
  as.data.frame() %>%
  mutate(name = case_match(
    name, 
    'diff_friends' ~ "Friends",
    'diff_rolemodels' ~ "Exemplar",
    'diff_self' ~ "Self"
    )) %>%
  ggplot(aes(name, emmean, fill = condition))+
  geom_col()+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = .2))+
  facet_wrap(~condition, scales = 'free')+ 
  scale_fill_brewer(palette = "Set1")+
  labs(x = NULL, y = "Difficulty rating (0-10)")+
  geom_text(aes(label = label, x = 1, y = 1), data = contrasts)+
  guides(fill = "none")
# In all cases, friends and role models are similar enough
ggsave("s2/p2/figures/p2_plot_diffs.png", width = 10, height = 5)

plot_main_sqrt = data |> 
  group_by(condition) %>%
  select(friend_min_sqrt, exemplar_minutes_sqrt, starts_with("standar"), sc_self) |>  
  pivot_longer(2:3) |> 
  pivot_longer(2:4, names_to = "outcome", values_to = "outcome_value") |> 
  mutate(
    name = ifelse(name == "friend_min_sqrt", "Friend Minutes","Role Model Minutes"),
    outcome = case_when(
      outcome == "standard0" ~ "Standard - 0",
      outcome == "standard5" ~ "Standard - 5",
      outcome == "standard10" ~ "Standard - 10",
      outcome == "sc_self" ~ "Self-reported self-control"
    ),
    outcome = fct_inorder(outcome) |> fct_rev()
  ) |> 
  ggplot(aes(value, outcome_value, color = name))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', se = T)+
  facet_wrap(condition~outcome, scales = "free", nrow = 4)+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Peer Minutes of Exercise - square-root", y = "Outcome Value", color = "")
plot_main_sqrt

# Bendy plot
plot_main_bendy = data |> 
  group_by(condition) %>%
  select(friend_min, exemplar_min, starts_with("standar"), sc_self) |>  
  pivot_longer(2:3) |> 
  pivot_longer(2:5, names_to = "outcome", values_to = "outcome_value") |> 
  mutate(
    name = ifelse(name == "friend_min", "Friend Minutes","Role Model Minutes"),
    outcome = case_when(
      outcome == "standard0" ~ "Standard - 0",
      outcome == "standard5" ~ "Standard - 5",
      outcome == "standard10" ~ "Standard - 10",
      outcome == "sc_self" ~ "Self-reported self-control"
    ),
    outcome = fct_inorder(outcome) |> fct_rev(),
    value_sqrt = sqrt(value)
  ) |> 
  group_by(outcome, name) |>
  nest() %>%
  mutate(
    model = map(data, ~lm(outcome_value ~ I(sqrt(value)), data = .x)),
    aug = map(model, ~broom::augment(.x, interval = 'confidence'))
  ) %>%
  unnest(aug) %>%
  mutate(value = `I(sqrt(value))`^2) %>%
  # filter(outcome != "Self-reported self-control") %>%
  ggplot(aes(value, outcome_value, color = name))+
  geom_point(alpha = .5)+
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = name), alpha = .2, color = NA)+
  geom_line(aes(y = .fitted), alpha = 1, linewidth = 1)+
  facet_wrap(condition~outcome, scales = "free", nrow = 4)+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1", guide = F)+
  labs(x = "Peer Minutes of Exercise", y = "Outcome Value", color = "")
plot_main_bendy
# Analyze

# Outliers
Ben::No_Out(data,data %>% select_if(is.numeric),alpha = .001) #Mahalanobis outliers
table_outliers = data %>%
group_by(condition) %>%
  mutate_if(is.numeric,scale) %>% 
  select_if(is.numeric) %>%
  psych::describe() |> 
  as.data.frame() |> 
  filter(min < -3 | max > 3) |> 
  rownames_to_column() |> 
  gt::gt() |> 
  gt::tab_footnote("We are not eliminating data based on outliers at the moment. Also, multivariate outliers were not computable (Too little data)") |> 
  gt::fmt_number()#Check for univariate outliers (Z scores > 3)

# lms
outcomes = c("sc_self", "standard10", "standard5","standard0", 'self_minutes')
sqrt = c(T,F)
controls = c(T,F)

run_lm = function(outcome, sqrt, controls, data = data){
  formula = glue::glue("{outcome} ~ friend_minutes+ exemplar_min")
  if(sqrt){formula = glue::glue("{outcome} ~ friend_min_sqrt + exemplar_minutes_sqrt")}
  if (controls){formula = glue::glue("{formula} + bl_age + bl_gender + bl_race")}
  
  # lm = lm(formula, data = data)
  return(formula)
}

data.z = data |> mutate_if(is.numeric, scale)

lms = expand_grid(outcomes, sqrt, controls) |> 
  mutate(formula = pmap(list(outcomes, sqrt, controls),run_lm)) |> 
  unnest(formula) |> 
  mutate(lm = map(formula, function(x){lm(as.formula(x), data = data |> mutate_if(is.numeric, scale))})) 

# list of models
models <- lms$lm

# new labels
labels <- lms$outcomes

# change the call of each model
for (i in 1:length(models)) {
  models[[i]]$call[[2]] <- as.name(labels[i])
}

# use stargazer
table_regressions = stargazer::stargazer(models,
          type = "latex",
          dep.var.labels.include = T)

output_regressions = lms |>
  mutate(tidy = map(lm, broom::tidy)) |> 
  unnest(tidy)

# Do standards predict self-ratings
models_sc = list(
lm(sc_self ~ standard10 + standard5 + standard0, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_min, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_minutes_sqrt, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_minutes + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_minutes_sqrt + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes + exemplar_min, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_minutes_sqrt, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_minutes + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_minutes_sqrt + bl_gender + bl_age + bl_race, data.z)
)

table_sc_regressions = stargazer::stargazer(models_sc, type = "text")

raw %>% slice_tail(n = 160) %>% 
select(`Duration (in seconds)`) %>% 
summarise_all(median)

save.image(file = "Output_p2.rda")


raw %>% select(suggestion_box) %>% 
filter(!is.na(suggestion_box)) %>% 
gt::gt()


mtcars %>% 
  mutate(am = as.factor(am)) %>%
  lm(mpg ~ disp + am:1, data = .)
