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
raw = qualtRics::fetch_survey("SV_5dREmhdIqSuJUG2", force_request = TRUE)

write_rds(raw, "s2/data/raw.rds")

data = raw %>% 
  # Drop people you don't want (previews, pilots, too young, attention checks...)
  filter(DistributionChannel == "anonymous") |> 
  filter(StartDate > lubridate::ymd_hms("2023-09-25 00:00:00")) |>
  filter(StartDate < lubridate::ymd_hms("2023-09-27 00:00:00")) %>%

  # Drop variables you wont need
  select(id = ResponseId,ex_sc_self:suggestion_box)


original_n = data %>% nrow()

colnames(data)

data = data %>% rename(
  p_diff_rolemodels1 = `p_diff_rolemodels1...78`, 
  p_diff_rolemodels2 = `p_diff_rolemodels1...80`,
  p_diff_friends1 = p_diff_friends,
  ex_minutes_f1 = ex_minutes_1,
  ex_minutes_f2 = ex_minutes_5,
  ex_minutes_rm1 = ex_minutes_8,
  ex_minutes_rm2 = ex_minutes_9,
  ex_minutes_self = ex_minutes_6,
  p_minutes_f1 = p_minutes_1,
  p_minutes_f2 = p_minutes_5,
  p_minutes_rm1 = p_minutes_8,
  p_minutes_rm2 = p_minutes_9,
  p_minutes_self = p_minutes_6,
  b_minutes_f1 = b_minutes_1,
  b_minutes_f2 = b_minutes_5,
  b_minutes_rm1 = b_minutes_8,
  b_minutes_rm2 = b_minutes_9,
  b_minutes_self = b_minutes_6,
  sav_minutes_f1 = sav_minutes_1,
  sav_minutes_f2 = sav_minutes_5,
  sav_minutes_rm1 = sav_minutes_8,
  sav_minutes_rm2 = sav_minutes_9,
  sav_minutes_self = sav_minutes_6,
  eat_minutes_f1 = eat_minutes_1,
  eat_minutes_f2 = eat_minutes_2,
  eat_minutes_self = eat_minutes_3,
  eat_minutes_rm1 = eat_minutes_4,
  eat_minutes_rm2 = eat_minutes_5,
  sm_minutes_f1 = sm_minutes_1,
  sm_minutes_f2 = sm_minutes_2,
  sm_minutes_rm1 = sm_minutes_4,
  sm_minutes_rm2 = sm_minutes_5,
  sm_minutes_self = sm_minutes_3,
  )

data = data %>% 
  select(-ends_with("NPS_GROUP"),-matches("_DO_")) %>%
  select(-matches("NPS_GROUP")) %>%
  mutate_at(vars(ex_sc_self:sm_diff_rolemodels2), as.character) %>%
  pivot_longer(ex_sc_self:sm_diff_rolemodels2) %>% 
  filter(!is.na(value)) %>% 
  mutate(condition = case_when(
    str_starts(name, "eat_") ~ "healthy eating",
    str_starts(name, "sav_") ~ "saving",
    str_starts(name, "sm_") ~ "social media",
    str_starts(name, "b_") ~ "bedtime",
    str_starts(name, "ex_") ~ "exercise",
    str_starts(name, "p_") ~ "punctuality",
    T ~ "error"
  ),
  name = str_remove(name, "eat_|sav_|sm_|b_|ex_|p_"), 
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
    friend1_minutes = `minutes_f1`,
    friend2_minutes = `minutes_f2`,
    exemplar1_minutes = `minutes_rm1`,
    exemplar2_minutes = `minutes_rm2`,
    self_minutes = minutes_self
    ) %>% 
    mutate_at(vars(sc_self, importance_self, standard10, standard5, standard0, friend1_minutes:diff_rolemodels2), as.numeric)
  
# Fix factor variables and other
data %>% filter(bl_gender == "Other")

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
  mutate(bl_gender = ifelse(bl_gender == "Other", NA, as.character(bl_gender))) |> 
  select(-c(bl_race_1:bl_ethnicity)) |> 
  mutate(
    bl_race = fct_inorder(bl_race),
    bl_gender = fct_inorder(bl_gender))
  
# Look at some data for monotonoic
data %>% 
  select(condition, standard10, standard5, standard0)

# Failed attention checks and other exclusions
# returns true if there is a problem

check_monotonic <- function(high, low, condition, strict=TRUE){
  if(!condition %in% c("bedtime", "punctuality", "social media")){
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
  select(id, starts_with("bl"), suggestion_box, condition, sc_self, importance_self, standard10, standard5, standard0, friends_1:diff_rolemodels2) %>%
  pivot_longer(c(
    "standard10":"standard0", 
    "self_minutes", "exemplar1_minutes":"exemplar2_minutes",
    'friend1_minutes':'friend2_minutes'
    )) %>% 
  # DOUBLE CHECK VALUES TO SUBTRACT
  mutate(value = case_when( 
    condition == "healthy eating" ~ value,
    condition == "saving" ~ value,
    condition == "exercise" ~ value,
    condition == "bedtime" ~ value*-1 + 240,
    condition == "punctuality" ~ value*-1 + 30,
    condition == "social media" ~ value*-1 + 120,
  )) %>% 
  pivot_wider(names_from = name, values_from = value)

data = data %>% 
  select(
    id, condition, starts_with("bl"), sc_self, standard0, standard5, standard10,
    self_minutes, friend1_minutes, friend2_minutes, exemplar1_minutes, exemplar2_minutes, 
    friends_1, friends_2, rolemodels_1, rolemodels_2, 
    starts_with("diff"), importance_self,suggestion_box) |> 
  mutate(importance_self = as.numeric(importance_self))

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

repeated  %>% filter(repeated > 0)

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
data = data %>% clnR2::cv_bake(diff_friends, quos(diff_friends1, diff_friends2))
data = data %>% clnR2::cv_bake(diff_rolemodels, quos(diff_rolemodels1, diff_rolemodels2))

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

# distribution of friend and role model minutes
data %>% 
  select(condition, friend1_minutes, friend2_minutes, exemplar1_minutes, exemplar2_minutes) %>% 
  pivot_longer(-condition)  %>% 
  mutate(name = str_remove(name, "_minutes"),
         name = tm::removeNumbers(name)) %>%
  group_by(condition) %>%
  summarise(
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    min = min(value, na.rm = T),
    max = max(value, na.rm = T),
    min3sd = mean - 2.5*sd,
    max3sd = mean + 2.5*sd
  )

# Dropping names now
data = data %>% 
  select(
    id, condition, starts_with("bl"), sc_self, standard0, standard5, standard10,
    self_minutes, friend_minutes, exemplar_minutes,
    # friends_1, friends_2, rolemodels_1, rolemodels_2, 
    starts_with("diff"),
    importance_self) 

#Histogram for all vars
data %>% 
  select(condition,standard10:standard0, self_minutes:exemplar_minutes) %>%
  pivot_longer(-condition) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(value))+
  geom_histogram(bins = 10)+
  labs(x = NULL, y = NULL)+
  facet_wrap(condition~name, scales = "free",nrow = 4) 
ggsave("s2/figures/histograms.png", width = 10, height = 10)

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
ggsave("s2/figures/histograms_condition.png", width = 10, height = 10)

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
ggsave("s2/figures/minutes.png", width = 7, height = 4)

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
ggsave("s2/figures/bars.png", width = 12, height = 4)

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
table_correlations_healthy_eating = data %>%filter(condition == "healthy eating") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_saving = data %>%filter(condition == "saving") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_bedtime = data %>%filter(condition == "bedtime") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_punctuality = data %>%filter(condition == "punctuality") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_social_media = data %>%filter(condition == "social media") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()

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
ggsave("s2/figures/scatterplot_friends_exemplars.png", width = 7, height = 4)

plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_minutes_sqrt, exemplar_minutes_sqrt))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~condition, scales = "free")+
  stat_cor(method = "pearson", label.x.npc = 1, label.y.npc = 0, cor.coef.name = "r", hjust = 1, r.accuracy = .01, p.accuracy = .001)+
  labs(x = "Friend Behavior",y = "Role Model Behavior", caption = "Square root transformation")
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
ggsave("s2/figures/mean_sc_across_conditions.png", width = 5, height = 3)

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
    out = "s2/output/refbias.tex"
    )

data.z = data %>% group_by(condition)  %>% mutate_at(vars(ends_with("minutes")), scale) %>% ungroup %>% 
  mutate(condition = factor(
    condition,
     levels = c('healthy eating', 'saving', 'work', 'bedtime'), 
     labels  = c('healthy eating', 'saving', 'work', 'bedtime')))
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
ggsave("s2/figures/refbias.png", width = 6, height = 3)

model %>%
  stargazer::stargazer(
    star.cutoffs = c(.05, .01, .001), 
    omit.stat = c("F", "ser"),
    no.space = T,
    out = "s2/output/refbias_2.tex"
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
ggsave("s2/figures/standards.png", width = 7, height = 3)
# some lavaan
library(lavaan)

model_unconstrained = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + friend_minutes + exemplar_minutes
standard10 ~ friend_minutes + exemplar_minutes
friend_minutes~~exemplar_minutes"

model_constrained = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + fb*friend_minutes + eb*exemplar_minutes
standard10 ~ fs*friend_minutes + es*exemplar_minutes
friend_minutes~~exemplar_minutes"

data.z = data %>% group_by(condition)  %>% mutate_if(is.numeric, scale) %>% ungroup 
write_rds(data.z, "s2/data/lavaan_data.rds")
fit_constrain = sem(model_constrained, data = data.z, group = "condition", group.equal = c("regressions"))
fit_medium_constrain = sem(model_constrained, data = data.z, group = "condition")
fit = sem(model_unconstrained, data = data.z, group = "condition")

fit %>% summary(standardized = T, fit.measures = T)
fit_constrain %>% summary(standardized = T, fit.measures = T)
modindices(fit) %>% arrange(mi)

# comapre cfi, tli, rmsea and srmr of fit constrain and fit
fitmeasures = fitmeasures(fit)[c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue")] %>% 
enframe() %>% 
rename(fit = value) %>%
left_join(
  fitmeasures(fit_constrain)[c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue")] %>% 
    enframe() %>% 
    rename(fit_constrain = value)
) %>% 
left_join(
  fitmeasures(fit_medium_constrain)[c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue")] %>% 
    enframe() %>% 
    rename(fit_medium_constrain = value)
)

fitmeasures %>% 
slice(1) %>% 
select(2,4,3) %>%
pivot_longer(1:3) %>% 
mutate(name = case_match(
  name,
  "fit" ~ "All parameters freely estimated",
  "fit_medium_constrain" ~ "Friends and exemplars to\nbehavior and standards constrained",
  "fit_constrain" ~ "All regressions\nconstrained"
)) %>%
mutate(name = fct_inorder(name)) %>%
ggplot(aes(name,value))+
geom_col()+ 
coord_cartesian(ylim = c(.8,1))+ 
geom_text(aes(label = Ben::numformat(value)), vjust = -1)+
labs(x = NULL, y = "Model Fit (CFI)")
ggsave("s2/figures/model_fit.png", width = 7, height = 3)

# compare models
anova_constrain = anova(fit, fit_constrain)
constrain_p = anova_constrain$`Pr(>Chisq)`[[2]] %>% Ben::formatps()

anova_constrain = anova(fit, fit_medium_constrain)
constrain_p = anova_constrain$`Pr(>Chisq)`[[2]] %>% Ben::formatps()

anova_constrain = anova(fit_medium_constrain, fit_constrain)
constrain_p = anova_constrain$`Pr(>Chisq)`[[2]] %>% Ben::formatps()

# Wald test
test_result <- lavTestWald(fit_constrain, constraints = "fs - es == 0")
p_friend_role_standards = test_result$p.value %>% Ben::formatps()

test_result <- lavTestWald(fit_constrain, constraints = "fb - eb == 0")
p_friend_role_behavior = test_result$p.value %>% Ben::formatps()


print(test_result)

multigroup_output = parameterEstimates(fit, standardized = T) %>% 
  filter(op %in% c("~", "~~")) %>% 
  select(lhs, rhs, est, std.all, pvalue, group) %>% 
  as_tibble() %>%
  # mutate(group = case_match(
  #   group,
  #   1 ~ "bedtime",
  #   2~ "saving",
  #   3~"healthy eating" ,
  #   4~"work"
  # )) %>%
  filter(lhs != rhs) %>%
  pivot_wider(names_from = group, values_from = c(est, std.all, pvalue), names_glue = "{group}_{.value}") %>%
  # select(lhs, rhs, starts_with("bedtime"), starts_with("healthy"), starts_with("work"), starts_with("saving"))  %>%  
  gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number() %>%
  gt_theme()

## IMportance 
model_constrained = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + fb*friend_minutes + eb*exemplar_minutes
standard10 ~ fs*friend_minutes + es*exemplar_minutes
friend_minutes~~exemplar_minutes"

model_unconstrained = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + friend_minutes + exemplar_minutes
standard10 ~ friend_minutes + exemplar_minutes
friend_minutes~~exemplar_minutes"

data.z = data %>% group_by(condition)  %>% mutate_if(is.numeric, scale) %>% ungroup 
write_rds(data.z, "s2/data/lavaan_data.rds")


fit_constrain = sem(model_constrained, data = data.z %>% mutate(importance_self = ifelse(importance_self > 0, "hi","lo")), group = "importance_self", group.equal = c("regressions"))
fit_medium_constrain = sem(model_constrained, data = data.z %>% mutate(importance_self = ifelse(importance_self > 0, "hi","lo")), group = "importance_self")
fit_free = sem(model_unconstrained, data = data.z %>% mutate(importance_self = ifelse(importance_self > 0, "hi","lo")), group = "importance_self")

c((fit_constrain %>% fitMeasures())[c("cfi")],
(fit_medium_constrain %>% fitMeasures())[c("cfi")],
(fit_free %>% fitMeasures())[c("cfi")]) %>% 
enframe() %>% 
mutate(name = c("All regressions\nconstrained","All parameters freely estimated", "Friends and exemplars to\nbehavior and standards constrained")) %>% 
mutate(name = fct_inorder(name)) %>%
ggplot(aes(name, value))+
geom_col()+
geom_text(aes(label = Ben::numformat(value)), vjust = -1)+
labs(x = NULL, y = "Model Fit (CFI)")+
coord_cartesian(ylim = c(.8,1))

anova(fit_free, fit_medium_constrain, fit_constrain)

summary(fit_constrain, standardized = T, fit.measures = T)


data %>% 
  select(starts_with("standa")) %>% 
  Ben::harcor()

# anova for difficulty
long_data = data %>%
  select(id, condition,  starts_with("diff")) %>% 
  select(1:3, 8,9) %>%
  pivot_longer(starts_with('diff')) %>%
  as.data.frame()

result = long_data %>%
  ez::ezANOVA(value, wid = id, within = name, detailed = T, between = condition)
result

library(emmeans)
model <- lme4::lmer(value ~ name * condition + (1|id), data = long_data)
posthoc <- emmeans(model, pairwise ~ name * condition, adjust = "BH")
cohen_d_values <- as.data.frame(pairs(emmeans(model, ~ name * condition))) %>%
  mutate(
    cohen_d = map2_dbl(.x = estimate.x - estimate.y, 
                       .y = sqrt((std.err.x^2 + std.err.y^2)/2), 
                       ~ .x / .y)
  )


contrasts = posthoc$contrasts %>% 
  as_tibble() %>% separate(contrast, c("l","r"), " - ") %>%
  separate(l, c("name", "condition"), " ") %>%
  separate(r, c("name2", "condition2"), " ") %>%
  filter(condition == condition2) %>%
  filter(name != name2) %>% 
  filter(name2 != "diff_self") %>% 
  mutate(
    condition = ifelse(condition == "healthy","healthy eating", condition),
    condition = ifelse(condition == "social","social media", condition),
    label = glue("difference = {round(estimate, 2)}\np = {round(p.value, 3)}")
    )


print(posthoc)

posthoc$emmeans %>% 
  as.data.frame() %>%
  mutate(name = case_match(
    name, 
    'diff_friends' ~ "Friends",
    'diff_rolemodels' ~ "Exemplars",
    'diff_self' ~ "Self"
    )) %>%
  ggplot(aes(name, emmean, fill = condition))+
  geom_col()+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, width = .2))+
  facet_wrap(~condition, nrow = 1)+ 
  scale_fill_brewer(palette = "Set1")+
  labs(x = NULL, y = "Confidence rating (0-10)")+
  geom_text(aes(label = label, x = 2, y = 1), data = contrasts)+
  scale_y_continuous(
    breaks = c(0:10),
    labels = c("Wild\nguess","1","2","3","4","5","6","7","8","9", "Know\nfor sure"),
    limits = c(0, 10)
    )+
  guides(fill = "none")
# In all cases, friends and role models are similar enough
ggsave("s2/figures/plot_diffs.png", width = 11, height = 3)



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
  formula = glue::glue("{outcome} ~ friend_minutes + exemplar_minutes")
  if(sqrt){formula = glue::glue("{outcome} ~ friend_minutes_sqrt + exemplar_minutes_sqrt")}
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
          out = "s2/p3/output/regressions.tex",
          dep.var.labels.include = T)

output_regressions = lms |>
  mutate(tidy = map(lm, broom::tidy)) |> 
  unnest(tidy)

# Do standards predict self-ratings
models_sc = list(
lm(sc_self ~ standard10 + standard5 + standard0, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_minutes, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes_sqrt + exemplar_minutes_sqrt, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_minutes + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_minutes_sqrt + exemplar_minutes_sqrt + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes + exemplar_minutes, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes_sqrt + exemplar_minutes_sqrt, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes+ exemplar_minutes + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_minutes_sqrt + exemplar_minutes_sqrt + bl_gender + bl_age + bl_race, data.z)
)

table_sc_regressions = stargazer::stargazer(models_sc, type = "latex", out = "s2/p3/output/sc_regressions.tex", dep.var.labels.include = T)

mean_duration = raw  %>% filter(ResponseId %in% data$id) %>% summarise(duration = (median(`Duration (in seconds)`))/60) %>% pull(duration) %>% Ben::numformat(2)

# Does importance matter?
datai = data |> group_by(condition) |> mutate_if(is.numeric,scale)
lm(sc_self ~ importance_self*exemplar_minutes + self_minutes, datai) |> summary()
lm(standard10 ~ importance_self*exemplar_minutes, datai) |> summary()
lm(exemplar_minutes ~ importance_self, datai) |> summary()
lm(friend_minutes ~ importance_self, datai) |> summary()

save.image(file = "s2/p3/output/output.rda")

