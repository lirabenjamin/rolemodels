# Data checklist: https://docs.google.com/document/d/1bdSqD7UkWOwNsbiu_8pQQo5l45s9YIb7Lkww01HY7sQ/edit
# latex table previewer http://www.tlhiv.org/ltxpreview/
# My approach is to work on a script, save the results, and then load them in a Rmarkdown for reporting. 

# Libraries
library(tidyverse)
library(qualtRics)

# Save your raw data, or better yet, read direclty from qualtrics ----

# IDs available at https://upenn.co1.qualtrics.com/Q/QualtricsIdsSection/IdsSection
raw = qualtRics::fetch_survey("SV_865OvT8hHfRoVXE", force_request = T)

colnames(raw)
data = raw %>% 
  
  # Drop people you don't want (previews, pilots, too young, attention checks...)
  filter(DistributionChannel == "anonymous") |> 
  slice_tail(n = 160) %>%

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
    friend1_minutes = `minutes_5`,
    friend2_minutes = `minutes_6`,
    exemplar1_minutes = `minutes_8`,
    exemplar2_minutes = `minutes_9`,
    self_minutes = minutes_1
    ) %>% 
    mutate_at(vars(sc_self, standard10, standard5, standard0, self_minutes:diff_rolemodels), as.numeric)
  
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
check_monotonic = function(high, low, condition){
  if(condition == "exercise"){
    # 10 needs to exercise more than 5
    if(high < low){
      return(T)
    } else {
      return(F)
    }
    # if note exercise, 10 needs to drink less than 5
  } else {
    if(high > low){
      return(T)
    } else {
      return(F)
    }
  }
}

data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_monotonic),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_monotonic),
    e101 = pmap_lgl(list(standard10, standard0, condition), check_monotonic),
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e != 0) %>% 
  count(condition)


# non monotonic
data = data |> 
  mutate(
    e105 = pmap_lgl(list(standard10, standard5, condition), check_monotonic),
    e51 = pmap_lgl(list(standard5, standard0, condition), check_monotonic),
    e101 = pmap_lgl(list(standard10, standard0, condition), check_monotonic),
  ) %>% 
  rowwise() %>% 
  mutate(e = mean(c(e105, e51, e101))) %>%
  filter(e == 0) |> 
  select(-e, -e105, -e51, -e101)

# invert scores - if not exercise, then invert (many drinks, many minutes on social media, and many minutes late is bad, many minutes of exercise is good)
# make all values positive. Add 30 to punctuality
data = data %>% 
  pivot_longer(c("standard10":"standard0", "self_minutes":"exemplar2_minutes")) %>% 
  mutate(value = case_when(
    condition == "exercise" ~ value,
    condition != "exercise" ~ value * -1)) %>%
  mutate(value = case_when( 
    condition == "punctuality" ~ value + 30,
    condition == "drinking" ~ value + 15,
    condition == "social media" ~ value + 120,
    condition == "exercise" ~ value
  )) %>% 
  pivot_wider(names_from = name, values_from = value)

# Managing your data (i.e., ensuring data hygiene) ----

data %>% 
select(condition,standard10:standard0, friend1_minutes:exemplar2_minutes, self_minutes) %>%
pivot_longer(-condition) %>%
mutate(name = fct_inorder(name)) %>%
ggplot(aes(value))+
geom_histogram(bins = 10)+
labs(x = NULL, y = NULL)+
facet_wrap(condition~name, scales = "free",nrow = 4) #Histogram for all vars

# Eyeball the data. Do you see any gaping “holes” (missing data)?
missing_col = data %>% is.na %>% colSums()
missing_row = data %>% is.na %>% rowSums()

missing_col;missing_row

# Are there redundant cases? Use the SPSS function to verify there are not.
data %>% unique()

# Is the sample size roughly what you expect?
data %>% nrow()

# Run frequency counts for all items. Are any “impossible” values evident (e.g., a response of “9” on a 1 through 8 scale).
# Are missing values properly encoded as missing – not as zeros? If a discreet number is used to signal “missingness” then make sure SPSS knows this.
table_describe = data %>% psych::describe() |> as.data.frame() |> rownames_to_column() |> gt::gt() |> gt::fmt_number()

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

# Creating scores for scales (this process does not apply to scoring items where there is a specific correct answer, such as a math problem) ---- 
data = data %>% clnR2::cv_bake(friend_min, quos(friend1_minutes, friend2_minutes))
data = data %>% clnR2::cv_bake(exemplar_min, quos(exemplar1_minutes, exemplar2_minutes))

# Optionally, if there is a lot of missing data, make a decision rule about whether to create scores for participants who skipped questions (e.g., only score when 80% or more items were answered).

# Explore the distributions of all your variables ----

#Histograms
plot_histograms = data %>% 
  group_by(condition) %>%
  select_if(is.numeric) %>% 
  pivot_longer(-condition) %>%
  ggplot(aes(value, color = condition))+
  geom_histogram(bins = 10)+
  facet_wrap(~name, scales = "free") #Histogram for all vars

plot_minutes = data %>% 
  select(condition, friend_min, exemplar_min, self_minutes) %>%
  pivot_longer(-condition) %>%
  group_by(condition, name) %>% 
  mutate(value = scale(value)) %>%
  ggplot(aes(value, color = condition))+
  geom_density()+
  facet_wrap(~name, scales = "free") #Histogram for all vars

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
  facet_wrap(~name, scales = "free") #Histogram for all vars

# Transformations
data = data |> 
  mutate(
    friend_min_sqrt = sqrt(friend_min),
    exemplar_min_sqrt = sqrt(exemplar_min)
    )

#Histograms
plot_histograms = data %>% 
  group_by(condition) %>%
  select_if(is.numeric) %>% 
  pivot_longer(-condition) %>%
  ggplot(aes(value))+
  geom_histogram(bins = 10)+
  facet_wrap(condition~name, scales = "free") #Histogram for all vars

# Explore and then clarify how variables are related to each other ----
# Bivariate correlations

table_correlations = data %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()

table_correlations_exercise = data %>%filter(condition == "exercise") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_social_media = data %>%filter(condition == "social media") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_drinking = data %>%filter(condition == "drinking") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()
table_correlations_punctuality = data %>%filter(condition == "punctuality") %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()

# Determine which are your most important variables and run scatterplots for each of them. Check for a linear relationship. If the data are not generally linear the bivariate correlations you just completed won’t reveal the true relationship between the variables. From this point on if the variables aren’t linear they should be examined using statistical methods that account for their true shape.

# 1. friends and role model min
cor(data$friend_min, data$exemplar_min)
plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_min, exemplar_min))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~condition, scales = "free")+
  labs(x = "Friend Minutes of Exercise",y = "Role Model Minutes of Exercise", subtitle = "r = .64")

cor(data$friend_min_sqrt, data$exemplar_min_sqrt)
plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_min_sqrt, exemplar_min_sqrt))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~condition, scales = "free")+
  labs(x = "Friend Minutes of Exercise",y = "Role Model Minutes of Exercise", subtitle = "r = .64")


# effect of friends and role models on self control, behavior, and standards
library(gt)
data %>% 
  group_by(condition) %>% 
  select(id,sc_self, self_minutes, standard10:standard0, friend_min, exemplar_min) %>% 
  pivot_longer(self_minutes:exemplar_min) %>% 
  mutate(value = case_when(
    condition == "exercise" ~ value,
    condition != "exercise" ~ value * -1
  )) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  group_by(condition) %>%
  pivot_longer(friend_min:exemplar_min, names_to = "pred_name", values_to = "pred_value") %>%
  pivot_longer(sc_self:standard0, names_to = "outcome_name", values_to = "outcome_value") %>% 
  group_by(condition, pred_name, outcome_name) %>%
  summarise(
    r = cor(pred_value, outcome_value, use = "pairwise.complete.obs"),
    p = cor.test(pred_value, outcome_value, use = "pairwise.complete.obs")$p.value
  ) %>% 
  pivot_wider(names_from = pred_name, values_from = c(r,p)) %>% 
  mutate(difference = `r_exemplar_min` - `r_friend_min`) %>% 
  gt()  %>% 
  fmt_number() %>% 
  data_color(columns = difference, method = "numeric", 
  colors = scales::col_numeric(
      palette = c("red","white", "blue"),
      domain = c(-.80, 0,.80)
    ))

# replicating reference bias
data %>% 
  group_by(condition) %>%
  nest() %>%
  mutate(
    lm_model = map(data, ~lm(sc_self ~ friend_min_sqrt + exemplar_min_sqrt + self_minutes, data = .x)),    
    lm_beta = map(lm_model, lm.beta::lm.beta),
    tidy = map(lm_beta, broom::tidy)
  ) %>% 
  unnest(tidy) %>% 
  select(condition, term, estimate, std_estimate, std.error, statistic, p.value) %>% 
  gt()  %>% 
  fmt_number()

# predicting standards
data %>% 
  group_by(condition) %>%
  pivot_longer(standard10:standard0) %>% 
  group_by(condition, name) %>%
  nest() %>%
  mutate(
    lm_model = map(data, ~lm(value ~ friend_min_sqrt + exemplar_min_sqrt + self_minutes, data = .x)),    
    lm_beta = map(lm_model, lm.beta::lm.beta),
    tidy = map(lm_beta, broom::tidy)
  ) %>% 
  unnest(tidy) %>% 
  filter(name == "standard10") %>%
  select(condition, term, estimate, std_estimate, std.error, statistic, p.value) %>% 
  gt()  %>% 
  fmt_number()

data %>% 
  select(condition, starts_with("diff")) %>% 
  pivot_longer(starts_with("diff")) %>%
  group_by(condition, name) %>%
  summarise(mean_cl_normal(value)) %>% 
  ggplot(aes(name, y, fill = condition))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = position_dodge(.9), width = .2)+ 
  facet_wrap(~condition, scales = "free")+ 
  scale_fill_brewer(palette = "Set1")
ggsave("figures/p2_plot_diffs.png", width = 10, height = 5)

plot_main_sqrt = data |> 
  group_by(condition) %>%
  select(friend_min_sqrt, exemplar_min_sqrt, starts_with("standar"), sc_self) |>  
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
  formula = glue::glue("{outcome} ~ friend_min + exemplar_min")
  if(sqrt){formula = glue::glue("{outcome} ~ friend_min_sqrt + exemplar_min_sqrt")}
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
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min + exemplar_min, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_min_sqrt, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min + exemplar_min + bl_gender + bl_age + bl_race, data.z),
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_min_sqrt + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min + exemplar_min, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_min_sqrt, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min + exemplar_min + bl_gender + bl_age + bl_race, data.z),
lm(self_minutes ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_min_sqrt + bl_gender + bl_age + bl_race, data.z)
)

table_sc_regressions = stargazer::stargazer(models_sc, type = "text")

raw %>% slice_tail(n = 160) %>% 
select(`Duration (in seconds)`) %>% 
summarise_all(median)

save.image(file = "Output_p2.rda")


raw %>% select(suggestion_box) %>% 
filter(!is.na(suggestion_box)) %>% 
gt::gt()
