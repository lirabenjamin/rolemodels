# Data checklist: https://docs.google.com/document/d/1bdSqD7UkWOwNsbiu_8pQQo5l45s9YIb7Lkww01HY7sQ/edit
# latex table previewer http://www.tlhiv.org/ltxpreview/
# My approach is to work on a script, save the results, and then load them in a Rmarkdown for reporting. 

# Libraries
library(tidyverse)
library(qualtRics)

# Save your raw data, or better yet, read direclty from qualtrics ----

# IDs available at https://upenn.co1.qualtrics.com/Q/QualtricsIdsSection/IdsSection
raw = qualtRics::fetch_survey("SV_4YParUIsOhhJjoy", force_request = T)

# Prior general cleaning ----
data = raw %>% 
  
  # Drop people you don't want (previews, pilots, too young, attention checks...)
  filter(DistributionChannel == "anonymous") |> 

  # Drop variables you wont need
  select(id = ResponseId,sc_self:bl_ethnicity) %>% 
  
  # Rename variables to useful names
  rename(
    standard10 = standard10_1,
    standard5 = standard5_1,
    standard0 = standard0_1,
    friend1_minutes = `1_friend_minutes_1`,
    friend2_minutes = `2_friend_minutes_1`,
    exemplar1_minutes = `1_role_model_minutes_1`,
    exemplar2_minutes = `2_role_model_minutes_1`
    )
  
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
# non monotonic
data = data |> 
  rowwise() |> 
  mutate(
    e105 = ifelse(standard10 < standard5, T, F),
    e51 = ifelse(standard5 < standard0, T, F),
    e101 = ifelse(standard10 < standard0, T, F),
    e = mean(c(e105, e51, e101))
  ) |> 
  ungroup() |> 
  filter(e == 0) |> 
  select(-e, -e105, -e51, -e101)

# role models exercise 0
data = data |> 
  filter(!(exemplar1_minutes == 0 | exemplar2_minutes == 0))

# Managing your data (i.e., ensuring data hygiene) ----

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
  select_if(is.numeric) %>% 
  pivot_longer(everything()) %>%
  ggplot(aes(value))+
  geom_histogram(bins = 10)+
  facet_wrap(~name, scales = "free") #Histogram for all vars

#Bar charts
plot_bars = data %>%
  select_if(is.factor) |> 
  pivot_longer(everything()) |> 
  ggplot(aes(value))+
  geom_bar() +
  facet_wrap(~name, scales = "free") #Histogram for all vars

# Transformations
data = data |> 
  mutate(
    friend_min_sqrt = sqrt(friend_min),
    exemplar_min_sqrt = sqrt(exemplar_min)
    )

#Histograms
plot_histograms = data %>% 
  select_if(is.numeric) %>% 
  pivot_longer(everything()) %>%
  ggplot(aes(value))+
  geom_histogram(bins = 10)+
  facet_wrap(~name, scales = "free") #Histogram for all vars

# Explore and then clarify how variables are related to each other ----
# Bivariate correlations
table_correlations = data %>% select_if(is.numeric) |> Ben::harcor() |> gt::gt()

# Determine which are your most important variables and run scatterplots for each of them. Check for a linear relationship. If the data are not generally linear the bivariate correlations you just completed won’t reveal the true relationship between the variables. From this point on if the variables aren’t linear they should be examined using statistical methods that account for their true shape.

# 1. friends and role model min
cor(data$friend_min, data$exemplar_min)
plot_sc_friends_exemplars = data |> 
  ggplot(aes(friend_min, exemplar_min))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Friend Minutes of Exercise",y = "Role Model Minutes of Exercise", subtitle = "r = .64")

cor(data$friend_min_sqrt, data$exemplar_min_sqrt)
plot_sc_friends_exemplars_sqrt = data |> 
  ggplot(aes(friend_min_sqrt, exemplar_min_sqrt))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Friend Minutes of Exercise - square-root",y = "Role Model Minutes of Exercise - square-root", subtitle = "r = .47")

plot_main = data |> 
  select(friend_min, exemplar_min, starts_with("standar"), sc_self) |>  
  pivot_longer(1:2) |> 
  pivot_longer(1:4, names_to = "outcome", values_to = "outcome_value") |> 
  mutate(
    name = ifelse(name == "friend_min", "Friend Minutes","Role Model Minutes"),
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
  facet_wrap(~outcome, scales = "free", nrow = 1)+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Peer Minutes of Exercise", y = "Outcome Value", color = "")

plot_main_sqrt = data |> 
  select(friend_min_sqrt, exemplar_min_sqrt, starts_with("standar"), sc_self) |>  
  pivot_longer(1:2) |> 
  pivot_longer(1:4, names_to = "outcome", values_to = "outcome_value") |> 
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
  facet_wrap(~outcome, scales = "free", nrow = 1)+
  scale_color_brewer(palette = "Set1")+
  labs(x = "Peer Minutes of Exercise - square-root", y = "Outcome Value", color = "")

# Analyze

# Outliers
# Ben::No_Out(data,data %>% select_if(is.numeric),alpha = .001) #Mahalanobis outliers
table_outliers = data %>%
  select_if(is.numeric) %>% scale %>% psych::describe() |> 
  as.data.frame() |> 
  filter(min < -3 | max > 3) |> 
  rownames_to_column() |> 
  gt::gt() |> 
  gt::tab_footnote("We are not eliminating data based on outliers at the moment. Also, multivariate outliers were not computable (Too little data)") |> 
  gt::fmt_number()#Check for univariate outliers (Z scores > 3)

# lms
outcomes = c("sc_self", "standard10", "standard5","standard0")
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
lm(sc_self ~ standard10 + standard5 + standard0 + friend_min_sqrt + exemplar_min_sqrt + bl_gender + bl_age + bl_race, data.z)
)

table_sc_regressions = stargazer::stargazer(models_sc)

save.image(file = "Output.rda")