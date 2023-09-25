load("s2/p2/Output_p2.rda")
data2 = data
data2 %>% 
  group_by(condition) %>%
  select(ends_with("minutes")) %>% 
  pivot_longer(-condition, names_to = "time", values_to = "minutes") %>%
  summarise(
    mean = mean(minutes),
    sd = sd(minutes),
    min = min(minutes),
    max = max(minutes),
    min3sd = mean - 2.5*sd,
    max3sd = mean + 2.5*sd
    )
