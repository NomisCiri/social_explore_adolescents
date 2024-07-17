## there are no differences in gem found
all_data %>% 
  filter(gempresent == 1) %>% 
  group_by(uniqueID, round, group) %>% 
  summarise(gem_found = ifelse(1 %in% gem, 1, 0)) %>% 
  group_by(uniqueID, group) %>% 
  summarise(n_gems = sum(gem_found)) %>% 
  infer::t_test(n_gems ~ group)

## 
all_data %>% 
  filter(gempresent == 1) %>% 
  group_by(uniqueID, round, group, demo_type) %>% 
  summarise(gem_found = ifelse(1 %in% gem, 1, 0)) %>% 
  group_by(uniqueID, group,demo_type) %>% 
  summarise(n_gems = sum(gem_found)) %>%
  ungroup() %>%
  group_by(demo_type) %>%
  group_split() -> data_t_test

data_t_test[[1]] %>% group_by(group) %>% 
  summarize(mean = mean(n_gems))

data_t_test[[2]] %>% group_by(group) %>% 
  summarize(mean = mean(n_gems))
summarise()

data_t_test[[3]] %>% group_by(group) %>% 
  summarize(mean = mean(n_gems))
summarise()

## 
infer::t_test(x = data_t_test[[1]], n_gems ~ group )
infer::t_test(x = data_t_test[[2]], n_gems ~ group )
infer::t_test(x = data_t_test[[3]], n_gems ~ group )


