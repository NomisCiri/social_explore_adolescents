## Sanity checks

# load data
all_data <- read_csv(file = paste0(here(), "/data/social/data_social_all_participants.csv")) 

## exclude rounds in which participants explored more than 3 rounds after finding the gem

problem_data <- all_data %>%
  #filter(remain_gem != 1) %>%  
  filter(gem_found == 1 & trial > round_gem_found) %>%
  ungroup() %>% 
  group_by(uniqueID, round) %>% 
  mutate(n_explore_after_gem = length(unique(cell)),
         remaining_trials = 25 - round_gem_found,
         n_clicks =  n_explore_after_gem )

problem_people <- problem_data %>% 
  select(uniqueID, group, round, n_explore_after_gem) %>% 
  filter(n_explore_after_gem > 3) %>% 
  distinct() %>% 
  group_by(uniqueID, group) %>% 
  summarise(n = n())

problem_adults <- problem_people %>% 
  filter(group == 'adults') 

problem_data %>%
  select(unique_rounds, group, n_explore_after_gem) %>% 
  filter(n_explore_after_gem > 3) %>% 
  distinct() %>% 
  select(unique_rounds) %>% 
  pull() -> problem_rounds

data <- data %>% 
  ungroup() %>% 
  group_by(soc_info_round) %>% 
  mutate(gem_found_social_info = ifelse(soc_info_round == 335, 11,
                                        ifelse(soc_info_round == 795, 16,
                                               ifelse(soc_info_round == 1625, 6,
                                                      ifelse(soc_info_round == 1912, 14,
                                                             ifelse(soc_info_round == 905, 8,
                                                                    ifelse(soc_info_round == 343, 17, NA)))))),
         gem_found_how = ifelse(is.na(round_gem_found), 'not_found',
                                ifelse( round_gem_found < gem_found_social_info,
                                        'alone',
                                        ifelse(
                                          round_gem_found >= gem_found_social_info,
                                          'copier', ""
                                        )
                                ))) %>%
  filter(unique_rounds %!in% problem_rounds) %>% 
  mutate(demo_quality = if_else(demo_type == "gem_found", "best",
                                ifelse((demo_type == "gem_not_found" | demo_type == "no_gem"), "medium", "worst"))) 



problem_data %>%
  select(unique_rounds, n_clicks, round_gem_found, demo_type, group) %>%
  distinct() %>%
  ggplot() +
  geom_histogram(aes(
  x = n_clicks,
  y = stat(density),
  group = group,
  fill = factor(demo_type)), binwidth = 1) +
  geom_vline(aes(xintercept = mean(n_clicks))) +
  theme_bw(base_size = 15) +
    labs(x = 'proportion of explore clicks after gem',
         y = 'n of unique rounds (1 environment)', title = "behavior after gem was found") +
  facet_wrap(~ group)
    

explore_after_gem %>% 
  filter(remain_gem != 1) %>% 
  group_by(unique_rounds, round_gem_found) %>%  
  summarise(n = n())


# 
# explore_exploit <- 

## what is this??
only_good_players %>%
  group_by(uniqueID, group) %>%
  mutate(exploit = ifelse(lead(cells) == cells, 1, 0),
         explore = ifelse(lead(cells) != cells, 1, 0)) %>%
  mutate(out_cat = case_when(points > 150 ~ 1,
                             points < 0 ~ 3,
                             TRUE ~ 2)) %>% pivot_longer(cols = c(exploit, explore), names_to = "explore_exploit") %>%
  ggplot(aes(
    x = out_cat,
    y = value,
    color = explore_exploit,
    shape = group
  )) +
  stat_summary(size = 1) +
  stat_summary(geom = "line") +
  scale_y_continuous(name = "proportion") +
  scale_x_continuous(
    name = "reward type",
    breaks = c(1, 2, 3),
    labels = c("gem", "no gem", "loss")
  ) +
  #scale_color_brewer(type = 'qual', palette = 6, name = 'explore/exploit')+
  scale_color_manual(values = c("#FFC20A", "#0C7BDC"), name = 'explore/exploit' )+
  theme_bw(15) +
  facet_wrap(~gemlabel)+
  # guides(shape = FALSE)+
  theme(axis.text=element_text(size=20))+
  labs(title = 'explore or exploit by reward type')

# 
#   ggsave(filename = 'explore_exploit.png', explore_exploit, width = 12.5,height=6  )



all_data %>%  
  ungroup() %>% 
  select(unique_rounds, gempresent, gem_found) %>% 
  group_by(gempresent, gem_found) %>%    distinct() %>% 
  summarise(n = n())