

pacman::p_load(tidyverse, rjson, data.table, gghalves, plotly, gganimate, av, colorspace)
`%!in%` = Negate(`%in%`)

explore_data <- explore_data %>% 
  filter(player != 188 & player > 50) %>% 
  group_by(player, round) %>% 
  mutate(trial = 1:25,
         unique_rounds = cur_group_id()) %>% 
  mutate(round_id = as.numeric(unique_rounds))


points_when_gem_found <- explore_data %>% 
  group_by(player, gempresent, tot_points, env_number) %>% 
  filter(gempresent == 1 & sum(gem) > 1) %>% 
  select(player, gempresent, tot_points, unique_rounds, env_number) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(env_number) %>% 
  mutate(performance_group = ntile(tot_points, 3)) %>% 
  mutate(performance_group_f = ifelse(performance_group == 1, "low",
                                      ifelse(performance_group == 2, "medium", 
                                             "high")))


avg_rounds <- points_when_gem_found %>% 
  filter(performance_group == 2 | performance_group == 1) %>% 
  ungroup() %>% 
  select(unique_rounds)

avg_rounds <- avg_rounds$unique_rounds

early_gems_idx <- which(explore_data$gem & explore_data$trial <= 5)
round_to_exclude <- unique(explore_data$unique_rounds[early_gems_idx])


late_gems <- explore_data %>% 
  filter(unique_rounds %in% avg_rounds)%>% 
  filter(unique_rounds %!in% round_to_exclude)


how_may_late_gems <- late_gems%>% 
  group_by(env_number) %>% 
  summarise(sum = n())

