

pacman::p_load(tidyverse, rjson, data.table, gghalves, plotly, gganimate, av, colorspace)
`%!in%` = Negate(`%in%`)



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

