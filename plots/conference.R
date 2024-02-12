### conference short summary main plots

library(ggthemes)

all_data %>%
  group_by(uniqueID, trial, group) %>%
  ggplot(aes(x = trial, y = points, color = factor(gem_found), shape = group)) +
  stat_summary() +
  theme_minimal(base_size = 15) +
  scale_color_brewer(type = "qual", palette = 2, name = "gem found", label = c("no", "yes"))+
  # facet_wrap( ~ gem_found) +
  labs(title = 'average points per trial') +
  theme_classic(base_size = 20) 




all_data %>% 
  group_by(uniqueID, group) %>% 
  select(uniqueID, group, demo_quality, tot_points) %>%
  distinct() %>% 
  ggplot(aes(x = group, y = tot_points, color = demo_quality, shape = group)) +
  stat_summary() +
  theme_minimal(base_size = 15) +
  #scale_color_brewer(type = "qual", palette = 2, name = "gem found", label = c("no", "yes"))+
  # facet_grid( ~ demo_quality) +
  labs(title = 'Adolescents score more points than adults',
       y = 'average points per round') +
  theme_classic(base_size = 20) 


all_data %>%
  group_by(uniqueID, group) %>%
  select(uniqueID, group, demo_quality, tot_points, gem_found) %>%
  distinct() %>%
  ggplot(aes(
    x = group,
    y = tot_points,
    #color = factor(gem_found),
    shape = group
  )) +
  # geom_signif(
  #   comparisons = list(c("adolescents", "adults")),
  #   map_signif_level = TRUE, y_position = 1050,tip_length = 0, textsize = 20, vjust = 500, )+
  stat_summary(size = 1, lwd = 1) +
  # scale_color_brewer(
  #   type = "qual",
  #   palette = 2,
  #   name = "quality of demonstrator",
  #   label = c("finds a gem", "settles for good option", "explores until the end")
  # ) +
  # facet_grid( ~ gem_found) +
  #ylim(c(500,1400))+
  labs(title = 'Adolescents score more points than adults',
       #subtitle = 'especially g',
       y = 'average points per round') +
  theme_base(base_size = 20)+
  # facet_grid( ~ demo_quality) +
  guides(shape = FALSE)

ggsave("plots/points_difference.png",plot = last_plot(), height = 6, width = 6)



interaction_data <- all_data %>% 
    ungroup() %>% 
  select(uniqueID, group, gem_found, tot_points) %>%
  distinct() 


summary(lmer(tot_points ~ gem_found  * group + (1|uniqueID) , data = interaction_data ))

summary(lmer(tot_points ~ group + (1|uniqueID) , data = interaction_data ))



all_data %>% 
  filter(gem_found == 1) %>% 
  select(round_gem_found, group, demo_type, uniqueID) %>% 
  group_by(uniqueID) %>% 
  distinct() %>% 
  ggplot(aes(x = group, y = round_gem_found, fill = group)) + 
  geom_half_boxplot(errorbar.draw = FALSE)+
  geom_half_point(aes(color = group), alpha = 0.2,
              )+
  stat_summary(   geom = "point",
                  shape =23,
                  size = 2,
                  stroke = 1,
                  color = "black",
                  fill = "white")+
  scale_color_brewer(
    type = "qual",
    palette = 2) +
  scale_fill_brewer(
    type = "qual",
    palette = 2) +
  labs(title = '',
       y = 'clicks before gem is found') +
facet_wrap(~demo_type) +
  theme_base(base_size = 20) +
  guides(color = FALSE, fill = FALSE)

ggsave("plots/gem_found_when.png",plot = last_plot(), width = 6, heigh = 6)
  

all_data %>% 
  filter(gem_found==1 & gem_found_how == 'copier') %>%
  select(round_gem_found, group, demo_type, uniqueID) %>% 
  group_by(uniqueID) %>% 
  distinct() %>% 
  lm(round_gem_found ~ group  , data = .) -> model_when_gem

summary(model_when_gem)




all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy"  & gempresent ==1) %>%
  count(social_info_use) %>%
  ggplot(aes(
    x = factor(demo_quality),
    y = n,
    shape = group,
    color = demo_quality
  )) +
  stat_summary(position = position_dodge(width =.5)) +
 # geom_hline(yintercept = 25 / 64,
  #           linetype = "dotted",
  #          color = "red") +
  labs(y='average number of copy per round',
       x= 'treatment (quality of demonstrator)')+
  scale_color_brewer(
    type = "qual",
    palette = 2,
    name = "demonstrator:",
    label = c("finds a gem", "settles for a positive option", "explores until the end")
  ) +
  scale_x_discrete(labels=c("high", "medium", "low")) +
 # facet_wrap(.~gem_found) +
  theme_base(20)
 # guides(color = FALSE)
ggsave("plots/social_info_use.png",plot = last_plot(), width = 6, heigh = 6)



## difference in copy by group and treatment
all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy") %>%
  count(social_info_use) %>% 
  lmer(n ~ demo_quality * group + (1|uniqueID), data = .) -> c

summary(c)

## condition on gem being present
all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy" & gempresent == 1 ) %>%
  count(social_info_use) %>% 
  lmer(n ~ demo_quality * group + (1|uniqueID), data = .) -> a

summary(a)

## bayesian model
all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy" & gempresent == 1 ) %>%
  count(social_info_use) %>% 
  brms::brm(n ~ demo_quality * group + (1|uniqueID), data = .) -> b

summary(b)
