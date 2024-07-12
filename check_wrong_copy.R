## checks


social_data <-  read_csv(file = here("data","social","data_social_all_participants.csv"))
social_data<-social_data%>%
  mutate(copy=ifelse((env_number == 6 | env_number == 7 | env_number == 8) & social_info==cells,"copy",
                     ifelse((env_number == 1| env_number == 2 | env_number == 3| env_number == 4 | env_number == 5 | env_number == 9| env_number == 10 | env_number == 11 | env_number == 12) & social_info ==cell, "copy", "ignore")))%>%
  mutate(copy_old=ifelse(social_info_use=="copy",1,0)) %>% 
  mutate(demo_quality_f = as.factor(demo_quality),
         age_f = factor(group, levels = c("adults", "adolescents"))) ## mutate IVs into factors

# 
# social_data %>% filter(env_number == 6) -> a
# 
# social_data%>%ggplot(aes(y=copy,x=as.factor(demonstrator),color=as.factor(env_number)))+
#   stat_summary()+
#   ggtitle("New (cells correct)")+
#   facet_wrap(.~demo_quality)
# 
# 
# social_data%>%ggplot(aes(y=copy_old,x=as.factor(demonstrator),color=as.factor(env_number)))+
#   stat_summary()+
#   ggtitle("Old (cell correct)")+
#   facet_wrap(.~demo_quality)

## create count d
model_random_slopes_data <- social_data %>%
  group_by(uniqueID, round, gem_found, age_f, demo_quality_f) %>%
  filter(copy == "copy") %>%
  count(copy)


## mean copy by group t_test
model_random_slopes_data %>% 
  ungroup() %>% 
  filter(n!=0) %>% 
  group_by(uniqueID) %>% 
  mutate(mean_copy = mean(n), na.rm = TRUE) %>% 
  select(age_f, uniqueID, mean_copy) %>% 
  distinct() %>% 
  t.test(mean_copy ~ age_f, data = ., alternative = "two.sided", paired = FALSE)


## mean copy by group %>% 
model_random_slopes_data %>% 
  ungroup() %>% 
  filter(n!=0) %>% 
  select(age_f, n,demo_quality_f ) %>% 
  group_by(age_f, demo_quality_f) %>% 
  summarise( mean_copy = mean(n),
             sd = sd (n))


b <-
  social_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(copy == "copy"  & gempresent == 1) %>%
  count(copy) %>%
  ggplot(aes(
    x = factor(demo_quality),
    y = n,
    shape = group,
    color = demo_quality,
    fill = demo_quality
  )) +
  
  geom_half_point(alpha = .1) +
  geom_half_boxplot(errorbar.draw = FALSE, notch = TRUE, alpha = .2)+
  stat_summary(
    geom = "point",
    size = 2,
    stroke = 1,
    color = "black",
    position = position_dodge(width = .75)
  ) +
  geom_hline(yintercept = 25 / 64,
             linetype = "dotted",
             color = "red") +
  labs(y = 'N of "copy" per round',
       x = "Quality of social information",
       tag = "B") +
  scale_color_brewer(
    type = "qual",
    palette =
      6,
    name = "demonstrator:",
    label = c(
      "finds a gem",
      "settles for a positive option",
      "explores until the end"
    )
  ) +
  scale_fill_brewer(
    type = "qual",
    palette =
      6,
    name = "demonstrator:",
    label = c(
      "finds a gem",
      "settles for a positive option",
      "explores until the end"
    )
  ) +
  
  scale_x_discrete(labels = c("High (Gem)", "Medium", "Low")) +
  scale_shape_manual(values = c(21, 23)) +
  
  # facet_wrap(.~gem_found) +
  theme_base(15) +
  theme(legend.position = "none" ,
        plot.background = element_blank())
b

## define priors
prior_cauchy <- brms::prior_string("cauchy(0, 2.5)", class = "b")

## specify model and fit
model_random_slopes <- brms::brm(formula = n ~ 1 + demo_quality_f * age_f + (1 + demo_quality_f | uniqueID),
                                 prior = prior_cauchy,
                                 sample_prior = TRUE,
                                 data = model_random_slopes_data,
                                 family = poisson(),
                                 iter = 6000,
                                 chains = 5) 

