###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 2                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## Script to reproduce figure 2 from paper

## load packages
pacman::p_load(tidyverse, gghalves, here, lmerTest, ggthemes, cowplot)

## load data
all_data <- read_csv(file = paste0(here(), "/data/social/data_social_all_participants.csv")) 


## panel A

a <- 
  all_data %>%
  group_by(uniqueID, group) %>%
  select(uniqueID, group, demo_quality, tot_points, gem_found) %>%
  distinct() %>%
  ggplot(aes(
    x = group,
    y = tot_points,
    color = group,
    shape = group
  )) +
  # geom_signif(
  #   comparisons = list(c("adolescents", "adults")),
  #   map_signif_level = TRUE, y_position = 1050,tip_length = 0, textsize = 20, vjust = 500, )+
  stat_summary(size = 1, lwd = 1) +
  scale_color_brewer(
    type = "qual",
    palette = 2) +
  # facet_grid( ~ gem_found) +
  #ylim(c(500,1400))+
  labs(
       #subtitle = 'especially g',
       y = 'average points per round') +
  theme_base(base_size = 15)+
  # facet_grid( ~ demo_quality) +
  guides(color = FALSE,
         shape = FALSE) +
  theme(legend.position = "none",
        plot.background = element_blank())



## panel B
b <- 
  all_data %>% 
  filter(gem_found == 1 & gem_found_how == 'copier') %>% 
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
  labs(
       y = 'clicks before gem is found') +
  #facet_wrap(~demo_type) +
  theme_base(base_size = 15) +
  guides(color = FALSE, fill = FALSE)+
  theme(plot.background = element_blank())

## panel C
 c <- all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy"  & gempresent ==1) %>%
  count(social_info_use) %>%
  ggplot(aes(
    x = factor(demo_quality),
    y = n,
    shape = group,
    color = demo_quality
  )) +
  stat_summary(size = 1, lwd = 1, 
               position = position_dodge(width =.5)) +
  geom_hline(yintercept = 25 / 64,
            linetype = "dotted",
           color = "red") +
  labs(y='average number of copy per round',
       x= 'treatment (quality of demonstrator)')+
  scale_color_brewer(
    type = "qual",
    palette = 
      6,
    name = "demonstrator:",
    label = c("finds a gem", "settles for a positive option", "explores until the end")
  ) +
  scale_x_discrete(labels=c("high", "medium", "low")) +
  # facet_wrap(.~gem_found) +
  theme_base(15) +
  theme(legend.position = "none" ,
        plot.background = element_blank())

## combine panels
figure2 <- 
  cowplot::plot_grid(
    a,b,c,
    labels = c("a","b","c"),
    #align = "H",
    nrow = 1,
    rel_widths =  c(.6, .6, .76)
  )

figure2

## save figure
ggsave("plots/figure2.png", figure2, width = 10)
