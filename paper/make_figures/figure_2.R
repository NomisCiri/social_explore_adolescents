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

labels_a <- c(
  `0` = "Gem not found",
  `1` = "Gem found")

## panel A
a <-
  all_data %>%
  group_by(uniqueID, group) %>%
  select(uniqueID,
         group,
         demo_quality,
         tot_points,
         gem_found,
         gempresent) %>%
  distinct() %>%
  ggplot(aes(x = group,
             y = tot_points,
             #color = group,
             shape = group)) +
  geom_half_boxplot(size = .7,
                    alpha = .2,
                    notch = TRUE) +
  geom_half_point(alpha = .4) +
  
  # geom_signif(
  #   comparisons = list(c("adolescents", "adults")),
  #   map_signif_level = TRUE, y_position = 1050,tip_length = 0, textsize = 20, vjust = 500, )+
  stat_summary(
    geom = "point",
    size = 2,
    stroke = 1,
    color = "black",
    fill = "red",
    position = position_dodge(width = -1)
  ) +
  scale_shape_manual(values = c(2, 23)) +
  facet_wrap(~ gem_found, labeller = as_labeller(labels_a), scales = "free_y") +
  #ylim(c(500,1400))+
  labs(#subtitle = 'especially g',
    y = 'Points per round') +
  theme_base(base_size = 15) +
  guides(color = FALSE,
         shape = FALSE) +
  theme(legend.position = "none",
        plot.background = element_blank())

a

## panel C## panel Cgempresent
b <-
  all_data %>% group_by(demo_type, uniqueID, round, group, gem_found, age, demo_quality) %>%
  filter(social_info_use == "copy"  & gempresent == 1) %>%
  count(social_info_use) %>%
  ggplot(aes(
    x = factor(demo_quality),
    y = n,
    shape = group,
    color = demo_quality,
    fill = demo_quality
  )) +
  
  geom_half_point(alpha = .1) +
  geom_half_boxplot(alpha = .1) +
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
       x = "Quality of social information") +
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
 
 ## combine panels
 upper <- 
   cowplot::plot_grid(
     a,b,
     labels = c("a","b"),
     #align = "H",
     nrow = 1,
     rel_widths =  c(.6, .6)
   )
 
 
 ## panel C
 
 ## re filter each treatment
 c <- 
   all_data %>% 
   filter(gem_found == 1) %>% 
   select(round_gem_found, group, demo_type, uniqueID) %>% 
   group_by(uniqueID) %>% 
   distinct() %>% 
   
   ggplot(aes(
     x = demo_type,
     y = round_gem_found,
     shape = group,
     color = demo_type,
     fill = demo_type
   )) +
   geom_half_boxplot(errorbar.draw = FALSE, notch = TRUE, alpha = .2)+
   geom_half_point( alpha = 0.2,
   ) +
   stat_summary(
     geom = "point",
     size = 2,
     stroke = 1,
     color = "black",
    # fill = "red",
    position = position_dodge(width = .75)
   ) +
   
  scale_color_brewer(
     type = "qual",
     palette = 6) +
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
   scale_shape_manual(values = c(21, 23)) +
   scale_x_discrete(labels = c("High (Gem)", "Medium", "Low")) +
   labs(
     x = "Quality of social information",
     y = 'N of clicks until gem is found (max = 25)') +
   #facet_wrap(~demo_type) +
   theme_base(base_size = 15) +
   guides( fill = FALSE)+
   theme(plot.background = element_blank())
 
 c

## combine panels
figure2 <- 
  cowplot::plot_grid(
    upper, c,
    labels = c("","c"),
    #align = "H",
    nrow = 2,
    rel_widths =  c(.6, .6, 1)
  )

figure2

## save figure
ggsave("plots/figure2.png", figure2, width = 10)
