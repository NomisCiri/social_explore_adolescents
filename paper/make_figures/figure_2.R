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
  geom_half_boxplot(errorbar.draw = FALSE, notch = TRUE) +
  geom_half_point(alpha = .4) +
  
  # geom_signif(
  #   comparisons = list(c("adolescents", "adults")),
  #   map_signif_level = TRUE, y_position = 1050,tip_length = 0, textsize = 20, vjust = 500, )+
  stat_summary(
    geom = "point",
    size = 2,
    stroke = 1,
    color = "black",
    fill = "orange",
    position = position_dodge(width = -1)
  ) +
  scale_shape_manual(values = c(21, 23)) +
  facet_wrap(~ gem_found, labeller = as_labeller(labels_a), scales = "free_y") +
  #ylim(c(500,1400))+
  labs(#subtitle = 'especially g',
    y = 'Points per round',
    tag = "A") +
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
 
 ## combine panels
 upper <- 
   cowplot::plot_grid(
     a,b,
     #labels = c("a","b"),
     #align = "H",
     nrow = 1,
     rel_widths =  c(.6, .6)
   )
 
 
 ## panel C
 
 ## re filter each treatment

 dataset1 <- all_data %>% 
   filter(gem_found_how == "copier" & demo_type == "gem_found")
 
 dataset2 <- all_data %>% 
   filter(demo_type != "gem_found" & gem_found==1)
 
 ## add information about proportion of gem found
 prop_gem_found <- all_data %>%
   filter(gempresent == 1)%>%
   ungroup %>%
   group_by(demo_type, gem_found, group) %>%
   select(uniqueID, round, gem_found, round_gem_found, gempresent, group) %>%
   distinct() %>%
   select(gem_found, round_gem_found, group) %>%
   summarise(mean_round_found = mean(round_gem_found, na.rm = TRUE),
             n = n()) %>%
   ungroup() %>%
   group_by(demo_type, group) %>%
   mutate(freq = round(n / sum(n),2)) %>% 
   filter(gem_found == 1) %>% 
   select(demo_type, group, freq) 
 

 data_plot <- bind_rows(dataset1, dataset2) %>% 
   select(round_gem_found, group, demo_type, uniqueID) %>%
   group_by(uniqueID) %>%
   distinct() %>%
   left_join(., prop_gem_found, by = c("group", "demo_type"))
  
c <- data_plot %>%
   ggplot(aes(
     x = demo_type,
     y = round_gem_found,
     shape = group,
     color = demo_type,
     fill = demo_type
   )) +
   geom_half_boxplot(
     errorbar.draw = FALSE,
     notch = TRUE,
     alpha = .2,
     show.legend = FALSE
   ) +
   geom_half_point(alpha = 0.2) +
   stat_summary(
     geom = "point",
     size = 2,
     stroke = 1,
     color = "black",
     position = position_dodge(width = .75)
   ) +
  geom_text(data = prop_gem_found, aes(x = demo_type, y = 26, label = freq), 
             ) +
   scale_color_brewer(type = "qual", palette = 6) +
   scale_fill_brewer(
     type = "qual",
     palette =
       6,
     name = "Quality of social information",
     label = c(
       "High: Finds a gem",
       "Medium: Exploits a non-gem",
       "Low: Explores until the end"
     )
   ) +
   scale_shape_manual(values = c(21, 23)) +
   scale_x_discrete(labels = c("High (Gem)", "Medium", "Low")) +
   labs(x = "Quality of social information",
        y = 'N of clicks to find a gem',
        tag = "C") +
   #facet_wrap(~demo_type) +
   theme_base(base_size = 15) +
   guides(
     color = FALSE,
    fill = guide_legend(override.aes = list(
      alpha = .5,
      shape = 21,
      size  =
        4,
      fill = c("#e41a1c", "#377eb8", "#4daf4a"))),
     #fill = FALSE,
     shape = guide_legend(override.aes = list(size = 4))
   ) +
   theme(plot.background = element_blank())
 
 c
 
## regression slopes 
#  
#   load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/poission_regression_all_rounds_slopes.RData'))
#   
#   poisson_plot <- 
#     plot_model(
#       model_random_slopes,
#       # axis.lim = c(.2, 2),
#       axis.labels = rev(
#         c(
#           "Quality (Medium)",
#           "Quality (Worst)",
#           "Adolescents",
#           "Quality (Medium) X Adolescents",
#           "Quality (Worst) X Adolescents"
#         )
#       ),
#       title = "", vline.color = "grey", vline = 2,show.values = TRUE, 
#     ) +
#     ylim(.2,2)+
#     theme_base(base_size = 15)+
#     theme(plot.background = element_blank())
#   
# 
  lower <-
    cowplot::plot_grid(
      c,
      #labels = c("a","b"),
      #align = "H",
      nrow = 1,
      rel_widths =  c(.6, .4)
    )


## combine panels
figure2 <-
  cowplot::plot_grid(
    upper, lower,
   # labels = c("","c"),
    #align = "H",
    nrow = 2,
    rel_widths =  c(1, 1)
  )

figure2

## save figure
ggsave("plots/figure2.png", figure2, height = 7, width = 10)
