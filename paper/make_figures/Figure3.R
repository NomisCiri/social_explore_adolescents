###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 3                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## Script to reproduce figure 3 from paper


## load packages
pacman::p_load(tidyverse, gghalves, here, lmerTest, ggthemes, cowplot)

## load data
all_data <- read_csv(file = paste0(here(), "/data/social/data_social_all_participants_07-2024.csv"))

## panel 1 data
load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/performance_age_demonstrator_model_scaled.RData'))
    
## panel 2 data
load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/gem_found_when_model.RData'))
    
## panel 3 data
load(file = paste0(here(),'/G_Analysis_bevioral_data_social/modelfits/poission_regression_all_rounds_slopes.RData'))


## make plots

## panel 1
panel_plot1 <- 
  plot_model(
    performance_age_demonstrator_model_scaled,
    # axis.lim = c(.2, 2),
    axis.labels = rev(
      c(
        "Quality (Medium)",
        "Quality (Worst)",
        "Adolescents",
        "Quality (Medium) X Adolescents",
        "Quality (Worst) X Adolescents"
      )
    ),
    title = "Points earned (std)", vline.color = "grey", vline = 2,show.values = TRUE, 
  ) +
  labs(tag = "a")+
  theme_base(base_size = 15)+
  theme(plot.background = element_blank(),
        plot.title = element_text(size=15, hjust = 0.5),
        plot.tag.position = c(0,1),
        plot.tag = element_text(hjust = -9)
        )

panel_plot1

## panel 2
panel_plot2 <- 
  plot_model(
    model_random_slopes,
    # axis.lim = c(.2, 2),
    axis.labels = rev(
      c(
        "Quality (Medium)",
        "Quality (Worst)",
        "Adolescents",
        "Quality (Medium) X Adolescents",
        "Quality (Worst) X Adolescents"
      )
    ),
    title = "N of copy decisions", 
    vline.color = "grey", vline = 2,
    show.values = TRUE, 
  ) +
  labs(tag = "b")+
  ylim(.2,2)+
  theme_base(base_size = 15)+
  theme(plot.background = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=15, hjust = 0.5),
        plot.tag.position = c(0,1),
  )


panel_plot2


## panel 3
panel_plot3 <- 
  plot_model(
    gem_found_when_model
    ,
    title = "N of clicks to find a gem", 
    vline.color = "grey", vline = 2,show.values = TRUE, 
  ) +
  ylim(.2,2)+
  labs(tag = "c")+
  theme_base(base_size = 15)+
  theme(plot.background = element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=15, hjust = 0.5),
        plot.tag.position = c(0,1),
  )

panel_plot3

## combine panels
## combine panels
figure3 <- 
  cowplot::plot_grid(
    panel_plot1, panel_plot2, panel_plot3,
   # labels = c("a", "b","c"),
    label_x = -.01,
    #align = "H",
    nrow = 1,
    rel_widths =  c(.95, .65, .65)
  )
figure3

ggsave("plots/figure3.png", figure3, height = 3, width = 10)

