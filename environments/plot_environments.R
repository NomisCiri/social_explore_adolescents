#################################################################
##             plot environments social experiment             ##
#################################################################

library(tidyverse)
library(colorspace)

## fucntion that loads environments
source("B_SimulationCode/load_environments_social_experiment.R")

## load environments
envs <- load_envs_social()


colors <- rev(c("#67001f","#d6604d", "#f4a582", "#fddbc7", "#FFFFFF",
            "#d1e5f0", "#92c5de", "#4393c3"))

## define categories
breaks <- envs %>% 
  filter(Mean <100) %>% 
  select(Mean) %>% 
  distinct() %>% 
  pull()

## add upper limit for gems
breaks <- c(breaks, Inf)


## cut data
envs$y_bin <- cut(envs$Mean, breaks = breaks, include.lowest = TRUE, labels = c("-50", "-35", "-7","0", "7", "35", "50", "240+ (gem)"))

## plot everything at once
environments <- 
ggplot() +
  geom_tile(data = envs, aes(x = x, y = y, fill = as.factor(y_bin))) +
  facet_wrap(~env, nrow = 3) +
  scale_fill_manual(values = colors, name = "Tile mean value") +
  theme_minimal(14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1)

ggsave(environments, filename = "plots/environments.png")\



## plot environments one by one
for (e in unique(envs$env)) {
  
  ## select environment
  d <- envs %>% dplyr::filter(env == e)
  
  ## make plot
  plot <- 
  ggplot() +
    geom_tile(data = d, aes(x = x, y = y, fill = as.factor(y_bin))) +
    scale_fill_manual(values = colors, name = "Tile mean value") +
    theme_minimal(14) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          aspect.ratio = 1)
  
  ## save plot
  ggsave(plot, filename = paste0("plots/environment", e, ".png"))

}
