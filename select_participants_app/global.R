
### Load ----------------------------------------------------------------------
# Load dataset and functions

pacman::p_load(tidyverse,
               plotly,
               colorspace)

`%!in%` = Negate(`%in%`)


data_1 <-  as_tibble(read.csv('data_coord.csv'))
#readRDS(paste0(here(),"/select_participants_app/explore_data.RDS"))

make_plot <- function(dat, gem_present, gem_coords) {
  
  gem <- gem_present
  data_round <- dat
  gem_coords_round <- gem_coords
  
  if(gem == 'yes'){
  
  plot <- 
  ggplot() +
    geom_rect(
      data = gem_coords_round,
      size = 1,
      aes(
        xmin = x - 1,
        xmax = x  ,
        ymin = y - 1,
        ymax = y
      ),   color = 'red',
        fill = 'white',
    ) +
    geom_rect(
      data = data_round,
      aes(
        xmin = x - 1,
        xmax = x  ,
        ymin = y - 1,
        ymax = y,
        fill = points,
        frame = trial
      ),
      color = "black",
      size = .2
    ) +
    scale_x_continuous(limits = c(-1, 7), breaks = -1:7) +
    scale_y_continuous(limits = c(-1, 7), breaks = -1:7) +
    scale_fill_continuous_divergingx(palette = "RdBu", mid = 0) 
  }
  
  else if(gem == 'no'){
    
    plot <-  ggplot() +
      geom_rect(
        data = data_round,
        aes(
          xmin = x - 1,
          xmax = x  ,
          ymin = y - 1,
          ymax = y,
          fill = points,
          frame = trial
        ),
        color = "black",
        size = .2
      ) +
      scale_x_continuous(limits = c(-1, 7), breaks = -1:7) +
      scale_y_continuous(limits = c(-1, 7), breaks = -1:7) +
      scale_fill_continuous_divergingx(palette = "RdBu", mid = 0) 
  }
 
  return(plot)
   
}
