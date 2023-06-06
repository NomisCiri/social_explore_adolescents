library(tidyverse)
# make envs
envirionmentMeanAdol <- seq(-50, 50, length.out = 8)
envirionmentVarianceAdol <- rep(c(2, 10, 15, 25), length.out = 8)
dat_social <- expand.grid(x1 = 1:8, x2 = 1:8)

# number of hidden gems
gem_envs <- list()
# make gem environments
for (i in 1:6) {
  # shuffle environment
  EnvirionemntAdol <- expand.grid(Mean = envirionmentMeanAdol, Variance = envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  index <- sample(x = 1:length(EnvirionemntAdol[, 1]), size = 2)
  Gem <- rnorm(2, mean = 150, sd = 10)
  EnvirionemntAdol[index, 1] <- Gem
  gem_envs[[i]] <- EnvirionemntAdol %>%
    mutate(env_idx = i) %>%
    mutate(x = dat_social$x1, y = dat_social$x2)
} # to be nested for SI - no SI

no_gem_envs <- list()
# make no gem environment
for (i in 1:6) {
  # shuffle environment
  EnvirionemntAdol <- expand.grid(Mean = envirionmentMeanAdol, Variance = envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  no_gem_envs[[i]] <- EnvirionemntAdol %>%
    mutate(env_idx = i) %>%
    mutate(x = dat_social$x1, y = dat_social$x2)
} # to be nested for SI - no SI





# make matrix with indices
index_matrix <- matrix(1:64, nrow = 8, ncol = 8) # %>%as.array()
# rotate function
rotate <- function(x) t(apply(x, 2, rev))

# save rotated index in array
order <- index_matrix %>%
  rotate() %>%
  as.numeric()

# check if it works
no_gem_envs[[2]]$rot_x <- no_gem_envs[[2]]$x[order]
no_gem_envs[[2]]$rot_y <- no_gem_envs[[2]]$y[order]

no_rot <- ggplot(no_gem_envs[[2]], aes(x = x, y = y, fill = Mean)) +
  geom_tile() #+
  #scale_fill_continuous_divergingx(palette = "Spectral", mid = 0)

rot <- ggplot(no_gem_envs[[2]], aes(x = rot_x, y = rot_y, fill = Mean)) +
  geom_tile() #+
  #scale_fill_continuous_divergingx(palette = "Spectral", mid = 0)

no_rot
rot
# rotation(ah,angle=90)
