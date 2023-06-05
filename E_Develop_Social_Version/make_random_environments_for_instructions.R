### Make random environments.
library(jsonlite)

### ADD SEED IF WE WANT TO KEEP THEM

library(jsonlite)
library(tidyverse)

# make envs
envirionmentMeanAdol = seq(-50, 50, length.out = 8)
envirionmentVarianceAdol = rep(c(2, 10, 15, 25), length.out = 8)
dat_social = expand.grid(x1 = 1:8, x2 = 1:8)

# number of hidden gems
gem_envs = list()
#make gem environments
for (i in 1:6) {
  #shuffle environment
  EnvirionemntAdol = expand.grid(Mean = envirionmentMeanAdol, Variance =
                                   envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  index = sample(x = 1:length(EnvirionemntAdol[, 1]), size = 2)
  Gem = rnorm(2, mean = 150, sd = 10)
  EnvirionemntAdol[index, 1] = Gem
  gem_envs[[i]] = EnvirionemntAdol %>% mutate(env_idx = i) %>% mutate(x =
                                                                        dat_social$x1, y = dat_social$x2)
}# to be nested for SI - no SI

no_gem_envs = list()
#make no gem environment
for (i in 1:6) {
  #shuffle environment
  EnvirionemntAdol = expand.grid(Mean = envirionmentMeanAdol, Variance =
                                   envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  no_gem_envs[[i]] = EnvirionemntAdol %>% mutate(env_idx = i) %>% mutate(x =
                                                                           dat_social$x1, y = dat_social$x2)
}# to be nested for SI - no SI

### Save 1 env for the practice round


#gems
one_env<-no_gem_envs[[1]]%>%toJSON(dataframe = 'columns')

ex_en <- cat('{"1":', one_env, 
                               '}',
                sep = ' ',
                file = "environments/example_environment.json"
)

