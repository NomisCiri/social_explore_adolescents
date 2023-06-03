library(tidyverse)
library(colorspace)
library(rjson)
library(jsonlite)

# these are the same that are used in the experiment
env_gems <- fromJSON("C:/Users/Andrea/OneDrive - UvA/Desktop/env_gems.json")

# save into different files
env1 <- as.data.frame(env_gems[[1]])
env2 <- as.data.frame(env_gems[[2]])
env3 <- as.data.frame(env_gems[[3]])
env4 <- as.data.frame(env_gems[[4]])
env5 <- as.data.frame(env_gems[[5]])
env6 <- as.data.frame(env_gems[[6]])

# make matrix with indices
index_matrix <- matrix(1:64, nrow = 8, ncol = 8)#%>%as.array()
#rotate function
rotate <- function(x)
  t(apply(x, 2, rev))

# save rotated index in array
order = index_matrix %>% rotate() %>% as.numeric()

# check if it works
env1$rot_x = env1$x[order]
env1$rot_y = env1$y[order]

no_rot <- ggplot(env2, aes(x = x, y = y, fill = Mean)) + geom_tile() +
  scale_fill_continuous_divergingx(palette = 'Spectral', mid = 0)

rot <-
  ggplot(env2, aes(x = rot_x, y = rot_y, fill = Mean)) + geom_tile() +
  scale_fill_continuous_divergingx(palette = 'Spectral', mid = 0)

no_rot
rot
#rotation(ah,angle=90)


### ok now save neew environments

# env7 (old 2)


env7 <- env2 %>% 
  mutate(order = order)
env7 <- env7[order(env7$order), ]


# env 8 (old 4)
env8 <- env4 %>% 
  mutate(order = order)
env8 <- env8[order(env8$order), ]


# env 9 (old 6)
env9 <- env6 %>% 
  mutate(order = order)
env9 <- env9[order(env9$order), ]



#gems
env7_json<-env7 %>% toJSON(dataframe = 'columns')
env8_json<-env8%>%toJSON(dataframe = 'columns')
env9_json<-env9%>%toJSON(dataframe = 'columns')


gem_json <- cat('{"7":', env7_json, 
                ',"8":', env8_json, 
                ',"9":', env9_json,
                '}',
                sep = ' ',
                file = "environments/extra_envs.json"
)
