rm(list = ls())

# DATA PREP

library(lme4)
library(lmerTest)
library(tidyverse)
library(tibble)
library(lawstat)
library(jtools)
library(kableExtra)
library(broom)
library(parameters)
library(cowplot)
library(gghalves)

# create variables

data <-
  as_tibble(read.csv('D_CleanExperimentalData/clean_data/clean_data.csv')) %>% 
  mutate(cell = cells - 1,
         x = cell%%8,
         y = trunc(cell/8,0)) 

write.csv(data, "DATA/data_coord.csv")
