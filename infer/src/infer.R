# infer implements an expressive grammar to perform statistical inference that coheres with the tidyverse design framework. 

library(dplyr,    verbose = F, warn.conflicts = F)
library(magrittr, verbose = F, warn.conflicts = F)
library(caret,    verbose = F, warn.conflicts = F)
library(recipes,  verbose = F, warn.conflicts = F)
library(janitor,  verbose = F, warn.conflicts = F)
library(skimr,    verbose = F, warn.conflicts = F)
library(infer,    verbose = F, warn.conflicts = F)

options(scipen = 100)
rm(list=ls())

data(gss)

gss %>%  glimpse()

# specify(): Specifying Response (and Explanatory) Variables
gss %>%  specify(response = age)
gss %>%  specify(response = age) %>% class()

gss %>%  specify(age ~ partyid  ) # 수 ~ 범 
gss %>%  specify(response = age, explanatory =  partyid  ) # 수 ~ 범 

gss %>%  specify(response = college, success = "degree") # 범

# hypothesize(): Declaring the Null Hypothesis
gss %>%  specify(college ~ partyid, success = "degree") %>%  # 범 ~ 수 
  hypothesise(null = "independence") # independence or point

gss %>%  glimpse()
gss %>%  specify(response = hours) %>%  
  hypothesise(null = "point", mu = 40 )

# generate(): Generating the Null Distribution
gss  %>%  
  specify(response = hours) %>%  
  hypothesise(null = "point", mu = 40 ) %>%  
  generate(reps = 1000, type = "bootstrap")
  
gss %>%
  specify(partyid ~ age) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")


gss %>%  
  specify(response = hours) %>%  
  hypothesise(null = "point", mu = 40) %>%  
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean") %>% 
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")

library(caretEnsemble)

vignette("caret")
vignette("infer")
vignette("caretEnsemble")

help(package = "infer")
