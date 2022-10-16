## ---------------------------
##
## Script: balance_data
##
## Purpose: Deal with imbalance in target variable stroke
##
## Author: Ikaia Leleiwi
##
## Date Created: October 16th, 2022
##
## Copyright (c) Ikaia Leleiwi, 2022
## Email: ileleiwi@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory

setwd(paste0("/Users/ikaialeleiwi/Desktop/School/Fall_2022/CIS575/",
             "Final Project/CIS575_Final_Project"))

## ---------------------------

##Set Seed
set.seed(123)

##Libraries

library(tidyverse)

##Data

stroke <- read_csv("raw_data/brain_stroke.csv")

#check target variable balance
stroke %>%
  group_by(stroke) %>%
  count()


