## ---------------------------
##
## Script: decision_tree
##
## Purpose: Deal with imbalance in target variable stroke 
##          and run a decision tree model
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

##Libraries

library(tidyverse)
library(DMwR) #SMOTE funciton
library(tree) #decision tree
library(randomForest)
library(rpart)

##Data

stroke <- read_csv("raw_data/brain_stroke.csv")

#check target variable balance
stroke %>%
  group_by(stroke) %>%
  count()

#oversample stroke = 1, target number to add 4485
add_df <- data.frame(gender = character(),
                     age = double(),
                     hypertension = double(),
                     heart_disease = double(),
                     ever_married = character(),
                     work_type = character(),
                     Residence_type = character(),
                     avg_glucose_level = double(),
                     bmi = double(),
                     smoking_status = character(),
                     stroke = double())

for(i in 1:2242){
  ad <- stroke%>%
    filter(stroke == 1) %>%
    sample_n(size = 2)
  
  add_df <- rbind(add_df, ad)
}

stroke_even <- rbind(stroke, add_df)


#split data

