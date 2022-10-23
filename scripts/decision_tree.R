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
library(caret) #decision tree

##Data

stroke <- read_csv("raw_data/brain_stroke.csv")


#split data
split_index <- createDataPartition(stroke$stroke,
                                   p = .3,
                                   list = FALSE,
                                   time = 1)

c_train <- stroke %>%
  slice(split_index)

c_test <- stroke %>%
  slice(-split_index)

#check target variable balance
c_train %>%
  group_by(stroke) %>%
  count()

#oversample stroke = 1, target number to add 1365
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

for(i in 1:682){
  ad <- c_train %>%
    filter(stroke == 1) %>%
    sample_n(size = 2)
  
  add_df <- rbind(add_df, ad)
}

c_train_even <- rbind(c_train, add_df) %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor




#pruning method Minimal Description Length (MDL) used in KNIME

#method = "rpart", CART model
set.seed(123)
cart_fit <- train(stroke ~ ., 
                    data = stroke_even,
                    method = "rpart",
)