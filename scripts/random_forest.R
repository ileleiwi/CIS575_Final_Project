## ---------------------------
##
## Script: random_forest
##
## Purpose: Perform a bagged random forest on stroke data
##
## Author: Ikaia Leleiwi
##
## Date Created: November 14th, 2022
##
## Copyright (c) Ikaia Leleiwi, 2022
## Email: ileleiwi@gmail.com
##
## ---------------------------
##
## Notes: https://www.listendata.com/2014/11/random-forest-with-r.html#id-8c2e5c
##        https://rpubs.com/abhaypadda/smote-for-imbalanced-data
##   
##
## ---------------------------

## set working directory

setwd(paste0("/Users/ikaialeleiwi/Desktop/School/Fall_2022/CIS575/",
             "Final Project/CIS575_Final_Project"))

## ---------------------------

##Libraries
library(tidyverse)
library(caret) #createDataPartition
#library(DMwR) #SMOTE funciton


##Seed
set.seed(123)

##Data
stroke <- read_csv("raw_data/brain_stroke.csv") %>%
  as.data.frame() %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor

stroke[sapply(stroke, is.character)] <- lapply(stroke[sapply(stroke, is.character)], 
                                                   as.factor) #transform chr cols to factors



#split data
set.seed(123)
split_index <- createDataPartition(stroke$stroke,
                                          p = .3, 
                                          list = FALSE,
                                          time = 1) #divide data into 30% train and 70% test

#run random forest with caret
trControl <- trainControl(method = "oob", 
                          number = 25,
                          sampling = "smote")

tuneGrid <- expand.grid(.mtry = c(1: 11))

rf_mtry <- train(stroke~.,
                 data = stroke,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 500)
print(rf_mtry)

#Predict 
pred <- predict(rf_mtry, stroke[-split_index,])
confusionMatrix(pred, reference = stroke[-split_index,"stroke"])
varImp(rf_mtry)

