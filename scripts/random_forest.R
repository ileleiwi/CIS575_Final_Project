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
library(doParallel)

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
                                          times = 1) #divide data into 30% train and 70% test

rf_train <- stroke[split_index,]
rf_test <- stroke[-split_index,]

#run random forest with caret
cv_folds <- createFolds(y = rf_train$stroke, 
                        k = 10, 
                        returnTrain = TRUE)

trControl <- trainControl(method = "repeatedcv", 
                          number = 10,
                          search = "grid",
                          classProbs = TRUE,
                          savePredictions = "final",
                          index = cv_folds,
                          sampling = "up",
                          summaryFunction = twoClassSummary)

tuneGrid <- expand.grid(.mtry = c(1: 11))

ntrees <- c(500, 700, 1000)
nodesize <- c(1,5)

params <- expand.grid(ntrees = ntrees,
                      nodesize = nodesize)

store_maxnode <- vector("list", nrow(params))

pb = txtProgressBar(min = 0, 
                    max = nrow(params), 
                    style = 3,
                    width = 50,
                    char = "=")

cl <- makePSOCKcluster(6)
registerDoParallel(cl)

for(i in 1:nrow(params)){
  
  nodesize <- params[i,2]
  ntree <- params[i,1]
  set.seed(123)
  rf_model <- train(stroke~.,
                    data = rf_train,
                    method = "rf",
                    metric = "ROC",
                    tuneGrid = tuneGrid,
                    trControl = trControl,
                    importance = TRUE,
                    nodesize = nodesize,
                    ntree = ntree)
  store_maxnode[[i]] <- rf_model
  
  setTxtProgressBar(pb,i)
}

close(pb)
stopCluster(cl)

names(store_maxnode) <- paste("ntrees:", params$ntrees,
                              "nodesize:", params$nodesize)

results_mtry <- resamples(store_maxnode)
summary(results_mtry)



#Predict 
# pred <- predict(rf_mtry, stroke[-split_index,])
# confusionMatrix(pred, reference = stroke[-split_index,"stroke"])
# varImp(rf_mtry)

