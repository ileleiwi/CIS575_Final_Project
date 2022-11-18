## ---------------------------
##
## Script: logistic_regression
##
## Purpose: run a logistic regression on the stroke dataset
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
library(caret) #createDataPartition
library(fastDummies)
library(doParallel)
library(ConfusionTableR)


##Seed
set.seed(123)

##Data
stroke <- read_csv("raw_data/brain_stroke.csv") %>%
  as.data.frame() 

str(stroke)
summary(stroke)
is.na(stroke) %>% sum()

stroke <- stroke %>%
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



lg_train <- stroke[-split_index,]
lg_test <- stroke[split_index,]

#plot data to determine appropriate transformations
for(i in colnames(select_if(lg_train, is.numeric))){
  hist(lg_train[,i], main = i)
}

#transform avg_glucose_level
lg_train[,'avg_glucose_level'] <- log10(lg_train[,'avg_glucose_level'])

#make dummy variables
keep_cols <- lg_train %>%
  map_dbl(~length(levels(.x))) %>%
  sapply(., function(x) x > 2) %>%
  unlist()

#look at counts per factor level and check diff factors to set which one we will use as reference
map(lg_train[,keep_cols], table)

#make dummy variable dataframe
dummy_df <- lg_train[,keep_cols] %>%
  dummy_cols(select_columns = c("work_type","smoking_status")) %>%
  select(-work_type_Private,
         -smoking_status_Unknown) #remove one categorical column from each factor

#transform remaining factors to 1,0 and scale all numeric columns
lg_train_dummy <- lg_train %>%
  cbind(dummy_df) %>%
  select(-work_type,
         -smoking_status) %>%
  mutate(ever_married = ifelse(ever_married == "No", 0, 1),
         Residence_type = ifelse(Residence_type == "Rural", 0, 1),
         gender = ifelse(gender == "Male", 1, 0)) %>%
  mutate_at(vars(-stroke),scale) 

#custom preprocessing function
pre_process_funct <- function(df){
  #make dummy variables
  #transform avg_glucose_level
  #remove one categorical column from each factor
  #transform remaining factors to 1,0 and scale all numeric columns
  
  out <- df %>%
    dummy_cols(select_columns = c("work_type","smoking_status")) %>%
    select(-c(work_type_Private, smoking_status_Unknown,
              work_type, smoking_status)) %>%
    mutate(avg_glucose_level = log10(avg_glucose_level),
           ever_married = ifelse(ever_married == "No", 0, 1),
           Residence_type = ifelse(Residence_type == "Rural", 0, 1),
           gender = ifelse(gender == "Male", 1, 0)) %>%
    mutate_at(vars(-stroke),scale) 
  
  return(out)
}

#transform test data
lg_test_dummy <-pre_process_funct(lg_test)


#reference class for work type is Private
#reference class for smoking_status is Unknown

model <- glm(stroke ~ ., lg_train_dummy, family = binomial(link = "logit")) 
pred <- cbind(lg_test_dummy,
              predict(model, newdata = lg_test_dummy, type = "response"))
colnames(pred) <- c(colnames(lg_test_dummy),"predicted_prob")


#recursive feature elimination function
rfe_funct <- function(df, iter, p_train, pred_prob_threshold = 0.75){
  
  #partition data into training and test/hold-back set
  split <- createDataPartition(stroke$stroke,
                                     p = 1-p_train, 
                                     list = FALSE,
                                     times = 1) #divide data into 30% train and 70% test
  TRAIN <- df[-split,]
  TEST <- df[split,]
  
  model_all <- glm(stroke ~ ., TRAIN, family = "binomial") 
  pred_all <- cbind(TEST,
                    predict(model_all, newdata = TEST, type = "response"))
  colnames(pred) <- c(colnames(TEST),"predicted_prob")
  
  predictions <- pred_all %>%
    mutate(predictions = ifelse(predicted_prob >= pred_prob_threshold, 
                                "stroke", "no_stroke")) %>%
    pull(predictions)
  
  
}


#train model
log.glmRFE <- lrFuncs
log.glmRFE$summary <- twoClassSummary

ctrl <- rfeControl(functions = lrFuncs,
                   method = "cv",
                   number = 10,
                   verbose = FALSE,
                   rerank = FALSE)

lr_rfe <- rfe(stroke ~., 
              x = lg_train_final,
              y = lg_train$stroke,
              sizes = c(1:10),
              rfeControl = ctrl,
              metric = "Accuracy")

logit.CV <- train(x= pP_dV_train , y= lg_train$stroke, 
                  method = 'glmnet',
                  trControl = trControl,
                  family = 'binomial' )

cors <- findCorrelation(cor(final_train), cutoff = 0.90)
if (length(cors) != 0)
  x <- final_train[,-cors]
lr_rfe <- rfe(stroke ~., 
              x = x,
              y = lg_train$stroke,
              sizes = c(1:11),
              rfeControl = ctrl,
              metric = "Accuracy")
