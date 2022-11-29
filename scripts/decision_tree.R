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
library(caret) #createDataPartition, confustionMatrix
library(rpart) #decision tree
library(rpart.plot) #visualize tree
library(insight)
library(knitr)
library(ConfusionTableR)
##Data

stroke <- read_csv("raw_data/brain_stroke.csv")

#split data
set.seed(123)
split_index <- caret::createDataPartition(stroke$stroke,
                                   p = .3, 
                                   list = FALSE,
                                   time = 1) #divide data into 30% test and 70% train

c_train <- stroke %>%
  slice(-split_index) 

c_test <- stroke %>%
  slice(split_index) %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor

#check target variable balance
c_train %>%
  group_by(stroke) %>%
  count()

##bootstrapping function
resample_up_down <- function(df, target_col, iter=1, up_or_down, lower_val = 1){
   
  if(up_or_down == "up"){
    df_out <- df
    for(i in 1:iter){
      ad <- df %>%
        filter(.[[target_col]] == lower_val) %>%
        sample_n(size = 1)
      
      df_out <- rbind(df_out, ad)
    }
  }else if (up_or_down == "down"){
    keep_df <- df %>%
      filter(.[[target_col]] == lower_val) 
    samp_df <- df %>%
      filter(.[[target_col]] != lower_val) %>%
      sample_n(size = iter)
    
    df_out <- rbind(keep_df, samp_df)
  }
  return(df_out)
}


#oversample stroke = 1, target number to add 3116
c_train_up <- resample_up_down(df = c_train,
                                 target_col = "stroke", 
                                 iter = 3116, 
                                 up_or_down = "up", 
                                 lower_val = 1) %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor

#downsample stroke = 0, target number to reduce to 185
c_train_down <- resample_up_down(df = c_train,
                                 target_col = "stroke", 
                                 iter = 185, 
                                 up_or_down = "down", 
                                 lower_val = 1) %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor


##SMOTE oversample
c_train_smote <- as.data.frame(c_train) #transform to dataframe
c_train_smote[sapply(c_train_smote, is.character)] <- lapply(c_train_smote[sapply(c_train_smote, is.character)], 
                                       as.factor) #transform chr cols to factors
c_train_smote$stroke <- factor(c_train_smote$stroke) #transform target col to factor
c_train_smote <- SMOTE(stroke ~ .,
                       data = c_train_smote,
                       perc.over = 1000,
                       perc.under = 100)

c_train_smote <- c_train_smote %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor

#change c_train to have character factor target var
c_train <- c_train %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) #modify target variable to be factor

#pruning method Minimal Description Length (MDL) used in KNIME
#method = "rpart", CART model
model_dfs <- list(c_train, c_train_up, c_train_down, c_train_smote)

set.seed(123)
models <- map(model_dfs, ~rpart(stroke ~ .,
                             data = as.data.frame(.x),
                             method = "class", 
                             cp=-1))

names(models) <- c("c_train", "c_train_up", "c_train_down", "c_train_smote")

#visualize models and check var importance

compare_trees <- function(mod_list){
  combined_cp <- data.frame(CP = numeric(),
                            nsplit = numeric(),
                            `rel error` = numeric(),
                            xerror = numeric(),
                            xstd = numeric(),
                            keep = character(),
                            model = character())
  
  for(i in 1:length(mod_list)){
    cp_df <- mod_list[[i]] %>%
      printcp() %>%
      as.data.frame()
    
    cp_df <- cp_df %>%
      mutate(keep = ifelse(xerror < min(cp_df$xerror) + xstd, "yes", "no" ),
             model = names(mod_list[i]))
    
    combined_cp <- rbind(combined_cp, cp_df)
  }
  
  return(combined_cp)
}

cp_df <- compare_trees(models)
cp_df %>%
  filter(nsplit != 0,
         keep == "yes") %>%
  arrange(xerror)

#observe model variable importance and complexity paramaters
map(models, ~printcp(.x))
map(models, ~plotcp(.x))

for(i in 1:length(models)){
  svg(paste0("figures/", names(models[i]), "_cp_plot.svg"))
  plotcp(models[[i]])
  dev.off()
}

#
names(models)
plotcp(models[[1]])
plotcp(models[[2]])
plotcp(models[[3]])
plotcp(models[[4]])

printcp(models[[2]]) %>%
  as.data.frame() %>%
  arrange(xerror, nsplit)


map(models, ~print(.x$variable.importance))

#predict test data using models
set.seed(123)
preds <- map(models, .f = ~predict(.x, c_test, type = "class"))

walk(preds, ~print(confusionMatrix(factor(.x), factor(c_test$stroke))))


#produce html tables for github
print_git_confmat <- function(prediction_obj, test_target_col, type, cmat_element){
  test_dat <- factor(test_target_col)
  cmat <- as.list(confusionMatrix(factor(prediction_obj), test_dat))
  return(kable(cmat[[cmat_element]], format = type))
}


#produce figure of confusion matrices
confusion_matrix_fig <- function(prediction_obj, test_target_col, fig_name){
  df <- cbind(data.frame(class_preds = prediction_obj), test_target_col)
  bin_cm <- binary_class_cm(df$class_preds, df$`test_target_col`)
  svg(paste0("figures/", fig_name, "_cm_stats.svg"))
  binary_visualiseR(train_labels = df$class_preds,
                    truth_labels = df$`test_target_col`,
                    class_label1 = "Stroke",
                    class_label2 = "No Stroke",
                    custom_title = "Confusion Matrix")
  dev.off()
}

map2(preds, names(preds), ~confusion_matrix_fig(.x, 
                                                c_test$stroke,
                                                .y))

#trim upsampled tree based on cp plot at 129 splits
set.seed(123)

ctrl <- rpart.control(maxdepth = 129)
up_model_trimmed <- rpart(stroke ~ ., 
                          data = as.data.frame(c_train_up),
                          method = "class", 
                          cp = 0.0009088155)

up_pred <- predict(up_model_trimmed, c_test, type = "class")
up_cm <- binary_class_cm(up_pred, c_test$stroke)

plotcp(up_model_trimmed)

svg(paste0("figures/", "upsampled_decision_tree_pruned", "_cm.svg"))
binary_visualiseR(train_labels = up_pred,
                  truth_labels = c_test$stroke,
                  class_label1 = "Stroke",
                  class_label2 = "No Stroke",
                  custom_title = "Confusion Matrix")
dev.off





                