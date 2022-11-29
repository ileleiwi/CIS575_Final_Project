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
library(fastDummies) #dummy variables
library(ConfusionTableR)
library(pROC) 
library(rethinking) #pior prob adjustment
map <- purrr::map

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
         stroke = factor(stroke, levels = c("no_stroke", "stroke"))) %>% #modify target variable to be factor, second factor level is default "success
  select(stroke, everything()) #move stroke to column 1
stroke[sapply(stroke, is.character)] <- lapply(stroke[sapply(stroke, is.character)], 
                                               as.factor) #transform chr cols to factors



#split data
set.seed(123)
split_index <- createDataPartition(stroke$stroke,
                                   p = .3, 
                                   list = FALSE,
                                   times = 1) #divide data into 30% test and 70% train



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
purrr::map(lg_train[,keep_cols], table)

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

#clean up column names
lg_train_dummy <- janitor::clean_names(lg_train_dummy)
 
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
  
  out <- janitor::clean_names(out)
  
  return(out)
}

#transform test data
lg_test_dummy <-pre_process_funct(lg_test)


#reference class for work type is Private
#reference class for smoking_status is Unknown




##functions for recursive feature elimination and picking best model
rfe_funct <- function(df, iter, p_train = 0.7 , pred_prob_threshold = 0.5, seed = 123){
  
  #df = training data set
  #iter = number of resampling iterations
  #p_train = sub split proportion to allocate to training data
  #pred_prob_threshold = predicted probability threshold for target class calling
  
  stat_list <- list()
  coef_list <- list()
  impvar_list <- list()
  impvar_list_next <- list()
  roc_list <- list()
  model_list <- list()
  output_names <- paste("resample", 1:iter, sep = "_")
  
  count <- 0
  for(it in 1:(iter+1)){
    if(count == iter){
      names(stat_list) <- paste(output_names, "stats", sep = "_")
      names(coef_list) <- paste(output_names, "coefs", sep = "_")
      names(impvar_list) <- paste(output_names, "impvars", sep = "_")
      names(roc_list) <- paste(output_names, "roc", sep = "_")
      names(model_list) <- paste(output_names, "model", sep = "_")
      out_obj <- list(stat_list, coef_list, impvar_list, roc_list, model_list)
      names(out_obj) <- c("stats", "coefs", "impvars", "roc", "models")
      return(out_obj)
    }else{
      #partition data into training and test/hold-back set
      set.seed(seed)
      split <- createDataPartition(df$stroke,
                                   p = 1-p_train, 
                                   list = FALSE,
                                   times = 1) #divide data into 30% test and 70% train
      TRAIN <- df[-split,]
      TEST <- df[split,]
      
      #subset by important vars
      if(length(impvar_list) == 0){
        TRAIN <- TRAIN
      }else{
        TRAIN <- TRAIN %>%
          select(stroke, rownames(impvar_list_next[[length(impvar_list_next)]]))
      }
      
      #train model
      model <- glm(stroke ~ ., TRAIN, family = binomial(link = "logit")) 
      pred <- cbind(TEST, predict(model, newdata = TEST, type = "response"))
      colnames(pred) <- c(colnames(TEST),"predicted_prob")
      
      #get predictions as factor
      predictions <- pred %>%
        mutate(predictions = ifelse(predicted_prob >= pred_prob_threshold, 
                                    "stroke", "no_stroke")) %>%
        pull(predictions) %>%
        factor(., levels = c("no_stroke","stroke"))
      
      #produce confusion matrix
      cm <- confusionMatrix(predictions, reference = TEST$stroke)
      
      #produce roc object
      r_obj <- roc(TEST$stroke, predict(model, newdata = TEST, type = "response"))
      stat_auc <- as.numeric(auc(r_obj))
      names(stat_auc) <- "AUC"
      #save model stats
      stat_vect <- c(cm$overall, cm$byClass, stat_auc)
      stat_list[[length(stat_list)+1]] <- stat_vect
      
      #save coef 
      coef_list[[length(coef_list)+1]] <- model$coefficients
      
      #save important variables
      impvar_list[[length(impvar_list)+1]] <- varImp(model) 
      
      important_vars_next <- varImp(model) %>%
        slice_max(Overall, n = nrow(varImp(model))-1) 
      impvar_list_next[[length(impvar_list_next)+1]] <- important_vars_next
     
      #save roc object
      roc_list[[length(roc_list)+1]] <- r_obj
      
      #save model
      model_list[[length(model_list)+1]] <- model
      
      #add to count
      count <- count+1
    } #end of else 
  } #end of for loop it
} #end of function

#find most accurate iter
find_stat <- function(l, s = "F1"){
  
  best_vec <-c()
  #find best instance in run based on statistic we wish to consider
  #resolve ties with lowest number of impvars (highest iter num) 
  for(i in 1:length(l)){
  #consider only one run at a time
  temp_list_vect <- unlist(l[[i]][["stats"]])
   #pull maximum value for statistic we're interested in
   max_val <- max(temp_list_vect[which(endsWith(names(temp_list_vect), paste0(".",s)))])
   max_s_names <- names(which(temp_list_vect == max_val)) 
   
   if(length(max_s_names) > 1){
     max_s_names_clean <- str_remove(max_s_names, paste0("_stats",".",s))
     idx_keep <- length(max_s_names_clean)
     keep_name <- max_s_names_clean[idx_keep]
   }else{
     keep_name <- str_remove(max_s_names, paste0("_stats",".",s))
   }
   
   best_vec <- c(best_vec, keep_name)
  }
  names(best_vec) <- paste("rep", seq(1:length(l)), sep = "_")
  return(best_vec)
}

#filter list to best F1
filter_list <- function(l, chr){

  s_l <- pluck(l$stats, paste(chr, "stats", sep = "_"))
  c_l <- pluck(l$coefs, paste(chr, "coefs", sep = "_"))
  iv_l <- pluck(l$impvars, paste(chr, "impvars", sep = "_"))
  r_l <- pluck(l$roc, paste(chr, "roc", sep = "_"))
  m_l <- pluck(l$models, paste(chr, "model", sep = "_"))
  
  return(list(stats = s_l, 
              coefs = c_l, 
              impvars = iv_l, 
              roc = r_l,
              models = m_l))
}

#pull stats from each iteration
pull_stats <- function(l){
  F1 <- l$stats["F1"]
  Sensitivity <- l$stats["Sensitivity"]
  Specificity <- l$stats["Specificity"]
  AUC <- l$stats["AUC"]
  Important_vars <- rownames(l$impvars) 
  
  out_list <- list(F1 = unlist(F1),
                   Sensitivity = unlist(Sensitivity),
                   Specificity = unlist(Specificity),
                   AUC = AUC,
                   Important_vars = paste(Important_vars,
                                          collapse = ", "))
  
  return(out_list)
}


#run backward feature elimination 10x
seeds <- sample(1:100, 10, replace = FALSE)
mod_list_10 <- map(seeds, ~rfe_funct(lg_train_dummy, iter = 14, seed = .x))

#best AUC iteration for each fold from 10x cross val
best_replication <- find_stat(mod_list_10, s = "AUC")
mod_list_10_best <- map2(mod_list_10, best_replication, ~filter_list(.x, .y))
names(mod_list_10_best) <- paste(best_replication, seq(1:10), sep = ".")

#pull stats from each iteration and compare
best_stats_list <- map(mod_list_10_best,pull_stats)
best_stats <- data.frame(matrix(unlist(best_stats_list), 
                                ncol = length(best_stats_list)))
colnames(best_stats) <- names(best_stats_list)


best_stats_df <- best_stats %>%
  mutate(metric = names(best_stats_list[[1]])) %>%
  pivot_longer(cols = -metric,
               names_to = "id",
               values_to = "value")



auc_acc_sen_spe <- best_stats_df %>%
  filter(metric != "Important_vars")


#roc list
roc_list <- map(mod_list_10_best, ~pluck(.x, "roc"))

#auc table
#AUC
auc_data <- roc_list %>%
  map(~tibble(AUC = as.numeric(.x$auc))) %>%
  bind_rows(.id = "name") 


#lables for plot
auc_data_labels <- auc_data %>%
  mutate(label_long = paste0(name, ", AUC = ", paste(round(AUC,3))),
         label_AUC = paste0("AUC = ", paste(round(AUC, 3)))) %>%
  arrange(desc(AUC))

#plot rocs
svg("figures/log_reg_AUC.svg")
ggroc(roc_list) +
  scale_color_discrete(labels = auc_data_labels$label_long) +
  theme_classic() +
  labs(color = "Logistic Regression 10 Best")
dev.off()

#plot important variables
impvars_df <- best_stats_df %>%
  filter(metric == "Important_vars") %>%
  mutate(num_imp_vars = str_count(value, ",")+1) %>%
  rename("imp_vars" = "value") %>%
  left_join(auc_data_labels, by = c("id" = "name")) 

num_imp_factor <- impvars_df %>%
  arrange(num_imp_vars) %>%
  pull(id)

impvars_df <- impvars_df %>%
  mutate(id = factor(id, levels = num_imp_factor))

svg("figures/log_reg_impvars.svg")
impvars_df %>%
  ggplot(aes(x = id, y = num_imp_vars)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(AUC,3))),
            size = 4,
            vjust = -.3) +
  scale_y_continuous(breaks = seq(1:13),
                     labels = as.character(seq(1:13))) +
  geom_label(x = 2.5, y = 11.5, label = "Bar Labels = AUC") +
  labs(y = "Number of Variables in Model",
       x = "Resample Iteration") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
dev.off()


#try updating prior probs
pred_best <- predict(mod_list_10_best$resample_14.5$models, lg_test_dummy, type = "response")

hist(pred_best)

#log odds of model predictions
lo.preds <- logit(pred_best)
#original B0
model.prior.lo <- mod_list_10_best$resample_14.5$models$coefficients[1]
#probability of having a stroke from training data
table(lg_train_dummy$stroke)[2]
prob_stroke <- table(lg_train_dummy$stroke)[2]/table(lg_train_dummy$stroke)[1]
#corrected B0
correct.lo <- logit(prob_stroke) #log(x/(1-x))
#updated predictions
updated.lo.preds <- lo.preds + (correct.lo - model.prior.lo)
#log odds converted to probabilities
updated.preds <- logistic(updated.lo.preds)

hist(updated.preds)

#add updated predictions to dataframe
pred_best_df <- cbind(lg_test_dummy, updated.preds)
colnames(pred_best_df) <- c(colnames(lg_test_dummy),"predicted_prob")

#get predictions as factor
predictions_best <- pred_best_df %>%
  mutate(predictions = ifelse(predicted_prob >= prob_stroke, 
                              "stroke", "no_stroke")) %>%
  pull(predictions) %>%
  factor(., levels = c("no_stroke","stroke"))

#produce confusion matrix
confusionMatrix(predictions_best, reference = lg_test_dummy$stroke)


#try pca
no_target_train <- lg_train_dummy[,-1]
no_target_test <- lg_test_dummy[,-1]

pca_train <- prcomp(no_target_train, scale. = FALSE) #data is already scaled
pca_test <- prcomp(no_target_test, scale. = FALSE)

pca_train_df <- cbind(lg_train_dummy[,1], pca_train$x) %>% 
  as.data.frame() %>%
  rename("stroke" = "V1")
pca_test_df <- cbind(lg_test_dummy[,1], pca_test$x) %>% 
  as.data.frame() %>%
  rename("stroke" = "V1")

#plot principle components to see which explain most variance
pca.var <- pca_train$sdev^2
pve <- pca.var/sum(pca.var)
plot(pve, xlab = "Principal component",
     ylab = "Proportion of variation explained",
     ylim = c(0,1),
     type = "b")

plot(cumsum(pve), xlab = "Principal component",
     ylab = "Accumulative Prop. of variation explained",
     ylim = c(0,1),
     type = "b")

#based on above plot we will choos PC1-PC11 which account from ~ 90% of variance in data
pca_train_df <- pca_train_df %>%
  select(stroke, PC1:PC11) %>%
  mutate(stroke = factor(stroke))

pca_test_df <- pca_test_df %>%
  select(stroke, PC1:PC11) %>%
  mutate(stroke = factor(stroke))

#train model
model_pca <- glm(stroke ~ ., pca_train_df, family = binomial(link = "logit")) 
pred_pca <- cbind(pca_test_df, predict(model_pca, newdata = pca_test_df, type = "response"))
colnames(pred_pca) <- c(colnames(pca_test_df),"predicted_prob")
#get predictions as factor
predictions_pca <- pred_pca %>%
  mutate(predictions = ifelse(predicted_prob >= prob_stroke, 
                              "stroke", "no_stroke")) %>%
  pull(predictions) %>%
  factor(., levels = c("no_stroke", "stroke"))

pca_test_df_factor <- pca_test_df %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = levels(predictions_pca)))

#produce confusion matrix
confusionMatrix(predictions_pca, reference = pca_test_df_factor$stroke)


  
#run best model
prob_stroke <- table(lg_train_dummy$stroke)[2]/table(lg_train_dummy$stroke)[1]
pred <- cbind(lg_test_dummy,
              predict(mod_list_10_best$resample_14.5$models, newdata = lg_test_dummy, type = "response"))
colnames(pred) <- c(colnames(lg_test_dummy),"predicted_prob")
predicts <- pred %>%
  mutate(predictions = ifelse(predicted_prob >= prob_stroke , 
                              "stroke", "no_stroke")) %>%
  pull(predictions) %>%
  factor(., levels = c("no_stroke","stroke"))

rt <- roc(lg_test_dummy$stroke, predict(mod_list_10_best$resample_14.5$models, newdata = lg_test_dummy, type = "response"))


confusionMatrix(predicts, 
                      reference = lg_test_dummy$stroke)

svg("figures/final_regression_confusion_matrix.svg", )
binary_visualiseR(train_labels = predicts,
                  truth_labels = lg_test_dummy$stroke,
                  class_label1 = "No Stroke",
                  class_label2 = "Stroke",
                  custom_title = "Final Logistic Regression Model Confusion Matrix")
dev.off()


summary(mod_list_10_best$resample_14.5$models)

numerator_age <- exp(-3.81931+1.5527)
denominator <- exp(-3.81931)
logodds <- numerator_age/denominator


ta <- as.numeric(auc(rt))
names(ta) <- "AUC"
ggroc(rt)

