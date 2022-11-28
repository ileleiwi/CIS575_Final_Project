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
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) %>% #modify target variable to be factor
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
        factor(., levels = c("stroke","no_stroke"))
      
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

#best F1 iteration for each fold from 10x cross val
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


#









#stats data frame
stats_df <- data.frame(matrix(unlist(mod_list_10$stats), 
                              ncol = length(mod_list_10$stats)))
colnames(stats_df) <- names(mod_list_10$stats)
stats_df <- stats_df %>%
  mutate(statistic = names(mod_list_10$stats$resample_1_stats)) %>%
  select(statistic, everything()) %>%
  pivot_longer(cols = -statistic,
               values_to = "value",
               names_to = "iter") %>%
  mutate(iter = str_extract(iter, "(\\d+)"),
         number_of_variables = case_when(iter == 1 ~ 14,
                                         iter == 2 ~ 13,
                                         iter == 3 ~ 12,
                                         iter == 4 ~ 11,
                                         iter == 5 ~ 10,
                                         iter == 6 ~ 9,
                                         iter == 7 ~ 8,
                                         iter == 8 ~ 7,
                                         iter == 9 ~ 6,
                                         iter == 10 ~ 5,
                                         iter == 11 ~ 4,
                                         iter == 12 ~ 3,
                                         iter == 13 ~ 2,
                                         iter == 14 ~ 1)) 

#plot stats
data_max <- stats_df %>%
  group_by(statistic) %>%
  summarise(max_val = max(value))

data_max_varnum <- stats_df %>%
  filter(value %in% data_max$max_val) %>%
  group_by(statistic, value) %>%
  summarise(number_of_variables = paste(number_of_variables, collapse = ", "))

svg("figures/log_reg_statistics.svg")
stats_df %>%
  ggplot(aes(x = number_of_variables, y = value, group = statistic)) +
  geom_point(aes(color = statistic)) +
  geom_line(aes(color = statistic)) +
  scale_x_continuous(breaks = seq(1:14),
                     labels = as.character(seq(1:14))) +
  theme_classic()
dev.off()

#important variable dataframe
fill_list_function <- function(x){
  
  l <- nrow(x)
  out <- x %>%
    rownames_to_column(var = "vars")
  
  if(l < 14){
    add_num <- 14-l
    add_df <- data.frame(vars = letters[1:add_num],
                         Overall = rep(NA, add_num))
    return(rbind(out, add_df))
  }else{
    return(out)
  }
}
 
impvars_df <- map(mod_list$impvars, fill_list_function)  %>%
  bind_rows(., .id = "column_label")
 
  
  
#run best model
model <- glm(stroke ~ ., lg_train_dummy, family = binomial(link = "logit")) 

t<- varimp.logistic(model)
t0 <- varImp(model) %>%
  arrange(desc(Overall))

pred <- cbind(lg_test_dummy,
              predict(model, newdata = lg_test_dummy, type = "response"))
colnames(pred) <- c(colnames(lg_test_dummy),"predicted_prob")
predicts <- pred %>%
  mutate(predictions = ifelse(predicted_prob >= 0.75, 
                              "stroke", "no_stroke")) %>%
  pull(predictions) %>%
  factor(., levels = c("stroke","no_stroke"))

rt <- roc(lg_test_dummy$stroke, predict(model, newdata = lg_test_dummy, type = "response"))

ta <- as.numeric(auc(rt))
names(ta) <- "AUC"
ggroc(rt)
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
