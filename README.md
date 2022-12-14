## CIS575_Final_Project

# Stroke Risk Prediction Analysis

# Problem/Opportunity   

Cardiovascular disease is the world’s leading cause of death<sup>1</sup>. Data from the CDC indicates that in 2020 16.7% of all deaths from cardiovascular disease in the United States were due to strokes, and costs associated with strokes in the U.S. were ~$53 billion in 2017 and 2018<sup>2</sup>. Furthermore, in 2018 a survey of general practitioners indicated a primary health care domain need for better systematic screening methods to predict patient stroke risk<sup>3</sup>. For these reasons our group has chosen to build a stroke prediction model to evaluate patient proclivity for a stroke event from various health and lifestyle characteristics. Clearly the ability for clinicians to better identify patients at risk for strokes would allow for greater prophylactic care, ultimately leading to fewer deaths or stroke-related impairments. Beyond the clinical value of stroke prediction, firms in the pharmaceutical and insurance domains may find interest in a stroke prediction model able to guide future drug discovery or allow for more accurate patient assessment to determine policy premiums respectively.



Sources:

World Health Organization. “Cardiovascular Diseases (CVDs).” Retrieved September 22, 2022 (https://www.who.int/news-room/fact-sheets/detail/cardiovascular-diseases-(cvds)).

CDC. 2022. “Stroke Facts | Cdc.Gov.” Centers for Disease Control and Prevention. Retrieved September 22, 2022 (https://www.cdc.gov/stroke/facts.htm).

Patomella, Ann-Helen, Gustav Mickols, Eric Asaba, Gunnar Nilsson, Cecilia Fridén, Anders Kottorp, Bo Christer Bertilson, and Kerstin Tham. 2018. “General Practitioners’ Reasoning on Risk Screening and Primary Prevention of Stroke – a Focus Group Study.” BMC Family Practice 19(1):190. doi: 10.1186/s12875-018-0883-6.



# Business Objective

Through the implementation of various machine learning and predictive modeling techniques our group intends to build a stroke prediction model using patient data of individual lifestyle and health characteristics. Specifically, our goal is to determine a patient's risk of having a stroke as early in life as possible with the clinical objective of summarily prescribing stroke prevention medication and appropriate lifestyle changes to produce more favorable patient outcomes. The intent is to also create a model with appeal to private firms with vested interests in an individual's stroke proclivity. Such targets include companies in the pharmaceutical, travel, insurance, healthcare, and health supplement sectors.

# Preliminary Data Exploration and Findings

Initial exploration of the Stroke dataset involved creating histograms of each variable to visualize the distributions. We also checked for the presence of missing values, of which there were none.

![variable plots](/figures/variable_plots.svg)

It seems that the target variable "stroke" is highly unbalanced, favoring the negative class (no stroke). The same imbalance favoring the negative class is observed in "hypertension" and "heart_disease" variables. "bmi" is relatively normally distributed while "avg_glucose_level" is right skewed. The nominal variables "gender" and "Residence_type" are the most evenly balanced variable of this data type, although the dataset included nearly 1000 more observations from Females than from Males.

Next each numeric variable was plotted against each other numeric variable to see if any correlation could be seen

![cor scatter plots](/figures/scatter_cor_plots.svg)

The scatterplots indicate that potential correlations could exist between "age", "bmi", and "avg_glucose_levels". Pearson correlation was calculated for each pair of these variables and the results are shown below.

![pearson cor plots](/figures/cor_plot.svg)

The correlation coefficients are relatively low so we may move forward with the assumption that variables are not correlated.

More summary statistics can be viewed [here](/documents/summary_stats.pdf)

# Description of Data Preparation

Our data analysis plan includes implementing cross-validated decision trees with different strategies to balance the data (no correction, up sampling positive class, down sampling negative class, SMOTE rebalancing) and then use the strategy that provides the best results (most accurate model) to train a bagged random forest model. The results of the random forest model will be used to further inform feature selection for a logistic regression. Finally, all models will be compared and the best one chosen. Prior to logistic regression, numeric variables will be min-max scaled and dummy variables will be created from categorical each variable in the data. We will divide the data into a training and testing set where appropriate, each partition containing 70% and 30% of the data respectively.

# Decision Tree Classification
A CART classification decision tree splitting with the gini index was implemented using the `rpart` library in R. Various balancing strategies were implemented on the training data to balance the target variable classes.

c_train = no balancing \
c_train_up = random upsampling of positive class observations \
c_train_smote = rebalancing using the SMOTE algorithm as implemented in the `DMwR`\
c_train_down = random downsampling of negative class observations

The default 10-fold cross-validation results are shown below relating the complexity parameter of each model to the cross-validated error.

## Unbalanced dataset
![c_train cp plot](/figures/c_train_cp_plot.svg)

## Up-sampled dataset
![c_train cp plot](/figures/c_train_up_cp_plot.svg)

## Rebalanced with SMOTE dataset
![c_train cp plot](/figures/c_train_smote_cp_plot.svg)

## Down-sampled dataset
![c_train cp plot](/figures/c_train_down_cp_plot.svg)

Each tree model was tested on the test dataset and the confusion matrices are displayed below
## Unbalanced dataset
![c_train cp plot](/figures/c_train_cm_stats.svg)

## Up-sampled dataset
![c_train cp plot](/figures/c_train_up_cm_stats.svg)

## Rebalanced with SMOTE dataset
![c_train cp plot](/figures/c_train_smote_cm_stats.svg)

## Down-sampled dataset
![c_train cp plot](/figures/c_train_down_cm_stats.svg)

Considering the results from each unpruned decision tree, upsampling to correct for target class imbalance seems to be the best option. Of the different rebalancing techniques, upsampling had the highest accuracy and Cohen's Kappa. Furthermore, While the downsampling method had a higher balanced accuracy, upsampling had, by far, the highest specificity. Considering the implications of a false negative when predicting if a patient will have a stroke, the specificity metric must be as high as possible. Moving forward with the Random Forest analysis, we will use upsampling to balance the data.

To produce a more refined decision tree model for future comparison with other modeling methods we pruned the upsampled decision tree at 129 splits based off of the upsampled model's cp plot. The tree optimization resulted in a more accurate model (shown below).

![pruned tree](/figures/upsampled_decision_tree_pruned_cm.svg)

# Random Forest Classification

A Random Forest model was run using the `train` function from the R package `caret`. A training partition of the original stroke data set was produced containing 70% of the data, and the remaining 30% of the data was reserved for testing. A 10 fold cross validation was performed on the training dataset to find optimal tuning parameters for number of trees (ntrees), number of variables (mtry), and number of observations in a terminal node (nodesize). The parameters tested are as follows.
| Param         | Values           
| ------------- |:-------------:|
| ntrees        | 500, 700, 1000|
| mtry          | 1 - 11        |
| nodesize      | 1, 5          |

The positive target class was upsampled for each cross validation fold and sub-training sets contained 70% of the training data. Below are the cross validation results.

![rf_tuning_plot](/figures/rf_tuning_plot.svg)

### Largest area under the curve (ROC) and corresponding mtry
|                           | mtry|       ROC|
|:--------------------------|----:|---------:|
|ntrees: 1000 nodesize: 1.6 |    6| 0.8174266|
|ntrees: 500 nodesize: 1.6  |    6| 0.8160570|
|ntrees: 700 nodesize: 1.5  |    5| 0.8156033|
|ntrees: 1000 nodesize: 5.6 |    6| 0.8145467|
|ntrees: 700 nodesize: 5.5  |    5| 0.8143032|
|ntrees: 500 nodesize: 5.5  |    5| 0.8141993|

Next, different resampling techniques were tested (upsampling, downsampling, and SMOTE). The optimal parameters were used as determined above and the best resampling technique was used for the final model as determined by examining each model's receiver operating curve and choosing the highest area under the curve (below).

![combined ROC sampling types](/figures/final_rf_auc.svg)

Finally, an upsampled 10-fold cross validated model using the optimal parameters of ntrees = 1000, nodesize = 1, and mtry = 6 was trained with the training data partition and tested against the 30% hold out data. Below is the resulting confusion matrix.

![final_rf confusion matrix](/figures/final_rf_confusion_matrix.svg)

# Logistic Regression

Next, we ran a logistic regression model with the `glm` function from the R `stats` package. Prior to training, the model variables were considered for transformation and scaling. Categorical variables with only 2 factor levels (gender, ever_married, Residence_type) were converted from character vectors to numeric factors with values 1 and 0. All other categorical variables (work_type and smoking_status) were transformed into dummy variables and the reference classes were chosen as "Private" for work_type and "Unknown" for smoking_status. All data was z-score transformed using the `scale` function in base R with default values. Prior to any transformations the data was split into training and test sets, containing 70% of the observations and 30% respectively. Each dataset was treated to the same preprocessing steps. Each logistic regression model that we trained produced a set of predicted probabilities that was used to determine target class predictions with a threshold of 0.5. Meaning, for a positive class prediction, the predicted probability needed to be 50% or more. Next, recursive feature elimination was performed to choose the best number of variables to retain in the model. 10 rounds of resampling with successively fewer variables in each model was done, and the best model was retained each time as determined by the largest AUC. We also considered the F1 score for each model, but they were so similar that AUC proved to be the better metric. Features were removed according to the lowest score from the `varImp` function from the `caret` R package. The ROC curves for the winning models can be seen below.

![roc curves](/figures/log_reg_AUC.svg)

Next, we considered the number of variables retained for each of the winning models taking into account AUC.

![important vars](/figures/log_reg_impvars.svg)

The above plot indicates that the model with the highest AUC (0.872) was resample_14.5 which retained only the predictor age. The F1 score was identical between this model and the model with the next highest AUC, so resample_14.5 was chosen as the best model. The model  is as follows:

![winning model](/figures/winning_model.jpg)

If we exponentiate the age coefficient (exp(1.5527) = 4.724208), while considering our preprocessing step (base 10 log transformation of variable age), we can interpret the model to indicate that the odds ratio of having a stroke associated with a 10-fold increase in age is 4.72. In other words, with every 10-fold increase in age the odds of having a stroke are 4.72 times higher.

We considered multiple methods for handling the effect of the unbalanced target variable bias on model predictions, including dimensional reduction (PCA) and adjusting predictions with a calculated prior probability (see ![logistic_regression.R](/scripts/logistic_regression.R)]). The best solution was to calculate the proportion of stroke/no_stroke observations in the training set and use that figure as the classification threshold. The resulting confusion matrix after applying the best model with the informed classification threshold is displayed below.

![winning model confusion matrix](/figures/final_regression_confusion_matrix.svg)

