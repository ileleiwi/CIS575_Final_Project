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

Initial exploration of the Stroke dataset involved creating histograms of each variable to visualize the distributions.

![variable plots](/figures/variable_plots.svg)

It seems that the target variable "stroke" is highly unbalanced, favoring the negative class (no stroke). The same imbalance favoring the negative class is observed in "hypertension" and "heart_disease" variables. "bmi" is relatively normally distributed while "avg_glucose_level" is right skewed. The nominal variables "gender" and "Residence_type" are the most evenly balanced variable of this data type, although the dataset included nearly 1000 more observations from Females than from Males.

Next each numeric variable was plotted against each other numeric variable to see if any correlation could be seen

![cor scatter plots](/figures/scatter_cor_plots.svg)

The scatterplots indicate that potential correlations could exist between "age", "bmi", and "avg_glucose_levels". Pearson correlation was calculated for each pair of these variables and the results are shown below.

![pearson cor plots](/figures/cor_plot.svg)

The correlation coefficients are relatively low so we may move forward with the assumption that variables are not correlated.

More summary statistics can be viewed [here](/documents/summary_stats.pdf)

# Description of Data Preparation

Our data analysis plan includes implementing cross-validated decision trees with different strategies to balance the data (no correction, up sampling positive class, down sampling negative class, SMOTE rebalancing) and then use the strategy that provides the best results (most accurate model) to train a bagged random forest model. The results of the random forest model will be used to further inform feature selection for a logistic regression. Finally, all models will be compared and the best one chosen. Prior to logistic regression, numeric variables will be min-max scaled and dummy variables will be created from categorical variable in the data. We will divide the data into a training and testing set where appropriate, each partition containing 30% and 70% of the data respectively.

# Decision Tree Classification
A CART classification decision tree splitting with the gini index was implemented using the `rpart` library in R. Various balancing strategies were implemented on the training data to balance the target variable classes.

c_train = no balancing \
c_train_p = random upsampling of positive class observations \
c_train_smote = rebalancing using the SMOTE algorithm as implemented in the `DMwR`\
c_train_down = random downsampling of negative class observations \

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
<table>
<tr><th>Confusion Matrix </th><th>Model Stats</th></tr>
<tr><td>

|          | stroke| no_stroke|
|:---------|------:|---------:|
|stroke    |     19|        73|
|no_stroke |    166|      3228|

</td><td>

|               |         x|
|:--------------|---------:|
|Accuracy       | 0.9314400|
|Kappa          | 0.1056568|
|AccuracyLower  | 0.9225402|
|AccuracyUpper  | 0.9396076|
|AccuracyNull   | 0.9469306|
|AccuracyPValue | 0.9999622|
|McnemarPValue  | 0.0000000|

</td></tr> </table>

## Up-sampled dataset
<table>
<tr><th>Confusion Matrix </th><th>Model Stats</th></tr>
<tr><td>

|          | stroke| no_stroke|
|:---------|------:|---------:|
|stroke    |     47|       238|
|no_stroke |    138|      3063|

</td><td>

|               |         x|
|:--------------|---------:|
|Accuracy       | 0.8921400|
|Kappa          | 0.1449696|
|AccuracyLower  | 0.8813683|
|AccuracyUpper  | 0.9022489|
|AccuracyNull   | 0.9469306|
|AccuracyPValue | 1.0000000|
|McnemarPValue  | 0.0000003|

</td></tr> </table>

## Rebalanced with SMOTE dataset
<table>
<tr><th>Confusion Matrix </th><th>Model Stats</th></tr>
<tr><td>

|          | stroke| no_stroke|
|:---------|------:|---------:|
|stroke    |    113|       683|
|no_stroke |     72|      2618|

</td><td>

|               |         x|
|:--------------|---------:|
|Accuracy       | 0.7834194|
|Kappa          | 0.1578487|
|AccuracyLower  | 0.7693673|
|AccuracyUpper  | 0.7969946|
|AccuracyNull   | 0.9469306|
|AccuracyPValue | 1.0000000|
|McnemarPValue  | 0.0000000|

</td></tr> </table>


## Down-sampled dataset
<table>
<tr><th>Confusion Matrix </th><th>Model Stats</th></tr>
<tr><td>

|          | stroke| no_stroke|
|:---------|------:|---------:|
|stroke    |    153|      1085|
|no_stroke |     32|      2216|

</td><td>

|               |         x|
|:--------------|---------:|
|Accuracy       | 0.6795754|
|Kappa          | 0.1351812|
|AccuracyLower  | 0.6637957|
|AccuracyUpper  | 0.6950535|
|AccuracyNull   | 0.9469306|
|AccuracyPValue | 1.0000000|
|McnemarPValue  | 0.0000000|

</td></tr> </table>

