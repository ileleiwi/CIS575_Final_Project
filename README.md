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
![variable plots](/figures/variable_plots.pdf)