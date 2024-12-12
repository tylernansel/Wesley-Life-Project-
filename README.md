# Predicting Food Insecure Seniors in Iowa 

This project analyzes factors contributing to food insecurity among seniors in Iowa at the PUMA (Public Use Microdata Area) level. Using advanced statistical modeling and machine learning techniques, the project identifies high-risk populations and provides insights for targeted interventions. 

## Introduction
Wesley Life Meals on Wheels in Des Moines Iowa is looking to potentally expand its operations beyod its current scope of the city of Des Moines and parts of Polk and Dallas Counties in central Iowa. The non-profit is searching for statistical guidance and predictions to guide its research targeting Food Insecure senior citizens who may be in need of assitance from the program. 

### Key Goal:
The key goal of this project is to make acurate and relavant reconmendations to our buisness stakeholder regarding the expansion of operations. Thus, we must build a strong model to locate food insecure seniors in the state of Iowa.

## Data Sources

### Primary Data: 
- CPS Data: Individual level data - aggregated onto household level - on demographics, income, and food security status.
- ACS Data: Demographic and socioeconomic data aggregated at the PUMA level.
### Secondary Data:
- Iowa shapefiles for PUMA boundaries.
- External datasets such as Meals on Wheels locations for additional resources.

## Methods

1. Data preperation:
- Aggregate both data sets to the family level and clean variables.
- Impute/Drop missing values (created models using both techniques to compare effectiveness).
2. Statistical Modeling:
- Create clusters using Gower distance based on metrics to measure food insecurity with a low amount of missing data.
- Fit and tune a random forest to predict variable importance.
- Utilize Ridge and Lasso regression to fit models.
- ROC Curve: Identified optimal probability threshold for classifying food insecurity.
3. Geographic Analysis:
- Mapping food insecurity levels and highlighting top PUMA's with high risk seniors throughout Iowa.
- Spatial join to link data with PUMA shapefiles.
  

## Results
- Our Analysis showed that household size, as well as marital status, are very important predictors of Food Insecurity.
- Built multiple models that predict the SouthWest Iowa PUMA to have the highest demand of Food Insecure seniors.

## Collaboration 

This project was completed in joint with Kelti Wise and Nathan VanDeWoestyne under the guidance of Dr. Lendie Follet.
