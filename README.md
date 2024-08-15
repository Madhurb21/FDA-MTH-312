# FDA-MTH-312
Functional data regression and outlier detection for Course project of MTH-312

# Functional Data Analysis: Regression and Outlier Detection

This repository contains two projects on Functional Data Analysis (FDA): one for Functional Regression and another for Outlier Detection. Both projects are implemented in R, and the reports explain the methods, results, and conclusions.

## Functional Data Regression

This project focuses on predicting a response variable using functional data. Two methods were compared: Functional Linear Regression model and a custom estimator inspired by the Nadaraya-Watson estimator. The custom method provided better predictions, with a lower mean squared error (MSE).

### Highlights:
- **Data Simulation:** Functional data was generated using a combination of exponential and sinusoidal functions.
- **Model Comparison:** A linear model was compared with a custom method.
- **Results:** The custom estimator performed better, concluding the data was not "linear".

## Outlier Detection in Functional Data

This project deals with identifying outliers in functional data. The data was generated to include known outliers, and a method based on depth measures was used to detect them. Two scenarios were explored: one with scale differences and another with frequency differences among the outliers.

### Highlights:
- **Data Simulation:** Functional data with outliers was generated, varying by scale and frequency.
- **Outlier Detection:** The method used Modified Band Depth (MBD) to identify outliers.
- **Results:** The method worked well for extreme value outliers but struggled with high-frequency outliers. Future improvements could focus on better handling frequency variations.