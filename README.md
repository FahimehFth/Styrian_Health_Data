# Styrian_Health_Data
A comprehensive geospatial analysis of blood pressure categories across the Austria region

## Table of Contents
- [Introduction](#introduction)
- [Data Sources](#data-sources)
- [Technologies Used](#technologies-used)


## Introduction

Blood pressure is a vital sign that reflects the health status of an individual. It can
indicate the risk of developing heart disease, stroke, and other complications. Therefore,
analyzing health data is important for understanding the factors that influence it and
predicting its future values.

In this project, visitors’ data which is collected by Austrian Research Centers GmbH in
the Styrian state exhibition in 2006 is used. The dataset contains the measured systolic
and diastolic blood pressure of 16386 visitors and 15 other characteristics of each. The
purpose of the analysis is to predict the blood pressure (bp) category using a multinomial
logistic regression model and find the most effective factors in determining it.

In the first step, the data is preprocessed by converting variables and computing new
variables such as age, and season. In fact, aiming the project objective is more convenient
using categorical variables and also age and time related variables instead of birthdate
and time in timestamp format. In addition, because of different equipment in terminal
3, it is split into 3a and 3b. Lastly, categorical values are considered for blood pressure
named subcategory. The multinomial logistic regression model is used to predict a
person blood pressure category. Descriptive analysis is done to summarize main features
and patterns using visualizations. Moreover, the data set is understood better. In
addition, stepwise selection is employed to choose a subset of predictors that have a
strong relationship with the target variable.

## Data Sources

The given dataset is provided by Austrian Research Centers GmbH in the Styrian state exhibition in 2006 

## Technologies Used

This project utilizes the following technologies and tools:

- **R Programming Language:** Main programming language used for data analysis and visualization.
- **RStudio:** Integrated development environment (IDE) for R.
- **R Libraries:** 
  - ggplot2: For creating elegant and customizable visualizations such as histograms, scatter plots, box plots, and mosaic plots.
  - dplyr: For data manipulation tasks such as filtering, summarizing, and mutating data frames.
  - lubridate: For working with date and time data.
  - gridExtra: For arranging multiple plots on the same page.
  - mosaic: For creating mosaic plots to visualize categorical data relationships.
  - **Multinomial Logistic Regression:** Used for modeling relationships between categorical dependent variables and one or more independent variables.


