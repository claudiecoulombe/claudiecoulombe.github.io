# Analytics Portfolio

Welcome to my analytics portfolio! This is a collection of projects I’ve created to highlight my approach to solving business challenges in people analytics, data science, and data analysis.

## Projects

### (1) Turnover Prediction App

[View app](https://5yurvz-claudiecoulombe.shinyapps.io/turnover_analysis/) | [View Code] (https://github.com/claudiecoulombe/claudiecoulombe.github.io/tree/main/employee_turnover_app)

Built in R Shiny, this app offers an interactive platform for employee turnover analysis. Users can upload their own dataset (or use the default dataset integrated into the app for demonstration purposes), choose which variable(s) to use as predictors and which as the outcome, train three machine learning models—logistic regression, random forest, and multilayer perceptron—and tune hyperparameters. The app allows users to evaluate model performance on both training and test sets, predict which employees are at risk of turnover, download a CSV with risk predictions, and run mini experiments to see how adjusting different predictors would influence turnover risk probabilities.

While designed with turnover analysis as the primary use case, the app's framework can easily be adapted to any binary classification problem, making it a versatile tool for a variety of predictive analysis tasks in people analytics and in other fields.

Examples of extensions of this app in people analytics include:
- Predicting promotion likelihood;
- Predicting if a candidate will accept (1) or decline (0) a job offer;
- Predicting if an employee will complete the onboarding process (1) or leave prematurely (0);
- Predicting if an employee is likely to succeed (1) or not (0) after transitioning to a new role.

Examples of extensions of this app in other fields include:
- Sales/Marketing: Predicting the likelihood of a customer churning (1) or not (0).
- Finance: Predicting if a transaction or account activity is fraudulent (1) or not  (0).
- Healthcare: Predicting if a patient will be readmitted to the hospital within a specific time frame (1) or not (0).
- Manufacturing: Predicting whether a piece of equipment will fail (1) or not fail (0) in a given time frame.

### (2) Exploring Employee Attrition with SQL
[View Report](attrition_analysis_SQL/docs/employee_attrition_sql.html) | [View Code](https://github.com/claudiecoulombe/claudiecoulombe.github.io/tree/main/attrition_analysis_SQL)

This project explores patterns of employee attrition at NovaTech Solutions, a fictional mid-sized tech company, using SQL for data extraction, transformation, and analysis, and R for visualization. The dataset was modeled in a PostgreSQL database, simulating real-world HR analytics workflows. Descriptive trends were analyzed to uncover potential areas of focus for improving retention. Actionable recommendations for next steps are provided. 
