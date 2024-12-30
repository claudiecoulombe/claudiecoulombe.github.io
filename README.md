# Analytics Portfolio

Welcome to my analytics portfolio! This is a collection of projects I’ve created to highlight my approach to solving business challenges in people analytics, data science, and data analysis.

## Projects

### (1) Turnover Prediction App

-[View app](https://5yurvz-claudiecoulombe.shinyapps.io/turnover_analysis/)

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

### (2) Predicting and Understanding Employee Turnover 
In this [project](https://github.com/claudiecoulombe/claudiecoulombe.github.io/tree/main/employee_turnover), I analyzed employee turnover at Fictional Solutions Inc. to uncover key drivers and identify employees at risk of leaving, offering insights to guide future retention programs and strategies.

**Key Deliverables**:
- **[Dashboard](https://5yurvz-claudiecoulombe.shinyapps.io/employee_turnover/)**: An interactive tool designed for key stakeholders to visualize turnover trends and explore relationships between turnover and variables such as satisfaction, workload, salary, and performance levels. More suitable for stakeholders, it provides an interactive overview of the analysis, including a summary of key findings on the main page.
- **[Report](employee_turnover/docs/employee_turnover_report.html)**: A detailed Markdown report summarizing the analysis, key analytical decisions, findings, and recommendations. More suitable for internal data professionals at Fictional Solutions Inc., it can be used for transparency and replicability purposes.

_**Skills Used**: R, Markdown/Quarto, Shiny, Exploratory Data Analysis, Logistic Regression, Random Forest, Neural Network, Data Cleaning, Data Visualization_

### (2) Validating a Selection Approach
In this project, I validated a selection methodology that incorporated multiple assessment scores. The analysis focused on evaluating whether the combined scores predicted performance more effectively than each score individually.
