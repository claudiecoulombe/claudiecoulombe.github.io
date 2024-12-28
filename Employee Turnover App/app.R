#-----------------------------------------------------------------------------#
# Unified App with Tabs for Logistic Regression and Random Forest
#-----------------------------------------------------------------------------#
library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(plotly)
library(randomForest)
library(car)
library(caret)
library(keras3)
library(Metrics)
library(shinydashboard)
library(shinyjs)
library(shinyFeedback)
library(shinyBS)
library(pROC)
library(corrplot)

default_data <- read.csv("data/hr_retention_data.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Define the UI
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

ui <- navbarPage(
  "Employee Turnover Analysis",
  id = "main_navbar",   # Assign an ID to the navbarPage
  
  
  # Home Page
  tabPanel("Home",
           fluidPage(
             tags$head(
               tags$style(HTML("
          body {
            font-family: Arial, sans-serif;
            background-color: #f8f9fa;
            color: #333333;
          }
          .hero-banner {
            background-color: #64B5F6;
            color: white;
            text-align: center;
            padding: 40px;
            border-radius: 10px;
            margin-bottom: 30px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
          }
          .hero-banner h1 {
            font-size: 36px;
            font-weight: bold;
          }
          .hero-banner p {
            font-size: 18px;
            margin-top: 10px;
          }
          .instruction-card {
            background-color: #ffffff;
            border: 1px solid #ddd;
            border-radius: 10px;
            padding: 20px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            transition: box-shadow 0.2s ease-in-out;
          }
          .instruction-card:hover {
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
          }
          .section-title {
            font-size: 22px;
            font-weight: bold;
            color: #FFB74D;
            margin-bottom: 10px;
          }
          .btn-primary {
            background-color: #FFB74D;
            border-color: #FFB74D;
          }
          .btn-primary:hover {
            background-color: #fcae5a;
            border-color: #fcae5a;
          }
          .tooltip-icon {
            font-size: 18px;
            color: #999;
            margin-left: 5px;
          }
          .nav-tabs > li > a {
        color: #2c3e50; /* Default text color */
        font-weight: normal;
        font-size: 12px;
      
      }
      .nav-tabs > li > a:hover {
        background-color: #ecf0f1; /* Hover background color */
        border-radius: 5px;
      }
        "))
             ),
             
             # Hero Section with Call-to-Action
             div(class = "hero-banner",
                 h1("Employee Turnover Analysis App"),
                 p("Understand and predict employee turnover using machine learning models."),
                 actionButton("go_models", "Learn About Available Models", class = "btn btn-primary")
             ),
             
             # Features Section
             div(class = "instruction-card",
                 div(class = "section-title", "What You Can Do With This App"),
                 
                 fluidRow(
                   # Feature 1: Upload Data
                   column(6,
                          div(class = "panel panel-default",
                              div(class = "panel-heading text-center", 
                                  tags$i(class = "fa fa-upload fa-3x", style = "color:#64B5F6;"),
                                  h4("Upload Your Own Data and Choose Your Variables")
                              ),
                              div(class = "panel-body text-center",
                                  p("Upload your own anonymized dataset or use the provided one to start analyzing turnover trends."),
                                  p("Choose predictors to evaluate their effects on turnover risk.")
                              )
                          )
                   ),
                   
                   # Feature 2: Train Models
                   column(6,
                          div(class = "panel panel-default",
                              div(class = "panel-heading text-center", 
                                  tags$i(class = "fa fa-cogs fa-3x", style = "color:#FFB74D;"),
                                  h4("Train and Evaluate Machine Learning Models")
                              ),
                              div(class = "panel-body text-center",
                                  p("Customize, train, and run models (Logistic Regression, Random Forest, MLP) to explore how predictors relate to turnover risk."),
                                  p("Compare models (i.e., performance, pros/cons) to identify the best one for your data.")
                              )
                          )
                   )
                 ),
                 
                 fluidRow(
                   # Feature 3: Predict Turnover Risk
                   column(6,
                          div(class = "panel panel-default",
                              div(class = "panel-heading text-center", 
                                  tags$i(class = "fa fa-line-chart fa-3x", style = "color:#4CAF50;"),
                                  h4("Identify At-Risk Employees")
                              ),
                              div(class = "panel-body text-center",
                                  p("Generate predictions to identify which employees are currently at risk of leaving."),
                                  p("Download results for easy identification and intervention.")
                              )
                          )
                   ),
                   
                   # Feature 4: Explore What-If Scenarios
                   column(6,
                          div(class = "panel panel-default",
                              div(class = "panel-heading text-center", 
                                  tags$i(class = "fa fa-lightbulb-o fa-3x", style = "color:#FF7043;"),
                                  h4("Explore 'What-If' Scenarios")
                              ),
                              div(class = "panel-body text-center",
                                  p("Simulate changes to predictors (e.g., satisfaction level) to understand how they influence turnover risk."),
                                  p("For example, what effect would increasing Predictor A by 2 points have on turnover risk among current employees?")
                              )
                          )
                   ))
                 
             )
             ,
             
             # Steps to Get Started in Boxed Cards
             div(class = "instruction-card",
                 div(class = "section-title", "How to Get Started"),
                 
                 # Step 1
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 1: Upload Data", href = "#step1", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step1")
                         )
                     ),
                     div(id = "step1", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Upload your own anonymized company dataset or use the default dummy dataset (from Kaggle)."),
                             p("If you choose to upload your own dataset, make sure it meets the following criteria:"),
                             tags$ul(
                               tags$li("The dataset must be a CSV file."),
                               tags$li("Data is clean with no missing values (i.e., missing values have already been dealt with)."),
                               tags$li("Each row reflects an individual employee and each column reflects a different variable."),
                               tags$li("There is a binary turnover column, where 1 indicates that the employee left and 0 indicates they stayed."),
                               tags$li("Variables are stored as the intended data type (e.g., numeric, factor/categorical)."),
                               tags$li("There is sufficient data to train a machine learning model (enough to divide into a training and test set). A rule of thumb is to have 10 times as many rows as there are predictors in your dataset. If datasets are too small, the model may not perform well."),
                             )
                         )
                     )
                 ),
                 
                 # Step 2
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 2: Specify the Outcome", href = "#step2", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step2")
                         )
                     ),
                     div(id = "step2", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Select the variable in your dataset that reflects turnover (e.g., Turnover: 1 = Left, 0 = Stayed).")
                         )
                     )
                 ),
                 
                 # Step 3
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 3: Choose Predictors", href = "#step3", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step3")
                         )
                     ),
                     div(id = "step3", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Select the predictor variables you would like to include in your model. These can be categorical or numeric."),
                             tags$ul(
                               tags$li("These are the factors that may influence turnover."),
                               tags$li("For Logistic Regression, avoid choosing predictors that are highly correlated with each-other.")
                             )
                         )
                     )
                 ),
                 
                 # Step 4
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 4: Configure the Models", href = "#step4", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step4")
                         )
                     ),
                     div(id = "step4", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("For each of the 3 models, you can choose a training/testing split. A common split is to have 80% of the data go into the training set and 20% into the test set."),
                             p("For the Random Forest and MLP models, you can also adjust the model-specific parameters:"),
                             tags$ul(
                               tags$li("Random Forest:",
                                       tags$ul(
                                         tags$li("Adjust the number of trees:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb: Start with 500-1000 trees."),
                                                   tags$li("More trees improve stability but increase computation time."),
                                                   tags$li("Stop increasing when the error stabilizes.")
                                                 )
                                         ),
                                         tags$li("Adjust the number of variables at each split:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb (Classification): mtry = sqrt(p), where p is the number of predictors."),
                                                   tags$li("Smaller mtry increases randomness and reduces overfitting."),
                                                   tags$li("Larger mtry reduces bias but makes trees more correlated.")
                                                 )
                                         ),
                                         tags$li("Adjust the minimum node size:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb (Classification): nodesize = 1."),
                                                   tags$li("Smaller node size allows deeper trees but may overfit."),
                                                   tags$li("Larger node size produces shallower trees and reduces computation time.")
                                                 )
                                         )
                                       )
                               ),
                               tags$li("MLP:",
                                       tags$ul(
                                         tags$li("Choose the number of epochs:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb: Start with 100-200 epochs, monitor performance, and stop early if validation loss stabilizes.")
                                                 )
                                         ),
                                         tags$li("Choose the number of hidden layers:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb: Start with 1-2 hidden layers; add more layers if the model underfits.")
                                                 )
                                         ),
                                         tags$li("Choose the number of neurons in each hidden layer:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb: Set neurons between the size of input and output layers; start with powers of 2 (e.g., 32, 64, 128).")
                                                 )
                                         ),
                                         tags$li("Choose the activation function at each hidden layer:",
                                                 tags$ul(
                                                   tags$li("Rule of thumb: Use ReLU for hidden layers; because this is a classification taks, sigmoid is used for the final output layer.")
                                                 )
                                         )
                                       )
                               )
                             )
                             
                         ))),
                 
                 # Step 5
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 5: Run and Compare Models", href = "#step5", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step5")
                         )
                     ),
                     div(id = "step5", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Train all three models to generate predictions."),
                             tags$ul(
                               tags$li("For Logistic Regression, look at the Assumption Testing tab to see if your data meets the assumptions needed for this model to be accurate (and try making any of the suggested modifications if it does not)."),
                               tags$li("For the three models, compare model performance: Accuracy, Precision, and Recall.")
                             ),
                             p("Use the insights to determine which model(s) work(s) best for your goal(s):"),
                             tags$ul(
                               tags$li("If your goal is to make the most accurate predictions possible, choose the model with the highest performance."),
                               tags$li("If your goal is to interpret the output of the model to understand what factors are related to turnover at your company, consider using Logistic Regression as it provides the highest interpretability.")
                             ),
                             
                         )
                     )
                 ),
                 
                 # Step 6
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 6: View Results and Predictions", href = "#step6", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step6")
                         )
                     ),
                     div(id = "step6", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Explore the model results:"),
                             tags$ul(
                               tags$li("Evaluate what predictors associate most strongly with turnover and/or contribute most to predictions."),
                               tags$li("Preview which employees, out of those who have not yet left, are predicted by the model(s) to be at risk of leaving."),
                               tags$li("Download updated datasets with turnover risk predictions for each employee.")
                             )
                         )
                     )
                 ),
                 
                 # Step 7
                 div(class = "panel panel-default",
                     div(class = "panel-heading", role = "tab",
                         h4(class = "panel-title",
                            tags$a("Step 7: Experiment with 'What-If' Scenarios", href = "#step7", "data-toggle" = "collapse", "aria-expanded" = "false", "aria-controls" = "step7")
                         )
                     ),
                     div(id = "step7", class = "panel-collapse collapse",
                         div(class = "panel-body",
                             p("Adjust numeric predictors to explore how changes influence turnover risk predictions among current employees."),
                             p("For example, how would increasing 'Satisfaction Level' by 1 point reduce turnover risk among current employees?")
                         )
                     )
                 )
             )))
  ,
  
  tabPanel("Model Overview",
           useShinyjs(), # Enable shinyjs for toggling elements
           fluidPage(
             div(class = "instruction-card",
                 div(class = "section-title", "Overview of Available Models"),
                 p("This app offers three machine learning models to help you analyze and predict employee turnover. Review each section to learn how they work, when to use them, and key considerations.")
             ),
             
             # Model Cards
             fluidRow(
               # Logistic Regression Section
               column(4, wellPanel(
                 h3("Logistic Regression"),
                 p("A statistical model that estimates the odds of a binary outcome (e.g., an employee leaving or staying) based on predictor variables like job satisfaction or workload. These odds are then converted into a probability between 0 and 1 using the logistic function."),
                 p(tags$b("When to Use:")),
                 tags$ul(
                   tags$li("You need a clear, interpretable model to explain results."),
                   tags$li("The relationships between predictors and the outcome are relatively simple (linear)."),
                   tags$li("Explaining which factors relate more/less strongly to the outcome is important for stakeholders.")
                 ),       
                 # Learn More Toggle Button
                 actionButton("logistic_collapse", "Learn More ▼", class = "btn btn-link"),
                 
                 # Collapsible Details Section
                 hidden(
                   div(id = "logistic_details", class = "well",
                       h4(tags$b("How Logistic Regression Works")),
                       p("Logistic Regression predicts the probability of a binary outcome (e.g., an employee leaving or staying) based on predictor variables like job satisfaction or workload. It uses predictor scores and assigns weights to calculate the logit, which is the log-odds of the event happening."),
                       
                       p(tags$b("Step 1: Calculating the Logit for Each Person")),
                       p("For each person, the predictor values are multiplied by their respective weights and summed to produce a logit (log-odds of the outcome):"),
                       tags$ul(
                         tags$li("Mathematically: Logitₐ = β0 + β1xᵢ₁ + β2xᵢ₂ + … + βnxᵢₙ"),
                         tags$li("Here:"),
                         tags$ul(
                           tags$li("xᵢ₁, xᵢ₂, … are the predictor values for person i."),
                           tags$li("β1, β2, … are the weights assigned to each predictor."),
                           tags$li("β0 is the intercept, which represents the baseline log-odds when all predictors are zero.")
                         )
                       ),
                       
                       p(tags$b("Step 2: Transforming the Logit to a Probability")),
                       p("Once the logit is calculated, it is transformed into a probability using the logistic function, which compresses the logit into a range between 0 and 1, making it interpretable as the likelihood of the outcome happening."),
                       
                       p(tags$b("Step 3: Making Predictions from Probabilities")),
                       p("The probability is compared to a threshold (e.g., 0.5) to classify the outcome:"),
                       tags$ul(
                         tags$li("If the probability is greater than or equal to the threshold, the outcome is predicted as 1 (e.g., employee leaves)."),
                         tags$li("If the probability is less than the threshold, the outcome is predicted as 0 (e.g., employee stays).")
                       ),
                       
                       p(tags$b("Step 4: Optimizing the Model (Iteration Until Convergence)")),
                       p("Logistic Regression uses Maximum Likelihood Estimation (MLE) to determine the best weights for the predictors. MLE finds the weights that maximize the likelihood of observing the actual outcomes in the data, meaning the model's predictions are as close as possible to the real-world results."),
                       
                       p(tags$b("Step 6: Evaluating the Model's Performance")),
                       p("Once the model has converged, we can evaluate its performance using metrics like accuracy, recall, and precision. To ensure the model generalizes well, we often split the data into training and test sets."),
                       p("If the accuracy on the test set is similar to the accuracy on the training set, it suggests the model is not overfitting. If the model performs well, we can proceed to evaluate the results."),
                       p(tags$b("Step 7: Interpreting Model Coefficients")),
                       p("Here, we can examine the coefficients for each predictor. The coefficients in Logistic Regression are initially expressed in terms of log-odds, which represent the change in the log-odds of the outcome for a one-unit increase in the predictor variable, holding all other predictors constant."),
                       p("To make the results easier to interpret, we can convert the coefficients into odds ratios:"),
                       tags$ul(
                         tags$li(tags$b("Odds Ratio > 1:"), " A one-unit increase in the predictor increases the odds of the outcome occurring."),
                         tags$li(tags$b("Odds Ratio < 1:"), " A one-unit increase in the predictor decreases the odds of the outcome occurring."),
                         tags$li(tags$b("Odds Ratio = 1:"), " The predictor has no effect on the odds of the outcome.")
                       ),
                       p("For example, if the odds ratio for job satisfaction is 0.8, it means that a one-unit increase in job satisfaction reduces the odds of turnover by 20%. Similarly, an odds ratio of 1.5 means the odds increase by 50%.")
                   )
                 )
                 ,
                 # Go to Logistic Regression Button
                 div(style = "margin-top: 10px;",
                     actionButton("nav_lr", "Try Logistic Regression", class = "btn btn-primary"))
               )),
               
               # Random Forest Section
               column(4, wellPanel(
                 h3("Random Forest"),
                 p("A machine learning algorithm that builds multiple decision trees to make predictions about an outcome (e.g., turnover). The final prediction combines results from all trees, improving accuracy and reducing overfitting."),
                 p(tags$b("When to Use:")),
                 tags$ul(
                   tags$li("You need to handle complex, non-linear relationships."),
                   tags$li("Predictive accuracy is more important than interpretability."),
                   tags$li("You want to identify which predictors are most important for the outcome.")
                 ),
                 # Learn More Toggle Button
                 actionButton("rf_collapse", "Learn More ▼", class = "btn btn-link"),
                 
                 # Collapsible Details Section
                 hidden(
                   div(id = "randomforest_details", class = "well",
                       h4(tags$b("How Random Forest Works")),
                       p("Random Forest is a machine learning method that combines multiple decision trees to make more accurate predictions. It works for both classification (e.g., predicting 'leave' or 'stay') and regression (predicting a number, like job satisfaction score)."),
                       
                       p(tags$b("What is a Decision Tree?")),
                       p("A decision tree is a simple model that makes predictions by splitting data into subsets based on predictors. It is made up of:"),
                       tags$ul(
                         tags$li(tags$b("Root node:"), " The starting point of the tree, containing the whole dataset."),
                         tags$li(tags$b("Internal nodes:"), " Points where the dataset is split based on a predictor and condition."),
                         tags$li(tags$b("Leaf nodes:"), " Endpoints that represent the final prediction for a subset of data.")
                       ),
                       p("The tree branches from the root node to the leaf nodes."),
                       
                       p(tags$b("How is a Decision Tree Built?")),
                       p(tags$i("Step 1: Start at the Root Node")),
                       p("The tree begins with all the data and selects the predictor and split point that best separates the outcome variable into 'pure' groups. Here, 'purity' means that the groups created by the split contain mostly the same outcome."),
                       
                       p(tags$i("Step 2: Choosing the Best Split")),
                       p("The tree evaluates all predictors and split points to find the one that creates the purest groups:"),
                       tags$ul(
                         tags$li(tags$b("For classification:"), " Purity is measured using:"),
                         tags$ul(
                           tags$li(tags$b("Gini Impurity:"), " Measures how mixed the outcomes are (lower Gini = purer group)."),
                           tags$li(tags$b("Entropy:"), " Measures the uncertainty or randomness of outcomes (lower entropy = purer group).")
                         ),
                         tags$li(tags$b("For regression:"), " Purity is measured using:"),
                         tags$ul(
                           tags$li(tags$b("Variance Reduction:"), " Measures how much the variability of the outcome decreases after the split.")
                         )
                       ),
                       
                       p(tags$i("Step 3: Repeat the Process")),
                       p("At each internal node, the tree splits the data into smaller subsets based on the predictor and split that best separates the outcomes."),
                       
                       p(tags$i("Step 4: Stopping Splits")),
                       p("The tree stops splitting when:"),
                       tags$ul(
                         tags$li("A group has too few data points to split further."),
                         tags$li("The maximum tree depth is reached."),
                         tags$li("All the data points in a group have the same outcome.")
                       ),
                       
                       p(tags$i("Step 5: Predictions in the Leaf Nodes")),
                       p("Once the tree stops splitting, the leaf nodes contain the final predictions:"),
                       tags$ul(
                         tags$li(tags$b("For classification:"), " A class label (e.g., 'leave' or 'stay') based on the majority class."),
                         tags$li(tags$b("For regression:"), " A single number (e.g., the average target value like satisfaction score).")
                       ),
                       
                       p(tags$b("How Does Prediction Work?")),
                       p("For a new data point:"),
                       tags$ul(
                         tags$li("The tree starts at the root node."),
                         tags$li("It follows the splits based on the data point's predictor values, moving down the branches."),
                         tags$li("It reaches a leaf node, where the prediction comes from the value stored in that node."),
                         tags$li(tags$b("For classification:"), " The prediction is the class label."),
                         tags$li(tags$b("For regression:"), " The prediction is the average target value of the group.")
                       ),
                       
                       p(tags$b("What is a Random Forest?")),
                       p("A Random Forest is essentially a collection (or 'forest') of decision trees. Each tree in the forest makes its own prediction for a data point, and the forest aggregates these predictions to produce the final result. By combining many trees, Random Forest reduces overfitting and increases robustness."),
                       
                       p(tags$b("How is a Random Forest Built?")),
                       p(tags$i("Step 1: Bootstrapping the Data (Sampling the Data)")),
                       p("Random Forest creates multiple bootstrap samples of the training data:"),
                       tags$ul(
                         tags$li("A bootstrap sample is a random sample of the data taken ", tags$b("with replacement"), "."),
                         tags$li("Some data points may appear multiple times in a sample."),
                         tags$li("Other data points are left out—these are called ", tags$b("Out-of-Bag (OOB) samples"), ".")
                       ),
                       p("Each bootstrap sample is used to grow one decision tree."),
                       
                       p(tags$i("Step 2: Growing Decision Trees with Random Predictors")),
                       p("At each split in a tree, only a ", tags$b("random subset of predictors"), " is considered when choosing the best split. This ensures each tree is slightly different and prevents the trees from all focusing on the same predictor."),
                       
                       p(tags$i("Step 3: Building the Random Forest")),
                       p("Random Forest repeats Steps 1 and 2 to grow many decision trees. Each tree is trained independently on its own bootstrap sample."),
                       
                       p(tags$i("Step 4: Combining Predictions from All Trees")),
                       p("Once the forest is built, predictions are made by combining the results of all the trees:"),
                       tags$ul(
                         tags$li(tags$b("For classification:"), " Each tree 'votes' for a class label. The final prediction is the class that receives the most votes (majority voting)."),
                         tags$li(tags$b("For regression:"), " Each tree predicts a number. The final prediction is the average of all the trees' predictions.")
                       ),
                       
                       p(tags$i("Step 5: Checking Model Accuracy with Out-of-Bag (OOB) Samples")),
                       p("The OOB samples (data points not included in a bootstrap sample) are used to test the model's performance. The OOB error provides an estimate of how well the model generalizes to new data without needing a separate test set."),
                       
                       p(tags$i("Step 6: Understanding Predictor Importance")),
                       p("Random Forest can measure which predictors are most important for making predictions:"),
                       tags$ul(
                         tags$li("It looks at how often each predictor is used to split the data across all the trees."),
                         tags$li("Predictors that lead to better splits (reducing error or improving purity) are ranked as more important."),
                         tags$li("For example, if satisfaction_level is frequently used, it is likely a key predictor.")
                       )
                   )
                 ),
                 # Go to Random Forest Button
                 div(style = "margin-top: 10px;",
                     actionButton("nav_rf", "Try Random Forest", class = "btn btn-primary"))
               )),
               
               # MLP Section
               column(4, wellPanel(
                 h3("Multilayer Perceptron (MLP)"),
                 p("A type of neural network that processes data through layers of interconnected nodes (neurons) to detect patterns and make predictions about an outcome (e.g., turnover)."),
                 p(tags$b("When to Use:")),
                 tags$ul(
                   tags$li("You need to capture highly complex, non-linear relationships."),
                   tags$li("Predictive accuracy is the top priority, even if the model is harder to interpret."),
                   tags$li("You have sufficient data for training.")
                 ),
                 # Learn More Toggle Button
                 actionButton("mlp_collapse", "Learn More ▼", class = "btn btn-link"),
                 
                 # Collapsible Details Section
                 hidden(
                   div(id = "mlp_details", class = "well",
                       h4(tags$b("What is a Neural Network?")),
                       p("A neural network is a model inspired by how the human brain works. It consists of 'neurons' (also called nodes) connected in layers. The layers process information from the predictors step by step to produce predictions."),
                       p("In a Multi-Layer Perceptron (MLP), the network is organized into different types of 'layers':"),
                       tags$ul(
                         tags$li(tags$b("Input layer:"), " Receives the input data (e.g., the predictors)."),
                         tags$li(tags$b("Hidden layer(s):"), " Layers between the input and output layers, where the network learns to detect patterns in the data. This is where the inputs are transformed and combined."),
                         tags$li(tags$b("Output layer:"), " Produces the final prediction:"),
                         tags$ul(
                           tags$li(tags$b("For classification problems:"), " The output layer contains neurons representing each class (e.g., 'stay' or 'leave'), and the network outputs a probability for each class."),
                           tags$li(tags$b("For regression problems:"), " The output layer has a single neuron that provides a continuous value.")
                         )
                       ),
                       
                       p(tags$b("How Neurons Process Inputs")),
                       p("In a neural network, each 'neuron' (or node) in the network receives some inputs from the predictors, performs calculations, and then decides:"),
                       tags$ul(
                         tags$li("(1) If the neuron will 'fire' (i.e., whether or not to pass its output on to the next layer), and"),
                         tags$li("(2) How much of the signal to send.")
                       ),
                       p("Each neuron:"),
                       tags$ul(
                         tags$li("Receives inputs from the previous layer (or, in the case of the input layer, the original input data)."),
                         tags$li("Multiplies each input by a ", tags$b("weight"), " that represents its importance, and adds a ", tags$b("bias term"), "."),
                         tags$li("Sums these weighted inputs."),
                         tags$li("Passes the result through an ", tags$b("activation function"), " to produce the neuron’s output, which is sent to the next layer.")
                       ),
                       
                       p(tags$b("Hidden Layers")),
                       p("The hidden layers are where the network learns and detects patterns. These layers are often referred to as the 'black box' of the network because we can’t directly observe the patterns they’re learning."),
                       p("In research terms, the hidden layers act as ", tags$b("mediators"), ":"),
                       tags$ul(
                         tags$li("They take in the inputs and don’t just pass the information to the output, but instead 'mediate' by processing, transforming, and combining the inputs."),
                         tags$li("They build complex representations that help make the final prediction."),
                         tags$li("Each hidden layer builds on the patterns detected by the previous layer, enabling the network to capture increasingly complex and non-linear relationships.")
                       ),
                       p("We can have more than one hidden layer, effectively creating multiple 'mediator steps' in the relationship. Each additional layer allows the network to learn and represent more abstract patterns."),
                       
                       p(tags$b("Non-Linearity and Activation Functions")),
                       p("An important part of neural networks is that they don’t assume linearity in the underlying relationship. By stacking multiple layers and using non-linear activation functions, an MLP can capture complex, non-linear relationships in the data, allowing it to model a wider range of patterns."),
                       p(tags$b("Activation function:"), " An activation function is a mathematical function applied to the output of each neuron. After a neuron calculates a weighted sum of inputs, it passes the result through an activation function to determine its output."),
                       p("The purpose of the activation function is to introduce ", tags$b("non-linearity"), " into the model. Without it, the network would behave like a single linear transformation, regardless of how many layers it has."),
                       p(tags$b("Common Activation Functions:")),
                       tags$ul(
                         tags$li(tags$b("ReLU (Rectified Linear Unit):"), " Outputs 0 for negative inputs and passes positive values unchanged."),
                         tags$li(tags$b("Sigmoid:"), " Produces values between 0 and 1, useful for representing probabilities."),
                         tags$li(tags$b("Softmax:"), " Converts outputs into probabilities for each class, typically used in the output layer for classification tasks.")
                       ),
                       
                       p(tags$b("Training the Neural Network")),
                       p("Imagine a neural network is making predictions (e.g., whether an employee will stay or leave) based on certain inputs. When the network makes a prediction, we can compare the prediction to the actual answer and see how far off it was. This difference is called the ", tags$b("error.")),
                       p("The process of training a neural network involves reducing this error by adjusting the importance (or 'weights') the network assigns to each input feature. This is done through a process called ", tags$b("backpropagation:")),
                       tags$ul(
                         tags$li("The network works backwards through its layers to figure out which weights contributed most to the error and need adjusting."),
                         tags$li("It adjusts the weights to reduce the error."),
                         tags$li("This process of making predictions, checking errors, and adjusting weights is repeated many times across the dataset."),
                         tags$li("Each time, the network improves slightly, learning which inputs are more important for accurate predictions.")
                       ),
                       p("Over many rounds called epochs, the network fine-tunes the weights, minimizing the error and better understanding the relationship between inputs and the desired output.")
                   )
                 )
                 ,
                 # Go to MLP Button
                 div(style = "margin-top: 10px;",
                     actionButton("nav_mlp", "Try MLP", class = "btn btn-primary"))
               ))
             ),
             
             # Comparison Table Section
             div(style = "margin-top: 20px;",
                 div(class = "section-title", "Comparing the Three Modeling Approaches"),
                 tags$table(class = "table table-bordered",
                            tags$thead(
                              tags$tr(
                                tags$th("Model"),
                                tags$th("Pros"),
                                tags$th("Cons")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("Logistic Regression"),
                                tags$td(
                                  tags$ul(
                                    tags$li("Simple and highly interpretable. Coefficients show how the log-odds of the outcome change with a one-unit increase in a predictor, making interpretation straightforward."),
                                    tags$li("Outputs probabilities of the binary outcome occurring, which can be useful for decision-making. "),
                                    tags$li("Computationally efficient and doesn’t require as much data."),
                                  )
                                ),
                                tags$td(
                                  tags$ul(
                                    tags$li("Assumes a linear relationship between predictors and the log odds of the outcome."),
                                    tags$li("Sensitive to multicollinearity among predictors. Highly correlated predictors can inflate standard errors of the coefficients, leading to unreliable estimates."),
                                    tags$li("Limited for complex patterns or interactions. Modeling non-linear relationships is not straightforward."),
                                    tags$li("Requires handling of missing values (e.g., imputation)."),
                                    tags$li("More suited to structured/tabular data.")
                                  )
                                )
                              ),
                              tags$tr(
                                tags$td("Random Forest"),
                                tags$td(
                                  tags$ul(
                                    tags$li("Can model non-linear relationships and interactions."),
                                    tags$li("By combining predictions from multiple decision trees, reduces the risk of overfitting compared to individual trees."),
                                    tags$li("Works with minimal pre-processing. Handles both numerical and categorical predictors without requiring extensive pre-processing like scaling or encoding."),
                                    tags$li("Calculates feature importance scores, which help identify which predictors have a bigger effect on model performance."),
                                    tags$li("Can handle missing values during training.")
                                    
                                  )
                                ),
                                tags$td(
                                  tags$ul(
                                    tags$li("Training a Random Forest with many trees or on large datasets requires significant time and computational resources."),
                                    tags$li("Less interpretable than simpler models. Feature importance scores don't quantify a direct effect like regression coefficients do."),
                                    tags$li("Features with many unique categories (like IDs or zip codes) can sometimes be overemphasized because decision trees can easily split on them."),
                                    tags$li("Works well with structured/tabular datasets but is less effective for unstructured data like text, images, or audio.")
                                    
                                  )
                                )
                              ),
                              tags$tr(
                                tags$td("Multilayer Perceptron (MLP)"),
                                tags$td(
                                  tags$ul(
                                    tags$li("Designed to model complex, non-linear relationships using activation functions and hidden layers."),
                                    tags$li("Highly customizable; allow flexibility in the network architecture, like choosing the number of layers, neurons, and activation functions."),
                                    tags$li("Can handle unstructured data (e.g., images, audio, text).")
                                    
                                  )
                                ),
                                tags$td(
                                  tags$ul(
                                    tags$li("Requires significant data and computational resources. Training MLPs, especially with many layers and neurons, requires significant computational power and time."),
                                    tags$li("Functions as a black box, making interpretability difficult. MLPs do not provide direct insights into feature importance or relationships, making it harder to explain predictions."),
                                    tags$li("Require large amounts of labeled data to generalize well and avoid overfitting."),
                                    tags$li("Requires pre-processing (e.g., one-hot encoding for categorical predictors)."),
                                    tags$li("Requires handling of missing values (e.g., imputation)."),
                                    
                                  )
                                )
                              )
                            )
                 )
             )
           )
  ),
  
  
  tabPanel("Logistic Regression",
           sidebarLayout(
             sidebarPanel(
               h4("1. Upload Your Anonymized Dataset (Optional)"),
               fileInput("file_lr", "Ensure the data meets criteria outlined on the Home page.", accept = ".csv"),
               tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
               
               h4("2. Select the Binary Turnover Variable"),
               uiOutput("turnover_column_ui_lr"),
               tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
               
               h4("3. Select Predictor Variable(s)"),
               uiOutput("predictor_columns_ui_lr"),
               tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
               
               h4("4. Determine the Training/Testing Split"),
               sliderInput("split_ratio_lr", "Select the Training Set Percentage (Default 80%)", min = 50, max = 90, value = 80),
               tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
               
               h4("5. Train and Apply the Model"),
               
               div(
                 style = "display: flex; align-items: center; gap: 10px;",
                 actionButton("run_model_lr", "Run Logistic Regression"),
                 tags$span(
                   "This may take a few moments. Wait until you see a 'Training Complete!' message under the button.",
                   style = "font-size: 12px; color: #555; margin-left: 10px;"
                 )
               ),
               textOutput("training_status_lr"),  # Add this line for the training status
               tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
               
               h4("6. Model Results"),
               tags$p("Download the predicted turnover risks for current employees."),
               useShinyjs(),  # Initialize shinyjs
               downloadButton("download_predictions_lr", "Download Predictions")
             ),
             mainPanel(
               # Styled header for consistency and professionalism
               div(
                 style = "background-color: #f1f1f1; padding: 20px; border-radius: 10px; border: 1px solid #ccc; margin-bottom: 20px;",
                 h2("Logistic Regression", style = "font-weight: bold; color: #333; text-align: center;"),
                 p(
                   "Use this section to explore your dataset, train a logistic regression model, evaluate key assumptions and model performance, analyze the relationship between predictors and turnover, identify employees at risk of leaving, and simulate the impact of predictor adjustments on turnover risk.",
                   style = "font-size: 15px; color: #555; text-align: center;"
                 )
               ),
               tabsetPanel(
                 tabPanel(
                   "Dataset Preview",
                   h4("Preview the Dataset"),  # Add a title for the Dataset Preview tab
                   tags$p("Preview the data used for logistic regression and view summary turnover statistics."),  # Add a short description
                   
                   # Add styling to limit the width of the data table
                   div(
                     style = "overflow-x: auto; max-width: 100%;",
                     DTOutput("data_table_lr")
                   ), 
                   
                   hr(),
                   uiOutput("turnover_stats_box_lr"),
                   hr(),
                   h4("Confirm Variable Types and Check Missing Values"),  
                   div(
                     style = "overflow-x: auto; max-width: 100%;",
                     DTOutput("data_type_table_lr")
                   )
                 )
               ,
                 tabPanel(
                   "Explore the Data",
                   h4("Visualize Predictor Distributions"),  # Title for the section
                   tags$p("Visualize how predictor variables are distributed when grouped by turnover status."),
                     selectInput(
                       inputId = "selected_predictor_lr",
                       label = "Select Predictor",
                       choices = NULL  # This will be updated dynamically
                     ),
                     plotlyOutput("predictor_histogram_lr"),
                     br(),  # Add more space before the next section
                     hr(),
                     h4("Summary Statistics for Numeric Variables"),
                     DTOutput("numeric_summary_table_lr"),
                     
                     br(),
                     hr(),
                     h4("View Counts for Categorical and Binary Variables"),
                     DTOutput("category_count_table_lr"),
                     uiOutput("small_count_warning_lr_1"),
                     
                     br(),
                     hr(),
                     h4("Correlation Matrix"),
                     plotlyOutput("correlation_matrix_plot_lr")
                 )
                 ,
                 tabPanel("Assumption Checks", 
                          h4("Evaluate Assumptions"),  # Add a title for the Assumption Checks tab
                          tags$p("Check key assumptions for logistic regression to ensure model validity."),
                          uiOutput("assumption_check_lr"),
                          uiOutput("small_count_warning_lr_2")
                 ),
                 
                 tabPanel(
                   "Model Performance",
                   h4("View Model Performance Metrics"),  # Add a title for the Model Performance tab
                   tags$p("Review metrics such as accuracy, precision, recall, and more to evaluate model performance on the test set."),
                   verbatimTextOutput("performance_metrics_lr"),  # Performance metrics output
                   br(),
                   
                   # Add a section to explain accuracy, precision, recall, and F1 score
                   bsCollapse(
                     bsCollapsePanel(
                       "Understanding Performance Metrics",
                       p("Here are definitions of key metrics used to evaluate model performance:"),
                       tags$ul(
                         tags$li(tags$b("Accuracy:"), " The percentage of correct predictions (both stayed and left) out of the total predictions. It provides an overall measure of correctness."),
                         tags$li(tags$b("Precision:"), " Among the employees predicted to leave, the percentage that actually left. It measures how trustworthy the model is when it predicts an employee will leave."),
                         tags$li(tags$b("Recall (Sensitivity):"), " Among the employees who actually left, the percentage that were correctly predicted to leave. It measures the model's ability to identify employees at risk of turnover."),
                         tags$li(tags$b("F1 Score:"), " The harmonic mean of precision and recall. It provides a balanced measure when there’s an uneven class distribution (e.g., fewer employees leave than stay).")
                       ),
                       style = "info"  # Optional: Set style to 'info', 'success', etc.
                     )
                   )
                 )
                 ,                
                 tabPanel("Model Summary", 
                          h4("Review Model Summary"),  # Add a title for the Model Summary tab
                          tags$p("Examine model coefficients and their statistical significance to interpret predictor impact. Note that the results may take a few seconds to appear, as the confidence intervals are generated."),
                          uiOutput("small_count_warning_lr_3"),
                          verbatimTextOutput("model_summary_lr")
                 ),
                 tabPanel(
                   "Risk Prediction",
                   h4("Predict Turnover Risk"),  # Add a title for the Risk Prediction tab
                   tags$p("Use the model to predict which of your current employees are predicted by the model to be at risk of leaving and visualize risk distributions. The default risk threshold is set to a probability of 0.5 of turning over. Those with a probability above 0.5 are categorized as being At-Risk (and vice versa). Modify this threshold as needed by adjusting the slider below."),
                   br(), br(),
                   sidebarLayout(
                     sidebarPanel(
                       h4("Adjust Risk Threshold"),
                       sliderInput(
                         "risk_threshold",
                         "Risk Threshold",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.01
                       )
                     ),
                     mainPanel(
                       plotlyOutput("risk_histogram_lr"),
                       br(), br(),
                       plotlyOutput("risk_piechart_lr")
                     )
                   )
                 ),
               tabPanel(
                 "Adjust Predictors and Re-Simulate",
                 h4("Experiment with Predictor Adjustments"),  # Add a title for the tab
                 tags$p("Modify predictor values and click Simulate Adjustments to simulate their impact on turnover risk predictions and to compare the results with original data."),
                 
                 # Sidebar and Main Panel for Adjust Numeric Predictors and Histogram
                 sidebarLayout(
                   sidebarPanel(
                     h4("Adjust Numeric Predictors"),
                     uiOutput("adjust_numeric_inputs_lr"),
                     helpText(
                       "Note: Adjusted values are limited to stay within the minimum and maximum observed values in the dataset. For example, if a variable's maximum value is 7, any adjustment that would result in a value above 7 will be capped at 7. This ensures that all adjusted values remain realistic and consistent with the data."
                     ),
                     helpText(
                       "Note: Please ensure you have already clicked on Run Logistic Regression."
                     ),
                     actionButton("simulate_adjustments_lr", "Simulate Adjustments")
                   ),
                   mainPanel(
                     plotlyOutput("adjusted_simulation_histogram_lr"),
                     br()
                   )
                 ),
                 
                 # Section for Data Tables Below the Main Panel
                 fluidRow(
                   column(
                     width = 12,
                     h4("Original Data"),
                     div(style = "width:100%; overflow-x:auto;", DTOutput("original_data_table")),
                     br(),
                     h4("Adjusted Data"),
                     div(style = "width:100%; overflow-x:auto;", DTOutput("adjusted_data_table"))
                   )
                 )
               )
               
               
               )
             )
           )
  ),
  
  tabPanel("Random Forest",
    sidebarLayout(
      sidebarPanel(
        h4("1. Upload Your Dataset (Optional)"),
        fileInput("file_rf", "Ensure the data meets criteria outlined on the Home page.", accept = ".csv"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("2. Select the Binary Turnover Variable"),
        uiOutput("target_column_ui_rf"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("3. Select Predictor Variable(s)"),
        uiOutput("predictor_columns_ui_rf"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("4. Determine the Training/Testing Split"),
        sliderInput("split_ratio_rf", "Select the Training Set Percentage (Default 80%)", min = 50, max = 90, value = 80),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("5. Adjust Model Hyperparameters"),
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          numericInput("ntree_rf", "Number of Trees (ntree)", value = 100, min = 10, max = 1000, step = 10),
          tags$span(
            icon("info-circle"),
            title = "The number of trees determines how many decision trees the random forest will build. A larger number of trees reduces variance and improves stability but increases computation time.",
            style = "margin-left: 10px; cursor: help; color: #FFB74D;"
          )
        ),
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          numericInput("mtry_rf", "Number of Variables at Each Split (mtry)", value = NULL, min = 1, step = 1),
          tags$span(
            icon("info-circle"),
            title = "This determines how many predictors are considered for each split at a tree node. A smaller value increases randomness and reduces overfitting, but may underfit the data.",
            style = "margin-left: 10px; cursor: help; color: #FFB74D;"
          )
        ),
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          numericInput("nodesize_rf", "Minimum Node Size (nodesize)", value = 1, min = 1, step = 1),
          tags$span(
            icon("info-circle"),
            title = "This determines the minimum number of observations required in a terminal/leaf node. Smaller values allow deeper trees and reduce bias but may increase overfitting.",
            style = "margin-left: 10px; cursor: help; color: #FFB74D;"
          )
        ),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("6. Train and Apply the Model"),
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          actionButton("run_model_rf", "Run Random Forest"),
          tags$span(
            "This may take a few moments. Wait until you see a 'Training Complete!' message under the button.",
            style = "font-size: 12px; color: #555; margin-left: 10px;"
          )
        ),
        textOutput("training_status_rf"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("7. Model Results"),
        tags$p("Download the predicted turnover risks for current employees."),
        useShinyjs(),  # Initialize shinyjs
        downloadButton("download_predictions_rf", "Download Predictions")
      ),
      mainPanel(
        # Styled header 
        div(
          style = "background-color: #f1f1f1; padding: 20px; border-radius: 10px; border: 1px solid #ccc; margin-bottom: 20px;",
          h2("Random Forest", style = "font-weight: bold; color: #333; text-align: center;"),
          p(
            "Use this section to explore your dataset, train a random forest model, adjust hyperparameters to optimize performance, assess model performance, identify key predictors driving turnover predictions, and simulate the impact of predictor adjustments on turnover risk.",
            style = "font-size: 15px; color: #555; text-align: center;"
          )
        ),
        tabsetPanel(
          tabPanel(
            "Dataset Preview", 
            h4("Preview the Dataset"),  # Add a title for the Dataset Preview tab
            tags$p("Preview the data used for the random forest model and view summary turnover statistics."),  # Add a short description
            
            # Add a scrollable container for data_table_rf
            div(
              style = "overflow-x: auto; max-width: 100%;",
              DTOutput("data_table_rf")
            ),
            uiOutput("turnover_stats_box_rf"),
            hr(),
            h4("Confirm Variable Types and Check Missing Values"),
            
            # Add a scrollable container for data_type_table_rf
            div(
              style = "overflow-x: auto; max-width: 100%;",
              DTOutput("data_type_table_rf")
            )
          ),
          tabPanel(
            "Explore the Data",          
            h4("Visualize Predictor Distributions"),  # Title for the section
            tags$p("Visualize how predictor variables are distributed when grouped by turnover status."),
            selectInput(inputId = "selected_predictor_rf", 
                        label = "Select Predictor", 
                        choices = NULL),
            plotlyOutput("predictor_histogram_rf"),
            br(),  # Add more space before the next section
            hr(),
            h4("Summary Statistics for Numeric Variables"),
            DTOutput("numeric_summary_table_rf"),
            br(),  # Add more space before the next section
            hr(), 
            h4("View Counts for Categorical and Binary Variables"),
            DTOutput("category_count_table_rf"),                
            uiOutput("small_count_warning_rf_1"),
            br(),  # Add more space before the next section
            hr(),
            
            h4("Correlation Matrix"),
            plotlyOutput("correlation_matrix_plot_rf") 
          ),
          tabPanel(
            "Model Performance",
            
            h4("Model Performance and Summary"),
            tags$p("Examine the model's configuration and results, including hyperparameters, out-of-bag error rate, and confusion matrix."),
            verbatimTextOutput("model_summary_rf"),
            tags$p("Review metrics such as accuracy, precision, recall, and more to evaluate model performance on the test set."),
            verbatimTextOutput("performance_metrics_rf"),
            br(),
            
            # Add a section to explain accuracy, precision, recall, and F1 score
            h4("Understanding Performance Metrics"),
            bsCollapse(
              bsCollapsePanel(
                "Definitions of Metrics",
                p("Here are definitions of key metrics used to evaluate model performance:"),
                tags$ul(
                  tags$li(tags$b("Accuracy:"), " The percentage of correct predictions (both stayed and left) out of the total predictions. It provides an overall measure of correctness."),
                  tags$li(tags$b("Precision:"), " Among the employees predicted to leave, the percentage that actually left. It measures how trustworthy the model is when it predicts an employee will leave."),
                  tags$li(tags$b("Recall (Sensitivity):"), " Among the employees who actually left, the percentage that were correctly predicted to leave. It measures the model's ability to identify employees at risk of turnover."),
                  tags$li(tags$b("F1 Score:"), " The harmonic mean of precision and recall. It provides a balanced measure when there’s an uneven class distribution (e.g., fewer employees leave than stay).")
                ),
                style = "info"  # Optional: Set style to 'info', 'success', etc.
              )
            )
            
          ),
          tabPanel("Feature Importance", 
                   h4("Review Feature Importance"),  
                   tags$p(" identify which predictors have the greatest influence on the model's performance, based on the Mean Decrease Gini index. This index reflects how much each predictor improves the clarity of the splits in the decision trees by reducing uncertainty in the data. Predictors with higher values contribute more to creating groups where outcomes (like 'leave' or 'stay') are more distinct, which enhances the overall accuracy of the Random Forest model."),
                   uiOutput("small_count_warning_rf_2"),
                   plotOutput("importance_plot_rf")),
          tabPanel(
            "Risk Prediction",
            h4("Predict Turnover Risk"),  # Add a title for the Risk Prediction tab
            tags$p("Use the model to predict which of your current employees are predicted by the model to be at risk of leaving and visualize risk distributions. The default risk threshold is set to a probability of 0.5 of turning over. Those with a probability above 0.5 are categorized as being At-Risk (and vice versa). Modify this threshold as needed by adjusting the slider below."),
            
            br(), br(),
            sidebarLayout(
              sidebarPanel(
                h4("Adjust Risk Threshold"),
                sliderInput(
                  "risk_threshold_rf",
                  "Risk Threshold",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.01
                )
              ),
              mainPanel(
                plotlyOutput("risk_histogram_rf"),
                br(), br(),
                plotlyOutput("risk_piechart_rf")
              )
            )
          ),
          tabPanel(
            "Adjust Predictors and Re-Simulate",
            h4("Experiment with Predictor Adjustments"),  # Add a title for the tab
            tags$p("Modify predictor values and click Simulate Adjustments to simulate their impact on turnover risk predictions and to compare the results with original data."),
            
            # Sidebar and Main Panel for Adjust Numeric Predictors and Histogram
            sidebarLayout(
              sidebarPanel(
                h4("Adjust Numeric Predictors"),
                uiOutput("adjust_numeric_inputs_rf"),
                helpText(
                  "Note: Adjusted values are limited to stay within the minimum and maximum observed values in the dataset. For example, if a variable's maximum value is 7, any adjustment that would result in a value above 7 will be capped at 7. This ensures that all adjusted values remain realistic and consistent with the data."
                ),
                helpText(
                  "Note: Please ensure you have already clicked on Run Random Forest."
                ),
                actionButton("simulate_adjustments_rf", "Simulate Adjustments")
              ),
              mainPanel(
                plotlyOutput("adjusted_simulation_histogram_rf"),
                br()
              )
            ),
            
            # Section for Data Tables Below the Main Panel
            fluidRow(
              column(
                width = 12,
                h4("Original Data"),
                div(style = "width:100%; overflow-x:auto;", DTOutput("original_data_table_rf")),
                br(),
                h4("Adjusted Data"),
                div(style = "width:100%; overflow-x:auto;", DTOutput("adjusted_data_table_rf"))
              )
            )
          )
        )
      )
    )
  )
  ,
  tabPanel(
    "Multilayer Perceptron",
    sidebarLayout(
      sidebarPanel(
        h4("1. Upload Your Dataset (Optional)"),
        fileInput("datafile", "Ensure the data meets criteria outlined on the Home page.", accept = ".csv"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("2. Select the Binary Turnover Variable"),
        uiOutput("outcome_selector"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("3. Select Predictor Variables"),
        uiOutput("features_selector"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("4. Determine the Training/Testing Split"),
        tags$p("Note that 20% of the training set percentage selected will be reserved for the validation set."),
        
        uiOutput("split_ratio_selector"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("5. Set Model Hyperparameters"),
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          sliderInput("epochs", "Number of Epochs", min = 1, max = 100, value = 10),
          tags$span(
            icon("info-circle"),
            title = "The number of iterations through the dataset during training. More epochs may improve performance but increase training time and risk of overfitting.",
            style = "margin-left: 10px; cursor: help; color: #FFB74D;"
          )
        ),
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          numericInput("num_layers", "Number of Hidden Layers", value = 1, min = 1, max = 10),
          tags$span(
            icon("info-circle"),
            title = "Specifies the number of hidden layers in the neural network. Increasing layers can capture complex patterns but also increases computation and risk of overfitting.",
            style = "margin-left: 10px; cursor: help; color: #FFB74D;"
          )
        ),
        uiOutput("layers_ui"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("6. Train and Apply the Model"),
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          actionButton("start_training", "Run MLP"),
          tags$span(
            "This may take a few moments (especially with more epochs). Wait until you see a 'Training Complete!' message under the button.",
            style = "font-size: 12px; color: #555; margin-left: 10px;"
          )
        ),
        textOutput("training_status"),
        tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"), # Subtle separator
        
        h4("7. Model Results"),
        tags$p("Download the predicted turnover risks for current employees."),
        useShinyjs(),  # Initialize shinyjs
        downloadButton("download_predictions_mlp", "Download Predictions")
      ),
      mainPanel(
        # Styled header for MLP tab
        div(
          style = "background-color: #f1f1f1; padding: 20px; border-radius: 10px; border: 1px solid #ccc; margin-bottom: 20px;",
          h2("Multilayer Perceptron (MLP)", style = "font-weight: bold; color: #333; text-align: center;"),
          p(
            "Use this section to explore your dataset, train a multilayer perceptron model, monitor training progress, assess model performance, adjust hyperparameters, identify employees at risk of leaving, and simulate the impact of predictor adjustments on turnover risk.",
            style = "font-size: 15px; color: #555; text-align: center;"
          )
        ),
        tabsetPanel(
          tabPanel(
            "Dataset Preview", 
            h4("Preview the Dataset"),  # Add a title for the Dataset Preview tab
            tags$p("Preview the data used for the MLP model and view summary turnover statistics."),
            div(style = "width:100%; overflow-x:auto;", DTOutput("data_table_mlp")),
            uiOutput("outcome_stats_box_mlp"),
            hr(),
            h4("Confirm Variable Types and Check Missing Values"),  
            div(style = "width:100%; overflow-x:auto;", DTOutput("data_type_table_mlp"))
          )
          ,
          tabPanel("Explore the Data",
                   h4("Visualize Predictor Distributions"),  
                   tags$p("Visualize how predictor variables are distributed when grouped by turnover status."),
                   selectInput(
                     "selected_predictor_mlp",
                     "Select Predictor to Visualize:",
                     choices = NULL,  # Choices will be populated dynamically
                     selected = NULL
                   ),
                   plotlyOutput("predictor_histogram_mlp"),
                   br(),
                   hr(),
                   h4("Summary Statistics for Numeric Variables"),
                   DTOutput("numeric_summary_table_mlp"),
                   br(),
                   hr(),
                   h4("View Counts for Categorical and Binary Variables"),
                   DTOutput("category_count_table_mlp"),
                   uiOutput("small_count_warning_mlp_1"),
                   br(),
                   hr(),
                   h4("Correlation Matrix"),
                   plotlyOutput("correlation_matrix_plot_mlp")
          ),
          tabPanel("Training Progress",
                   h4("View Training Progress"), 
                   tags$p("Visualize the training and validation loss and accuracy over epochs."),
                   
                   plotlyOutput("combined_plot", height = "500px"),
                   
                   tags$p(HTML(
                     "<span style='color: blue;'>Training loss</span> tracks how well the model is learning from the training data over epochs. A decreasing trend indicates that the model is learning patterns and improving its fit on the training data."
                   )),
                   
                   tags$p(HTML(
                     "<span style='color: red;'>Validation loss</span> tracks how well the model generalizes to unseen validation data during training. It helps monitor potential overfitting or underfitting."
                   )),
                   
                   tags$p(HTML(
                     "<span style='color: green;'>Training accuracy</span> measures how well the model is performing on the training dataset. It represents the proportion of correctly predicted instances compared to the total number of training instances. It indicates how well the model has learned patterns from the data it's being trained on."
                   )),
                   
                   tags$p(HTML(
                     "<span style='color: orange;'>Validation accuracy</span> measures how well the model is performing on the validation dataset, which is data the model has not seen during training. It gives an estimate of the model's ability to generalize to unseen data."
                   ))
          ),
          
          
          tabPanel("Model Performance",
                   h4("Model Performance"),
                   tags$p("Review metrics such as accuracy, precision, recall, and more to evaluate model performance on the test set."),
                   
                   verbatimTextOutput("evaluation_metrics"),
                   verbatimTextOutput("confusion_matrix"),
                   uiOutput("small_count_warning_mlp_2"),
                   bsCollapse(
                     bsCollapsePanel(
                       "Understanding Performance Metrics",
                       p("Here are definitions of key metrics used to evaluate model performance:"),
                       tags$ul(
                         tags$li(tags$b("Accuracy:"), " The percentage of correct predictions (both stayed and left) out of the total predictions. It provides an overall measure of correctness."),
                         tags$li(tags$b("Precision:"), " Among the employees predicted to leave, the percentage that actually left. It measures how trustworthy the model is when it predicts an employee will leave."),
                         tags$li(tags$b("Recall (Sensitivity):"), " Among the employees who actually left, the percentage that were correctly predicted to leave. It measures the model's ability to identify employees at risk of turnover."),
                         tags$li(tags$b("F1 Score:"), " The harmonic mean of precision and recall. It provides a balanced measure when there’s an uneven class distribution (e.g., fewer employees leave than stay).")
                       ),
                       style = "info"  # Optional: Set style to 'info', 'success', etc.
                     )
                   )),
          
          tabPanel("Risk Prediction",
                   h4("Predict Turnover Risk"),  # Add a title for the Risk Prediction tab
                   tags$p("Use the model to predict which of your current employees are predicted by the model to be at risk of leaving and visualize risk distributions. The default risk threshold is set to a probability of 0.5 of turning over. Those with a probability above 0.5 are categorized as being At-Risk (and vice versa). Modify this threshold as needed by adjusting the slider below."),
                   
                   sidebarLayout(
                     sidebarPanel(
                       h4("Adjust Risk Threshold"),
                       sliderInput(
                         "risk_threshold_mlp",
                         "Risk Threshold",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.01
                       )
                     ),
                     mainPanel(
                       plotlyOutput("risk_histogram_mlp"),
                       br(),
                       plotlyOutput("risk_piechart_mlp"),
                       verbatimTextOutput("summary_preds_mlp"),
                       verbatimTextOutput("debug_probs")
                     )
                   )),
          tabPanel(
            "Adjust Predictors and Re-Simulate",
            h4("Experiment with Predictor Adjustments"),  # Add a title for the tab
            tags$p("Modify predictor values and click Simulate Adjustments to simulate their impact on turnover risk predictions and to compare the results with original data."),
            
            # Sidebar and Main Panel for Adjust Numeric Predictors and Histogram
            sidebarLayout(
              sidebarPanel(
                h4("Adjust Numeric Predictors"),
                uiOutput("adjust_numeric_inputs_mlp"),
                helpText(
                  "Note: Adjusted values will remain within the observed minimum and maximum range of the dataset."
                ),
                actionButton("simulate_adjustments_mlp", "Simulate Adjustments")
              ),
              mainPanel(
                plotlyOutput("adjusted_simulation_histogram_mlp"),
                br()
              )
            ),
            
            # Section for Data Tables Below the Main Panel
            fluidRow(
              column(
                width = 12,
                h4("Original Data"),
                div(style = "width:100%; overflow-x:auto;", DTOutput("original_data_table_mlp")),
                br(),
                h4("Adjusted Data"),
                div(style = "width:100%; overflow-x:auto;", DTOutput("adjusted_data_table_mlp"))
              )
            )
          )
          
        )
      )
    )
  )
  
)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Define the Server
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

server <- function(input, output, session) {
  
  # Navigate to Model Overview
  observeEvent(input$go_models, {
    updateNavbarPage(session, "main_navbar", selected = "Model Overview")
  })
  
  # Navigate to Logistic Regression
  observeEvent(input$nav_lr, {
    updateNavbarPage(session, "main_navbar", selected = "Logistic Regression")
  })
  
  # Navigate to Random Forest
  observeEvent(input$nav_rf, {
    updateNavbarPage(session, "main_navbar", selected = "Random Forest")
  })
  
  # Navigate to MLP
  observeEvent(input$nav_mlp, {
    updateNavbarPage(session, "main_navbar", selected = "MLP")
  })
  
  # Toggles for collapsible content
  observeEvent(input$logistic_collapse, {
    shinyjs::toggle("logistic_details")
  })
  observeEvent(input$rf_collapse, {
    shinyjs::toggle("randomforest_details")
  })
  observeEvent(input$mlp_collapse, {
    shinyjs::toggle("mlp_details")
  })
  
  # Helper function to split dataset
  split_data <- function(data, split_ratio) {
    set.seed(123) # For reproducibility
    train_index <- createDataPartition(data[[1]], p = split_ratio / 100, list = FALSE)
    list(train = data[train_index, ], test = data[-train_index, ])
  }
  
  calculate_metrics <- function(actual, predicted) {
    confusion <- table(Actual = actual, Predicted = predicted)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    precision <- ifelse("1" %in% colnames(confusion), confusion["1", "1"] / sum(confusion[, "1"], na.rm = TRUE), 0)
    recall <- ifelse("1" %in% rownames(confusion), confusion["1", "1"] / sum(confusion["1", ], na.rm = TRUE), 0)
    f1 <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
    list(confusion = confusion, accuracy = accuracy, precision = precision, recall = recall, f1 = f1)
  }
  
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  # Logistic Regression
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Dataset Preview
  #-----------------------------------------------------------------------------#
  
  uploaded_data_lr <- reactive({
    if (is.null(input$file_lr)) {
      # Use default dataset if no file is uploaded
      data <- default_data
    } else {
      # Use the uploaded file
      data <- read.csv(input$file_lr$datapath)
    }
    
    # Convert all character variables to factors
    data <- data %>%
      mutate(across(where(is.character), as.factor))
    
    return(data)
  })
  
  
  # Render the turnover column UI
  output$turnover_column_ui_lr <- renderUI({
    req(uploaded_data_lr())
    
    data <- uploaded_data_lr()
    default_turnover <- if ("left" %in% names(data)) "left" else names(data)[1]
    
    selectInput(
      inputId = "turnover_column_lr",
      label = "Select Turnover Column",
      choices = names(data),
      selected = input$turnover_column_lr %||% default_turnover  # Keep the user selection, default to "left"
    )
  })
  
  # Render the predictor columns UI dynamically
  output$predictor_columns_ui_lr <- renderUI({
    req(uploaded_data_lr(), input$turnover_column_lr)
    
    # Exclude the selected outcome column from the predictor choices
    excluded_column <- input$turnover_column_lr
    available_predictors <- setdiff(names(uploaded_data_lr()), excluded_column)
    
    checkboxGroupInput(
      inputId = "predictor_columns_lr",
      label = "Select Predictor Variables",
      choices = available_predictors,
      selected = available_predictors  # Default to all predictors except the outcome column
    )
  })
  
  # Observe to update predictor columns dynamically based on selected outcome column
  observe({
    req(uploaded_data_lr(), input$turnover_column_lr)
    data <- uploaded_data_lr()
    
    # Exclude the selected outcome column from the predictor choices
    excluded_column <- input$turnover_column_lr
    available_predictors <- setdiff(names(data), excluded_column)
    
    updateCheckboxGroupInput(
      session,
      "predictor_columns_lr",
      choices = available_predictors,
      selected = available_predictors  # Update the selected predictors as well
    )
  })
  
  # Check if the selected outcome column is binary
  observe({
    req(uploaded_data_lr(), input$turnover_column_lr)
    
    # Get the selected turnover column
    turnover_column <- input$turnover_column_lr
    data <- uploaded_data_lr()
    
    # Check if the column is binary with values 0 and 1
    if (!is.numeric(data[[turnover_column]]) || !all(sort(unique(data[[turnover_column]])) == c(0, 1))) {
      showNotification(
        "Error: The selected turnover column must be binary (0 and 1).",
        type = "error"
      )
    }
  })
  
  output$data_table_lr <- renderDT({
    req(uploaded_data_lr())
    datatable(uploaded_data_lr(), options = list(pageLength = 5))
  })
  
  turnover_stats_lr <- reactive({
    req(uploaded_data_lr(), input$turnover_column_lr)
    data <- uploaded_data_lr()
    turnover_column <- input$turnover_column_lr
    
    if (is.numeric(data[[turnover_column]])) {
      # Ensure the turnover column is binary (0 and 1)
      counts <- table(data[[turnover_column]])
      if (length(counts) == 2) {
        total <- sum(counts)
        turnover_rate <- counts["1"] / total
        imbalance_ratio <- max(counts) / min(counts)  # Calculate imbalance ratio
        
        # Return relevant stats and imbalance ratio
        list(
          total = total,
          zero_count = counts["0"],
          one_count = counts["1"],
          turnover_rate = turnover_rate,
          imbalance_ratio = imbalance_ratio
        )
      } else {
        return(NULL)  # If not binary
      }
    } else {
      return(NULL)  # If not numeric
    }
  })
  
  # Render turnover statistics box in the dataset preview panel
  output$turnover_stats_box_lr <- renderUI({
    req(uploaded_data_lr(), input$turnover_column_lr)
    data <- uploaded_data_lr()
    turnover_column <- input$turnover_column_lr
    stats <- turnover_stats_lr()
    
    if (!is.null(stats)) {
      # Check if the imbalance ratio is too high (e.g., ratio > 5 indicates significant imbalance)
      imbalance_warning <- ""
      if (stats$imbalance_ratio > 5) {  # Fixed the missing closing parenthesis
        imbalance_warning <- HTML(
          "<div style='color: red;'>
          Warning: The dataset is highly imbalanced (the difference between outcome = 0 and outcome = 1 is too large, with an imbalance ratio greater than 5). This could make the model biased towards predicting the larger class.<br><br>
          Suggestions:<br>
          <ul style='margin-left: 20px;'>
            <li>Increase the number of examples for the smaller class (e.g., oversampling).</li>
            <li>Reduce the number of examples from the larger class (e.g., undersampling).</li>
            <li>Use a model that gives more attention to the smaller class by adjusting weights for the classes.</li>
          </ul>
          <p style='margin-top: 10px;'>Please note: These solutions must be applied <b>outside</b> the app. Using the app with the current dataset is not advised, as the model may not perform well with such a high imbalance.</p>
          </div>"
        )    
        }
      
      # Render the statistics for a binary outcome
      tagList(
        div(class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
            h4("Turnover Statistics", style = "font-weight: bold;"),
            tags$ul(
              tags$li(sprintf("Total Employees: %d", stats$total), style = "font-size: 14px;"),
              tags$li(sprintf("Number of 0s (Employees Remaining): %d", stats$zero_count), style = "font-size: 14px;"),
              tags$li(sprintf("Number of 1s (Employees who Left): %d", stats$one_count), style = "font-size: 14px;"),
              tags$li(sprintf("Turnover Rate: %.2f%%", stats$turnover_rate * 100), style = "font-size: 14px;")
            ),
            # Add imbalance warning if necessary
            if (imbalance_warning != "") {
              tags$p(style = "color: red;", imbalance_warning)
            }
        )
      )
    } else {
      # Render the error message for invalid outcome variables
      div(
        class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
        h4("Turnover Statistics", style = "font-weight: bold;"),
        p("Selected column is not binary.")
      )
    }
  })
  
  output$data_type_table_lr <- renderDT({
    req(uploaded_data_lr())
        
    # Create a summary of the dataset
    df <- uploaded_data_lr()
    summary_table <- data.frame(
      Variable = names(df),                         # Variable names
      DataType = sapply(df, class),                 # Data types
      UniqueValues = sapply(df, function(x) length(unique(x))),  # Number of unique values
      MissingValues = sapply(df, function(x) sum(is.na(x))),     # Count of missing values
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #-----------------------------------------------------------------------------#
  # Exploring the Data
  #-----------------------------------------------------------------------------#
  
  # Table for numeric summary (Mean, SD, Min, Q1, Median, Q3, Max)
  output$numeric_summary_table_lr <- renderDT({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    data <- uploaded_data_lr()
    
    # Identify numeric columns
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    # Exclude binary predictors (columns with only 2 unique values)
    non_binary_numeric_vars <- numeric_vars[sapply(data[numeric_vars], function(x) length(unique(x)) > 2)]
    
    # Filter based on user selection
    selected_vars <- intersect(non_binary_numeric_vars, input$predictor_columns_lr)
    
    # Check if there are no selected numeric predictors
    validate(
      need(length(selected_vars) > 0, "No numeric predictors selected")
    )
    
    # Create a summary table for each selected numeric variable
    summary_table <- data.frame(
      Variable = selected_vars,
      Mean = round(sapply(data[selected_vars], mean, na.rm = TRUE), 2),
      SD = round(sapply(data[selected_vars], sd, na.rm = TRUE), 2),
      Min = round(sapply(data[selected_vars], min, na.rm = TRUE), 2),
      Q1 = round(sapply(data[selected_vars], function(x) quantile(x, 0.25, na.rm = TRUE)), 2),
      Median = round(sapply(data[selected_vars], median, na.rm = TRUE), 2),
      Q3 = round(sapply(data[selected_vars], function(x) quantile(x, 0.75, na.rm = TRUE)), 2),
      Max = round(sapply(data[selected_vars], max, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  categorical_columns_lr <- reactive({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    data <- uploaded_data_lr()
    
    # Identify factor variables and binary variables
    factor_or_binary_vars <- names(data)[sapply(data, function(x) is.factor(x) | (is.numeric(x) & length(unique(x)) == 2))]
    
    # Filter the selected predictors to include only factor or binary variables
    selected_factor_or_binary_vars <- intersect(factor_or_binary_vars, input$predictor_columns_lr)
    
    selected_factor_or_binary_vars
  })
  
  output$category_count_table_lr <- renderDT({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    df <- uploaded_data_lr()
    
    # Identify factor or binary variables
    factor_or_binary_vars <- names(df)[sapply(df, function(x) is.factor(x) | (is.numeric(x) & length(unique(x)) == 2))]
    
    # Filter the selected predictors to include only factor or binary variables
    selected_vars <- intersect(factor_or_binary_vars, input$predictor_columns_lr)
    
    # Check if there are no selected categorical predictors
    validate(
      need(length(selected_vars) > 0, "No categorical predictors selected")
    )
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Render the table with counts for selected categorical variables
    datatable(count_summary, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Calculate small count warning for Linear Regression
  # Calculate small count warning for Linear Regression
  output$small_count_warning_lr_1 <- renderUI({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    df <- uploaded_data_lr()
    
    # Identify factor or character variables
    factor_or_character_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Filter based on selected predictors
    selected_vars <- intersect(factor_or_character_vars, input$predictor_columns_lr)
    
    # If no categorical predictors are selected, return NULL (no message displayed)
    if (length(selected_vars) == 0) {
      return(NULL)
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of categorical predictor variables have sufficient counts for analysis.")
    }
  })
  
  # Calculate small count warning for Linear Regression
  output$small_count_warning_lr_2 <- renderUI({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    df <- uploaded_data_lr()
    
    # Identify factor or character variables
    factor_or_character_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Filter based on selected predictors
    selected_vars <- intersect(factor_or_character_vars, input$predictor_columns_lr)
    
    # If no categorical predictors are selected, return NULL (no message displayed)
    if (length(selected_vars) == 0) {
      return(NULL)
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of categorical predictor variables have sufficient counts for analysis.")
    }
  })
  
  # Calculate small count warning for Linear Regression
  output$small_count_warning_lr_3 <- renderUI({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    
    # Get the uploaded dataset
    df <- uploaded_data_lr()
    
    # Identify factor or character variables
    factor_or_character_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Filter based on selected predictors
    selected_vars <- intersect(factor_or_character_vars, input$predictor_columns_lr)
    
    # If no categorical predictors are selected, return NULL (no message displayed)
    if (length(selected_vars) == 0) {
      return(NULL)
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of categorical predictor variables have sufficient counts for analysis.")
    }
  })
  
  

  output$correlation_matrix_plot_lr <- renderPlotly({
    req(uploaded_data_lr(), input$predictor_columns_lr, input$turnover_column_lr)
    
    # Get the uploaded dataset
    df <- uploaded_data_lr()
    
    # Identify numeric variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    # Ensure the outcome variable is included
    turnover_var <- input$turnover_column_lr
    selected_numeric_vars <- intersect(numeric_vars, input$predictor_columns_lr)
    if (turnover_var %in% numeric_vars) {
      selected_numeric_vars <- unique(c(selected_numeric_vars, turnover_var))
    }
    
    # Subset the data to selected numeric variables
    numeric_data <- df[selected_numeric_vars]
    
    # Calculate the correlation matrix
    correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    # Calculate p-values for significance tests
    p_matrix <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
    for (i in 1:ncol(numeric_data)) {
      for (j in 1:ncol(numeric_data)) {
        if (i != j) {
          test <- cor.test(numeric_data[[i]], numeric_data[[j]], use = "pairwise.complete.obs")
          p_matrix[i, j] <- test$p.value
        } else {
          p_matrix[i, j] <- NA  # No p-value for diagonal elements
        }
      }
    }
    
    # Create significance stars based on p-values
    significance_stars <- apply(p_matrix, c(1, 2), function(p) {
      if (is.na(p)) {
        return("")
      } else if (p < 0.001) {
        return("***")
      } else if (p < 0.01) {
        return("**")
      } else if (p < 0.05) {
        return("*")
      } else {
        return("")
      }
    })
    
    # Combine correlation values and stars for annotations
    annotation_text <- matrix(
      paste0(
        round(correlation_matrix, 2),
        " ",
        significance_stars
      ),
      nrow = nrow(correlation_matrix),
      ncol = ncol(correlation_matrix)
    )
    
    # Create the heatmap with annotations
    correlation_plot <- plot_ly(
      x = colnames(correlation_matrix),
      y = rownames(correlation_matrix),
      z = correlation_matrix,
      type = "heatmap",
      colorscale = "YlGnBu",
      zmin = -1,
      zmax = 1
    ) %>%
      add_annotations(
        x = rep(colnames(correlation_matrix), each = nrow(correlation_matrix)),
        y = rep(rownames(correlation_matrix), times = ncol(correlation_matrix)),
        text = as.vector(annotation_text),
        showarrow = FALSE,
        font = list(color = "black", size = 10)
      ) %>%
      layout(
        xaxis = list(tickangle = 45),
        yaxis = list(title = "")
      )%>%
      config(displayModeBar = FALSE)
    
    
    return(correlation_plot)
  })
  
  observe({
    req(uploaded_data_lr())
    print("Uploaded data column names:")
    print(names(uploaded_data_lr()))
    print("Predictor columns:")
    print(input$predictor_columns_lr)  # Should show valid column names
  })
  
  observe({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    updateSelectInput(session, "selected_predictor_lr", choices = input$predictor_columns_lr)
  })
  predictor_histogram_data_lr <- reactive({
    req(uploaded_data_lr(), input$turnover_column_lr, input$selected_predictor_lr)
    
    data <- uploaded_data_lr()
    turnover_column <- input$turnover_column_lr
    predictor <- input$selected_predictor_lr
    
    # Ensure both the predictor and turnover column exist
    if (!predictor %in% colnames(data) || !turnover_column %in% colnames(data)) {
      showNotification("Predictor or Turnover column not found in dataset", type = "error")
      return(NULL)
    }
    
    # Select the relevant columns and ensure the Turnover column is treated as a factor
    data <- data %>%
      select(all_of(turnover_column), all_of(predictor)) %>%
      mutate(Turnover = as.factor(.data[[turnover_column]]))
    
    # Check if predictor is numeric, categorical, or binary
    predictor_data <- data[[predictor]]
    if (!is.numeric(predictor_data) && !is.character(predictor_data) && !is.factor(predictor_data)) {
      showNotification("Selected predictor column must be numeric, categorical, or binary", type = "error")
      return(NULL)
    }
    
    data
  })
  
  output$predictor_histogram_lr <- renderPlotly({
    req(predictor_histogram_data_lr())
    data <- predictor_histogram_data_lr()
    
    predictor <- input$selected_predictor_lr
    
    # Define custom colors for the Turnover groups
    custom_colors <- c("#FFB74D", "#64B5F6")  # Your color scheme
    
    # Ensure Turnover is a factor
    data <- data %>%
      mutate(Turnover = as.factor(Turnover))
    
    # Check the type of the predictor
    predictor_data <- data[[predictor]]
    is_numeric <- is.numeric(predictor_data)
    is_binary_or_categorical <- !is_numeric || length(unique(predictor_data)) == 2
    
    if (is_numeric && !is_binary_or_categorical) {
      # Numeric (non-binary): Create histogram
      plot_ly(
        data = data,
        x = ~get(predictor),
        color = ~Turnover,
        colors = custom_colors,
        type = "histogram",
        nbinsx = 30,
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "overlay",
          legend = list(title = list(text = "Turnover"))
        )%>%
        config(displayModeBar = FALSE)
    } else {
      # Binary or categorical: Create bar plot
      data_summary <- data %>%
        group_by(.data[[predictor]], Turnover) %>%
        summarize(Count = n(), .groups = "drop")
      
      plot_ly(
        data = data_summary,
        x = ~.data[[predictor]],
        y = ~Count,
        color = ~Turnover,
        colors = custom_colors,
        type = "bar",
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "group",
          legend = list(title = list(text = "Turnover"))
        )%>%
        config(displayModeBar = FALSE)
    }
  })
  
  #-----------------------------------------------------------------------------#
  # Assumptions Checking
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Assumptions Checking
  #-----------------------------------------------------------------------------#
  
  check_assumptions_lr <- reactive({
    req(input$predictor_columns_lr, input$turnover_column_lr, model_results_lr())
    data <- uploaded_data_lr()
    predictors <- input$predictor_columns_lr
    turnover_column <- input$turnover_column_lr
    
    # Return if no predictors are selected
    if (length(predictors) == 0) {
      return(list(
        binary_check = "Not Applicable (No predictors selected)",
        sample_size_check = "Not Applicable (No predictors selected)",
        linearity = "Not Applicable (No predictors selected)",
        multicollinearity = "Not Applicable (No predictors selected)"
      ))
    }
    
    # Create formula and logistic regression model
    formula <- as.formula(paste(turnover_column, "~", paste(predictors, collapse = "+")))
    model <- glm(formula, data = data, family = binomial())
    
    results <- list()
    
    # Check if turnover column is binary
    if (is.numeric(data[[turnover_column]]) && length(unique(data[[turnover_column]])) == 2) {
      binary_check <- "Met (Turnover column is binary)"
    } else {
      binary_check <- "Not Met (Turnover column must be numeric with exactly two unique values)"
    }
    results$binary_check <- binary_check
    
    # Check sample size
    least_frequent_count <- min(table(data[[turnover_column]]))
    num_predictors <- length(predictors)
    required_sample_size <- 10 * num_predictors / (least_frequent_count / nrow(data))
    
    if (least_frequent_count >= required_sample_size) {
      sample_size_check <- paste("Met (Minimum sample size satisfied: least frequent outcome has", 
                                 least_frequent_count, "cases, requires at least", 
                                 round(required_sample_size), "cases)")
    } else {
      sample_size_check <- paste("Not Met (Minimum sample size not satisfied: least frequent outcome has", 
                                 least_frequent_count, "cases, requires at least", 
                                 round(required_sample_size), "cases)")
    }
    results$sample_size_check <- sample_size_check
    
    # Check linear relationship with logit
    linearity_results <- list()
    for (predictor in predictors) {
      if (is.numeric(data[[predictor]]) && length(unique(data[[predictor]])) > 1) {
        logit <- log(model$fitted.values / (1 - model$fitted.values))
        scatter <- cor(data[[predictor]], logit)
        correlation_result <- if (abs(scatter) < 0.3) {
          "Weak correlation with logit"
        } else {
          "Met"
        }
        
        data[[paste0("log_", predictor)]] <- log(data[[predictor]] + 1e-5)
        bt_formula <- as.formula(paste(turnover_column, "~", predictor, "+", 
                                       paste0(predictor, ":log_", predictor)))
        bt_model <- glm(bt_formula, data = data, family = binomial())
        bt_summary <- summary(bt_model)
        interaction_term <- paste0(predictor, ":log_", predictor)
        
        if (interaction_term %in% rownames(bt_summary$coefficients)) {
          bt_result <- if (bt_summary$coefficients[interaction_term, "Pr(>|z|)"] < 0.05) {
            "Box-Tidwell significant"
          } else {
            "Box-Tidwell test passed"
          }
        } else {
          bt_result <- "Box-Tidwell term missing (possibly constant or invalid values)"
        }
        
        if (correlation_result == "Met" && bt_result == "Box-Tidwell test passed") {
          linearity_results[[predictor]] <- "Met"
        } else {
          issues <- c()
          if (correlation_result != "Met") issues <- c(issues, correlation_result)
          if (bt_result != "Box-Tidwell test passed") issues <- c(issues, bt_result)
          linearity_results[[predictor]] <- paste("Not Met (", paste(issues, collapse = ", "), ")", sep = "")
        }
      } else {
        linearity_results[[predictor]] <- "Not Applicable (Categorical or constant predictor)"
      }
    }
    results$linearity <- linearity_results
    
    # Check multicollinearity using VIF
    valid_predictors <- predictors[sapply(data[predictors], is.numeric) & sapply(data[predictors], function(x) length(unique(x)) > 1)]
    
    if (length(valid_predictors) < 2) {
      multicollinearity_result <- "Not Applicable (Too few predictors for VIF calculation)"
    } else {
      valid_formula <- as.formula(paste(turnover_column, "~", paste(valid_predictors, collapse = "+")))
      valid_model <- glm(valid_formula, data = data, family = binomial())
      vif_values <- vif(valid_model)
      
      if (all(vif_values <= 5)) {
        multicollinearity_result <- "Met (All VIF values are <= 5)"
      } else {
        high_vif <- names(vif_values[vif_values > 5])
        multicollinearity_result <- paste("Not Met (High VIF for:", paste(high_vif, collapse = ", "), ")")
      }
    }
    results$multicollinearity <- multicollinearity_result
    
    return(results)
  })
  
  output$assumption_check_lr <- renderUI({
    req(check_assumptions_lr())
    results <- check_assumptions_lr()
    
    binary_message <- results$binary_check
    sample_size_message <- results$sample_size_check
    
    linearity_message <- if (is.character(results$linearity)) {
      results$linearity
    } else {
      paste(
        lapply(names(results$linearity), function(predictor) {
          paste("Predictor", predictor, ":", results$linearity[[predictor]])
        }),
        collapse = "<br>"
      )
    }
    
    multicollinearity_message <- results$multicollinearity
    
    # Check if recommendations are needed
    binary_recommendation <- if (grepl("Not Met", binary_message)) {
      div(
        p("Recommendation: Ensure the outcome variable is binary (e.g., 0 = No, 1 = Yes). If the variable is categorical with more than two levels, recode it into a binary format. Re-run the app with the updated dataset.", 
          style = "color: orange; font-style: italic;")
      )
    } else {
      NULL
    }
    
    sample_size_recommendation <- if (grepl("Not Met", sample_size_message)) {
      div(
        p("Recommendation: Increase your sample size or reduce the number of predictors in the model to satisfy the minimum sample size requirement. Re-run the app.", 
          style = "color: orange; font-style: italic;")
      )
    } else {
      NULL
    }
    
    linearity_not_met <- any(sapply(results$linearity, function(x) grepl("Not Met", x)))
    linearity_recommendation <- if (linearity_not_met) {
      div(
        p("Recommendation: Consider turning continuous predictors into categories (e.g., using quantiles) or applying transformations (e.g., logarithmic or polynomial) to improve linearity with the logit. Re-run the app with the updated dataset.", 
          style = "color: orange; font-style: italic;")
      )
    } else {
      NULL
    }
    
    multicollinearity_recommendation <- if (grepl("Not Met", multicollinearity_message)) {
      div(
        p("Recommendation: Address multicollinearity by removing highly correlated predictors or combining them into a single composite variable.", 
          style = "color: orange; font-style: italic;")
      )
    } else {
      NULL
    }
    
    # Render the full UI
    div(
      h4("Assumption Checks", style = "font-weight: bold;"),
      
      # Binary check
      div(style = "margin-bottom: 10px;",
          h5("Binary Outcome:", style = "font-weight: bold;"),
          p(binary_message, style = ifelse(grepl("Not Met", binary_message), "color: red;", "color: green;")),
          binary_recommendation
      ),
      
      # Sample size check
      div(style = "margin-bottom: 10px;",
          h5("Sample Size:", style = "font-weight: bold;"),
          p(sample_size_message, style = ifelse(grepl("Not Met", sample_size_message), "color: red;", "color: green;")),
          sample_size_recommendation
      ),
      
      # Multicollinearity
      div(style = "margin-bottom: 10px;",
          h5("Multicollinearity:", style = "font-weight: bold;"),
          p(multicollinearity_message, style = ifelse(grepl("Not Met", multicollinearity_message), "color: red;", "color: green;")),
          multicollinearity_recommendation
      ),
      
      # Linearity with logit
      div(style = "margin-bottom: 10px;",
          h5("Linearity with Logit:", style = "font-weight: bold;"),
          HTML(linearity_message),
          linearity_recommendation
      )
    )
  })
  
  
  #-----------------------------------------------------------------------------#
  # Model Summary
  #-----------------------------------------------------------------------------#
  
  # Create a reactive value for training status
  training_status_lr <- reactiveVal("Idle")
  
  # Logistic Regression Model Training
  
  
  model_results_lr <- eventReactive(input$run_model_lr, {
    req(input$turnover_column_lr, input$predictor_columns_lr)
    data <- uploaded_data_lr()
    split <- split_data(data, input$split_ratio_lr)
    formula <- as.formula(paste(input$turnover_column_lr, "~", paste(input$predictor_columns_lr, collapse = "+")))
    model <- glm(formula, data = split$train, family = binomial())
    predictions <- ifelse(predict(model, newdata = split$test, type = "response") > 0.5, "1", "0")
    metrics <- calculate_metrics(split$test[[input$turnover_column_lr]], predictions)
    list(model = model, test_data = split$test, metrics = metrics)
  })
  
  observeEvent(input$run_model_lr, {
    req(input$turnover_column_lr, input$predictor_columns_lr)
    data <- uploaded_data_lr()
    split <- split_data(data, input$split_ratio_lr)
    
    # Start training
    training_status_lr("Training in progress...")
    
    # Logistic regression model
    formula <- as.formula(paste(input$turnover_column_lr, "~", paste(input$predictor_columns_lr, collapse = "+")))
    model <- glm(formula, data = split$train, family = binomial())
    
    # Update status to "Training complete"
    training_status_lr("Training complete!")
  })
  
  # Render the training status message
  output$training_status_lr <- renderText({
    training_status_lr()
  })
  
  # Render the training status in the UI
  output$training_status_lr <- renderText({
    training_status_lr()
  })
  
  
  output$model_summary_lr <- renderPrint({
    req(model_results_lr())
    
    # Extract the model
    model <- model_results_lr()$model
    summary_model <- summary(model)
    
    # Extract coefficients and statistics
    coefficients <- summary_model$coefficients
    conf_intervals <- confint(model)  # Confidence intervals
    
    # Create a formatted table
    formatted_output <- data.frame(
      Predictor = rownames(coefficients),
      Estimate = round(coefficients[, "Estimate"], 3),
      `Std. Error` = round(coefficients[, "Std. Error"], 3),
      `z-value` = round(coefficients[, "z value"], 3),
      `p-value` = ifelse(coefficients[, "Pr(>|z|)"] < .001, "< .001", round(coefficients[, "Pr(>|z|)"], 3)),
      `95% CI (Lower)` = round(conf_intervals[, 1], 3),
      `95% CI (Upper)` = round(conf_intervals[, 2], 3)
    )
    
    # Extract reference categories for categorical predictors
    reference_categories <- sapply(model$xlevels, function(levels) levels[1])
    
    # Add APA formatting
    cat("Logistic Regression Model Summary\n")
    cat("=================================\n")
    cat("Dependent Variable: ", deparse(formula(model)[[2]]), "\n\n")
    
    # Clean column names and print the table
    names(formatted_output) <- c("Predictor", "Estimate", "Std. Error", "z-value", "p-value", "95% CI (Lower)", "95% CI (Upper)")
    print(formatted_output, row.names = FALSE)
    
    cat("\nReference Categories for Categorical Variables:\n")
    if (length(reference_categories) > 0) {
      for (var in names(reference_categories)) {
        cat(sprintf("- %s: '%s'\n", var, reference_categories[[var]]))
      }
    } else {
      cat("None (No categorical variables detected)\n")
    }
    
    cat("\nModel Fit:\n")
    cat(sprintf("AIC: %.2f\n", summary_model$aic))
  })
  
  #-----------------------------------------------------------------------------#
  # Model Performance
  #-----------------------------------------------------------------------------#
  
  
  output$performance_metrics_lr <- renderPrint({
    req(model_results_lr())
    metrics <- model_results_lr()$metrics
    cat("Confusion Matrix:\n")
    print(metrics$confusion)
    cat(sprintf("\nAccuracy: %.2f\nPrecision: %.2f\nRecall: %.2f\nF1 Score: %.2f", 
                metrics$accuracy, metrics$precision, metrics$recall, metrics$f1))
  })
  
  #-----------------------------------------------------------------------------#
  # Risk Prediction (Pie Chart and Histogram)
  #-----------------------------------------------------------------------------#
  
  # Render the histogram
  output$risk_histogram_lr <- renderPlotly({
    req(predictions_lr(), input$risk_threshold)
    
    preds <- predictions_lr()
    
    plot_ly(
      data = preds,
      x = ~risk_score,
      color = ~risk_category,
      type = "histogram",
      nbinsx = 20,
      alpha = 0.7,
      colors = c("#FFB74D", "#64B5F6")
    ) %>%
      layout(
        title = "Risk Score Distribution (Remaining Employees)",
        xaxis = list(title = "Risk Score"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Risk Category"))
      )%>%
      config(displayModeBar = FALSE)
  })
  
  # Render the pie chart
  output$risk_piechart_lr <- renderPlotly({
    req(predictions_lr(), input$risk_threshold)
    
    preds <- predictions_lr()
    
    # Group by risk category
    risk_summary <- preds %>%
      group_by(risk_category) %>%
      summarise(Count = n())
    
    print(head(predictions_lr()))
    print(risk_summary)
    
    
    plot_ly(
      data = risk_summary,
      labels = ~risk_category,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#FFB74D", "#64B5F6"))
    ) %>%
      layout(
        title = "Proportion of Remaining Employees Predicted as 'At Risk'",
        showlegend = TRUE,
        margin = list(t = 100)  # Increase the top margin to create more space
      )%>%
      config(displayModeBar = FALSE)
  })
  
  #-----------------------------------------------------------------------------#
  # Downloading Predictions
  #-----------------------------------------------------------------------------#
  
  # Reactive value to track if the logistic regression model has been run
  model_status_lr <- reactiveValues(ran = FALSE)
  
  # Update the status when the logistic regression model is run
  observeEvent(input$run_model_lr, {
    # Simulate logistic regression training (replace with actual training logic)
    Sys.sleep(2)  # Simulate delay for training
    model_status_lr$ran <- TRUE
    output$training_status_lr <- renderText("Training Complete!")
  })
  
  # Dynamically enable or disable the download button
  observe({
    if (model_status_lr$ran) {
      shinyjs::enable("download_predictions_lr")
    } else {
      shinyjs::disable("download_predictions_lr")
    }
  })
  
  # Predictions for Logistic Regression
  predictions_lr <- reactive({
    req(model_status_lr$ran)  # Require that the model has been run
    req(model_results_lr(), uploaded_data_lr(), input$turnover_column_lr)
    
    # Retrieve the entire dataset
    full_data <- uploaded_data_lr()
    
    # Ensure turnover column exists and contains at least one "0"
    req(input$turnover_column_lr %in% names(full_data))
    req(any(full_data[[input$turnover_column_lr]] == 0))
    
    # Filter to include only remaining employees (turnover column == 0)
    filtered_data <- full_data[full_data[[input$turnover_column_lr]] == 0, ]
    
    # Calculate risk scores (predicted probabilities)
    filtered_data$risk_score <- predict(model_results_lr()$model, newdata = filtered_data, type = "response")
    
    # Categorize based on the threshold
    filtered_data$risk_category <- ifelse(
      filtered_data$risk_score > input$risk_threshold,
      "Predicted At Risk",
      "Predicted Not At Risk"
    )
    
    # Return the full dataset for the filtered rows
    filtered_data
  })
  
  # Download Handler for Logistic Regression Predictions
  output$download_predictions_lr <- downloadHandler(
    filename = function() {
      paste0("predicted_risks_lr_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(predictions_lr())
      write.csv(predictions_lr(), file, row.names = FALSE)
    }
  )
  
  #-----------------------------------------------------------------------------#
  # What-if simulation
  #-----------------------------------------------------------------------------#
  
  # Reactive expression for filtered dataset (Turnover == 0)
  filtered_data_lr <- reactive({
    req(uploaded_data_lr(), input$turnover_column_lr)
    data <- uploaded_data_lr()
    turnover_column <- input$turnover_column_lr
    
    # Filter dataset for turnover == 0
    data[data[[turnover_column]] == 0, ]
  })
  
  # Dynamic UI for adjusting numeric predictors
  output$adjust_numeric_inputs_lr <- renderUI({
    req(filtered_data_lr(), numeric_predictors_lr())
    
    data <- filtered_data_lr()  # Use the filtered dataset here
    predictors <- numeric_predictors_lr()
    
    lapply(predictors, function(predictor) {
      predictor_min <- min(data[[predictor]], na.rm = TRUE)
      predictor_max <- max(data[[predictor]], na.rm = TRUE)
      adjustment_range <- predictor_max - predictor_min
      step_size <- adjustment_range / 20
      
      sliderInput(
        inputId = paste0("adjust_", predictor),
        label = paste("Adjustment for", predictor),
        min = -adjustment_range,
        max = adjustment_range,
        step = step_size,
        value = 0
      )
    })
  })
  
  # Reactive for non-binary numeric predictors
  numeric_predictors_lr <- reactive({
    req(uploaded_data_lr(), input$predictor_columns_lr)
    data <- uploaded_data_lr()
    
    # Identify numeric predictors that are not binary
    numeric_predictors <- input$predictor_columns_lr[sapply(data[input$predictor_columns_lr], is.numeric)]
    non_binary_predictors <- numeric_predictors[sapply(data[numeric_predictors], function(x) length(unique(x)) > 2)]
    
    non_binary_predictors
  })
  
  # EventReactive to calculate adjusted probabilities
  adjusted_predictions_lr <- eventReactive(input$simulate_adjustments_lr, {
    req(filtered_data_lr(), numeric_predictors_lr(), model_results_lr())
    
    # Use the filtered dataset
    data <- filtered_data_lr()
    adjusted_data <- data
    
    for (predictor in numeric_predictors_lr()) {
      adjustment <- input[[paste0("adjust_", predictor)]]
      if (!is.null(adjustment)) {
        adjusted_values <- adjusted_data[[predictor]] + adjustment
        
        # Enforce bounds within the original range of the predictor
        predictor_min <- min(data[[predictor]], na.rm = TRUE)
        predictor_max <- max(data[[predictor]], na.rm = TRUE)
        adjusted_data[[predictor]] <- pmax(pmin(adjusted_values, predictor_max), predictor_min)
      }
    }
    
    # Calculate probabilities for original and adjusted data
    original_probs <- predict(model_results_lr()$model, newdata = data, type = "response")
    adjusted_probs <- predict(model_results_lr()$model, newdata = adjusted_data, type = "response")
    
    list(
      original_data = data,
      adjusted_data = adjusted_data,
      probabilities = data.frame(
        Original_Probabilities = original_probs,
        Adjusted_Probabilities = adjusted_probs
      )
    )
  })
  
  # Render histogram to compare probabilities
  output$adjusted_simulation_histogram_lr <- renderPlotly({
    req(adjusted_predictions_lr())
    preds <- adjusted_predictions_lr()$probabilities
    
    plot_ly() %>%
      add_histogram(
        x = ~preds$Original_Probabilities,
        name = "Original Probabilities",
        opacity = 0.6
      ) %>%
      add_histogram(
        x = ~preds$Adjusted_Probabilities,
        name = "Adjusted Probabilities",
        opacity = 0.6
      ) %>%
      layout(
        title = "Comparison of Predicted Probabilities (Original vs Adjusted)",
        xaxis = list(title = "Predicted Probability"),
        yaxis = list(title = "Count"),
        barmode = "overlay",
        legend = list(title = list(text = "Probability Type"))
      )%>%
      config(displayModeBar = FALSE)
  })
  
  # Render the original data table (Filtered dataset)
  output$original_data_table <- renderDT({
    req(filtered_data_lr())
    datatable(filtered_data_lr(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Render the adjusted data table
  output$adjusted_data_table <- renderDT({
    req(adjusted_predictions_lr())
    datatable(adjusted_predictions_lr()$adjusted_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  # Random Forest
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Dataset Preview
  #-----------------------------------------------------------------------------#

  # Reactive to load the dataset (use uploaded file or default data)
  uploaded_data_rf <- reactive({
    if (is.null(input$file_rf)) {
      # Use default dataset if no file is uploaded
      data <- default_data
    } else {
      # Use the uploaded file
      data <- read.csv(input$file_rf$datapath)
    }
    
    # Convert all character variables to factors
    data <- data %>%
      mutate(across(where(is.character), as.factor))
    
    return(data)
  })
  
  # Render the turnover column UI (target column)
  output$target_column_ui_rf <- renderUI({
    req(uploaded_data_rf())
    
    # Preselect "left" if it exists, otherwise default to the first column
    default_target <- if ("left" %in% names(uploaded_data_rf())) "left" else names(uploaded_data_rf())[1]
    
    selectInput(
      inputId = "target_column_rf",
      label = "Select Turnover Column",
      choices = names(uploaded_data_rf()),
      selected = input$target_column_rf %||% default_target  # Keep the user selection, default to "left"
    )
  })
  
  # Render the predictor columns UI dynamically (exclude the target column)
  output$predictor_columns_ui_rf <- renderUI({
    req(uploaded_data_rf(), input$target_column_rf)
    
    # Exclude the selected outcome column from the predictor choices
    excluded_column <- input$target_column_rf
    available_predictors <- setdiff(names(uploaded_data_rf()), excluded_column)
    
    checkboxGroupInput(
      inputId = "predictor_columns_rf",
      label = "Select Predictor Variables",
      choices = available_predictors,
      selected = available_predictors  # Default to all predictors except the target column
    )
  })
  
  # Observe to handle dynamic default selections on dataset load
  observe({
    req(uploaded_data_rf())
    data <- uploaded_data_rf()
    
    # Only update if target_column_rf is NULL or empty (avoid forcing when user selects a different target column)
    if (is.null(input$target_column_rf) || input$target_column_rf == "") {
      if ("left" %in% names(data)) {
        updateSelectInput(
          session,
          "target_column_rf",
          selected = "left"
        )
      } else {
        # If "left" is not available, select the first column
        updateSelectInput(
          session,
          "target_column_rf",
          selected = names(data)[1]
        )
      }
    }
    
    # Update predictor selection to exclude the outcome column
    excluded_column <- input$target_column_rf
    available_predictors <- setdiff(names(data), excluded_column)
    updateCheckboxGroupInput(
      session,
      "predictor_columns_rf",
      selected = available_predictors
    )
  })
  
  # Check if the selected target column is binary (0 and 1)
  observe({
    req(uploaded_data_rf(), input$target_column_rf)
    
    target_column <- input$target_column_rf
    data <- uploaded_data_rf()
    
    # Check if the column has two unique values, and the values are 0 and 1
    if (length(unique(data[[target_column]])) != 2 || !all(sort(unique(data[[target_column]])) == c(0, 1))) {
      showNotification(
        "Error: The selected turnover column must be binary (exactly two unique values: 0 and 1).",
        type = "error"
      )
    }
  })
  
  output$data_table_rf <- renderDT({
    req(uploaded_data_rf())
    datatable(uploaded_data_rf(), options = list(pageLength = 5))
  })
  
  turnover_stats_rf <- reactive({
    req(uploaded_data_rf(), input$target_column_rf)
    data <- uploaded_data_rf()
    turnover_column <- input$target_column_rf
    
    if (is.numeric(data[[turnover_column]])) {
      # Ensure the turnover column is binary (0 and 1)
      counts <- table(data[[turnover_column]])
      if (length(counts) == 2) {
        total <- sum(counts)
        turnover_rate <- counts["1"] / total
        list(
          total = total,
          zero_count = counts["0"],
          one_count = counts["1"],
          turnover_rate = turnover_rate
        )
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  # Render turnover statistics box in the dataset preview panel for Random Forest
  output$turnover_stats_box_rf <- renderUI({
    req(uploaded_data_rf(), input$target_column_rf)
    data <- uploaded_data_rf()
    turnover_column <- input$target_column_rf
    
    if (is.numeric(data[[turnover_column]])) {
      counts <- table(data[[turnover_column]])
      if (length(counts) == 2) {
        total <- sum(counts)
        turnover_rate <- counts["1"] / total
        
        # Calculate the imbalance ratio (largest class / smallest class)
        imbalance_ratio <- max(counts) / min(counts)
        
        # Create the imbalance warning message if the ratio is above a threshold (e.g., > 5)
        imbalance_warning <- ""
        if (imbalance_ratio > 5) {
          imbalance_warning <- HTML(
            "<div style='color: red;'>
          Warning: The dataset is highly imbalanced (the difference between outcome = 0 and outcome = 1 is too large, with an imbalance ratio greater than 5). This could make the model biased towards predicting the larger class.<br><br>
          Suggestions:<br>
          <ul style='margin-left: 20px;'>
            <li>Increase the number of examples for the smaller class (e.g., oversampling).</li>
            <li>Reduce the number of examples from the larger class (e.g., undersampling).</li>
            <li>Use a model that gives more attention to the smaller class by adjusting weights for the classes.</li>
          </ul>
          <p style='margin-top: 10px;'>Please note: These solutions must be applied <b>outside</b> the app. Using the app with the current dataset is not advised, as the model may not perform well with such a high imbalance.</p>
          </div>"
          )    
          }
        
        tagList(
          div(class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
              h4("Turnover Statistics", style = "font-weight: bold;"),
              tags$ul(
                tags$li(sprintf("Total Employees: %d", total), style = "font-size: 14px;"),
                tags$li(sprintf("Number of 0s (Employees Remaining): %d", counts["0"]), style = "font-size: 14px;"),
                tags$li(sprintf("Number of 1s (Employees who Left): %d", counts["1"]), style = "font-size: 14px;"),
                tags$li(sprintf("Turnover Rate: %.2f%%", turnover_rate * 100), style = "font-size: 14px;")
              ),
              # Add imbalance warning if necessary
              if (imbalance_warning != "") {
                tags$p(style = "color: red;", imbalance_warning)
              }
          )
        )
      } else {
        div(h4("Turnover Statistics", style = "font-weight: bold;"), p("Selected column does not have valid binary values (0 and 1)."))
      }
    } else {
      div(h4("Turnover Statistics", style = "font-weight: bold;"), p("Selected column is not numeric."))
    }
  })
  
  output$data_type_table_rf <- renderDT({
    req(uploaded_data_rf())
    
    # Create a summary of the dataset
    df <- uploaded_data_rf()
    summary_table <- data.frame(
      Variable = names(df),                         # Variable names
      DataType = sapply(df, class),                 # Data types
      UniqueValues = sapply(df, function(x) length(unique(x))),  # Number of unique values
      MissingValues = sapply(df, function(x) sum(is.na(x))),     # Count of missing values
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #-----------------------------------------------------------------------------#
  # Exploring Data
  #-----------------------------------------------------------------------------#
  
  observe({
    req(uploaded_data_rf())
    updateSelectInput(session, "selected_predictor_rf", choices = input$predictor_columns_rf)
  })
  
  predictor_histogram_data_rf <- reactive({
    req(uploaded_data_rf(), input$target_column_rf, input$selected_predictor_rf)
    
    data <- uploaded_data_rf()
    turnover_column <- input$target_column_rf
    predictor <- input$selected_predictor_rf
    
    # Ensure the selected predictor and turnover column exist
    if (!predictor %in% colnames(data) || !turnover_column %in% colnames(data)) {
      return(NULL)
    }
    
    # Filter the data for visualization
    data <- data %>%
      select(all_of(turnover_column), all_of(predictor)) %>%
      mutate(Turnover = as.factor(data[[turnover_column]]))
    
    data
  })
  
  output$predictor_histogram_rf <- renderPlotly({
    req(predictor_histogram_data_rf())
    data <- predictor_histogram_data_rf()
    
    predictor <- input$selected_predictor_rf
    
    # Define custom colors for the Turnover groups
    custom_colors <- c("#FFB74D", "#64B5F6")  # Your color scheme
    
    # Check the type of the predictor
    predictor_data <- data[[predictor]]
    is_numeric <- is.numeric(predictor_data)
    is_binary_or_categorical <- !is_numeric || length(unique(predictor_data)) == 2
    
    if (is_numeric && !is_binary_or_categorical) {
      # Numeric (non-binary): Create histogram
      plot_ly(
        data = data,
        x = ~get(predictor),
        color = ~Turnover,
        colors = custom_colors,
        type = "histogram",
        nbinsx = 30,
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "overlay",
          legend = list(title = list(text = "Turnover"))
        )%>%
        config(displayModeBar = FALSE)
    } else {
      # Binary or categorical: Create bar plot
      data_summary <- data %>%
        group_by(.data[[predictor]], Turnover) %>%
        summarize(Count = n(), .groups = "drop")
      
      plot_ly(
        data = data_summary,
        x = ~.data[[predictor]],
        y = ~Count,
        color = ~Turnover,
        colors = custom_colors,
        type = "bar",
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "group",
          legend = list(title = list(text = "Turnover"))
        )%>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$numeric_summary_table_rf <- renderDT({
    req(uploaded_data_rf(), input$predictor_columns_rf)
    
    # Get the uploaded dataset
    data <- uploaded_data_rf()
    
    # Identify numeric columns
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    # Exclude binary predictors (columns with only 2 unique values)
    non_binary_numeric_vars <- numeric_vars[sapply(data[numeric_vars], function(x) length(unique(x)) > 2)]
    
    # Filter based on user selection
    selected_vars <- intersect(non_binary_numeric_vars, input$predictor_columns_rf)
    
    # Check if there are no selected numeric predictors
    validate(
      need(length(selected_vars) > 0, "No numeric predictors selected")
    )
    
    # Create a summary table for each selected numeric variable
    summary_table <- data.frame(
      Variable = selected_vars,
      Mean = round(sapply(data[selected_vars], mean, na.rm = TRUE), 2),
      SD = round(sapply(data[selected_vars], sd, na.rm = TRUE), 2),
      Min = round(sapply(data[selected_vars], min, na.rm = TRUE), 2),
      Q1 = round(sapply(data[selected_vars], function(x) quantile(x, 0.25, na.rm = TRUE)), 2),
      Median = round(sapply(data[selected_vars], median, na.rm = TRUE), 2),
      Q3 = round(sapply(data[selected_vars], function(x) quantile(x, 0.75, na.rm = TRUE)), 2),
      Max = round(sapply(data[selected_vars], max, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Render the count summary table for categorical variables for Random Forest
  output$category_count_table_rf <- renderDT({
    req(uploaded_data_rf(), input$predictor_columns_rf)
    
    # Get the uploaded dataset
    data <- uploaded_data_rf()
    
    # Identify factor or binary variables
    factor_or_binary_vars <- names(data)[sapply(data, function(x) is.factor(x) | (is.numeric(x) & length(unique(x)) == 2))]
    
    # Filter based on user selection
    selected_vars <- intersect(factor_or_binary_vars, input$predictor_columns_rf)
    
    # Check if there are no selected categorical predictors
    validate(
      need(length(selected_vars) > 0, "No categorical predictors selected")
    )
    
    # Create a count table for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      count_table <- as.data.frame(table(data[[var]]))
      colnames(count_table) <- c("Level", "Count")
      count_table$Variable <- var
      count_table[, c("Variable", "Level", "Count")]  # Reorder columns
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Render the table
    datatable(count_summary, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  # Calculate small count warning for Random Forest
  output$small_count_warning_rf_1 <- renderUI({
    req(uploaded_data_rf(), input$predictor_columns_rf)
    
    # Get the uploaded dataset
    df <- uploaded_data_rf()
    
    # Identify factor or binary variables
    factor_or_binary_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Filter based on user selection
    selected_vars <- intersect(factor_or_binary_vars, input$predictor_columns_rf)
    
    # Check if there are no selected categorical predictors
    validate(
      need(length(selected_vars) > 0, "No categorical predictors selected")
    )
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels from the dataset for the analysis or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of categorical variables have sufficient counts for analysis.")
    }
  })
  
  output$small_count_warning_rf_2 <- renderUI({
    req(uploaded_data_rf(), input$predictor_columns_rf)
    
    # Get the uploaded dataset
    df <- uploaded_data_rf()
    
    # Identify factor or binary variables
    factor_or_binary_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Filter based on user selection
    selected_vars <- intersect(factor_or_binary_vars, input$predictor_columns_rf)
    
    # Check if there are no selected categorical predictors
    validate(
      need(length(selected_vars) > 0, "No categorical predictors selected")
    )
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical variable
      count_table <- as.data.frame(table(df[[var]]))
      
      # Rename the columns to be more meaningful
      colnames(count_table) <- c("Level", "Count")
      
      # Add the variable name as a new column and reorder the columns
      count_table$Variable <- var
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels from the dataset for the analysis or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of categorical variables have sufficient counts for analysis.")
    }
  })
  
  output$correlation_matrix_plot_rf <- renderPlotly({
    req(uploaded_data_rf(), input$predictor_columns_rf, input$target_column_rf)
    
    # Get the uploaded dataset
    df <- uploaded_data_rf()
    
    # Identify numeric variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    # Include the outcome variable in selected variables
    outcome_var <- input$target_column_rf
    selected_numeric_vars <- intersect(numeric_vars, input$predictor_columns_rf)
    if (outcome_var %in% numeric_vars) {
      selected_numeric_vars <- unique(c(selected_numeric_vars, outcome_var))
    }
    
    # Create correlation matrix
    numeric_data <- df[selected_numeric_vars]
    correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    # Calculate p-values for significance tests
    p_matrix <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
    for (i in 1:ncol(numeric_data)) {
      for (j in 1:ncol(numeric_data)) {
        if (i != j) {
          test <- cor.test(numeric_data[[i]], numeric_data[[j]], use = "pairwise.complete.obs")
          p_matrix[i, j] <- test$p.value
        } else {
          p_matrix[i, j] <- NA
        }
      }
    }
    
    # Create significance stars based on p-values
    significance_stars <- apply(p_matrix, c(1, 2), function(p) {
      if (is.na(p)) {
        return("")
      } else if (p < 0.001) {
        return("***")
      } else if (p < 0.01) {
        return("**")
      } else if (p < 0.05) {
        return("*")
      } else {
        return("")
      }
    })
    
    # Combine correlation values and stars for annotations
    annotation_text <- matrix(
      paste0(
        round(correlation_matrix, 2),
        " ",
        significance_stars
      ),
      nrow = nrow(correlation_matrix),
      ncol = ncol(correlation_matrix)
    )
    
    # Create heatmap with annotations
    correlation_plot <- plot_ly(
      x = colnames(correlation_matrix),
      y = rownames(correlation_matrix),
      z = correlation_matrix,
      type = "heatmap",
      colorscale = "YlGnBu",
      zmin = -1,
      zmax = 1
    ) %>%
      add_annotations(
        x = rep(colnames(correlation_matrix), each = nrow(correlation_matrix)),
        y = rep(rownames(correlation_matrix), times = ncol(correlation_matrix)),
        text = as.vector(annotation_text),
        showarrow = FALSE,
        font = list(color = "black", size = 10)
      ) %>%
      layout(
        xaxis = list(tickangle = 45),
        yaxis = list(title = "")
      )%>%
      config(displayModeBar = FALSE)
    
    return(correlation_plot)
  })
  
  #-----------------------------------------------------------------------------#
  # Model Summary
  #-----------------------------------------------------------------------------#
  
  observe({
    req(input$predictor_columns_rf)
    updateNumericInput(session, "mtry_rf", value = floor(sqrt(length(input$predictor_columns_rf))), max = length(input$predictor_columns_rf))
  })
  
  # Reactive value to track training status for Random Forest
  training_status_rf <- reactiveVal("Idle")
  
  observeEvent(input$run_model_rf, {
    req(input$target_column_rf, input$predictor_columns_rf)
    data <- uploaded_data_rf()
    data[[input$target_column_rf]] <- as.factor(data[[input$target_column_rf]])
    split <- split_data(data, input$split_ratio_rf)
    
    # Start training
    training_status_rf("Training in progress...")
    
    # Random Forest model
    formula <- as.formula(paste(input$target_column_rf, "~", paste(input$predictor_columns_rf, collapse = "+")))
    ntree <- input$ntree_rf
    mtry <- if (is.null(input$mtry_rf)) floor(sqrt(length(input$predictor_columns_rf))) else input$mtry_rf
    nodesize <- input$nodesize_rf
    
    model <- randomForest(formula, data = split$train, ntree = ntree, mtry = mtry, nodesize = nodesize)
    
    # Evaluate model (simulate some delay if needed to mimic training time)
    Sys.sleep(2)  # Optional: Simulate a delay for the training
    
    # Update status to "Training complete"
    training_status_rf("Training complete!")
  })
  
  # Render the training status message
  output$training_status_rf <- renderText({
    training_status_rf()
  })
  
  model_results_rf <- eventReactive(input$run_model_rf, {
    req(input$target_column_rf, input$predictor_columns_rf)
    data <- uploaded_data_rf()
    data[[input$target_column_rf]] <- as.factor(data[[input$target_column_rf]])
    split <- split_data(data, input$split_ratio_rf)
    formula <- as.formula(paste(input$target_column_rf, "~", paste(input$predictor_columns_rf, collapse = "+")))
    
    # Use user-defined hyperparameters
    ntree <- input$ntree_rf
    mtry <- if (is.null(input$mtry_rf)) floor(sqrt(length(input$predictor_columns_rf))) else input$mtry_rf
    nodesize <- input$nodesize_rf
    
    model <- randomForest(formula, data = split$train, ntree = ntree, mtry = mtry, nodesize = nodesize)
    predictions <- predict(model, newdata = split$test)
    predictions <- factor(predictions, levels = levels(split$test[[input$target_column_rf]]))
    metrics <- calculate_metrics(split$test[[input$target_column_rf]], predictions)
    list(model = model, test_data = split$test, metrics = metrics, ntree = ntree, mtry = mtry, nodesize = nodesize)
  })
  
  output$model_summary_rf <- renderPrint({
    req(model_results_rf())
    model <- model_results_rf()$model
    cat("Random Forest Model Summary\n")
    cat("===========================\n")
    cat(sprintf("Number of Trees (ntree): %d\n", model_results_rf()$ntree))
    cat(sprintf("Number of Variables at Each Split (mtry): %d\n", model_results_rf()$mtry))
    cat(sprintf("Minimum Node Size (nodesize): %d\n\n", model_results_rf()$nodesize))
    print(model)
  })
  
  #-----------------------------------------------------------------------------#
  # Model Performance
  #-----------------------------------------------------------------------------#
  
  output$performance_metrics_rf <- renderPrint({
    req(model_results_rf())
    metrics <- model_results_rf()$metrics
    cat("Confusion Matrix:\n")
    print(metrics$confusion)
    cat(sprintf("\nAccuracy: %.2f\nPrecision: %.2f\nRecall: %.2f\nF1 Score: %.2f", 
                metrics$accuracy, metrics$precision, metrics$recall, metrics$f1))
  })
  
  #-----------------------------------------------------------------------------#
  # Feature Importance
  #-----------------------------------------------------------------------------#
  
  output$importance_plot_rf <- renderPlot({
    req(model_results_rf())
    varImpPlot(model_results_rf()$model, main = "Feature Importance")
  })
  
  #-----------------------------------------------------------------------------#
  # Risk Prediction (Pie chart and histogram)
  #-----------------------------------------------------------------------------#
  
  # Reactive value to track if the model has been run
  model_status_rf <- reactiveValues(ran = FALSE)
  
  # Update the flag when the model is run
  observeEvent(input$run_model_rf, {
    req(model_results_rf())  # Ensure the model runs successfully
    model_status_rf$ran <- TRUE
  })
  
  # Predictions
  predictions_rf <- reactive({
    req(model_status_rf$ran)  # Require that the model has been run
    req(model_results_rf(), uploaded_data_rf(), input$target_column_rf)
    
    # Retrieve the entire dataset
    full_data <- uploaded_data_rf()
    
    # Ensure target column exists and contains at least one "0"
    req(input$target_column_rf %in% names(full_data))
    req(any(full_data[[input$target_column_rf]] == 0))
    
    # Filter to include only remaining employees (target column == 0)
    filtered_data <- full_data[full_data[[input$target_column_rf]] == 0, ]
    
    # Predicted probabilities for remaining employees
    filtered_data$risk_score <- predict(model_results_rf()$model, newdata = filtered_data, type = "prob")[, 2]
    
    # Categorize based on the threshold
    filtered_data$risk_category <- ifelse(
      filtered_data$risk_score > input$risk_threshold_rf,
      "Predicted At Risk",
      "Predicted Not At Risk"
    )
    
    # Retain all columns from the original dataset along with the added columns
    filtered_data
  })
  
  output$risk_histogram_rf <- renderPlotly({
    req(predictions_rf(), input$risk_threshold_rf)
    
    preds <- predictions_rf()
    
    plot_ly(
      data = preds,
      x = ~risk_score,
      color = ~risk_category,
      type = "histogram",
      nbinsx = 20,
      alpha = 0.7,
      colors = c("#FFB74D", "#64B5F6")
    ) %>%
      layout(
        title = "Risk Score Distribution (Remaining Employees)",
        xaxis = list(title = "Risk Score"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Risk Category"))
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$risk_piechart_rf <- renderPlotly({
    req(predictions_rf(), input$risk_threshold_rf)
    
    preds <- predictions_rf()
    
    # Group by risk category
    risk_summary <- preds %>%
      group_by(risk_category) %>%
      dplyr::summarise(Count = dplyr::n())
    
    plot_ly(
      data = risk_summary,
      labels = ~risk_category,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#FFB74D", "#64B5F6"))
    ) %>%
      layout(
        title = "Proportion of Remaining Employees Predicted as 'At Risk'",
        showlegend = TRUE,
        margin = list(t = 100)  # Increase the top margin to create more space
      )%>%
      config(displayModeBar = FALSE)
  })
  
  # In your server function
  observe({
    if (model_status_rf$ran) {
      shinyjs::enable("download_predictions_rf")  # Enable the button when the model has run
    } else {
      shinyjs::disable("download_predictions_rf")  # Disable the button otherwise
    }
  })
  
  
  
  # Download Filtered Predictions
  output$download_predictions_rf <- downloadHandler(
    filename = function() {
      paste0("predicted_risks_rf_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!model_status_rf$ran) {
        showNotification("Error: Please run the model first.", type = "error")
        stop("Model has not been run.")  # Prevent further execution
      }
      req(predictions_rf())
      write.csv(predictions_rf(), file, row.names = FALSE)
    }
  )
  
  #-----------------------------------------------------------------------------#
  # What-if simulation
  #-----------------------------------------------------------------------------#  
  
  # Reactive expression for filtered dataset (Turnover == 0)
  filtered_data_rf <- reactive({
    req(uploaded_data_rf(), input$target_column_rf)
    data <- uploaded_data_rf()
    turnover_column <- input$target_column_rf
    
    # Filter dataset for turnover == 0
    data[data[[turnover_column]] == 0, ]
  })
  
  # Reactive to filter non-binary numeric predictors
  numeric_predictors_rf <- reactive({
    req(filtered_data_rf(), input$predictor_columns_rf)
    data <- filtered_data_rf()
    
    # Identify numeric predictors that are not binary
    numeric_predictors <- input$predictor_columns_rf[sapply(data[input$predictor_columns_rf], is.numeric)]
    non_binary_predictors <- numeric_predictors[sapply(data[numeric_predictors], function(x) length(unique(x)) > 2)]
    
    non_binary_predictors
  })
  
  # Dynamic UI for adjusting numeric predictors
  output$adjust_numeric_inputs_rf <- renderUI({
    req(filtered_data_rf(), numeric_predictors_rf())
    data <- filtered_data_rf()
    predictors <- numeric_predictors_rf()
    
    # Create sliders for non-binary predictors
    lapply(predictors, function(predictor) {
      predictor_min <- min(data[[predictor]], na.rm = TRUE)
      predictor_max <- max(data[[predictor]], na.rm = TRUE)
      adjustment_range <- predictor_max - predictor_min
      step_size <- adjustment_range / 20
      
      sliderInput(
        inputId = paste0("adjust_", predictor),
        label = paste("Adjustment for", predictor),
        min = -adjustment_range,
        max = adjustment_range,
        step = step_size,
        value = 0
      )
    })
  })
  
  # EventReactive to calculate adjusted probabilities
  adjusted_predictions_rf <- eventReactive(input$simulate_adjustments_rf, {
    req(filtered_data_rf(), numeric_predictors_rf(), model_results_rf())
    
    # Use the filtered dataset
    data <- filtered_data_rf()
    adjusted_data <- data
    
    for (predictor in numeric_predictors_rf()) {
      adjustment <- input[[paste0("adjust_", predictor)]]
      if (!is.null(adjustment)) {
        adjusted_values <- adjusted_data[[predictor]] + adjustment
        
        # Enforce bounds within the original range of the predictor
        predictor_min <- min(data[[predictor]], na.rm = TRUE)
        predictor_max <- max(data[[predictor]], na.rm = TRUE)
        adjusted_data[[predictor]] <- pmax(pmin(adjusted_values, predictor_max), predictor_min)
      }
    }
    
    # Calculate probabilities for original and adjusted data
    original_probs <- predict(model_results_rf()$model, newdata = data, type = "prob")[, 2]
    adjusted_probs <- predict(model_results_rf()$model, newdata = adjusted_data, type = "prob")[, 2]
    
    list(
      original_data = data,
      adjusted_data = adjusted_data,
      probabilities = data.frame(
        Original_Probabilities = original_probs,
        Adjusted_Probabilities = adjusted_probs
      )
    )
  })
  
  # Render histogram to compare probabilities
  output$adjusted_simulation_histogram_rf <- renderPlotly({
    req(adjusted_predictions_rf())
    preds <- adjusted_predictions_rf()$probabilities
    
    plot_ly() %>%
      add_histogram(
        x = ~preds$Original_Probabilities,
        name = "Original Probabilities",
        opacity = 0.6
      ) %>%
      add_histogram(
        x = ~preds$Adjusted_Probabilities,
        name = "Adjusted Probabilities",
        opacity = 0.6
      ) %>%
      layout(
        title = "Comparison of Predicted Probabilities (Original vs Adjusted)",
        xaxis = list(title = "Predicted Probability"),
        yaxis = list(title = "Count"),
        barmode = "overlay",
        legend = list(title = list(text = "Probability Type"))
      )%>%
      config(displayModeBar = FALSE)
  })
  
  # Render the original data table (Filtered dataset)
  output$original_data_table_rf <- renderDT({
    req(filtered_data_rf())
    datatable(filtered_data_rf(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Render the adjusted data table
  output$adjusted_data_table_rf <- renderDT({
    req(adjusted_predictions_rf())
    datatable(adjusted_predictions_rf()$adjusted_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  # Multilayer Perceptron
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Dataset Preview
  #-----------------------------------------------------------------------------#
  
  # Reactive values to store training data and metrics
  data <- reactiveVal(NULL)
  metrics <- reactiveValues(loss = NULL, val_loss = NULL, epochs = NULL, status = "Idle", evaluation = NULL, confusion = NULL, predictions = NULL)
  
  # Load the uploaded data or default data
  observe({
    if (is.null(input$datafile)) {
      # Use default dataset if no file is uploaded
      df <- default_data
    } else {
      # Use the uploaded file
      req(input$datafile)
      df <- read.csv(input$datafile$datapath)
    }
    
    # Automatically convert character/string variables to factors
    df <- df %>%
      mutate(across(where(is.character), as.factor))
    
    # Store the modified dataset
    data(df)
  })
  
  
  # Create UI selectors for outcome and features
  output$outcome_selector <- renderUI({
    req(data())
    
    # Preselect "left" if it exists, otherwise default to the first column
    default_outcome <- if ("left" %in% names(data())) "left" else names(data())[1]
    
    selectInput(
      inputId = "outcome_var",
      label = "Select Turnover Column",
      choices = names(data()),
      selected = input$outcome_var %||% default_outcome  # Keep the user selection, default to "left"
    )
  })
  
  # Render the features UI dynamically
  output$features_selector <- renderUI({
    req(data(), input$outcome_var)
    
    # Exclude the selected outcome column from the feature choices
    excluded_column <- input$outcome_var
    available_features <- setdiff(names(data()), excluded_column)
    
    checkboxGroupInput(
      inputId = "feature_vars",
      label = "Select Feature Variables",
      choices = available_features,
      selected = available_features # Default to all features except the outcome column
    )
  })
  
  # Observe to handle dynamic default selections on dataset load
  observe({
    req(data())
    df <- data()
    
    # Only update if outcome_var is NULL or empty (avoid forcing when user selects a different target column)
    if (is.null(input$outcome_var) || input$outcome_var == "") {
      if ("left" %in% names(df)) {
        updateSelectInput(
          session,
          "outcome_var",
          selected = "left"
        )
      } else {
        # If "left" is not available, select the first column
        updateSelectInput(
          session,
          "outcome_var",
          selected = names(df)[1]
        )
      }
    }
    
    # Update feature selection to exclude the outcome column
    excluded_column <- input$outcome_var
    available_features <- setdiff(names(df), excluded_column)
    updateCheckboxGroupInput(
      session,
      "feature_vars",
      selected = available_features
    )
  })
  
  # Check if the selected outcome variable is binary
  observe({
    req(data(), input$outcome_var)
    
    outcome_column <- input$outcome_var
    df <- data()
    
    # Check if the column has two unique values, and the values are 0 and 1
    if (length(unique(df[[outcome_column]])) != 2 || !all(sort(unique(df[[outcome_column]])) == c(0, 1))) {
      showNotification(
        "Error: The selected turnover column must be binary (exactly two unique values: 0 and 1).",
        type = "error"
      )
    }
  })
  
  
  # Add UI for specifying the training/testing split ratio
  output$split_ratio_selector <- renderUI({
    sliderInput(
      inputId = "split_ratio",
      label = "Training Set Percentage",
      min = 50,
      max = 90,
      value = 80,  # Default split: 80% training, 20% testing
      step = 1
    )
  })
  
  # Dynamically create UI for layers
  output$layers_ui <- renderUI({
    req(input$num_layers)
    
    layers <- lapply(1:input$num_layers, function(i) {
      if (i == 1) {
        # First hidden layer with tooltips
        tagList(
          div(
            style = "display: flex; align-items: center;",
            numericInput(paste0("neurons_layer_", i), paste("Neurons in Layer", i), value = 32, min = 1, max = 512),
            tags$span(
              icon("info-circle"),
              title = "The number of neurons in this layer. More neurons allow capturing more complex patterns but increase computation time.
    \nRule of Thumb: Start with a value between the size of the input and output layers, or use powers of 2 (e.g., 16, 32, 64). Gradually increase if the model underfits, but monitor for overfitting.",
              style = "margin-left: 10px; cursor: help; color: #FFB74D;"
            )
          ),
          div(
            style = "display: flex; align-items: center;",
            selectInput(
              paste0("activation_layer_", i),
              paste("Activation Function for Layer", i),
              choices = c("relu", "sigmoid", "tanh", "softmax", "linear"),
              selected = "relu"
            ),
            tags$span(
              icon("info-circle"),
              title = "Specifies the activation function for this layer. ReLU is a common choice for hidden layers.",
              style = "margin-left: 10px; cursor: help; color: #FFB74D;"
            )
          )
        )
      } else {
        # Subsequent hidden layers without tooltips
        tagList(
          numericInput(paste0("neurons_layer_", i), paste("Neurons in Layer", i), value = 32, min = 1, max = 512),
          selectInput(
            paste0("activation_layer_", i),
            paste("Activation Function for Layer", i),
            choices = c("relu", "sigmoid", "tanh", "softmax", "linear"),
            selected = "relu"
          )
        )
      }
    })
    
    do.call(tagList, layers)
  })
  
  
  output$data_table_mlp <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 5))
  })
  
  outcome_stats_mlp <- reactive({
    req(data(), input$outcome_var)
    df <- data()
    outcome <- input$outcome_var
    
    if (is.numeric(df[[outcome]])) {
      # Ensure the outcome column is binary (0 and 1)
      counts <- table(df[[outcome]])
      if (length(counts) == 2) {
        total <- sum(counts)
        rate <- counts["1"] / total
        list(
          total = total,
          zero_count = counts["0"],
          one_count = counts["1"],
          rate = rate
        )
      } else {
        return(NULL) # Not binary
      }
    } else {
      return(NULL) # Not numeric
    }
  })
  
  output$outcome_stats_box_mlp <- renderUI({
    # Ensure data and outcome_var exist
    if (is.null(data()) || is.null(input$outcome_var)) {
      return(
        div(
          class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
          h4("Outcome Variable Statistics", style = "font-weight: bold;"),
          p("No data or outcome variable selected.")
        )
      )
    }
    
    # Get the statistics
    stats <- outcome_stats_mlp()
    
    if (!is.null(stats)) {
      # Calculate imbalance ratio
      imbalance_warning <- NULL
      if (stats$zero_count > 0 && stats$one_count > 0) {
        imbalance_ratio <- max(stats$zero_count, stats$one_count) / min(stats$zero_count, stats$one_count)
        
        # Create the imbalance warning message if the ratio is above a threshold (e.g., > 5)
        if (imbalance_ratio > 5) {
          imbalance_warning <- HTML(
            "<div style='color: red;'>
                    Warning: The dataset is highly imbalanced (the difference between outcome = 0 and outcome = 1 is too large, with an imbalance ratio greater than 5). This could make the model biased towards predicting the larger class.<br><br>
                    Suggestions:<br>
                    <ul style='margin-left: 20px;'>
                        <li>Increase the number of examples for the smaller class (e.g., oversampling).</li>
                        <li>Reduce the number of examples from the larger class (e.g., undersampling).</li>
                        <li>Use a model that gives more attention to the smaller class by adjusting weights for the classes.</li>
                    </ul>
                    <p style='margin-top: 10px;'>Please note: These solutions must be applied <b>outside</b> the app. Using the app with the current dataset is not advised, as the model may not perform well with such a high imbalance.</p>
                    </div>"
          )
        }
      }
      
      # Render the statistics for a binary outcome
      tagList(
        div(class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
            h4("Outcome Variable Statistics", style = "font-weight: bold;"),
            tags$ul(
              tags$li(sprintf("Total Observations: %d", stats$total), style = "font-size: 14px;"),
              tags$li(sprintf("Number of 0s: %d", stats$zero_count), style = "font-size: 14px;"),
              tags$li(sprintf("Number of 1s: %d", stats$one_count), style = "font-size: 14px;"),
              tags$li(sprintf("Rate of 1s: %.2f%%", stats$rate * 100), style = "font-size: 14px;")
            ),
            # Add imbalance warning if necessary
            imbalance_warning
        )
      )
    } else {
      # Render the error message for invalid outcome variables
      div(
        class = "well", style = "background-color: #f7f7f7; padding: 15px; border-radius: 10px;",
        h4("Outcome Variable Statistics", style = "font-weight: bold;"),
        p("Selected outcome variable is not valid. Ensure it is numeric and binary (0 and 1).")
      )
    }
  })
  
  output$data_type_table_mlp <- renderDT({
    req(data())  # Ensure the MLP dataset is loaded
    
    # Create a summary of the dataset
    df <- data()
    summary_table <- data.frame(
      Variable = names(df),                         # Variable names
      DataType = sapply(df, class),                 # Data types
      UniqueValues = sapply(df, function(x) length(unique(x))),  # Number of unique values
      MissingValues = sapply(df, function(x) sum(is.na(x))),     # Count of missing values
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #-----------------------------------------------------------------------------#
  # Exploring the Data
  #-----------------------------------------------------------------------------#
  # Observe predictor selection
  observe({
    req(data(), input$feature_vars)
    updateSelectInput(
      session,
      "selected_predictor_mlp",
      choices = input$feature_vars,  # Restrict to selected features
      selected = input$feature_vars[1]  # Default to the first selected feature
    )
  })
  
  # Reactive to prepare data for visualization
  predictor_histogram_data_mlp <- reactive({
    req(data(), input$selected_predictor_mlp, input$outcome_var, input$feature_vars)
    
    df <- data()
    predictor <- input$selected_predictor_mlp
    outcome <- input$outcome_var
    
    # Ensure predictor is a selected feature
    if (!predictor %in% input$feature_vars) {
      return(NULL)
    }
    
    # Prepare data for visualization (always group by outcome)
    df %>%
      select(all_of(predictor), all_of(outcome)) %>%
      mutate(Outcome = as.factor(df[[outcome]]))  # Group by outcome
  })
  
  # Render histogram or bar plot
  output$predictor_histogram_mlp <- renderPlotly({
    req(predictor_histogram_data_mlp())
    data <- predictor_histogram_data_mlp()
    
    predictor <- input$selected_predictor_mlp
    
    # Define custom colors for the Outcome groups
    custom_colors <- c("#FFB74D", "#64B5F6")
    
    # Check if the predictor is numeric, categorical, or binary
    predictor_data <- data()[[predictor]]
    is_binary <- length(unique(predictor_data)) == 2
    is_numeric <- is.numeric(predictor_data) && !is_binary
    
    if (is_numeric) {
      # Numeric: Create histogram
      plot_ly(
        data = data,
        x = ~get(predictor),
        color = ~Outcome,
        colors = custom_colors,
        type = "histogram",
        nbinsx = 30,
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "overlay",
          legend = list(title = list(text = "Outcome"))
        )%>%
        config(displayModeBar = FALSE)
    } else {
      # Categorical or binary: Create bar plot
      data_summary <- data %>%
        group_by(.data[[predictor]], Outcome) %>%
        summarize(Count = n(), .groups = "drop")
      
      plot_ly(
        data = data_summary,
        x = ~.data[[predictor]],
        y = ~Count,
        color = ~Outcome,
        colors = custom_colors,
        type = "bar",
        alpha = 0.7
      ) %>%
        layout(
          title = paste("Distribution of", predictor, "Grouped by Turnover"),
          xaxis = list(title = predictor),
          yaxis = list(title = "Count"),
          barmode = "group",
          legend = list(title = list(text = "Outcome"))
        )%>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$numeric_summary_table_mlp <- renderDT({
    req(data(), input$feature_vars)  # Ensure data and selected predictors are available
    
    # Get the dataset
    df <- data()
    
    # Identify numeric variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    # Exclude binary predictors (columns with only 2 unique values)
    non_binary_numeric_vars <- numeric_vars[sapply(df[numeric_vars], function(x) length(unique(x)) > 2)]
    
    # Filter to include only numeric variables selected as predictors
    selected_numeric_vars <- intersect(non_binary_numeric_vars, input$feature_vars)
    
    # Check if any numeric variables are selected as predictors
    if (length(selected_numeric_vars) == 0) {
      return(
        datatable(
          data.frame(
            Message = "No numeric predictor variables selected."
          ),
          options = list(pageLength = 1),
          rownames = FALSE
        )
      )
    }
    
    # Create a summary table for each numeric variable
    summary_table <- data.frame(
      Variable = selected_numeric_vars,
      Mean = round(sapply(df[selected_numeric_vars], mean, na.rm = TRUE), 2),
      SD = round(sapply(df[selected_numeric_vars], sd, na.rm = TRUE), 2),
      Min = round(sapply(df[selected_numeric_vars], min, na.rm = TRUE), 2),
      Q1 = round(sapply(df[selected_numeric_vars], function(x) quantile(x, 0.25, na.rm = TRUE)), 2),
      Median = round(sapply(df[selected_numeric_vars], median, na.rm = TRUE), 2),
      Q3 = round(sapply(df[selected_numeric_vars], function(x) quantile(x, 0.75, na.rm = TRUE)), 2),
      Max = round(sapply(df[selected_numeric_vars], max, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
    
    # Render the table
    datatable(summary_table, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Render the count summary table for categorical variables for MLP
  output$category_count_table_mlp <- renderDT({
    req(data(), input$feature_vars)  # Ensure data and selected predictors are available
    
    df <- data()
    
    # Identify categorical variables (factors or character columns)
    categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Identify binary variables (numeric variables with exactly 2 unique values)
    binary_vars <- names(df)[sapply(df, function(x) is.numeric(x) && length(unique(x)) == 2)]
    
    # Combine categorical and binary variables
    binary_categorical_vars <- union(categorical_vars, binary_vars)
    
    # Filter to include only binary and categorical variables selected as predictors
    selected_vars <- intersect(binary_categorical_vars, input$feature_vars)
    
    # Check if any categorical or binary predictors are selected
    if (length(selected_vars) == 0) {
      return(
        datatable(
          data.frame(
            Message = "No binary or categorical predictor variables selected."
          ),
          options = list(pageLength = 1),
          rownames = FALSE
        )
      )
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical or binary variable
      count_table <- as.data.frame(table(df[[var]], useNA = "ifany"))  # Include NA counts
      colnames(count_table) <- c("Level", "Count")  # Rename columns
      count_table$Variable <- var  # Add variable name
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Render the table with counts for binary and categorical variables
    datatable(count_summary, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  # Calculate small count warning for MLP
  output$small_count_warning_mlp_1 <- renderUI({
    req(data(), input$feature_vars)  # Ensure data and selected predictors are available
    
    df <- data()
    
    # Identify categorical variables (factors or character columns)
    categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Identify binary variables (numeric variables with exactly 2 unique values)
    binary_vars <- names(df)[sapply(df, function(x) is.numeric(x) && length(unique(x)) == 2)]
    
    # Combine categorical and binary variables
    binary_categorical_vars <- union(categorical_vars, binary_vars)
    
    # Filter to include only binary and categorical variables selected as predictors
    selected_vars <- intersect(binary_categorical_vars, input$feature_vars)
    
    # Check if there are no selected binary or categorical predictors
    if (length(selected_vars) == 0) {
      return(
        tags$p(style = "color: orange;", "No binary or categorical predictor variables selected. Please select valid variables for analysis.")
      )
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical or binary variable
      count_table <- as.data.frame(table(df[[var]], useNA = "ifany"))  # Include NA counts
      colnames(count_table) <- c("Level", "Count")  # Rename columns
      count_table$Variable <- var  # Add variable name
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels from the dataset for the analysis or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of binary and categorical variables have sufficient counts for analysis.")
    }
  })
  
  output$small_count_warning_mlp_2 <- renderUI({
    req(data(), input$feature_vars)  # Ensure data and selected predictors are available
    
    df <- data()
    
    # Identify categorical variables (factors or character columns)
    categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    # Identify binary variables (numeric variables with exactly 2 unique values)
    binary_vars <- names(df)[sapply(df, function(x) is.numeric(x) && length(unique(x)) == 2)]
    
    # Combine categorical and binary variables
    binary_categorical_vars <- union(categorical_vars, binary_vars)
    
    # Filter to include only binary and categorical variables selected as predictors
    selected_vars <- intersect(binary_categorical_vars, input$feature_vars)
    
    # Check if there are no selected binary or categorical predictors
    if (length(selected_vars) == 0) {
      return(
        tags$p(style = "color: orange;", "No binary or categorical predictor variables selected. Please select valid variables for analysis.")
      )
    }
    
    # Create a list to store tables of counts for each selected variable
    count_tables <- lapply(selected_vars, function(var) {
      # Create a count table for each categorical or binary variable
      count_table <- as.data.frame(table(df[[var]], useNA = "ifany"))  # Include NA counts
      colnames(count_table) <- c("Level", "Count")  # Rename columns
      count_table$Variable <- var  # Add variable name
      count_table <- count_table[, c("Variable", "Level", "Count")]  # Reorder columns
      count_table  # Return the count table
    })
    
    # Combine all the count tables into a single data frame
    count_summary <- do.call(rbind, count_tables)
    
    # Identify levels with small counts (e.g., fewer than 20 counts per level)
    small_count_warning <- ""
    small_count_rows <- count_summary$Count < 20  # Adjust this threshold as needed
    if (any(small_count_rows)) {
      small_count_warning <- "Warning: Some levels have very small counts (less than 20 occurrences). Analysis involving these levels may not be meaningful. Consider removing these levels from the dataset for the analysis or combining them into higher-level categories and re-uploading the updated dataset."
    }
    
    # If there's a warning, display it
    if (small_count_warning != "") {
      tags$p(style = "color: red;", small_count_warning)
    } else {
      # No warning if all counts are sufficiently large
      tags$p(style = "color: green;", "All levels of binary and categorical variables have sufficient counts for analysis.")
    }
  })
  
  
  output$correlation_matrix_plot_mlp <- renderPlotly({
    req(data(), input$feature_vars, input$outcome_var)  # Ensure data, predictors, and outcome are available
    
    df <- data()
    
    # Identify numeric variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    # Ensure the outcome variable is included if it is numeric
    outcome_var <- input$outcome_var
    if (outcome_var %in% numeric_vars) {
      selected_numeric_vars <- union(intersect(numeric_vars, input$feature_vars), outcome_var)
    } else {
      selected_numeric_vars <- intersect(numeric_vars, input$feature_vars)
    }
    
    # Check for sufficient numeric variables
    if (length(selected_numeric_vars) < 2) {
      output$error_message_mlp <- renderUI({
        tags$p(style = "color: red;", "Error: Not enough numeric variables for analysis. At least two numeric variables (including the outcome) are required.")
      })
      return(NULL)
    }
    
    # Validate the outcome variable
    if (outcome_var %in% selected_numeric_vars) {
      outcome_values <- unique(df[[outcome_var]])
      if (length(outcome_values) != 2 || !all(sort(outcome_values) == c(0, 1))) {
        output$error_message_mlp <- renderUI({
          tags$p(style = "color: red;", "Error: The selected outcome variable must be binary (exactly two unique values: 0 and 1).")
        })
        return(NULL)
      }
    } else {
      output$error_message_mlp <- renderUI({
        tags$p(style = "color: red;", "Error: The selected outcome variable is not numeric and cannot be included in the analysis.")
      })
      return(NULL)
    }
    
    # Clear any previous error message
    output$error_message_mlp <- renderUI({
      NULL
    })
    
    # Create correlation matrix
    numeric_data <- df[selected_numeric_vars]
    correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    # Calculate p-values for significance tests
    p_matrix <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
    for (i in 1:ncol(numeric_data)) {
      for (j in 1:ncol(numeric_data)) {
        if (i != j) {
          test <- cor.test(numeric_data[[i]], numeric_data[[j]], use = "pairwise.complete.obs")
          p_matrix[i, j] <- test$p.value
        } else {
          p_matrix[i, j] <- NA
        }
      }
    }
    
    # Create significance stars based on p-values
    significance_stars <- apply(p_matrix, c(1, 2), function(p) {
      if (is.na(p)) {
        return("")
      } else if (p < 0.001) {
        return("***")
      } else if (p < 0.01) {
        return("**")
      } else if (p < 0.05) {
        return("*")
      } else {
        return("")
      }
    })
    
    # Combine correlation values and stars for annotations
    annotation_text <- matrix(
      paste0(
        round(correlation_matrix, 2),
        " ",
        significance_stars
      ),
      nrow = nrow(correlation_matrix),
      ncol = ncol(correlation_matrix)
    )
    
    # Create heatmap with annotations
    correlation_plot <- plot_ly(
      x = colnames(correlation_matrix),
      y = rownames(correlation_matrix),
      z = correlation_matrix,
      type = "heatmap",
      colorscale = "YlGnBu",
      zmin = -1,
      zmax = 1
    ) %>%
      add_annotations(
        x = rep(colnames(correlation_matrix), each = nrow(correlation_matrix)),
        y = rep(rownames(correlation_matrix), times = ncol(correlation_matrix)),
        text = as.vector(annotation_text),
        showarrow = FALSE,
        font = list(color = "black", size = 10)
      ) %>%
      layout(
        xaxis = list(tickangle = 45),
        yaxis = list(title = "")
      )%>%
      config(displayModeBar = FALSE)
    
    return(correlation_plot)
  })

  #-----------------------------------------------------------------------------#
  # Training Progress
  #-----------------------------------------------------------------------------#
  
  # Train the model
  observeEvent(input$start_training, {
    req(data(), input$outcome_var, input$feature_vars, input$split_ratio)  # Include split ratio in requirements
    
    # Reset metrics
    metrics$status <- "Training in progress..."
    metrics$loss <- NULL
    metrics$val_loss <- NULL
    metrics$accuracy <- NULL
    metrics$val_accuracy <- NULL
    metrics$epochs <- NULL
    
    df <- data()
    outcome <- input$outcome_var
    features <- input$feature_vars
    
    # Prepare outcome variable
    y <- as.numeric(df[[outcome]])
    if (length(unique(y)) > 2) {
      stop("The outcome variable must be binary for this model.")
    }
    
    # Prepare feature variables with proper encoding
    X <- df %>%
      select(all_of(features)) %>%
      mutate(across(where(is.character), as.factor)) %>%  # Convert character to factor
      model.matrix(~ . - 1, .) %>%  # One-hot encode factors
      as.matrix()  # Ensure it's a numeric matrix
    
    # Ensure all columns are numeric
    if (!all(sapply(X, is.numeric))) {
      stop("Some columns are not numeric after encoding. Please check your data.")
    }
    
    # Normalize numeric features
    X <- scale(X)
    
    metrics$scaling <- attr(X, "scaled:center")
    metrics$scale <- attr(X, "scaled:scale")
    
    # Split data into training, validation, and test sets
    set.seed(123)
    train_ratio <- input$split_ratio / 100
    val_ratio <- 0.2  # Specify a portion of training data for validation
    train_indices <- sample(seq_len(nrow(X)), size = train_ratio * nrow(X))
    train_val_split <- floor(length(train_indices) * (1 - val_ratio))
    
    X_train <- X[train_indices[1:train_val_split], ]
    y_train <- y[train_indices[1:train_val_split]]
    X_val <- X[train_indices[(train_val_split + 1):length(train_indices)], ]
    y_val <- y[train_indices[(train_val_split + 1):length(train_indices)]]
    X_test <- X[-train_indices, ]
    y_test <- y[-train_indices]
    
    # Dynamically build the MLP model
    model <- keras_model_sequential()
    model %>% layer_dense(units = input[["neurons_layer_1"]], activation = input[["activation_layer_1"]], input_shape = ncol(X))
    
    # Add additional layers dynamically if specified
    if (input$num_layers > 1) {
      for (i in 2:input$num_layers) {
        model %>% layer_dense(units = input[[paste0("neurons_layer_", i)]], activation = input[[paste0("activation_layer_", i)]])
      }
    }
    
    # Output layer with sigmoid activation for binary classification
    model %>% layer_dense(units = 1, activation = "sigmoid")  
    
    # Compile the model with binary crossentropy loss and Adam optimizer
    model %>% compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_adam(),
      metrics = c("accuracy")
    )
    
    # Train the model with a callback to capture metrics
    model %>% fit(
      X_train, y_train,
      epochs = input$epochs,
      verbose = 0,
      validation_data = list(X_val, y_val),
      callbacks = list(
        callback_lambda(
          on_epoch_end = function(epoch, logs) {
            print(logs)  # Debugging output
            metrics$loss <- c(metrics$loss, logs$loss)
            metrics$val_loss <- c(metrics$val_loss, logs$val_loss)
            metrics$accuracy <- c(metrics$accuracy, logs$accuracy)
            metrics$val_accuracy <- c(metrics$val_accuracy, logs$val_accuracy)
            metrics$epochs <- c(metrics$epochs, epoch)
          }
        )
      )
    )
    
    # Update status to "Training complete"
    model$save("trained_model.keras")
    
    metrics$model <- model
    metrics$columns <- colnames(X)
    
    # Evaluate the model
    y_pred <- model %>% predict(X_test)
    y_pred_class <- ifelse(y_pred > 0.5, 1, 0)  # Binary classification threshold
    
    # Compute confusion matrix and evaluation metrics
    confusion <- confusionMatrix(factor(y_pred_class), factor(y_test))
    metrics$evaluation <- paste0(
      "Accuracy: ", round(confusion$overall["Accuracy"], 3),
      "\nPrecision: ", round(confusion$byClass["Pos Pred Value"], 3),
      "\nRecall: ", round(confusion$byClass["Sensitivity"], 3),
      "\nF1 Score: ", round(2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
                              (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"]), 3)
    )
    
    # Capture confusion matrix output
    metrics$confusion <- capture.output(print(confusion))    
    
    # Predict risk for the entire dataset and add predictions to the original data
    full_predictions <- model %>% predict(X)
    df$Risk <- ifelse(full_predictions > 0.5, "At Risk", "Not At Risk")
    df$Probability <- round(full_predictions, 3)  # Add probabilities
    metrics$predictions <- df
    
    # Update status to indicate training completion
    metrics$status <- "Training complete!"
  })
  
  #-----------------------------------------------------------------------------#
  # Evaluation Metrics
  #-----------------------------------------------------------------------------#
  
  # Render combined interactive plot
  output$combined_plot <- renderPlotly({
    cat("RenderPlotly executed\n")
    
    # Ensure all metrics are available
    req(metrics$loss, metrics$val_loss, metrics$accuracy, metrics$val_accuracy, metrics$epochs)
    
    # Create a data frame for the metrics
    data <- data.frame(
      Epoch = metrics$epochs,  # Epoch numbers
      Training_Loss = metrics$loss,
      Validation_Loss = metrics$val_loss,
      Training_Accuracy = metrics$accuracy,
      Validation_Accuracy = metrics$val_accuracy
    )
    
    cat("Data frame created:\n")
    print(data)  # Debugging output
    
    # Create the interactive plot with Plotly
    plot <- plot_ly(data, x = ~Epoch) %>%
      add_lines(y = ~Training_Loss, name = "Training Loss", line = list(color = "blue")) %>%
      add_lines(y = ~Validation_Loss, name = "Validation Loss", line = list(color = "red")) %>%
      add_lines(y = ~Training_Accuracy, name = "Training Accuracy", line = list(color = "green")) %>%
      add_lines(y = ~Validation_Accuracy, name = "Validation Accuracy", line = list(color = "orange")) %>%
      layout(
        title = list(
          text = "Training and Validation Metrics Over Epochs",
          x = 0.5  # Center-align the title
        ),
        xaxis = list(title = "Epoch"),
        yaxis = list(title = "Metrics"),
        legend = list(
          orientation = "v",  # Vertical legend
          x = 1.05,           # Position slightly outside the plot area on the right
          y = 0.5,            # Center the legend vertically
          xanchor = "left",   # Align the left side of the legend to the specified `x` value
          yanchor = "middle"  # Align the center of the legend to the specified `y` value
        )
      )%>%
      config(displayModeBar = FALSE)
    
    return(plot)
  })
  
  # Render the evaluation metrics
  output$evaluation_metrics <- renderText({
    req(metrics$evaluation)
    metrics$evaluation
  })
  
  # Render the confusion matrix
  output$confusion_matrix <- renderText({
    req(metrics$confusion)
    paste(metrics$confusion, collapse = "\n")
  })
  
  # Render the training status
  output$training_status <- renderText({
    metrics$status
  })
  
  #-----------------------------------------------------------------------------#
  # Risk Prediction
  #-----------------------------------------------------------------------------#
  
  # Reactive value to track if the MLP model has been run
  model_status_mlp <- reactiveValues(ran = FALSE)
  
  # Update the status when the MLP model is run
  observeEvent(input$start_training, {
    # Simulate MLP model training (replace with actual training logic)
    model_status_mlp$ran <- TRUE
    output$training_status_mlp <- renderText("Training Complete!")
  })
  
  # Dynamically enable or disable the download button
  observe({
    if (model_status_mlp$ran) {
      shinyjs::enable("download_predictions_mlp")
    } else {
      shinyjs::disable("download_predictions_mlp")
    }
  })
  
  # Predictions for MLP
  predictions_mlp <- reactive({
    req(model_status_mlp$ran)  # Require that the model has been run
    req(metrics$predictions, input$risk_threshold_mlp, input$outcome_var)
    
    # Original predictions
    df <- metrics$predictions
    
    # Ensure selected outcome column exists
    outcome_col <- input$outcome_var
    validate(need(outcome_col %in% names(df), "Outcome column not found in the dataset."))
    
    # Filter rows where outcome variable == 0
    df <- df[df[[outcome_col]] == 0, , drop = FALSE]
    
    # Ensure Probability column exists
    if ("V1" %in% names(df)) {
      colnames(df)[colnames(df) == "V1"] <- "Probability"
    }
    df$Probability <- as.numeric(df$Probability)
    df <- df[!is.na(df$Probability), ]
    
    # Add risk category based on threshold
    df$risk_category <- factor(
      ifelse(df$Probability > input$risk_threshold_mlp, "Predicted At Risk", "Predicted Not At Risk"),
      levels = c("Predicted Not At Risk", "Predicted At Risk")
    )
    
    # Retain all columns from the original dataset
    df
  })
  
  output$risk_piechart_mlp <- renderPlotly({
    req(predictions_mlp())
    
    preds <- predictions_mlp()
    
    # Proceed with plotting
    risk_summary <- preds %>%
      group_by(risk_category) %>%
      summarise(Count = n())
    
    plot_ly(
      data = risk_summary,
      labels = ~risk_category,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#64B5F6", "#FFB74D"))  # Fixed colors: Blue, Yellow
    ) %>%
      layout(
        title = "Proportion of Predicted Risk",
        showlegend = TRUE,
        margin = list(t = 100)
      )%>%
      config(displayModeBar = FALSE)
    
  })
  
  output$risk_histogram_mlp <- renderPlotly({
    req(predictions_mlp())
    
    preds <- predictions_mlp()
    
    plot_ly(
      data = preds,
      x = ~Probability,
      color = ~risk_category,
      colors = c("Predicted Not At Risk" = "#64B5F6", "Predicted At Risk" = "#FFB74D"),
      type = "histogram",
      nbinsx = 30,
      opacity = 0.9,
      histnorm = "count"
    ) %>%
      layout(
        title = "Risk Score Distribution (Filtered Employees: Outcome = 0)",
        xaxis = list(title = "Risk Score", zeroline = FALSE),
        yaxis = list(title = "Count"),
        barmode = "overlay",
        legend = list(title = list(text = "Risk Category"))
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$summary_preds_mlp <- renderPrint({
    req(predictions_mlp())
    
    preds <- predictions_mlp()
    summary <- preds %>%
      group_by(risk_category) %>%
      summarise(Count = n())
    
    print(summary)
  })
  
  # Download Handler for MLP Predictions
  output$download_predictions_mlp <- downloadHandler(
    filename = function() {
      paste0("predicted_risks_mlp_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(predictions_mlp())
      write.csv(predictions_mlp(), file, row.names = FALSE)
    }
  )
  
  #-----------------------------------------------------------------------------#
  # What-if Simulator
  #-----------------------------------------------------------------------------#
  
  # Reactive for numeric predictors
  numeric_predictors_mlp <- reactive({
    req(data(), input$feature_vars)
    features <- input$feature_vars
    numeric_predictors <- features[sapply(data()[features], is.numeric)]
    
    # Debug: Check numeric predictors
    print("Numeric Predictors Identified:")
    print(numeric_predictors)
    
    numeric_predictors
  })
  
  
  # Dynamic UI for sliders
  output$adjust_numeric_inputs_mlp <- renderUI({
    req(data(), numeric_predictors_mlp())
    df <- data()
    predictors <- numeric_predictors_mlp()
    
    # Debug: Check predictor ranges
    print("Numeric Predictors Ranges:")
    print(sapply(predictors, function(p) range(df[[p]], na.rm = TRUE)))
    
    lapply(predictors, function(predictor) {
      predictor_min <- min(df[[predictor]], na.rm = TRUE)
      predictor_max <- max(df[[predictor]], na.rm = TRUE)
      adjustment_range <- predictor_max - predictor_min
      step_size <- adjustment_range / 20
      
      sliderInput(
        inputId = paste0("adjust_", predictor),
        label = paste("Adjustment for", predictor),
        min = -adjustment_range,
        max = adjustment_range,
        step = step_size,
        value = 0
      )
    })
  })
  
  
  
  # Function to rename column if needed
  rename_v1_column <- function(df, new_col_name) {
    if ("V1" %in% names(df)) {
      colnames(df)[colnames(df) == "V1"] <- new_col_name
    }
    df
  }
  
  # Adjusted predictions based on slider inputs
  adjusted_predictions_mlp <- eventReactive(input$simulate_adjustments_mlp, {
    req(data(), numeric_predictors_mlp(), metrics$model, metrics$predictions)
    
    adjusted_data <- metrics$predictions
    predictors <- numeric_predictors_mlp()
    
    # Apply adjustments
    for (predictor in predictors) {
      adjustment <- input[[paste0("adjust_", predictor)]]
      if (!is.null(adjustment)) {
        adjusted_values <- adjusted_data[[predictor]] + adjustment
        predictor_min <- min(data()[[predictor]], na.rm = TRUE)
        predictor_max <- max(data()[[predictor]], na.rm = TRUE)
        adjusted_data[[predictor]] <- pmin(pmax(adjusted_values, predictor_min), predictor_max)
      }
    }
    
    # Ensure missing columns are added with zeros
    missing_cols <- setdiff(metrics$columns, colnames(adjusted_data))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        adjusted_data[[col]] <- 0
      }
    }
    
    # One-hot encode and scale adjusted data
    X_adjusted <- adjusted_data %>%
      select(all_of(metrics$columns)) %>%
      mutate(across(where(is.character), as.factor)) %>%
      model.matrix(~ . - 1, .) %>%
      scale(center = metrics$scaling, scale = metrics$scale)
    
    # Predict adjusted probabilities
    adjusted_probs <- metrics$model %>% predict(X_adjusted)
    adjusted_data$Adjusted_Probability <- round(as.vector(adjusted_probs), 3)
    
    # Debugging Outputs
    print("Adjusted Data After Processing:")
    print(head(adjusted_data))
    print("Adjusted Probabilities:")
    print(head(adjusted_probs))
    
    list(
      original_data = metrics$predictions,
      adjusted_data = adjusted_data
    )
  })
  
  
  
  
  
  
  # Histogram for comparing original and adjusted probabilities
  output$adjusted_simulation_histogram_mlp <- renderPlotly({
    req(adjusted_predictions_mlp())
    
    preds <- adjusted_predictions_mlp()
    
    # Combine original and adjusted probabilities
    long_data <- rbind(
      data.frame(Probability = preds$original_data$Probability, Type = "Original Probabilities"),
      data.frame(Probability = preds$adjusted_data$Adjusted_Probability, Type = "Adjusted Probabilities")
    )
    
    # Remove NAs
    long_data <- long_data[!is.na(long_data$Probability), ]
    print("Long Data for Histogram:")
    print(head(long_data))
    
    custom_colors <- c("Original Probabilities" = "#64B5F6", "Adjusted Probabilities" = "#FFB74D")
    
    plot_ly(
      data = long_data,
      x = ~Probability,
      color = ~Type,
      colors = custom_colors,
      type = "histogram",
      opacity = 0.6,
      nbinsx = 30
    ) %>%
      layout(
        title = "Comparison of Original and Adjusted Probabilities",
        xaxis = list(title = "Probability"),
        yaxis = list(title = "Frequency"),
        barmode = "overlay",
        legend = list(title = list(text = "Type"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # Original data table
  output$original_data_table_mlp <- renderDT({
    req(adjusted_predictions_mlp())
    preds <- adjusted_predictions_mlp()
    print("Original Data Table:")
    print(head(preds$original_data))
    datatable(preds$original_data)
  })
  
  output$adjusted_data_table_mlp <- renderDT({
    req(adjusted_predictions_mlp())
    preds <- adjusted_predictions_mlp()
    print("Adjusted Data Table:")
    print(head(preds$adjusted_data))
    datatable(preds$adjusted_data)
  })
  
  
  
  
  
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  # Running the App
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  
}

shinyApp(ui = ui, server = server)
