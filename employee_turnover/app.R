#-----------------------------------------------------------------------------
# Initial Setup & Packages ---------------------------------------------------
#-----------------------------------------------------------------------------

pacman::p_load(shiny, 
               readxl,
               shinydashboard, 
               tidyverse, 
               ggplot2,
               dplyr,
               DT,
               plotly,
               highcharter,
               caret,
               randomForest,
               keras,
               rsconnect)

tensorflow::set_random_seed(42) # setting a seed so the random starting values it assigns to the weights in the model are repeatable.

# Load  a dataset
employee_retention_data <- read_excel("data/hr_retention_data.xlsx")

# Convert relevant columns to factors
employee_retention_data <- employee_retention_data %>%
  rename(average_monthly_hours = average_montly_hours,
         work_accident = Work_accident,
         department=sales) %>%
  mutate(
    left = as.factor(left),
    promotion_last_5years = as.factor(promotion_last_5years),
    work_accident = as.factor(work_accident),
    salary = as.factor(salary),
    department = as.factor(department)
  )


#-----------------------------------------------------------------------------
# Preparing the MLP, LR, and RF ----------------------------------------------
#-----------------------------------------------------------------------------

# ------- Preparing the MLP Data ------- #

# Load the pre-trained model
best_mlp_model <- load_model_hdf5("models/best_model.h5")

# Load testing dataset
test_data <- readRDS("data/test_data_encoded.rds")

# Remove columns 'left0' and 'left1' (one-hot encoded target columns)
test_data <- test_data[, !(colnames(test_data) %in% c("left0", "left1"))]

# List of columns expected by the model (to ensure the data matches the model's input format)
expected_columns <- c("satisfaction_level", "last_evaluation", "number_project", 
                      "average_monthly_hours", "time_spend_company", "work_accident", 
                      "promotion_last_5years", "departmenthr", "departmentIT", 
                      "departmentmanagement", "departmentmarketing", "departmentproduct_mng", 
                      "departmentRandD", "departmentsales", "departmentsupport", 
                      "departmenttechnical", "salarylow", "salarymedium")

# ------- Preparing the Logistic Regression Data ------- #

# Load the logistic regression model 
lr_model <- readRDS("models/logistic_model.rds")

# Load testing dataset
test_data_lr <- readRDS("data/test_data_LR.rds")

# Load training dataset
train_data_lr <- readRDS("data/train_data_LR.rds")

# ------- Preparing the Random Forest Data ------- #

# Load the random forest model 
rf_model <- readRDS("models/rf_model.rds")

# Load testing dataset
test_data_rf <- readRDS("data/test_data_RF.rds")

# Load training dataset
train_data_rf <- readRDS("data/train_data_RF.rds")

#-----------------------------------------------------------------------------
# Defining the UI ------------------------------------------------------------
#-----------------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Employee Turnover Dashboard"),
  
  # Sidebar menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Distributions", tabName = "distributions", icon = icon("chart-bar")),
      menuItem("Summary Statistics", tabName = "descriptives", icon = icon("chart-bar")),
      menuItem("Predictor Importance", tabName = "contributions", icon = icon("lightbulb")),
      menuItem("What-If Analysis", tabName = "simulation", icon = icon("sliders-h"))
    )
  ),
  
  # ------- Dashboard Body ------- #
  
  dashboardBody(
    tags$style(HTML("
  body {
    font-size: 12px; /* Set your desired base font size */
  }
")),
    # Include CSS for styling
    tags$style(HTML("
    .equal-height-right {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    gap: 5px; /* Small consistent spacing between infoBoxes */
    height: 100%; /* Ensure the height adjusts dynamically */
    box-sizing: border-box; /* Include padding and borders in calculations */
    overflow: hidden; /* Prevent any overflow outside the parent box */
  }
  .info-box {
    margin: 0; /* Remove external margin */
    padding: 5px; /* Adjust padding inside each box */
    flex-grow: 1; /* Allow boxes to expand if needed */
    height: auto; /* Let content determine height */
    min-height: 60px; /* Ensure a minimum height */
    box-sizing: border-box; /* Include padding and borders in calculations */
  }
  .info-box-content {
    margin: 0; /* Remove extra margin */
    padding: 5px; /* Reduce internal padding */
    display: flex;
    flex-direction: column;
    justify-content: center; /* Center content vertically */
  }
  .info-box-icon {
    margin: 0;
    padding: 0;
    height: 60px; /* Adjust icon height */
    width: 60px; /* Adjust icon width */
    line-height: 60px;
    flex-shrink: 0; /* Prevent icon from resizing */
  }
  
  .value-box-container {
  display: flex;
  flex-wrap: wrap;
  gap: 2px; /* Consistent spacing between value boxes */
  justify-content: space-evenly; /* Space boxes evenly */
  padding: 5px; /* Add internal padding */
  align-items: flex-start; /* Align items at the top */
}

.value-box {
  flex: 1 1 calc(33.33% - 0px); /* Ensure three equal-width columns per row */
  max-width: calc(33.33% - 0px); /* Limit width for consistency */
  min-height: 120px; /* Set a consistent height for all boxes */
  margin: 0; /* Remove external margins */
  padding: 5px; /* Add consistent padding */
  box-sizing: border-box; /* Include padding in width/height calculations */
}

.value-box-container2 {
  display: flex;
  flex-wrap: wrap;
  gap: 2px; /* Increase the gap for better spacing */
  justify-content: space-between; /* Distribute boxes evenly with space between */
  padding: 5px; /* Add padding to the container */
}

.value-box2 {
  flex: 1 1 calc(48% - 10px); /* Two columns with spacing accounted for */
  max-width: calc(48% - 10px); /* Match width to flex-basis */
  min-height: 150px; /* Increase minimum height for better readability */
  padding: 6px; /* Add internal padding */
  margin: 0; /* Remove external margin */
  box-sizing: border-box; /* Ensure padding is included in size */
  background-color: #f9f9f9; /* Optional: Add a background color */
  border: 1px solid #ccc; /* Optional: Add a border for separation */
}
  
  ")),
    
    tabItems(
      # Overview page
      tabItem(
        tabName = "overview",
        # Title, subtitle, and top cards
        fluidRow(
          class = "equal-height-row", # Add class for equal heights
          # Left column: Title and subtitle
          
          box(
            title = NULL,
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(
              style = "font-size: 150%; font-weight: bold; margin-bottom: 10px;",
              "Employee Turnover at Fictional Solutions Inc."
            ),
            div(
              style = "font-size: 120%; margin-bottom: 20px;",
              "Employee turnover is employees' voluntary severance of employment ties (Hom & Griffeth, 1995). High turnover in an organization can carry with it important costs, including on finances (e.g., cost of replacement), operational efficiency (e.g., knowledge loss, team dynamics), employee morale and engagement (e.g., stress) and organizational performance (e.g., consistency, talent quality). Understanding what contributes to turnover in an organization can empower decision-makers with information to help create a workplace where people choose to stay."
            )
          ),
          
          # Right column: Stacked infoBoxes
          
          box(
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(
              style = "display: flex; flex-direction: column; gap: 5px; align-items: stretch;",
              infoBoxOutput("turnover_rate", width = NULL),
              infoBoxOutput("number_stayed", width = NULL),
              infoBoxOutput("number_left", width = NULL)
            )
          )
          
        ),
        
        
        # Combined "Who Left?" and "Who is at Risk?" sections
        fluidRow(
          
          box(
            title = div(style = "font-size: 100%; font-weight: bold; color: white;", "Who Left?"),
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(
              class = "value-box-container",
              valueBoxOutput("avg_satisfaction", width = NULL),
              valueBoxOutput("common_salary", width = NULL),
              valueBoxOutput("avg_performance", width = NULL),
              valueBoxOutput("avg_monthly_hours", width = NULL),
              valueBoxOutput("avg_projects", width = NULL),
              valueBoxOutput("avg_tenure", width = NULL)
            ),
            plotlyOutput("department_left_bar", height = "auto")
          ),
          # Right half: Who is at Risk
          box(
            title = div(style = "font-size: 100%; font-weight: bold; color: white;", "Who is at Risk?"),
            width = 6,
            height = 750,
            solidHeader = TRUE,
            status = "primary",
            style = "
      height: calc(100vh); /* Dynamically adjust height based on viewport */
      min-height: 500px; /* Ensure a minimum height */
      max-height: 700px; /* Cap height for larger screens */
      display: flex; /* Use flexbox for centering */
      flex-direction: column; /* Stack items vertically */
      justify-content: space-evenly; /* Even spacing between items */
      align-items: center; /* Horizontally center content */
    ",
            # Centered slider with improved label
            div(
              style = "width: 80%; text-align: center; margin-bottom: 10px;",
              sliderInput(
                inputId = "risk_threshold",
                label = div(style = "font-weight: bold; font-size: 120%;", 
                            "Select a risk threshold (i.e., the probability cutoff above which employees are classified as 'at risk' of leaving):"),
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.025
              )
            ),
            # Center the donut chart
            plotlyOutput("at_risk_donut_chart", height = "300px", width = "80%"),
            # Center the descriptive text with improved style
            div(
              style = "margin-top: 20px; font-size: 100%; text-align: center; color: #666;",
              "This chart shows the proportion of employees predicted by the Random Forest model to be at risk of leaving based on the selected probability threshold."
            )
          )
          
        )
        ,
        
        # Key Drivers and Recommendations
        
        fluidRow(
          
          box(
            title = div(style = "font-size: 100%; font-weight: bold; color: white;", "What Drives Turnover?"),
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            div(
              style = "font-size: 100%; line-height: 1.6;",
              
              # Detailed Insights
              p(tags$b("A logistic regression suggests that the following factors are the strongest predictors of whether employees stay or leave (of those considered):")),
              tags$ul(
                tags$li(
                  tags$b("📉 Low Satisfaction: "), 
                  "Employees with low satisfaction levels are ", 
                  tags$b("8.2 times more likely"), 
                  " to leave compared to those with high satisfaction levels.",
                  tags$ul(
                    tags$li("Dissatisfied employees may actively seek opportunities elsewhere.")
                  )
                ),
                tags$li(
                  tags$b("💰 Pay Level: "), 
                  "Compared to employees with high pay levels, those with low pay levels are ", 
                  tags$b("6.27 times more likely"), 
                  " to leave, while those with medium pay levels are ", 
                  tags$b("3.44 times more likely."),
                  tags$ul(
                    tags$li("Employees with lower or medium pay may feel undervalued, driving them to explore other opportunities.")
                  )
                ),
                tags$li(
                  tags$b("🌟 Performance Ratings: "), 
                  "Employees with lower-mid performance evaluations are ", 
                  tags$b("86% less likely to leave "), 
                  "than those with high performance evaluations.",
                  tags$ul(
                    tags$li("High performers may face external offers or pressure, while mid-level performers feel secure and less visible to recruiters.")
                  )
                ),
                tags$li(
                  tags$b("⏱️ Workload (Monthly Hours): "), 
                  "Employees working fewer monthly hours (lower-mid range) are ", 
                  tags$b("85% less likely to leave "), 
                  "than those working very long hours.",
                  tags$ul(
                    tags$li("High workloads may contribute to stress, while moderate workloads strike a healthy balance.")
                  )
                ),
                tags$li(
                  tags$b("📊 Number of Projects: "), 
                  "Employees handling fewer projects (lower-mid range) are ", 
                  tags$b("75% less likely to leave "), 
                  "compared to those handling many projects.",
                  tags$ul(
                    tags$li("A balanced number of projects may promotes engagement and reduce turnover risk.")
                  )
                )
              )
            )
            
            
          ),
          
          box(
            title = div(style = "font-size: 100%; font-weight: bold; color: white;", "Modeling Approach and Recommendations"),
            width = 6,
            # height=600,
            solidHeader = TRUE,
            status = "primary",
            div(
              style = "font-size: 100%;",
              p("Three models were compared to predict turnover:"),
              tags$ul(
                tags$li(tags$b("Logistic Regression:"), " Best for understanding drivers of turnover due to interpretability."),
                tags$li(tags$b("Neural Network:"), " Captures complex patterns but functions as a black box."),
                tags$li(tags$b("Random Forest:"), " Most accurate for predicting turnover in this use case.")
              ),
              
              p("For each model, the general approach taken was as follows:"),
              tags$ul(
                tags$li("The broader dataset was separated into a training and test set, using a 70/30 split."),
                tags$li("The model was trained on the data contained in the training set and evaluated on the test set."),
                tags$li("The model's accuracy (i.e., the proportion of predictions it got right) was evaluated on both the training set and the test set, to determine the extent of overfitting."),
                tags$li("Performance on the test set was also indexed by looking at precision (i.e., the proportion of predicted positives that are actual positives) and recall (the proportion of true positives correctly identified). "),
              ),
              
              p("Overall, the Random Forest model yielded the highest accuracy, with good precision and recall (see 'What-If Analysis tab'). However, it's important to balance accuracy with interpretability."),
              tags$ul(
                tags$li("Often, the more accurate a model is given its higher complexity, the less easily interpretable it tends to be."),
                tags$li("If the goal is to explain factors associted with turnover to stakeholders, the results of the logistic regression model may prove most useful."),
                tags$li("If the goal is to predict turnover risks (e.g., to flag employees who would be good candidates for a retention program), the random forest model may prove most useful."),
              )
            )
          )
        )
        
      ),
      
      
      # Distributions page
      
      tabItem(tabName = "distributions",
              fluidRow(
                box(title = "Turnover by Satisfaction Level", width = 6, plotlyOutput("satisfaction_hist")),
                box(title = "Turnover by Performance Level", width = 6, plotlyOutput("hist_perf")),
                box(title = "Turnover by Average Monthly Hours", width = 6, plotlyOutput("hist_avg_hours")),
                box(title = "Turnover by Number of Projects", width = 6, plotlyOutput("hist_num_projects")),
                box(title = "Turnover by Time Spent at Company", width = 6, plotlyOutput("hist_time_spent")),
                box(title = "Turnover by Salary Level", width = 6, plotlyOutput("hist_salary")),
                box(title = "Turnover by Department", width = 6, plotlyOutput("hist_department"))
              )
      ),
      
      # Predictor contributions page
      
      tabItem(
        tabName = "contributions",
        fluidRow(
          # Add an explanation box at the top
          box(
            width = 12,
            solidHeader = TRUE,
            status = "info",
            h4("Understanding These Charts"),
            p("This section helps us understand patterns related to employee turnover. These charts show patterns to help guide our understanding, but we should still be cautious not to draw causal inferences. Here’s what the charts show:"),
            tags$ul(
              tags$li(
                strong("Logistic Regression Chart:"),
                " Blue dots highlight predictors that have a statistically significant relationship with turnover, while gray dots represent predictors that do not have a statistically significant relationship with turnover.",
                tags$ul(
                  tags$li(
                    "The length of the lines reflects the size of the odds ratio. The odds ratio shows how much a predictor is associated with turnover compared to a baseline group. If the odds ratio is exactly 1, it suggests that the predictor has no impact on the likelihood of turnover relative to the reference group. If the odds ratio is less than 1, it suggests that the predictor is associated with a lower likelihood of turnover compared to the reference group. If the odds ratio is greater than 1, it suggests that the predictor is associated with a higher likelihood of turnover compared to the reference group."
                  )
                )
              ),
              tags$li(
                strong("Random Forest Chart:"),
                " This chart shows which of the predictors examined are most useful for predicting whether someone might leave the company. Longer bars mean the factor is more important for predictions."
              )
            )
          )
        ),
        fluidRow(
          # Tabs for visualizations
          box(
            title = "Predictor Contributions",
            width = 12,
            tabsetPanel(
              tabPanel(
                "Logistic Regression Coefficients",
                div(style = "padding-top: 20px;", plotlyOutput("logit_lollipop_plot"))
              ),
              tabPanel(
                "Random Forest Variable Importance",
                div(style = "padding-top: 20px;", plotlyOutput("rf_var_importance_plot"))
              )
            )
          )
        )
      ),
      
      # Summary Statistics Explorer Page
      
      tabItem(tabName = "descriptives",
              
              fluidRow(
                box(title = "Summary Statistics Explorer", width = 12, 
                    fluidRow(
                      column(12,
                             HTML("<p>This tool allows you to explore summary statistics dynamically. 
                                   Select a specific department(s), salary level(s), and promotion status(es), 
                                   and choose a metric variable to calculate key statistics (e.g., min, max, 
                                   mean, median, SD) and view the breakdown of employees who stayed 
                                   or left the company.</p>")
                      )
                    ),
                    fluidRow(
                      column(4, 
                             checkboxGroupInput(
                               "selected_departments", 
                               "Select Departments:", 
                               choices = unique(employee_retention_data$department), 
                               selected = character(0)  # Default: Nothing selected
                             )
                      ),
                      column(4, 
                             checkboxGroupInput(
                               "selected_salaries", 
                               "Select Salaries:", 
                               choices = unique(employee_retention_data$salary), 
                               selected = character(0)  # Default: Nothing selected
                             )
                      ),
                      column(4, 
                             checkboxGroupInput(
                               "selected_promotions", 
                               "Select Promotion Status (Last 5 Years):", 
                               choices = unique(employee_retention_data$promotion_last_5years), 
                               selected = character(0)  # Default: Nothing selected
                             )
                      )
                    ),
                    selectizeInput(
                      "metric_var", "Metric Variable:", 
                      choices = c(
                        "Satisfaction Level" = "satisfaction_level", 
                        "Performance Rating on Last Evaluation" = "last_evaluation", 
                        "Average Monthly Hours" = "average_monthly_hours", 
                        "Number of Projects" = "number_project"
                      ),
                      multiple = FALSE,
                      options = list(placeholder = "Select a variable to calculate metrics")
                    ),
                    DTOutput("calculated_metrics_table")  # Use datatable output
                )
              )
      ),
      
      # Simulation page
      tabItem(tabName = "simulation",
              fluidRow(
                # Add a description box at the top
                box(
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  h4("What-If Analysis"),
                  p("This page allows you to explore how changes in key predictors, such as satisfaction level or average monthly hours, 
              would influence employee turnover predictions. You can adjust these parameters and compare predictions made using three different 
              approaches—Logistic Regression, Neural Network, and Random Forest."),
                  p("Choose the model you want to use (logistic regression by default), make any adjustments you would like, and click the Simulate button. You'll be able to see two histograms - the adjusted one and the old one with no changes made."),
                  p(strong("Note that the Random Forest model yielded the highest accuracy on the test set and had good precision and recall."))
                )
              ),
              fluidRow(
                # Logistic Regression Performance Boxes
                box(
                  title = "Logistic Regression Approach: Performance on Test Set", 
                  width = 4, 
                  div(
                    class = "value-box-container",
                    valueBoxOutput("test_accuracy_lr", width = NULL),
                    valueBoxOutput("precision_lr", width = NULL),
                    valueBoxOutput("recall_lr", width = NULL),
                    valueBoxOutput("f1_lr", width = NULL)
                  )
                ),
                
                # Neural Network Performance Boxes
                box(
                  title = "Neural Network Approach: Performance on Test Set", 
                  width = 4, 
                  div(
                    class = "value-box-container",
                    valueBoxOutput("test_accuracy_nn", width = NULL),
                    valueBoxOutput("precision_nn", width = NULL),
                    valueBoxOutput("recall_nn", width = NULL),
                    valueBoxOutput("f1_nn", width = NULL)
                  )
                ),
                
                # Random Forest Performance Boxes
                box(
                  title = "Random Forest Approach: Performance on Test Set", 
                  width = 4, 
                  div(
                    class = "value-box-container",
                    valueBoxOutput("test_accuracy_rf", width = NULL),
                    valueBoxOutput("precision_rf", width = NULL),
                    valueBoxOutput("recall_rf", width = NULL),
                    valueBoxOutput("f1_rf", width = NULL)
                  )
                )
              ),
              
              fluidRow(
                div(
                  style = "margin-left: 20px; margin-top: 10px;",  # Add spacing
                  selectInput(
                    inputId = "model_choice",
                    label = "Choose Prediction Model:",
                    choices = c("Logistic Regression" = "lr", "Neural Network" = "nn", "Random Forest" = "rf"),
                    selected = "lr"
                  )
                )
                ,
                
                # Parameter Adjustment Box
                box(
                  title = "Parameter Adjustment",
                  width = 4,
                  # Slider for Satisfaction Level
                  sliderInput(
                    inputId = "satisfaction_slider",
                    label = "Adjust Satisfaction Level",
                    min = -0.5, max = 0.5,
                    value = 0.00, step = 0.01
                  ),
                  # Slider for Last Evaluation
                  sliderInput(
                    inputId = "evaluation_slider",
                    label = "Adjust Last Evaluation",
                    min = -0.5, max = 0.5,
                    value = 0.00, step = 0.01
                  ),
                  
                  # Slider for Average Monthly Hours
                  sliderInput(
                    inputId = "avg_monthly_hours_slider",
                    label = "Adjust Average Monthly Hours",
                    min = -20, max = 20,
                    value = 0.00, step = 1
                  ),
                  
                  # Slider for Number of Projects
                  sliderInput(
                    inputId = "number_projects_slider",
                    label = "Adjust Number of Projects",
                    min = -5, max = 5,
                    value = 0.00, step = 1
                  ),
                  
                  # Slider for Time Spent at Company
                  sliderInput(
                    inputId = "time_spend_slider",
                    label = "Adjust Time Spent at Company (Years)",
                    min = -5, max = 5,
                    value = 0.00, step = 1
                  ),
                  
                  # Simulate Button
                  actionButton(
                    inputId = "simulate",
                    label = "Simulate"
                  )
                ),
                
                # Turnover Predictions after adjustment
                box(
                  title = "Turnover Predictions After Adjustment",
                  width = 8,
                  
                  # Prediction Outputs
                  plotlyOutput(outputId = "adjusted_predictions")
                )
              )
      )
    )
  )
)

#-----------------------------------------------------------------------------
# Defining the Server Logic --------------------------------------------------
#-----------------------------------------------------------------------------

server <- function(input, output) {
  
  # ------- Overview Page ------- #
  
  # Create the KPI metric boxes at the top - average values for people who left. 
  
  # Create breakdown of left vs stayed
  breakdown_left <- table(employee_retention_data$left)
  num_left <- breakdown_left[["1"]]  # Employees who left
  num_stayed <- breakdown_left[["0"]]  # Employees who stayed
  
  # Turnover rate value box
  output$turnover_rate <- renderInfoBox({
    infoBox(
      title = tags$div(
        style = "text-transform: none; word-wrap: break-word; font-size: 100%;", 
        "Turnover Rate:"
      ), # Custom font size for title
      value = tags$div(
        style = "word-wrap: break-word;  font-size: 100%; font-weight: bold;", 
        paste0(round((num_left / (num_left + num_stayed)) * 100, 2), "%")
      ), # Custom font size and bold styling for value
      icon = icon("chart-line"), # Replace "chart-line" with any Font Awesome icon
      color = "blue" # Optional: Choose a color for the infoBox
    )
  })
  
  # Number who left box
  output$number_left <- renderInfoBox({
    infoBox(
      title = tags$div(
        style = "text-transform: none; word-wrap: break-word; font-size: 100%;", 
        "Number of Employees Who Left:"
      ), # Ensures title wraps within the box
      value = tags$div(
        style = "word-wrap: break-word;  font-size: 100%; font-weight: bold;", 
        num_left
      ), # Ensures value wraps and stays inside the box      
      icon = icon("user-minus"), # Replace "chart-line" with any Font Awesome icon
      color = "blue"  # Optional: Choose a color for the infoBox
    )
  })
  
  # Number who stayed box
  output$number_stayed <- renderInfoBox({
    infoBox(
      title = tags$div(
        style = "text-transform: none; word-wrap: break-word; font-size: 100%;", 
        "Number of Employees Remaining:"
      ), # Ensures title wraps within the box
      value = tags$div(
        style = "word-wrap: break-word; font-size: 100%; font-weight: bold;", 
        num_stayed
      ), # Ensures value wraps and stays inside the box        
      icon = icon("user-check"), # Replace "chart-line" with any Font Awesome icon
      color = "yellow" # Optional: Choose a color for the infoBox
    )
  })
  
  output$avg_satisfaction <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       round(mean(employee_retention_data$satisfaction_level[employee_retention_data$left == 1], na.rm = TRUE), 2)),
      subtitle = tags$div(style = "font-size: 80%;", "Average Satisfaction (0-1)"),
      icon = icon("smile"),
      color = "blue"
    )
  })
  
  output$common_salary <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       names(sort(table(employee_retention_data$salary[employee_retention_data$left == 1]), decreasing = TRUE)[1])),
      subtitle = tags$div(style = "font-size: 80%;", "Most Common Salary Level"),
      icon = icon("smile"),
      color = "blue"
    )
  })
  
  output$avg_performance <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       round(mean(employee_retention_data$last_evaluation[employee_retention_data$left == 1], na.rm = TRUE), 2)),
      subtitle = tags$div(style = "font-size: 80%;", "Average Performance (0-1)"),
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$avg_monthly_hours <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       round(mean(employee_retention_data$average_monthly_hours[employee_retention_data$left == 1], na.rm = TRUE), 2)),
      subtitle = tags$div(style = "font-size: 80%;", "Average Monthly Hours"),
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$avg_projects <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       round(mean(employee_retention_data$number_project[employee_retention_data$left == 1], na.rm = TRUE), 2)),
      subtitle = tags$div(style = "font-size: 80%;", "Average Project Count"),
      icon = icon("project-diagram"),
      color = "blue"
    )
  })
  
  output$avg_tenure <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 100%; font-weight: bold;", 
                       round(mean(employee_retention_data$time_spend_company[employee_retention_data$left == 1], na.rm = TRUE), 2)),
      subtitle = tags$div(style = "font-size: 80%;", "Average Tenure (Years)"),
      icon = icon("hourglass-half"),
      color = "blue"
    )
  }) 
  
  # Render the bar graph for employees who left by department
  output$department_left_bar <- renderPlotly({
    department_data <- employee_retention_data %>%
      group_by(department) %>% # Group by department
      summarise(
        total_employees = n(), # Total employees in each department
        num_left = sum(left == 1), # Total employees who left
        prop_left = round((num_left / total_employees) * 100, 2) # Proportion who left as a percentage
      ) %>%
      arrange(desc(prop_left)) # Sort by proportion left
    
    # Create the bar graph
    plot_ly(
      department_data,
      x = ~prop_left, # Proportion left
      y = ~reorder(department, prop_left), # Reorder departments by proportion
      type = 'bar',
      orientation = 'h', # Horizontal bar chart
      marker = list(
        color = 'rgba(243, 156, 18, 0.5)', # Add transparency to the color (alpha = 0.5)
        line = list(color = 'rgba(243, 156, 18, 1)', width = 1) # Keep the outline fully opaque
      ),
      text = ~paste0(
        "Department: ", department, "<br>",
        "Number Left: ", num_left, "<br>",
        "Total Employees: ", total_employees, "<br>",
        "Proportion Left: ", prop_left, "%"
      ), # Custom hover text
      hoverinfo = "text", # Show only the custom hover text
      textposition = "none"
    ) %>%
      layout(
        title = "Proportion of Employees Who Left by Department",
        font = list(size = 10), # Adjust the size as needed, e.g., 16 for smaller text
        xaxis = list(title = "Proportion of Employees Who Left (%)"),
        yaxis = list(title = "Department"),
        margin = list(l = 100, r = 20, t = 30, b = 30),
        showlegend = FALSE, # Hide legends
        annotations = NULL # Remove any auto-generated annotations
      ) %>%
      config(displayModeBar = FALSE) # Optional: Hide the Plotly toolbar
  })
  
  # Filter employees who stayed
  stayed_employees <- reactive({
    employee_retention_data %>% filter(left == 0)
  })
  
  predicted_risk <- reactive({
    validate(
      need(!is.null(rf_model), "Random Forest model not loaded."),
      need(nrow(stayed_employees()) > 0, "Employee data not available.")
    )
    
    stayed_employees_clean <- stayed_employees()
    
    # Debugging: Check cleaned data
    print("Debug: stayed_employees_clean")
    print(head(stayed_employees_clean))
    
    predict(rf_model, newdata = stayed_employees_clean, type = "prob")[, 2]
  })
  
  # Classify employees as "At Risk" or "Not At Risk"
  stayed_risk_summary <- reactive({
    stayed_employees() %>%
      mutate(
        risk_score = predicted_risk(),
        risk_category = ifelse(risk_score > input$risk_threshold, "At Risk", "Not At Risk")
      ) %>%
      group_by(risk_category) %>%
      summarise(Count = n(), .groups = "drop")
  })
  
  # Donut chart for "At Risk" employees
  output$at_risk_donut_chart <- renderPlotly({
    validate(
      need(nrow(stayed_risk_summary()) > 0, "No data available for the selected threshold.")
    )
    
    risk_summary <- stayed_risk_summary()
    
    # Explicit color mapping
    color_mapping <- c("At Risk" = "#F39C12", "Not At Risk" = "#006DB4")
    risk_summary <- risk_summary %>%
      mutate(color = color_mapping[risk_category]) # Add colors to the data
    
    plot_ly(
      data = risk_summary,
      labels = ~risk_category,
      values = ~Count,
      type = "pie",
      textinfo = "label+value",
      hoverinfo = "label+value",
      marker = list(colors = ~color), # Use mapped colors
      hole = 0.5,
      textfont = list(size = 10)
    ) %>%
      layout(
        title = list(
          text = paste0("Number of Employees At Risk (Threshold:", input$risk_threshold, ")"),
          x = 0.5,          # Center align horizontally
          y = 0.07,         # Move below the chart
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  # Set the title font size
          
        ),
        showlegend = TRUE
      )
  })
  
  # ------- Summary Statistics Explorer ------- #
  
  output$calculated_metrics_table <- renderDT({
    req(input$metric_var)  # Ensure a metric variable is selected
    
    # Step 1: Check if any filters are applied
    
    if (length(input$selected_departments) == 0 && 
        length(input$selected_salaries) == 0 && 
        length(input$selected_promotions) == 0) {
      # Show an empty table with a message if no filters are selected
      return(datatable(
        data.frame(Message = "No data available. Please select at least one filter."),
        options = list(dom = 't', paging = FALSE, ordering = FALSE),  # Simplify table display
        rownames = FALSE
      ))
    }
    
    # Step 2: Filter the dataset based on selections
    
    filtered_data <- employee_retention_data %>%
      filter(
        if (length(input$selected_departments) > 0) department %in% input$selected_departments else TRUE,
        if (length(input$selected_salaries) > 0) salary %in% input$selected_salaries else TRUE,
        if (length(input$selected_promotions) > 0) promotion_last_5years %in% input$selected_promotions else TRUE
      )
    
    # Step 3: Determine grouping variables dynamically
    
    grouping_vars <- c()
    if (length(input$selected_departments) > 0) grouping_vars <- c(grouping_vars, "department")
    if (length(input$selected_salaries) > 0) grouping_vars <- c(grouping_vars, "salary")
    if (length(input$selected_promotions) > 0) grouping_vars <- c(grouping_vars, "promotion_last_5years")
    
    # Step 4: Handle single selections explicitly
    
    if (length(input$selected_departments) == 1 &&
        length(input$selected_salaries) == 0 &&
        length(input$selected_promotions) == 0) {
      # Only 1 department selected
      grouping_vars <- c("department")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) == 1 &&
               length(input$selected_promotions) == 0) {
      # Only 1 salary level selected
      grouping_vars <- c("salary")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) == 0 &&
               length(input$selected_promotions) == 1) {
      # Only 1 promotion status selected
      grouping_vars <- c("promotion_last_5years")
    } else if (length(input$selected_departments) == 1 &&
               length(input$selected_salaries) > 0 &&
               length(input$selected_promotions) == 0) {
      # 1 department and multiple salaries selected
      grouping_vars <- c("department", "salary")
    } else if (length(input$selected_departments) == 1 &&
               length(input$selected_salaries) == 0 &&
               length(input$selected_promotions) > 0) {
      # 1 department and multiple promotions selected
      grouping_vars <- c("department", "promotion_last_5years")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) == 1 &&
               length(input$selected_promotions) > 0) {
      # 1 salary and multiple promotions selected
      grouping_vars <- c("salary", "promotion_last_5years")
    } else if (length(input$selected_departments) > 1 &&
               length(input$selected_salaries) == 0 &&
               length(input$selected_promotions) == 0) {
      # Multiple departments selected
      grouping_vars <- c("department")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) > 1 &&
               length(input$selected_promotions) == 0) {
      # Multiple salaries selected
      grouping_vars <- c("salary")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) == 0 &&
               length(input$selected_promotions) > 1) {
      # Multiple promotions selected
      grouping_vars <- c("promotion_last_5years")
    } else if (length(input$selected_departments) > 1 &&
               length(input$selected_salaries) > 1 &&
               length(input$selected_promotions) == 0) {
      # Multiple departments and salaries selected
      grouping_vars <- c("department", "salary")
    } else if (length(input$selected_departments) > 1 &&
               length(input$selected_salaries) == 0 &&
               length(input$selected_promotions) > 0) {
      # Multiple departments and promotions selected
      grouping_vars <- c("department", "promotion_last_5years")
    } else if (length(input$selected_departments) == 0 &&
               length(input$selected_salaries) > 1 &&
               length(input$selected_promotions) > 0) {
      # Multiple salaries and promotions selected
      grouping_vars <- c("salary", "promotion_last_5years")
    } else if (length(input$selected_departments) > 1 &&
               length(input$selected_salaries) > 1 &&
               length(input$selected_promotions) > 1) {
      # All departments, salaries, and promotions selected
      grouping_vars <- c("department", "salary", "promotion_last_5years")
    }
    
    # Step 5: Group and calculate metrics
    
    grouped_summary <- filtered_data %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarise(
        Count_Stayed = sum(left == "0"),
        Count_Left = sum(left == "1"),
        Min = min(.data[[input$metric_var]], na.rm = TRUE),
        Max = max(.data[[input$metric_var]], na.rm = TRUE),
        Mean = mean(.data[[input$metric_var]], na.rm = TRUE),
        Median = median(.data[[input$metric_var]], na.rm = TRUE),
        SD = sd(.data[[input$metric_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 6: Add a combined summary row if more than one value is selected within a variable
    
    if (length(input$selected_departments) > 1 || 
        length(input$selected_salaries) > 1 || 
        length(input$selected_promotions) > 1) {
      combined_summary <- filtered_data %>%
        summarise(
          department = if (length(input$selected_departments) > 1) "All Selected" else NA,
          salary = if (length(input$selected_salaries) > 1) "All Selected" else NA,
          promotion_last_5years = if (length(input$selected_promotions) > 1) "All Selected" else NA,
          Count_Stayed = sum(left == "0"),
          Count_Left = sum(left == "1"),
          Min = min(.data[[input$metric_var]], na.rm = TRUE),
          Max = max(.data[[input$metric_var]], na.rm = TRUE),
          Mean = mean(.data[[input$metric_var]], na.rm = TRUE),
          Median = median(.data[[input$metric_var]], na.rm = TRUE),
          SD = sd(.data[[input$metric_var]], na.rm = TRUE)
        )
      grouped_summary <- bind_rows(grouped_summary, combined_summary)
    }
    
    # Step 7: Reorder columns and round numeric values
    
    grouped_summary <- grouped_summary %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      relocate(Count_Stayed, Count_Left, .after = last_col())
    
    # Render table
    datatable(grouped_summary, options = list(pageLength = 100))
  })
  
  output$dataset_table <- renderDT({
    datatable(employee_retention_data)
  })
  
  # ------- Turnover Distributions by Categories------- #
  
  ## Turnover by Satisfaction Level
  
  output$satisfaction_hist <- renderPlotly({
    
    opacity_level <- 0.5 # Set your desired opacity level here
    min <- min(employee_retention_data$satisfaction_level)
    max <- max(employee_retention_data$satisfaction_level)
    bin_size <- (max - min) / 10
    
    plot_ly(data = employee_retention_data) %>%  # Set global data source
      add_histogram(
        x = ~satisfaction_level,  # Satisfaction level column
        name = "Stayed",          # Name for legend
        data = employee_retention_data %>% filter(left == "0"),  # Filter for Stayed
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#006DB4",
                      opacity = 0.5,
                      line = list(color = "#006DB4", width = 1))         
      ) %>%
      add_histogram(
        x = ~satisfaction_level,
        name = "Left",
        data = employee_retention_data %>% filter(left == "1"),  # Filter for Left
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#F39C12", 
                      opacity = 0.5,
                      line = list(color = "#F39C12", width = 1))
      ) %>%
      layout(
        barmode = "overlay",  # Overlay histograms
        xaxis = list(title = "Satisfaction Level"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = "Turnover Status"))
      )
    
  })
  
  ## Turnover by Performance Level on Last Evaluation
  
  output$hist_perf <- renderPlotly({
    
    opacity_level <- 0.5 # Set your desired opacity level here
    min <- min(employee_retention_data$last_evaluation)
    max <- max(employee_retention_data$last_evaluation)
    bin_size <- (max - min) / 10
    
    plot_ly(data = employee_retention_data) %>%  # Set global data source
      add_histogram(
        x = ~last_evaluation,  # Satisfaction level column
        name = "Stayed",          # Name for legend
        data = employee_retention_data %>% filter(left == "0"),  # Filter for Stayed
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#006DB4",
                      opacity = 0.5,
                      line = list(color = "#006DB4", width = 1))         
      ) %>%
      add_histogram(
        x = ~last_evaluation,
        name = "Left",
        data = employee_retention_data %>% filter(left == "1"),  # Filter for Left
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#F39C12", 
                      opacity = 0.5,
                      line = list(color = "#F39C12", width = 1))
      ) %>%
      layout(
        barmode = "overlay",  # Overlay histograms
        xaxis = list(title = "Performance Rating on Last Evaluation"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = "Turnover Status"))
      )
    
  })
  
  ## Turnover by Avg Number of Monthly Hours
  
  output$hist_avg_hours <- renderPlotly({
    
    opacity_level <- 0.5 # Set your desired opacity level here4
    min <- min(employee_retention_data$average_monthly_hours)
    max <- max(employee_retention_data$average_monthly_hours)
    bin_size <- (max - min) / 10
    
    plot_ly(data = employee_retention_data) %>%  # Set global data source
      add_histogram(
        x = ~average_monthly_hours,  # Satisfaction level column
        name = "Stayed",          # Name for legend
        data = employee_retention_data %>% filter(left == "0"),  # Filter for Stayed
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#006DB4",
                      opacity = 0.5,
                      line = list(color = "#006DB4", width = 1))         
      ) %>%
      add_histogram(
        x = ~average_monthly_hours,
        name = "Left",
        data = employee_retention_data %>% filter(left == "1"),  # Filter for Left
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#F39C12", 
                      opacity = 0.5,
                      line = list(color = "#F39C12", width = 1))
      ) %>%
      layout(
        barmode = "overlay",  # Overlay histograms
        xaxis = list(title = "Average Monthly Hours"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = "Turnover Status"))
      )
  })
  
  ## Turnover by Number of Projects
  
  output$hist_num_projects <- renderPlotly({
    
    opacity_level <- 0.5 # Set your desired opacity level here
    min <- min(employee_retention_data$number_project)
    max <- max(employee_retention_data$number_project)
    bin_size <-(max - min) / 10
    
    plot_ly(data = employee_retention_data) %>%  # Set global data source
      add_histogram(
        x = ~number_project,  # Satisfaction level column
        name = "Stayed",          # Name for legend
        data = employee_retention_data %>% filter(left == "0"),  # Filter for Stayed
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#006DB4",
                      opacity = 0.5,
                      line = list(color = "#006DB4", width = 1))         
      ) %>%
      add_histogram(
        x = ~number_project,
        name = "Left",
        data = employee_retention_data %>% filter(left == "1"),  # Filter for Left
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#F39C12", 
                      opacity = 0.5,
                      line = list(color = "#F39C12", width = 1))
      ) %>%
      layout(
        barmode = "overlay",  # Overlay histograms
        xaxis = list(title = "Number of Projects"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = "Turnover Status"))
      )
    
  })
  
  
  ## Turnover by Time Spent at Company
  
  output$hist_time_spent <- renderPlotly({
    
    opacity_level <- 0.5 # Set your desired opacity level here
    min <- min(employee_retention_data$time_spend_company)
    max <- max(employee_retention_data$time_spend_company)
    bin_size <-(max - min) / 10
    
    plot_ly(data = employee_retention_data) %>%  # Set global data source
      add_histogram(
        x = ~time_spend_company,  # Satisfaction level column
        name = "Stayed",          # Name for legend
        data = employee_retention_data %>% filter(left == "0"),  # Filter for Stayed
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#006DB4",
                      opacity = 0.5,
                      line = list(color = "#006DB4", width = 1))         
      ) %>%
      add_histogram(
        x = ~time_spend_company,
        name = "Left",
        data = employee_retention_data %>% filter(left == "1"),  # Filter for Left
        xbins = list(start = min, end = max, size = bin_size),           # Bin settings
        marker = list(color = "#F39C12", 
                      opacity = 0.5,
                      line = list(color = "#F39C12", width = 1))
      ) %>%
      layout(
        barmode = "overlay",  # Overlay histograms
        xaxis = list(title = "Number of Years at Company"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = "Turnover Status"))
      )
  })
  
  ## Turnover by Salary Level
  
  output$hist_salary <- renderPlotly({
    # Group data by salary and left
    salary_summary <- employee_retention_data %>%
      group_by(salary, left) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(salary) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Map levels to meaningful labels
    salary_summary <- salary_summary %>%
      mutate(left = factor(left, levels = c("0", "1"), labels = c("Stayed", "Left")))
    
    # Create a bar chart
    plot_ly(
      data = salary_summary,
      x = ~salary,
      y = ~percentage,
      color = ~left,  # Use the mapped factor for color
      type = "bar",
      colors = c("Stayed" = "#006DB4", "Left" = "#F39C12"),  # Match color to new labels
      opacity = 0.5,  # Set bar opacity
      marker = list(
        line = list(color = "black", width = 1)  # Add border to bars
      )
    ) %>%
      layout(
        barmode = "group",  # Group the bars
        xaxis = list(title = "Salary Level"),
        yaxis = list(title = "Percentage of Employees"),
        legend = list(title = list(text = "Turnover Status"))  # Custom legend title
      )
  })
  
  ## Turnover by Department
  
  output$hist_department <- renderPlotly({
    
    # Group data by department and left
    department_summary <- employee_retention_data %>%
      group_by(department, left) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(department) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Map levels to meaningful labels
    department_summary <- department_summary %>%
      mutate(left = factor(left, levels = c("0", "1"), labels = c("Stayed", "Left")))
    
    # Create a bar chart
    plot_ly(
      data = department_summary,
      x = ~department,
      y = ~percentage,
      color = ~left,  # Use the mapped factor for color
      type = "bar",
      colors = c("Stayed" = "#006DB4", "Left" = "#F39C12"),  # Match color to new labels
      opacity = 0.5,  # Set bar opacity
      marker = list(
        line = list(color = "black", width = 1)  # Add border to bars
      )
    ) %>%
      layout(
        barmode = "group",  # Set bar mode to grouped
        xaxis = list(title = "Department"),
        yaxis = list(title = "Percentage"),
        legend = list(title = list(text = "Turnover Status"))  # Legend title
      )
  })
  
  ## Turnover by Promotion Status in Last 5 Years
  
  output$hist_promotion <- renderPlotly({
    
    # Group data by department and left
    promotion_summary <- employee_retention_data %>%
      group_by(promotion_last_5years, left) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(promotion_last_5years) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Map levels to meaningful labels
    promotion_summary <- promotion_summary %>%
      mutate(left = factor(left, levels = c("0", "1"), labels = c("Stayed", "Left")))
    
    # Create a bar chart
    plot_ly(
      data = promotion_summary,
      x = ~promotion_last_5years,
      y = ~percentage,
      color = ~left,  # Use the mapped factor for color
      type = "bar",
      colors = c("Stayed" = "#006DB4", "Left" = "#F39C12"),  # Match color to new labels
      opacity = 0.5,  # Set bar opacity
      marker = list(
        line = list(color = "black", width = 1)  # Add border to bars
      )
    ) %>%
      layout(
        barmode = "group",  # Set bar mode to grouped
        xaxis = list(title = "Promotion in Last 5 Years"),
        yaxis = list(title = "Percentage"),
        legend = list(title = list(text = "Turnover Status"))  # Legend title
      )
  })
  
  ## Turnover by Work Accident Status
  
  output$hist_accident <- renderPlotly({
    
    # Group data by department and left
    accident_summary <- employee_retention_data %>%
      group_by(work_accident, left) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(work_accident) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Map levels to meaningful labels
    accident_summary <- accident_summary %>%
      mutate(left = factor(left, levels = c("0", "1"), labels = c("Stayed", "Left")))
    
    # Create a bar chart
    plot_ly(
      data = accident_summary,
      x = ~work_accident,
      y = ~percentage,
      color = ~left,  # Use the mapped factor for color
      type = "bar",
      colors = c("Stayed" = "#006DB4", "Left" = "#F39C12"),  # Match color to new labels
      opacity = 0.5,  # Set bar opacity
      marker = list(
        line = list(color = "black", width = 1)  # Add border to bars
      )
    ) %>%
      layout(
        barmode = "group",  # Set bar mode to grouped
        xaxis = list(title = "Work Accident"),
        yaxis = list(title = "Percentage"),
        legend = list(title = list(text = "Turnover Status"))  # Legend title
      )
  })
  
  # ---------- Predictor Importance ---------- #
  
  output$logit_lollipop_plot <- renderPlotly({
    # Extract logistic regression coefficients
    coef_summary <- as.data.frame(summary(lr_model)$coefficients)
    coef_summary <- coef_summary %>%
      mutate(
        Feature = rownames(coef_summary),
        Odds_Ratio = exp(Estimate),
        Significance = ifelse(`Pr(>|z|)` < 0.05, "Significant", "Not Significant")
      ) %>%
      filter(Feature != "(Intercept)")
    
    # Define colors for the groups
    significance_colors <- c("Significant" = "#006DB4", "Not Significant" = "gray")
    
    # Create lollipop chart using plotly
    plot_ly() %>%
      # Add lines for lollipop sticks
      add_segments(
        data = coef_summary,
        x = ~1, xend = ~Odds_Ratio,
        y = ~reorder(Feature, Odds_Ratio), yend = ~reorder(Feature, Odds_Ratio),
        line = list(color = "gray", width = 1),
        showlegend = FALSE
      ) %>%
      # Add points for the lollipop heads
      add_trace(
        data = coef_summary,
        x = ~Odds_Ratio,
        y = ~reorder(Feature, Odds_Ratio),
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          color = ~significance_colors[Significance],  # Explicitly map colors
          opacity = 0.8
        ),
        text = ~paste0(
          "Feature: ", Feature, "<br>",
          "Odds Ratio: ", round(Odds_Ratio, 2), "<br>",
          "Significance: ", Significance
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Logistic Regression: Odds Ratios for Predictors",
        xaxis = list(
          title = "Odds Ratio",
          showgrid = FALSE  # Remove vertical gridlines
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE  # Remove horizontal gridlines
        ),
        margin = list(l = 150),  # Add extra space for long labels
        annotations = list(
          list(
            x = 0.5,  # Position in the center of the chart
            y = -0.3,  # Move the annotation further below the chart
            text = "Note: Blue indicates significant predictors (p < 0.05), gray indicates non-significant predictors.",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            font = list(size = 12)
          )
        )
      )
  })
  
  output$rf_var_importance_plot <- renderPlotly({
    importance_rf <- as.data.frame(importance(rf_model))
    importance_rf <- importance_rf %>%
      rownames_to_column("Feature") %>%
      arrange(desc(MeanDecreaseGini))
    
    plot_ly(
      data = importance_rf,
      x = ~MeanDecreaseGini,
      y = ~reorder(Feature, MeanDecreaseGini),
      type = "bar",
      text = ~paste("Feature:", Feature, "<br>Importance:", round(MeanDecreaseGini, 2)),
      marker = list(color = "#F39C12")
    ) %>%
      layout(
        title = "Random Forest: Predictor Importance",
        xaxis = list(title = "Mean Decrease in Gini Index"),
        yaxis = list(title = "")
      )
  })
  
  
  # ---------- Neural Network Simulator ---------- #
  
  # Create the KPI metric boxes at the top
  
  output$test_accuracy_lr <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "84.89%"),
      subtitle = tags$div(style = "font-size: 80%;", "Accuracy"),
      color = "aqua"
    )
  })
  
  output$precision_lr <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "70.97%"),
      subtitle = tags$div(style = "font-size: 80%;", "Precision"),
      color = "aqua"
    )
  })
  
  output$recall_lr <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "61.45%"),
      subtitle = tags$div(style = "font-size: 80%;", "Recall/Sensitivity"),
      color = "aqua"
    )
  })
  
  output$specificity_lr <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "92.18%"),
      subtitle = tags$div(style = "font-size: 80%;", "Specificity"),
      color = "aqua"
    )
  })
  
  output$f1_lr <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "65.87%"),
      subtitle = tags$div(style = "font-size: 80%;", "F1 Score"),
      color = "aqua"
    )
  })
  
  output$test_accuracy_nn <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "96.55%"),
      subtitle = tags$div(style = "font-size: 80%;", "Accuracy"),
      color = "blue"
    )
  })
  
  output$precision_nn <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "92.74%"),
      subtitle = tags$div(style = "font-size: 80%;", "Precision"),
      color = "blue"
    )
  })
  
  output$recall_nn <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "92.74%"),
      subtitle = tags$div(style = "font-size: 80%;", "Recall/Sensitivity"),
      color = "blue"
    )
  })
  
  output$specificity_nn <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "97.74%"),
      subtitle = tags$div(style = "font-size: 80%;", "Specificity"),
      color = "blue"
    )
  })
  
  output$f1_nn <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "92.74%"),
      subtitle = tags$div(style = "font-size: 80%;", "F1 Score"),
      color = "blue"
    )
  })
  
  output$test_accuracy_rf <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "99.20%"),
      subtitle = tags$div(style = "font-size: 80%;", "Accuracy"),
      color = "navy"
    )
  })
  
  output$precision_rf <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "99.24%"),
      subtitle = tags$div(style = "font-size: 80%;", "Precision"),
      color = "navy"
    )
  })
  
  output$recall_rf <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "97.39%"),
      subtitle = tags$div(style = "font-size: 80%;", "Recall/Sensitivity"),
      color = "navy"
    )
  })
  
  output$specificity_rf <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "99.77%"),
      subtitle = tags$div(style = "font-size: 80%;", "Specificity"),
      color = "navy"
    )
  })
  
  output$f1_rf <- renderValueBox({
    valueBox(
      value = tags$div(style = "font-size: 70%; font-weight: bold;", "98.31%"),
      subtitle = tags$div(style = "font-size: 80%;", "F1 Score"),
      color = "navy"
    )
  })
  
  # Reactive value to track the selected model
  selected_model <- reactiveVal(NULL)
  output_data <- reactiveVal(NULL)
  
  # Observe model changes and reset graph
  observeEvent(input$model_choice, {
    selected_model(input$model_choice)  # Update the reactive value
    output_data(NULL)  # Reset model output
    output$adjusted_predictions <- NULL  # Reset the plot
    cat("Model switched to:", input$model_choice, "\n")
  })
  
  # Reactive to adjust the data based on user inputs and model choice
  adjusted_data <- eventReactive(input$simulate, {
    
    req(input$model_choice)  # Ensure model choice is set
    
    isolate({
      
      # Step 1: Choose the dataset based on the model
      if (input$model_choice == "nn") {
        modified_data <- data.frame(test_data)  # For Neural Network
      } else if (input$model_choice == "lr") {
        modified_data <- test_data_lr  # For Logistic Regression
      } else if (input$model_choice == "rf") {
        modified_data <- test_data_rf  # For Random Forest
      }
      
      # Step 2: Adjust features using sliders
      modified_data <- modified_data %>%
        mutate(
          satisfaction_level = satisfaction_level + input$satisfaction_slider,
          last_evaluation = last_evaluation + input$evaluation_slider,
          average_monthly_hours = average_monthly_hours + input$avg_monthly_hours_slider,
          number_project = number_project + input$number_projects_slider,
          time_spend_company = time_spend_company + input$time_spend_slider
        )
      
      # Step 3: Ensure values remain within valid ranges (common to both models)
      modified_data <- modified_data %>%
        mutate(
          satisfaction_level = pmax(pmin(satisfaction_level, 1), 0),
          last_evaluation = pmax(pmin(last_evaluation, 1), 0),
          average_monthly_hours = pmax(average_monthly_hours, 0),
          number_project = pmax(number_project, 0),
          time_spend_company = pmax(time_spend_company, 0)
        )
      
      # Step 4: Model-specific transformations
      
      if (input$model_choice == "lr") {
        
        # Ensure training data quantiles exist
        
        validate(need(!is.null(train_data_lr), "Training data for quantiles is missing."))
        
        # Calculate quartiles from training data
        
        satisfaction_level_quantiles <- quantile(train_data_lr$satisfaction_level, probs = c(0.25, 0.50, 0.75))
        last_evaluation_quantiles <- quantile(train_data_lr$last_evaluation, probs = c(0.25, 0.50, 0.75))
        average_monthly_hours_quantiles <- quantile(train_data_lr$average_monthly_hours, probs = c(0.25, 0.50, 0.75))
        number_project_quantiles <- quantile(train_data_lr$number_project, probs = c(0.25, 0.50, 0.75))
        time_spend_company_quantiles <- quantile(train_data_lr$time_spend_company, probs = c(0.25, 0.50, 0.75))
        
        # Re-categorize predictors
        
        modified_data <- modified_data %>%
          mutate(
            satisfaction_level_cat = case_when(
              satisfaction_level <= satisfaction_level_quantiles[1] ~ "Low",
              satisfaction_level > satisfaction_level_quantiles[1] & satisfaction_level <= satisfaction_level_quantiles[2] ~ "Lower-Mid",
              satisfaction_level > satisfaction_level_quantiles[2] & satisfaction_level <= satisfaction_level_quantiles[3] ~ "Upper-Mid",
              satisfaction_level > satisfaction_level_quantiles[3] ~ "High"
            ),
            last_evaluation_cat = case_when(
              last_evaluation <= last_evaluation_quantiles[1] ~ "Low",
              last_evaluation > last_evaluation_quantiles[1] & last_evaluation <= last_evaluation_quantiles[2] ~ "Lower-Mid",
              last_evaluation > last_evaluation_quantiles[2] & last_evaluation <= last_evaluation_quantiles[3] ~ "Upper-Mid",
              last_evaluation > last_evaluation_quantiles[3] ~ "High"
            ),
            average_monthly_hours_cat = case_when(
              average_monthly_hours <= average_monthly_hours_quantiles[1] ~ "Low",
              average_monthly_hours > average_monthly_hours_quantiles[1] & average_monthly_hours <= average_monthly_hours_quantiles[2] ~ "Lower-Mid",
              average_monthly_hours > average_monthly_hours_quantiles[2] & average_monthly_hours <= average_monthly_hours_quantiles[3] ~ "Upper-Mid",
              average_monthly_hours > average_monthly_hours_quantiles[3] ~ "High"
            ),
            number_project_cat = case_when(
              number_project <= number_project_quantiles[1] ~ "Low",
              number_project > number_project_quantiles[1] & number_project <= number_project_quantiles[2] ~ "Lower-Mid",
              number_project > number_project_quantiles[2] & number_project <= number_project_quantiles[3] ~ "Upper-Mid",
              number_project > number_project_quantiles[3] ~ "High"
            ),
            time_spend_company_cat = case_when(
              time_spend_company <= time_spend_company_quantiles[1] ~ "Low",
              time_spend_company > time_spend_company_quantiles[1] & time_spend_company <= time_spend_company_quantiles[2] ~ "Lower-Mid",
              time_spend_company > time_spend_company_quantiles[2] & time_spend_company <= time_spend_company_quantiles[3] ~ "Upper-Mid",
              time_spend_company > time_spend_company_quantiles[3] ~ "High"
            )
          ) %>%
          mutate(across(ends_with("_cat"), as.factor))  # Convert categories to factors
      }
      
      # Always return as a data.frame for downstream compatibility
      if (is.matrix(modified_data)) {
        modified_data <- as.data.frame(modified_data)
      }
      
      return(modified_data)
    })
  })
  
  observeEvent(input$simulate, {
    if (input$model_choice == "nn") {
      modified_data <- adjusted_data()
      cat("NN Input Data Dimensions:\n")
      print(dim(modified_data))
      cat("NN Input Data Column Names:\n")
      print(colnames(modified_data))
      cat("Expected Columns for NN:\n")
      print(expected_columns)
    }
  })
  
  observeEvent(input$simulate, {
    req(adjusted_data())  # Ensure adjusted data is available
    
    isolate({
      # Prepare baseline data by reversing slider adjustments
      baseline_data <- adjusted_data() %>%
        mutate(
          satisfaction_level = satisfaction_level - input$satisfaction_slider,
          last_evaluation = last_evaluation - input$evaluation_slider,
          average_monthly_hours = average_monthly_hours - input$avg_monthly_hours_slider,
          number_project = number_project - input$number_projects_slider,
          time_spend_company = time_spend_company - input$time_spend_slider
        )
      
      # Function to get predictions based on the model
      get_predictions <- function(model_choice, data) {
        if (model_choice == "nn") {
          # Neural Network requires a matrix
          as.vector(best_mlp_model %>% predict(as.matrix(data[, expected_columns])))
        } else if (model_choice == "rf") {
          # Random Forest works with data.frames
          predict(rf_model, newdata = data, type = "prob")[, 2]  # Probability of "Left"
        } else if (model_choice == "lr") {
          # Logistic Regression works with data.frames
          predict(lr_model, newdata = data, type = "response")
        } else {
          stop("Unsupported model choice")
        }
      }
      
      # Generate predictions for both scenarios
      baseline_predictions <- get_predictions(input$model_choice, baseline_data)
      adjusted_predictions <- get_predictions(input$model_choice, adjusted_data())
      
      # Combine predictions into a single dataset
      combined_results <- tibble(
        Predicted_Probability = c(baseline_predictions, adjusted_predictions),
        Scenario = rep(c("Baseline", "Adjusted"), each = length(baseline_predictions))
      )
      
      # Render the overlay histogram
      output$adjusted_predictions <- renderPlotly({
        plot_ly(
          combined_results,
          x = ~Predicted_Probability,
          color = ~Scenario,
          type = "histogram",
          colors = c("Baseline" = "#006DB4", "Adjusted" = "#F39C12"),
          opacity = 0.4,
          marker = list(line = list(color = "black", width = 1)),
          xbins = list(
            start = 0,  # Start of the x-axis
            end = 1,    # End of the x-axis
            size = 1 / 20  # Bin size to create 20 bins
          )
        ) %>%
          layout(
            title = paste("Turnover Predictions: Baseline vs. Adjusted (", toupper(input$model_choice), ")", sep = ""),
            xaxis = list(title = "Predicted Turnover Probability", range = c(0, 1)),
            yaxis = list(title = "Frequency", range = c(0, 1600)),  # Fix y-axis range
            barmode = "overlay"
          )
      })
    })
  })
}

#-----------------------------------------------------------------------------
# Running the App ------------------------------------------------------------
#-----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)