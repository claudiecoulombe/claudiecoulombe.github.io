library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(plotly)
library(shinyBS)

# Helper function to calculate the score of groups
score_groups <- function(groups, distance_matrix) {
  score <- 0
  for (group in groups) {
    for (i in 1:(length(group)-1)) {
      for (j in (i+1):length(group)) {
        score <- score + distance_matrix[group[i], group[j]]
      }
    }
  }
  return(score)
}

#-----------------------------------------------------------------------------
# UI Code --------------------------------------------------------------------
#-----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Team Formation Optimizer"),
  
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
      .footer {
        text-align: center;
        font-size: 10px; /* Small font size */
        font-style: italic; /* Italicized text */
        color: #555;
        padding: 10px 0;
        margin-top: 20px;
        border-top: 1px solid #ddd;
        background-color: #f8f9fa;
      }
    "))
  ),
  
  # Use tabPanel for the Introduction page outside the sidebar
  tabsetPanel(
    id = "tabs",
    
    tabPanel("Introduction",
             fluidPage(
               # Hero Section
               div(class = "hero-banner",
                   h1("Team Formation Optimizer"),
                   p("Easily create groups optimized for similarity or differences on selected characteristics.")
               ),
               
               # Features Section
               div(class = "instruction-card",
                   div(class = "section-title", "What This App Can Do"),
                   p("This app allows you to create groups in such a way as to maximize (or minimize) within-group differences on the variables you select, enabling tailored group formation for diverse or cohesive teams."),
                   tags$b("Potential Use Cases:"),
                   tags$ul(
                     tags$li("Creating diverse teams for brainstorming sessions where different perspectives are valued, using variables such as competencies, personality traits, or decision-making styles."),
                     tags$li("Forming cohesive teams for tasks requiring high levels of trust, coordination, or shared expertise."),
                     tags$li("Allocating participants into groups for workshops, training programs, or educational activities based on shared or differing characteristics."),
                     tags$li("Optimizing team assignments in corporate or educational settings to align with specific project goals.")
                   )
               ),
               
               
               # How It Works
               div(class = "instruction-card",
                   div(class = "section-title", "How It Works"),
                   withMathJax(
                   p("This app offers two approaches to optimize group formation:"),
                   tags$ul(
                       tags$li(tags$b("Recursive Brute-Force Partitioning:"), 
                               "This approach uses a recursive algorithm to create all possible ways to divide a dataset into groups of specified sizes, without factoring in the order of the groups. For example, the groups [A, B, C] and [C, B, A] are treated as identical, which eliminates duplicate groupings.",
                               tags$ul(
                                 tags$li("This approach guarantees that the solution we obtain is the optimal one, as it tries all possible combinations."),
                                 tags$li("However, this method is computationally intensive and time-consuming, especially as the number of groups and the size of groups increases. For example, it takes approximately 2 minutes to calculate all combinations for 4 groups of 4 people (a total of 2,627,625 possible combinations)."),
                                 tags$li("To ensure feasibility and performance, the use of this approach is capped to cases where the dataset has 12 or fewer rows/people. For situations that don't meet these criteria, the random sampling approach is recommended.")
                               ),
                             br(),
                             tags$div(class = "panel panel-default",
                                      tags$div(class = "panel-heading", role = "tab",
                                               h4(class = "panel-title",
                                                  tags$a("Technical Details", href = "#recursivePartitioningDetails", "data-toggle" = "collapse", 
                                                         "aria-expanded" = "false", "aria-controls" = "recursivePartitioningDetails")
                                               )
                                      ),
                                      tags$div(id = "recursivePartitioningDetails", class = "panel-collapse collapse",
                                               tags$div(class = "panel-body",
                                                        p("Imagine we have a group of 8 people: [A, B, C, D, E, F, G, H]. We want to divide them into two groups of 3 and one group of 2."),
                                                        br(),
                                                        p(tags$b("Step 1: Define the Problem")),
                                                        tags$ul(
                                                          tags$li("Dataset: [A, B, C, D, E, F, G, H]"),
                                                          tags$li("Group sizes: [3, 3, 2]")
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 2: Define the Base Case")),
                                                        tags$ul(
                                                          tags$li("If only one group is left (e.g., the last group), assign all remaining people to that group. This marks the stopping condition for the recursion.")
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 3: Start with the First Group")),
                                                        tags$ul(
                                                          tags$li("Generate all possible combinations of 3 people to be in that group."),
                                                          tags$li("Examples:"),
                                                          tags$ul(
                                                            tags$li("[A, B, C]"),
                                                            tags$li("[A, B, D]"),
                                                            tags$li("[A, B, E]"),
                                                            tags$li("And so on...")
                                                          ),
                                                          tags$li("The number of combinations for Group 1 is calculated as:"),
                                                          tags$ul(
                                                            tags$li("\\( \\binom{8}{3} = 56 \\)")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 4: Remove Selected People and Partition the Rest")),
                                                        tags$ul(
                                                          tags$li("After selecting a combination for Group 1, remove those people from the dataset and recursively partition the remaining people into the other groups."),
                                                          tags$li("Example:"),
                                                          tags$ul(
                                                            tags$li("If Group 1 = [A, B, C], the remaining people are [D, E, F, G, H].")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 5: Generate Group 2 Combinations")),
                                                        tags$ul(
                                                          tags$li("From the remaining people [D, E, F, G, H], generate all possible combinations of 3 people to form Group 2."),
                                                          tags$li("Examples:"),
                                                          tags$ul(
                                                            tags$li("[D, E, F]"),
                                                            tags$li("[D, E, G]"),
                                                            tags$li("[D, E, H]"),
                                                            tags$li("And so on...")
                                                          ),
                                                          tags$li("The number of combinations for Group 2 is:"),
                                                          tags$ul(
                                                            tags$li("\\( \\binom{5}{3} = 10 \\)")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 6: Assign Remaining People to Group 3")),
                                                        tags$ul(
                                                          tags$li("After selecting a combination for Group 2, assign all remaining people to Group 3."),
                                                          tags$li("Example:"),
                                                          tags$ul(
                                                            tags$li("If Group 2 = [D, E, F], then Group 3 = [G, H].")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 7: Combine Results")),
                                                        tags$ul(
                                                          tags$li("Combine each choice for Group 1 with the results of further recursive steps to create a set of partitions."),
                                                          tags$li("Example Partitions:"),
                                                          tags$ul(
                                                            tags$li("Partition 1: Group 1 = [A, B, C], Group 2 = [D, E, F], Group 3 = [G, H]"),
                                                            tags$li("Partition 2: Group 1 = [A, B, C], Group 2 = [D, E, G], Group 3 = [F, H]"),
                                                            tags$li("Partition 3: Group 1 = [A, B, C], Group 2 = [D, E, H], Group 3 = [F, G]"),
                                                            tags$li("And so on...")
                                                            
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 8: Avoid Duplicate Partitions")),
                                                        tags$ul(
                                                          tags$li("Ensure that partitions like [Group 1 = A, B, C; Group 2 = D, E, F; Group 3 = G, H] are not duplicated as [Group 1 = D, E, F; Group 2 = A, B, C; Group 3 = G, H].")
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 9: Output All Unique Partitions")),
                                                        tags$ul(
                                                          tags$li("Once the recursion completes, the algorithm outputs all unique partitions."),
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 10: Evaluate and Score Each Partition")),
                                                        tags$ul(
                                                          tags$li("For each partition, calculate the total Euclidean distance for each group and sum the results across all groups."),
                                                          tags$ul(
                                                            tags$li("\\( \\text{Distance}(A, B) = \\sqrt{\\sum_{i=1}^n (A_i - B_i)^2} \\)"),
                                                            tags$li("Higher distances indicate greater differences."),
                                                            tags$li("Lower distances indicate smaller differences (greater similarity).")
                                                          )
                                                        ),
                                                        br(),
                                                        p("The app evaluates every partition and selects the one that best aligns with the chosen goal (maximizing differences or similarity within groups).")
                                               )
                                  )
                                
                             )
                     ),
                     tags$li(tags$b("Random Sampling:"),
                             "This method randomly samples a subset of possible configurations, making it faster and scalable for larger datasets. Specifically, all elements (e.g., people) are shuffled such that they are arranged in a random order, and then assigned to groups sequentially to form a given partition. A Euclidean distance score is calculated for the partition. The process is repeated for several iterations (e.g., 1000). ",
                             tags$ul(
                               tags$li("This approach avoids generating all possible combinations, saving significant computation time."),
                               tags$li("It is ideal for datasets with more than 12 rows/people, where Recursive Partitioning becomes infeasible."),
                               tags$li("Although it doesn’t guarantee the most optimal solution, it reliably provides near-optimal results.")
                             ),
                             br(),
                             tags$div(class = "panel panel-default",
                                      tags$div(class = "panel-heading", role = "tab",
                                               h4(class = "panel-title",
                                                  tags$a("Technical Details", href = "#samplingDetails", "data-toggle" = "collapse", 
                                                         "aria-expanded" = "false", "aria-controls" = "samplingDetails")
                                               )
                                      ),
                                      tags$div(id = "samplingDetails", class = "panel-collapse collapse",
                                               tags$div(class = "panel-body",
                                                        p("Imagine we have a group of 16 people: [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]. We want to divide them into 4 groups of 4."),
                                                        br(),
                                                        p(tags$b("Step 1: Define the Problem")),
                                                        tags$ul(
                                                          tags$li("Dataset: [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]"),
                                                          tags$li("Group sizes: [4, 4, 4, 4]")
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 2: Randomly Shuffle the Dataset")),
                                                        tags$ul(
                                                          tags$li("The dataset is shuffled to ensure unbiased group assignments."),
                                                          tags$li("Example: After shuffling, [D, C, A, B, F, E, G, H, J, I, K, L, P, O, M, N].")
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 3: Create Groups")),
                                                        tags$ul(
                                                          tags$li("Based on the shuffled dataset and specified group sizes, people are sequentially assigned to groups."),
                                                          tags$li("Example Groups:"),
                                                          tags$ul(
                                                            tags$li("Group 1: [D, C, A, B]"),
                                                            tags$li("Group 2: [F, E, G, H]"),
                                                            tags$li("Group 3: [J, I, K, L]"),
                                                            tags$li("Group 4: [P, O, M, N]")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 4: Evaluate the Partition")),
                                                        tags$ul(
                                                          tags$li("For each partition, calculate the total Euclidean distance for each group and sum the results across all groups."),
                                                          tags$li("Formula: \\( \\text{Distance}(A, B) = \\sqrt{\\sum_{i=1}^n (A_i - B_i)^2} \\)"),
                                                          tags$ul(
                                                            tags$li("Higher distances indicate greater diversity."),
                                                            tags$li("Lower distances indicate greater similarity.")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 5: Repeat for a Specified Number of Iterations")),
                                                        tags$ul(
                                                          tags$li("The algorithm repeats the shuffling, grouping, and scoring process for a user-defined number of iterations (e.g., 10,000)."),
                                                          tags$li("Example Scores:"),
                                                          tags$ul(
                                                            tags$li("Iteration 1: Score = 245.6"),
                                                            tags$li("Iteration 2: Score = 262.3"),
                                                            tags$li("And so on...")
                                                          )
                                                        ),
                                                        br(),
                                                        p(tags$b("Step 6: Select the Best Configuration")),
                                                        tags$ul(
                                                          tags$li("After completing all iterations, the algorithm identifies the partition with the highest score for diversity or the lowest score for cohesion."),
                                                          tags$li("Example: Best Partition = [E, A, B, D], [G, H, F, C], [L, J, K, I], [N, M, P, O].")
                                                        ),
                                                        br(),
                                                        p("This approach is particularly useful for large datasets, as it balances efficiency and accuracy by avoiding brute-force computation.")
                                               )
                                      )
                             )
                     )
                     )
                   )
               ),
               
               
               # Instructions Section
               div(class = "instruction-card",
                   div(class = "section-title", "How to Use This App"),
                   tags$ol(
                     tags$li(tags$b("Upload Your Dataset (Optional)"), 
                             "Upload your dataset in CSV or Excel format. The dataset should include a unique ID column and numeric variables for analysis. Alternatively, you can use the demo dataset provided to explore the app’s functionality."),
                     tags$li(tags$b("Select Columns"), 
                             "Identify the ID column (e.g., names or unique identifiers) and choose the numeric variables you want to consider for grouping (e.g., competencies, personality traits, or scores)."),
                     tags$li(tags$b("Specify the Number of Groups and Group Sizes"), 
                             "Enter the desired group sizes as a comma-separated list. For example, entering '4, 4, 4' creates three groups of four people each."),
                     tags$li(tags$b("Choose Your Optimization Goal"), 
                             "Select whether you want to maximize differences within groups (for diversity) or minimize differences within groups (for similarity and cohesion)."),
                     tags$li(tags$b("Select the Optimization Approach"), 
                             "Choose either the Recursive Brute Force or Random Sampling approach. Recursive Brute Force is recommended for smaller datasets (12 or fewer rows), while Random Sampling works well for larger datasets."),
                     tags$li(tags$b("Run the Optimization"), 
                             "Click the 'Create Groups' button to start the process. The app will calculate the best group configuration based on your selected criteria and approach."),
                     tags$li(tags$b("Review and Apply Results"), 
                             "Examine the results displayed in tables and visualizations. Use the optimized groups to inform team formation or decision-making.")
                   )
               ),
               
               
               )
    ),
    
    tabPanel("App",
             sidebarLayout(
               
               sidebarPanel(
                 fileInput("datafile", "Upload Dataset (CSV or Excel)", 
                           accept = c(".csv", ".xlsx")),
                 div(textOutput("row_count"),  style = "margin-bottom: 12px;"),
                 uiOutput("id_column_selector"), 
                 uiOutput("column_selector"),
                 div(
                   span(
                     textOutput("total_combinations", inline = TRUE),
                     tags$span(
                       tags$b("ⓘ"), 
                       id = "info_icon",
                       style = "margin-left: 5px; cursor: pointer; color: #007bff; font-size: 16px;"
                     )
                   ),
                   bsTooltip(
                     "info_icon",
                     title = "The total possible combinations are calculated as factorial(total rows) divided by the product of factorials of group sizes, adjusted for group order. For example, if we have 12 people and want to create three groups of 4 people each, the formula is 12! / (4! * 4! * 4! * 3!).",
                     placement = "right",
                     trigger = "hover"
                   ),
                   style = "margin-bottom: 12px;"
                 ),
                 textInput("group_sizes", "Specify Group Sizes (comma-separated)", "4, 4, 4"),
                 div(
                   actionButton("auto_suggest", "Auto-Suggest Group Sizes"),
                   style = "margin-bottom: 12px;"
                 ),
                 radioButtons("optimize", "Optimize for:",
                              choices = list("Maximize Differences Within Groups" = "maximize", 
                                             "Maximize Similarity Within Groups" = "minimize"),
                              selected = "maximize"),
                 numericInput("iterations", "Number of Iterations (for Sampling)", 1000, min = 100, max = 10000),
                 actionButton("run", "Create Groups")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Recursive Brute Force Partitioning",
                            h3("Explanation"),
                            p("Recursive brute force generates all possible partitions of the dataset into groups while avoiding duplicate group orders."),
                            h3("Results"),
                            div(textOutput("optimized_score_recursive"), style = "margin-bottom: 15px;"),
                            DTOutput("recursive_table"),
                            h3("Visualization"),
                            plotlyOutput("recursive_chart")
                   ),
                   tabPanel("Random Sampling",
                            h3("Explanation"),
                            p("Random sampling avoids generating all possible partitions by randomly shuffling and evaluating subsets of the dataset."),
                            h3("Results"),
                            div(textOutput("best_score_sampling"), style = "margin-bottom: 15px;"),
                            DTOutput("best_partition_table_sampling"),
                            h3("Visualization"),
                            plotlyOutput("partition_plot_sampling")
                   )
                 )
               )
             )
    ),
    # Footer section
    div(class = "footer",
        "© Claudie Coulombe 2025. All rights reserved."
    )
  )
)

#-----------------------------------------------------------------------------
# Server Code ----------------------------------------------------------------
#-----------------------------------------------------------------------------

server <- function(input, output, session) {
  # Default dataset path
  default_dataset <- "big_five_scores.csv"
  
  # Reactive data loading
  data <- reactiveVal(read.csv(default_dataset)) # Load default dataset initially
  
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    ext <- tools::file_ext(input$datafile$name)
    if (ext == "csv") {
      data(read.csv(file))
    } else if (ext == "xlsx") {
      data(readxl::read_excel(file))
    } else {
      showNotification("Unsupported file type. Please upload a CSV or Excel file.")
    }
  }) # end of observe event
  
  output$id_column_selector <- renderUI({
    req(data())
    selectInput("id_column", "Select ID Column", 
                choices = colnames(data()), 
                selected = colnames(data())[1]) # Default: first column
  })
  
  output$column_selector <- renderUI({
    req(data())
    # Filter columns to include only numeric or integer variables
    numeric_columns <- colnames(data())[sapply(data(), is.numeric) | sapply(data(), is.integer)]
    
    # Render the selectInput with only numeric/integer columns
    selectInput("columns", "Select Columns for Grouping", 
                choices = numeric_columns, 
                selected = NULL, # No default selection
                multiple = TRUE)
  })
  
  output$row_count <- renderText({
    req(data()) # Ensure the dataset is loaded
    paste("Total number of rows:", nrow(data()))
  })
  
  observeEvent(input$auto_suggest, {
    req(data())
    
    # Total number of rows in the dataset
    total_rows <- nrow(data())
    
    # Suggested number of groups (default: 4)
    num_groups <- 3
    
    # Calculate base group size and remainder
    base_size <- total_rows %/% num_groups
    remainder <- total_rows %% num_groups
    
    # Generate group sizes
    suggested_sizes <- rep(base_size, num_groups)
    if (remainder > 0) {
      suggested_sizes[1:remainder] <- suggested_sizes[1:remainder] + 1
    }
    
    # Update the group_sizes input
    updateTextInput(session, "group_sizes", 
                    value = paste(suggested_sizes, collapse = ","))
    
    showNotification("Group sizes auto-suggested based on dataset size.", type = "message")
  })
  
  output$total_combinations <- renderText({
    req(input$group_sizes, data()) # Ensure group sizes and dataset are available
    
    # Get total number of rows
    n <- nrow(data())
    
    # Parse group sizes
    group_sizes <- as.numeric(unlist(strsplit(input$group_sizes, ",")))
    
    # Validate group sizes
    if (any(is.na(group_sizes)) || sum(group_sizes) != n || n <= 0) {
      return("Invalid group sizes or dataset.")
    }
    
    # Factorial limit handling
    if (n > 20) {
      return("Total combinations are too large to compute.")
    }
    
    # Calculate total combinations
    total_combinations <- factorial(n) / (prod(factorial(group_sizes)) * factorial(length(group_sizes)))
    
    # Return result as formatted text
    paste("Total possible combinations:", format(total_combinations, big.mark = ","))
  })
  
  
  
  # Code to Execute Both Approaches
  
 observeEvent(input$run, {
  # Log inputs for debugging
  print(input$group_sizes)
  print(input$columns)
  print(nrow(data()))
  
   # Ensure data and columns are selected
  if (is.null(data())) {
    showNotification("Dataset not loaded. Please upload a dataset or use the default.", type = "error")
    return(NULL)
  }
  
  if (is.null(input$columns) || length(input$columns) == 0) {
    showNotification("Please select at least one column for grouping.", type = "error")
    return(NULL)
  }
  
  # Subset selected columns
  selected_data <- data() %>%
    select(input$columns)
  
  # Validate group sizes
  if (is.null(input$group_sizes) || input$group_sizes == "") {
    showNotification("Please specify valid group sizes.", type = "error")
    return(NULL)
  }
  group_sizes <- as.numeric(unlist(strsplit(input$group_sizes, ",")))
  if (any(is.na(group_sizes)) || sum(group_sizes) != nrow(selected_data)) {
    showNotification("Group sizes must be numeric and add up to the total number of rows in the dataset.", type = "error")
    return(NULL)
  }
  
  # Ensure numeric columns for distance matrix
  numeric_data <- selected_data %>%
    select(where(is.numeric))
  if (ncol(numeric_data) == 0) {
    showNotification("Please select at least one numeric column for grouping.", type = "error")
    return(NULL)
  }
  print("Data validation successful.")
  
  # Calculate distance matrix
  distance_matrix <- as.matrix(dist(numeric_data))
  print("Distance matrix calculated.")
  
  all_partitions <- function(groups, remaining) {
    if (length(remaining) == 0) {
      # Normalize group order to avoid duplicates
      normalized_groups <- groups[order(sapply(groups, min))]
      return(list(normalized_groups))
    }
    results <- list()
    for (i in seq_along(groups)) {
      if (length(groups[[i]]) < group_sizes[i]) {
        new_groups <- groups
        new_groups[[i]] <- c(new_groups[[i]], remaining[1])
        results <- c(results, all_partitions(new_groups, remaining[-1]))
      }
    }
    # Filter valid partitions and normalize their order
    results <- Filter(function(x) sum(sapply(x, length)) == sum(group_sizes), results)
    results <- lapply(results, function(partition) partition[order(sapply(partition, min))])
    return(unique(results))
  }
  
  
  ### Recursive Brute Force
  
  if (nrow(selected_data) <= 12) {
    showNotification("App running. This may take a few seconds to a few minutes, depending on the number of combinations. Please wait...", type = "message", duration = NULL)
    
    print("Running recursive brute force.")
    leaders <- 1:nrow(selected_data)
    groups <- replicate(length(group_sizes), numeric(0), simplify = FALSE)
    partitions <- all_partitions(groups, leaders)
    
    print(paste("Number of partitions generated:", length(partitions)))
    
    if (length(partitions) == 0) {
      showNotification("No valid partitions generated.", type = "error", duration = 5)
      output$recursive_results <- renderText("No valid partitions generated.")
      return(NULL)
    }
    
    best_score <- if (input$optimize == "maximize") -Inf else Inf
    best_partition <- NULL
    
    for (partition_idx in seq_along(partitions)) {
      partition <- partitions[[partition_idx]]
      current_score <- score_groups(partition, distance_matrix)
      
      # Log progress in the R console
      cat(sprintf("Evaluating partition %d of %d: Current Score = %.2f\n", 
                  partition_idx, length(partitions), current_score))
      flush.console() # Ensure real-time output in the console
      
      # Check if this partition is the best so far
      if ((input$optimize == "maximize" && current_score > best_score) ||
          (input$optimize == "minimize" && current_score < best_score)) {
        best_score <- current_score
        best_partition <- partition
        
        # Log a message when a new best score is found
        cat(sprintf("New best score found at partition %d: %.2f\n", partition_idx, best_score))
      }
    }
    
    if (!is.null(best_partition)) {
      output$recursive_results <- renderText({
        recursive_results <- "Recursive brute force executed. Results:\n"
        for (i in seq_along(best_partition)) {
          group_members <- paste(data()[best_partition[[i]], input$id_column], collapse = ", ")
          recursive_results <- paste0(recursive_results, "Group ", i, ": ", group_members, "\n")
        }
        recursive_results <- paste0(recursive_results, "Optimized Score: ", best_score)
        recursive_results
      })
      print("Results successfully rendered.")
      showNotification("Optimization completed successfully.", type = "message", duration = 5)
    } else {
      output$recursive_results <- renderText("No optimal partition found.")
      showNotification("No optimal partition found during brute force optimization.", type = "warning", duration = 5)
    }
    print("Recursive brute force completed.")
  } else {
    output$recursive_results <- renderText("Dataset too large for recursive brute force.")
    showNotification("Dataset too large for recursive brute force optimization.", type = "warning", duration = 5)
    print("Recursive brute force skipped due to dataset size.")
  }
  
  # Render datatable for the best partition
  if (!is.null(best_partition)) {
    
    # Render the optimized score
    output$optimized_score_recursive <- renderText({
      paste("Optimized Score (Brute Force):", format(best_score, digits = 4))
    })
    
    output$recursive_table <- renderDT({
      # Create a data frame from the best partition
      data.frame(
        Group = seq_along(best_partition),
        Members = sapply(best_partition, function(group) {
          paste(data()[group, input$id_column], collapse = ", ")
        })
      ) %>% 
        datatable(options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
    })
  } else {
    output$recursive_table <- renderDT({
      datatable(data.frame(Message = "No valid partition found."))
    })
  }

  output$recursive_chart <- renderPlotly({
    req(best_partition, input$columns) # Ensure valid partition and selected columns
    
    # Initialize a results data frame
    stats <- data.frame()
    
    for (group_idx in seq_along(best_partition)) {
      group_members <- best_partition[[group_idx]]
      group_data <- data()[group_members, input$columns, drop = FALSE]
      
      # Compute statistics
      means <- colMeans(group_data, na.rm = TRUE)
      stats <- rbind(stats, 
                     data.frame(Group = paste("Group", group_idx),
                                Variable = names(means),
                                Mean = means))
    }
    
    if (nrow(stats) == 0) {
      showNotification("No data available to plot.", type = "error")
      return(NULL)
    }
    
    # Create a grouped bar chart using Plotly
    plot <- plot_ly(data = stats, 
                    x = ~Variable, 
                    y = ~Mean, 
                    color = ~Group, 
                    type = 'bar',
                    opacity = 0.6) %>%
      layout(
        title = "Mean Scores by Group and Variable",
        xaxis = list(title = "Variable"),
        yaxis = list(title = "Mean Score"),
        barmode = "group" # Group bars by variable
      )
    
    plot
  })
  
  ### Random Sampling
  print("Running random sampling.")
  best_score_sampling <- if (input$optimize == "maximize") -Inf else Inf
  best_partition_sampling <- NULL
  
  for (iter in 1:input$iterations) {
    shuffled <- sample(1:nrow(selected_data))
    groups <- list()
    start_idx <- 1
    for (size in group_sizes) {
      end_idx <- start_idx + size - 1
      groups <- append(groups, list(shuffled[start_idx:end_idx]))
      start_idx <- end_idx + 1
    }
    current_score <- score_groups(groups, distance_matrix)
    
    if ((input$optimize == "maximize" && current_score > best_score_sampling) ||
        (input$optimize == "minimize" && current_score < best_score_sampling)) {
      best_score_sampling <- current_score
      best_partition_sampling <- groups
    }
  }
  
  # Render the optimized score
  output$best_score_sampling <- renderText({
    paste("Optimized Score (Random Sampling):", format(best_score_sampling, digits = 4))
  })
  
  output$best_partition_table_sampling <- renderDT({
    data.frame(Group = seq_along(best_partition_sampling),
               Members = sapply(best_partition_sampling, function(group) paste(data()[group, input$id_column], collapse = ", "))) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
  })
  
  output$partition_plot_sampling <- renderPlotly({
    req(best_partition_sampling, input$columns) # Ensure valid partition and selected columns
    
    # Initialize a results data frame
    stats <- data.frame()
    
    for (group_idx in seq_along(best_partition_sampling)) {
      group_members <- best_partition_sampling[[group_idx]]
      group_data <- data()[group_members, input$columns, drop = FALSE]
      
      # Compute statistics (e.g., means for each variable)
      means <- colMeans(group_data, na.rm = TRUE)
      stats <- rbind(stats, 
                     data.frame(Group = paste("Group", group_idx),
                                Variable = names(means),
                                Mean = means))
    }
    
    if (nrow(stats) == 0) {
      showNotification("No data available to plot.", type = "error")
      return(NULL)
    }
    
    # Create a grouped bar chart using Plotly
    plot <- plot_ly(data = stats, 
                    x = ~Variable, 
                    y = ~Mean, 
                    color = ~Group, 
                    type = 'bar',
                    opacity = 0.6) %>%
      layout(
        title = "Mean Scores by Group and Variable",
        xaxis = list(title = "Variable"),
        yaxis = list(title = "Mean Score"),
        barmode = "group" # Group bars by variable
      )
    
    plot
  })
  
  print("Random sampling completed.")
})

}

shinyApp(ui, server)
