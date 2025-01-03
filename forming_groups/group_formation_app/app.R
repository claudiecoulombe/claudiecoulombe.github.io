library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(plotly)

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
  titlePanel("Group Formation Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Dataset (CSV or Excel)", 
                accept = c(".csv", ".xlsx")),
      uiOutput("id_column_selector"), # ID column dropdown
      uiOutput("column_selector"),    # Column selector for grouping
      div(textOutput("row_count"), style = "margin-bottom: 15px;"), # Adds space below
      textInput("group_sizes", "Specify Group Sizes (comma-separated)", "4, 4, 4"),
      div(actionButton("auto_suggest", "Auto-Suggest Groups"), style = "margin-bottom: 15px;"),
      div(textOutput("total_combinations"), style = "margin-bottom: 15px;"),
      radioButtons("optimize", "Optimize for:",
                   choices = list("Maximize Differences Within Groups" = "maximize", 
                                  "Maximize Similarity Within Groups" = "minimize"),
                   selected = "maximize"),
      numericInput("iterations", "Number of Iterations (for Sampling)", 1000, min = 100, max = 10000),
      actionButton("run", "Run Optimization")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Recursive Brute Force",
                 h3("Explanation"),
                 p("Recursive brute force generates all possible partitions of the dataset into groups while avoiding duplicate group orders. 
                   For example, it treats groups (A, B, C) and (C, B, A) as identical, significantly reducing computational redundancy."),
                 p("It is suitable for small datasets where the total number of partitions is computationally manageable (i.e., 12 or less rows)."),
                 h3("Results"),
                 div(textOutput("optimized_score_recursive"), style = "margin-bottom: 15px;"),
                 DTOutput("recursive_table"),
                 h3("Visualization"),
                 plotlyOutput("recursive_chart")),
        
        tabPanel("Random Sampling",
                 h3("Explanation"),
                 p("Random sampling avoids generating all possible partitions by randomly shuffling and evaluating subsets of the dataset.
                   This approach sacrifices completeness for speed, making it ideal for larger datasets or scenarios where approximate solutions are acceptable."),
                 h3("Results"),
                 div(textOutput("best_score_sampling"), style = "margin-bottom: 15px;"),
                 DTOutput("best_partition_table_sampling"),
                 h3("Visualization"),
                 plotlyOutput("partition_plot_sampling"))
      )
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
    
    # Calculate factorials
    total_combinations <- factorial(n) / prod(factorial(group_sizes))
    
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
      return(list(groups))
    }
    results <- list()
    for (i in seq_along(groups)) {
      if (length(groups[[i]]) < group_sizes[i]) {
        new_groups <- groups
        new_groups[[i]] <- c(new_groups[[i]], remaining[1])
        results <- c(results, all_partitions(new_groups, remaining[-1]))
      }
    }
    results <- Filter(function(x) sum(sapply(x, length)) == sum(group_sizes), results)
    return(results)
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
