library(shiny)
library(DT)

# Sample data
sample_data <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  Age = c(25, 30, 35, 28, 32),
  Department = c("HR", "IT", "Finance", "Marketing", "IT"),
  Salary = c(50000, 75000, 65000, 55000, 70000),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  titlePanel("Editable Table Example"),
  
  fluidRow(
    column(12,
      h4("Double-click any cell to edit it"),
      br(),
      DT::dataTableOutput("editable_table"),
      br(),
      h4("Current Data:"),
      verbatimeTextOutput("current_data")
    )
  ),
  
  fluidRow(
    column(6,
      actionButton("save_data", "Save Changes", class = "btn-success")
    ),
    column(6,
      actionButton("reset_data", "Reset to Original", class = "btn-warning")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the data
  values <- reactiveValues(
    data = sample_data,
    original_data = sample_data
  )
  
  # Render the editable table
  output$editable_table <- DT::renderDataTable({
    DT::datatable(
      values$data,
      editable = list(
        target = "cell",
        disable = list(columns = c(0))  # Disable editing for ID column (0-indexed)
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })
  
  # Handle cell editing
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    
    # Get the edited value
    row_num <- info$row
    col_num <- info$col + 1  # DT uses 0-indexed columns, R uses 1-indexed
    new_value <- info$value
    
    # Update the data
    values$data[row_num, col_num] <- new_value
    
    # Show a brief notification
    showNotification(
      paste("Cell updated: Row", row_num, "Column", names(values$data)[col_num]),
      type = "message",
      duration = 2
    )
  })
  
  # Display current data
  output$current_data <- renderPrint({
    values$data
  })
  
  # Save data action (in real app, you'd save to file/database)
  observeEvent(input$save_data, {
    # Here you would typically save to a file or database
    # For this example, we'll just show a notification
    showNotification(
      "Data saved successfully!",
      type = "success",
      duration = 3
    )
    
    # Optionally, you could save to CSV:
    # write.csv(values$data, "saved_data.csv", row.names = FALSE)
  })
  
  # Reset data to original
  observeEvent(input$reset_data, {
    values$data <- values$original_data
    showNotification(
      "Data reset to original values",
      type = "warning",
      duration = 3
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
