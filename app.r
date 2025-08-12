library(shiny)
library(DT)

# Sample data - small test dataset
sample_data <- data.frame(
  ID = 1:4,
  Product = c("Laptop", "Mouse", "Keyboard", "Monitor"),
  Price = c(999.99, 29.99, 79.99, 249.99),
  Stock = c(15, 50, 30, 8),
  Category = c("Electronics", "Accessories", "Accessories", "Electronics"),
  stringsAsFactors = FALSE
)

# Define UI - Simplified for Posit Connect
ui <- fluidPage(
  # Basic meta tags only
  tags$head(
    tags$meta(name = "viewport", 
             content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    
    # Custom CSS for better appearance
    tags$style(HTML("
      @media (max-width: 768px) {
        .container-fluid {
          padding: 10px;
        }
        .btn-block {
          margin-bottom: 10px;
        }
        .dataTables_wrapper {
          font-size: 12px;
        }
      }
      
      .alert-info {
        background-color: #d1ecf1;
        border-color: #bee5eb;
        color: #0c5460;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      
      .table-container {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
    "))
  ),
  
  titlePanel("Editable Product Inventory Table"),
  
  div(class = "container-fluid",
    fluidRow(
      column(12,
        div(class = "alert alert-info",
          HTML("<strong>📝 Instructions:</strong> Double-click any cell (except ID) to edit it. 
               Press Enter to save your changes.")
        )
      )
    ),
    
    fluidRow(
      column(12,
        div(class = "table-container",
          DT::dataTableOutput("editable_table")
        )
      )
    ),
    
    fluidRow(
      column(4,
        actionButton("save_data", "💾 Save Changes", 
                    class = "btn-success btn-block")
      ),
      column(4,
        actionButton("reset_data", "↶ Reset to Original", 
                    class = "btn-warning btn-block")
      ),
      column(4,
        actionButton("add_row", "➕ Add New Product", 
                    class = "btn-primary btn-block")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
        h4("📊 Current Data Preview:"),
        div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
          verbatimTextOutput("current_data")
        )
      )
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
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 2, 3)),  # Center align ID, Price, Stock
          list(width = "80px", targets = c(0, 3)),              # Set width for ID and Stock
          list(width = "100px", targets = 2)                    # Set width for Price
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#337ab7', 'color': '#fff'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = "display table-striped table-hover"
    ) %>%
      DT::formatCurrency("Price", currency = "$", digits = 2) %>%
      DT::formatStyle(
        "Stock",
        backgroundColor = DT::styleInterval(c(10, 25), c("#ffcccc", "#ffffcc", "#ccffcc"))
      )
  })
  
  # Handle cell editing
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    
    # Get the edited information
    row_num <- info$row
    col_num <- info$col + 1  # DT uses 0-indexed columns, R uses 1-indexed
    new_value <- info$value
    col_name <- names(values$data)[col_num]
    
    # Type conversion based on column
    if (col_name == "Price") {
      new_value <- as.numeric(new_value)
    } else if (col_name == "Stock") {
      new_value <- as.integer(new_value)
    }
    
    # Update the data
    values$data[row_num, col_num] <- new_value
    
    # Show notification with better formatting
    showNotification(
      paste0("✅ Updated ", col_name, " for ", values$data[row_num, "Product"], 
             " to: ", new_value),
      type = "message",
      duration = 3
    )
  })
  
  # Add new row functionality
  observeEvent(input$add_row, {
    new_id <- max(values$data$ID) + 1
    new_row <- data.frame(
      ID = new_id,
      Product = "New Product",
      Price = 0.00,
      Stock = 0,
      Category = "Uncategorized",
      stringsAsFactors = FALSE
    )
    
    values$data <- rbind(values$data, new_row)
    
    showNotification(
      "➕ New row added! Double-click cells to edit.",
      type = "success",
      duration = 3
    )
  })
  
  # Display current data
  output$current_data <- renderPrint({
    values$data
  })
  
  # Save data action
  observeEvent(input$save_data, {
    # For Posit Connect, we'll show the save action but won't actually write files
    # since file system access may be restricted
    showNotification(
      "💾 Data changes saved to session! In production, this would save to database.",
      type = "success",
      duration = 4
    )
    
    # You can add actual save logic here based on your deployment needs
    # For example: database connection, API calls, etc.
  })
  
  # Reset data to original
  observeEvent(input$reset_data, {
    values$data <- values$original_data
    showNotification(
      "↶ Data reset to original values",
      type = "warning",
      duration = 3
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
