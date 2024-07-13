library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(reactable)

# Sample input table
input_data <- data.frame(
  list(
    Unique_Identifier = c(1, 2),
    Item = c("Costco", "Target"),
    Category = c("Groceries", "Shopping"),
    Month = c("07/2024", "07/2024"),
    Price = c(108.70, 54.23),
    CL_Ratio = c("80%", "50%")
  )
)

# Input UID
input_unique_identifier <- 1

# Cards Module
cards <- list(
  
  # Components for the settlement table/inputs
  entries_card = card(
    card_header(
      h4("Settlement Entry")
    ),
    card_body(
      reactable::reactableOutput("settlement_entries"),
      layout_columns(
        col_widths = c(3, 3, 2, 2, 2),
        textInput(
          "item_input", 
          "Item"
        ),
        selectInput(
          "category_input", 
          "Category", 
          choices = c(
            "Rent",
            "Groceries",
            "Travel",
            "Shopping",
            "Wedding",
            "Credit",
            "Income",
            "Other"
          )
        ),
        textInput(
          "month_input",
          "Month"
        ),
        numericInput(
          "price_input",
          "Price",
          0
        ),
        numericInput(
          "cl_input",
          "CL %",
          50
        )
      ),
      actionButton(
        "submit_entry",
        "Submit"
      )
    )
  )
)

# UI Module
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "lux"),
  title = "Settlement UI",
  sidebar = sidebar(
    title = "Page Settings",
    open = FALSE
  ),
  layout_columns(
    col_widths = c(6),
    cards$entries_card
  )
)

# Server Module
server <- function(input, output) {
  
  # Reactive data frame representing current settlement data 
  current_entries <- reactiveVal(input_data)
  
  # Reactive value for current unique identifier
  current_unique_identifier <- reactiveVal(input_unique_identifier)
  
  # Entries table with a delete button
  output$settlement_entries <- renderReactable({
    
    current_entries() %>% 
      cbind(delete = NA) %>% 
      reactable(
        bordered = TRUE,
        compact = TRUE,
        outlined = TRUE,
        columns = list(
          Unique_Identifier = colDef(
            name = "UID"
          ),
          delete = colDef(
            name = "",
            sortable = FALSE,
            cell = function() htmltools::tags$button("Remove")
          )
        ),
        onClick = JS("function(rowInfo, colInfo) {
          // Only handle click events on the 'details' column
          if (colInfo.id !== 'delete') {
            return
          }
          if (window.Shiny) {
            Shiny.setInputValue('settlement_remove', { index: rowInfo.index + 1 }, { priority: 'event' })
          }
        }"
      )
    )
  })
  
  # Event triggered by settlement delete button
  observeEvent(input$settlement_remove, {
    
    # Pull information about settlement to be deleted
    uid <- current_entries() %>% 
      purrr::pluck("Unique_Identifier", input$settlement_remove$index) 
    
    # Update the current settlement table data
    new_entries <- current_entries() %>% 
      dplyr::filter(
        Unique_Identifier != uid
      )
    current_entries(new_entries)
    
  })
  
  # Event triggered by clicking the Submit button submit_entry
  observeEvent(input$submit_entry, {
    
    new_entry <- data.frame(list(
      Unique_Identifier = current_unique_identifier() + 1,
      Item = input$item_input,
      Category = input$category_input,
      Month = input$month_input,
      Price = input$price_input,
      CL_Ratio = input$cl_input
    ))
    
    current_entries() %>% 
      rbind(new_entry) %>% 
      current_entries()
      
    
  })
  
  
}

shinyApp(ui, server)
