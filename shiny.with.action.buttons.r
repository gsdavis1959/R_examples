# Adapted from https://stackoverflow.com/questions/45739303/r-shiny-handle-action-buttons-in-data-table

library(shiny)
library(DT)
library(tibble)

#' Programmatically create a Shiny input
#' 
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
shinyInput <- function(FUN, n, id, ...) {
  
  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(seq_len(n), function(i){
    as.character(FUN(paste0(id, i), ...))
  }, character(1))
  
}

shinyApp(
  ui <- fluidPage(
    DTOutput("data"),
    textOutput('myText')
  ),
  
  server <- function(input, output) {
    
    # reactive data frame which creates the number of actionButtons needed
    df <- reactiveVal(
      tibble(
        
        Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
        Motivation = c(62, 73, 3, 99, 52),
        
        # parameters here:
        #   * actionButton - type of input to create
        #   * 5 - how many we need
        #   * 'button_' - the ID prefix
        #   * label - label to show on the button
        #   * onclick - what to do when clicked
        Actions = shinyInput(
          FUN = actionButton,
          n = 5,
          id = 'button_',
          label = "Fire",
          onclick = 'Shiny.setInputValue(\"select_button\", this.id)'
        )
      )
    )
    
    # Table showing the data
    output$data <- DT::renderDT({
      
      df()
      
    }, 
    
    # Don't escape any HTML in the table (i.e. the actionButton)
    escape = FALSE,
    
    # turn off row selection otherwise you'll also select that row when you
    # click on the actionButton 
    selection = 'none'
    )
    
    # When a button is clicked, employee is set to the employee name
    #  associated with the clicked row
    employee <- eventReactive(input$select_button, {
      # take the value of input$select_button, e.g. "button_1"
      # get the button number (1) and assign to selectedRow
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # get the value of the "Name" column in the data.frame for that row
      paste('click on ',df()[selectedRow,"Name"])
    })
    
    # Show the name of the employee that has been clicked on
    output$myText <- renderText({
      
      employee()
      
    })
    
  }
)
