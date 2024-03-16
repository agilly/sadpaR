library(shiny)
library(shinyWidgets)

# Define the UI part of the module
checkboxModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selector"), "Choose Update", choices = c("Group 1", "Group 2")),
    hr(),
    checkboxGroupButtons(
      inputId = ns("checkboxes"),
      label = "Checkbox Group Buttons",
      choices = c("Item 1", "Item 2"), # Initial choices
      selected = "Item 1")
    )
  
}

# Define the server part of the module
checkboxModule <- function(input, output, session) {
  observeEvent(input$selector, {
    status_selected <- if (input$selector == "Group 1") {
      "success"
    } else {
      "danger"
    }
    updateCheckboxGroupButtons(session, "checkboxes", label = "Checkbox Group Buttons",
      choices = c("Item 1", "Item 2"), # Initial choices
      selected = "Item 1", status=status_selected)
  })
}


bugDemo=function(){

    # Application UI
    ui <- fluidPage(
    checkboxModuleUI("checkboxMod")
    )

    # Application server
    server <- function(input, output, session) {
    callModule(checkboxModule, "checkboxMod")
    }

    shinyApp(ui, server)
}