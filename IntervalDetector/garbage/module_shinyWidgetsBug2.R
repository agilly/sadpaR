library(shiny)
library(shinyWidgets)

# Define UI for the module
dynamicCheckboxModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_btn"), "Add Checkboxes"),
    uiOutput(ns("checkboxes_ui"))
  )
}

dynamicCheckboxModule <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    counter <- reactiveVal(0)

    # Storage for dynamically created checkbox IDs
    checkbox_ids <- reactiveValues(ids = list())

    observeEvent(input$add_btn, {
      cnt <- counter()
      new_ids <- vector("list", 3)
      
      for (i in 1:3) {
        new_id <- paste0("checkbox_", cnt + i)
        new_ids[[i]] <- new_id
        checkbox_ids$ids[[length(checkbox_ids$ids) + 1]] <- new_id
      }
      
      counter(cnt + 3)
      
      output$checkboxes_ui <- renderUI({
        ui_elems <- lapply(new_ids, function(id) {
          checkboxGroupButtons(
            inputId = ns(id),
            label = paste("Dynamic Checkbox ", id),
            choices = c("Option 1", "Option 2"),
            selected = character(0),
            status = "primary"
          )
        })
        do.call(tagList, ui_elems)
      })
    })

    # Automatic status update based on selections
    observe({
      selected_options=lapply(checkbox_ids$ids, function(id) {
        # Namespace must be stripped for input$id to work in observe/observeEvent
        stripped_id <- gsub(ns(""), "", id)
        # Check if all options have been selected for a checkbox group
        input[[stripped_id]]
      })
    print(selected_options)

        if (!is.null(selected_options) && !any(sapply(selected_options, is.null))) { # Assuming all options selected
          print("all checkboxes have at least 1 selection, change to green")
          lapply(checkbox_ids$ids, function(id) {
            updateCheckboxGroupButtons(session, id,label = paste("Dynamic Checkbox ", id),
            choices = c("Option 1", "Option 2"),
            selected = input[[id]],status = "success")
        }
        )
        }
      })
    
  })
}


# Shiny app UI
ui <- fluidPage(
  dynamicCheckboxModuleUI("dynamic_mod")
)

# Shiny server function
server <- function(input, output, session) {
  dynamicCheckboxModule("dynamic_mod")
}

# Run the app
shinyApp(ui, server)
