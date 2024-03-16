library(shiny)
library(shinyWidgets)

# Define UI for the module
dynamicCheckboxModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
    mainPanel(
      uiOutput(ns("checkboxes_ui"))
    )
    )
  )
}

dynamicCheckboxModule <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    counter <- reactiveVal(0)

    output$checkboxes_ui <- renderUI({
      new_ids=paste0("checkbox_", 1:3)
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
#    })

    # Automatic status update based on selections
    observe({
      selected_options=lapply(paste0("checkbox_", 1:3), function(id) {
        # Namespace must be stripped for input$id to work in observe/observeEvent
        stripped_id <- gsub(ns(""), "", id)
        # Check if all options have been selected for a checkbox group
        input[[stripped_id]]
      })
      print(selected_options)

        if (!is.null(selected_options) && !any(sapply(selected_options, is.null))) { # Assuming all options selected
         print("YHAOO")
          lapply(paste0("checkbox_", 1:3), function(id) {
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
