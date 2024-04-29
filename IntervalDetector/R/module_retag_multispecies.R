VERBOSE=F
IMG_PER_PAGE=10
retagMultiUI = function(id, appLang) {
  ns = NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("ctid_select"), appLang$sleectCTTooltip, choices = NULL),
        selectInput(ns("interval_select"), appLang$selectSequenceTooltip, choices = NULL),
        sliderInput(ns("imgSize"), appLang$pictureSizeTooltip,
                    min = 100, max = 2000,
                    value = 300, step = 10, round = 0)  # Adding the sliderInput here
      ,
      radioGroupButtons(
        inputId = ns("onlyShow"),
        label = "Only show",
        choiceNames= c(appLang$onlyShowAll, appLang$onlyShowIncomplete, appLang$markedForAttention),
        choiceValues =c("all", "incomplete", "marked for attention"),
          status = "primary"
      ),
      hr(),
      h4("Information"),
      uiOutput(ns("numOfEventsText")),
      tableOutput(ns("referenceStatusTable"))
      , 
      width=2 # sidebar panel width
      ),
      mainPanel(
        br(),
        actionBttn(
          inputId=ns("markAttention"),
          label=appLang$markedForAttention,
          style="material-flat",
          color = "danger"
          ),
        uiOutput(ns("photos_display")),
        fluidRow(
          column(1,
          actionBttn(
            inputId = ns("previousPage"),
            icon = icon("arrow-left")
            )
          ),
          column(2,
          selectInput(ns("pageSelect"), "Page", choices = 1)
          ),
          column(1, 
          actionBttn(
            inputId = ns("nextPage"),
            icon = icon("arrow-right")
            )
          )
        ),
        actionBttn(
          inputId = ns("markAsComplete"),
          label = "mark as complete", 
            style = "material-flat",
          color = "success"
        )
      )
    )
  )
}

retagMultiServer = function(id, merged_dt, species_dt, appLang, savedRetag, rootPath, dataDir) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    # Init state of the checkboxes
    ## this is used to determine if the user has just changed the view
    ## and will prevent the observeEvent for the checkboxes from triggering
    ## this is necessary because for some reason the observer is triggered when the view is changed
    initState=reactiveVal(FALSE)
    observersAlreadyCalled=reactiveVal()

    # stores images and their tagged species
    userSelections = reactiveVal(data.frame(fn = character(), species = character(), stringsAsFactors = FALSE))
    displayedImages = reactiveVal(list())  # To track displayed images and their checkbox IDs
    


    # stores events and their status
    # eventsStatus = reactive({
    #   req(merged_dt())  
    #   return(unique(data.table(ctid=merged_dt()$ctid, interval=merged_dt()$interval, status="wip")))
    # })

    dataToDisplay=reactiveVal()
    eventsStatus=reactiveVal()
    observeEvent(merged_dt(), {
      if(VERBOSE) print("MERGED_DT OBSERVE CALLED")
      if(is.null(savedRetag$status)) eventsStatus(unique(data.table(ctid=merged_dt()$ctid, interval=merged_dt()$interval, status="wip")))
      else eventsStatus(savedRetag$status)
      if(is.null(savedRetag$tags)) {
        userSelections(data.frame(fn = character(), species = character(), stringsAsFactors = FALSE))
      }
      else {
        userSelections(savedRetag$tags)
      } 
      dataToDisplay(merged_dt())
    })
    
    numberOfPages = reactive({
      req(dataToDisplay())
      #if(!nrow(dataToDisplay)) return(1)
      nrow(dataToDisplay()[ctidint == paste(input$ctid_select, input$interval_select)]) %/% IMG_PER_PAGE + 1
    })

    # previous page button observer. Decreases the page number by 1 except if it is already 1
    observeEvent(input$previousPage, {
      if(input$pageSelect > 1){
        updateSelectInput(session, "pageSelect", choices=1:numberOfPages(), selected = as.integer(input$pageSelect) - 1)
      }
    })

    # next page button observer. Increase the page number by 1 except if it is already the last page
    observeEvent(input$nextPage, {
      if(input$pageSelect < numberOfPages()){
        updateSelectInput(session, "pageSelect", choices=1:numberOfPages(), selected = as.integer(input$pageSelect) + 1)
      }
    })

    observe({
      req(dataToDisplay())
      if(VERBOSE) print("CTID/DTD OBSERVE CALLED")
      # Update Camera ID selectInput choices
      these_choices=unique(dataToDisplay()$ctid)
      if(VERBOSE) print(glue("THESE_CHOICES: {these_choices[1]}"))
      updateSelectInput(session, "ctid_select", choices = these_choices, selected=these_choices[1])
      
    })

    # Observe selection of ctid to update intervals
    observeEvent(input$ctid_select, {
      req(dataToDisplay())
      if(VERBOSE) print("INTERVAL OBSERVE CALLED")
        if(!is.null(input$ctid_select)){
      if(VERBOSE) print("INTERVAL SELECT CALLED")
            selected_ctid = input$ctid_select
            these_choices=dataToDisplay()[ctid == selected_ctid]$interval
            if(VERBOSE) print(glue("THESE_CHOICES: {these_choices[1]}"))
            updateSelectInput(session, "interval_select", choices = these_choices, selected=these_choices[1])
        }
    })

  triggerRedraw=function(){
    req(merged_dt())
    req(eventsStatus())
    if(VERBOSE) print("TRIGGER REDRAW FN CALLED")
    if(VERBOSE) print(glue("onlyShow: {input$onlyShow}"))
    if(input$onlyShow=="all") dataToDisplay(merged_dt())
    if(input$onlyShow=="incomplete") {
      incompletes=eventsStatus()[status=="wip"]
      incompletes[,ctidint:=paste(ctid, interval)]
      dataToDisplay(merged_dt()[ctidint %in% incompletes$ctidint])
    }
    if(input$onlyShow=="marked for attention") {
      incompletes=eventsStatus()[status=="attention"]
      incompletes[,ctidint:=paste(ctid, interval)]
      dataToDisplay(merged_dt()[ctidint %in% incompletes$ctidint])      
    }
  }

  observeEvent(input$onlyShow, {
    if(VERBOSE) print("ONLYSHOW TRIGGERED")
    triggerRedraw()
    initState(T)
  })

  changeEventStatus=function(this_ctid, this_interval, new_status){
    req(eventsStatus())
    if(VERBOSE) print(glue("CHANGE EVENT ST CALLED: ctid: {this_ctid}, interval: {this_interval}, status: {new_status}"))
    stati=copy(eventsStatus())
    stati[ctid == ctid & interval == this_interval, status:=new_status]
    eventsStatus(stati)  
  }

  refreshBtnColors=function(){
    if(VERBOSE) print("REFRESH COL CALLED")
    req(eventsStatus())
    #print(1)
    stati=eventsStatus()
    #print(2)
    this_status=stati[ctid == input$ctid_select & interval == input$interval_select]$status
    #print(3)
    #if(!length(this_status)) this_status="wip"  # if the event is not in the status table, it is a new event
    #print(4)
    if(VERBOSE) print(glue("this status: {this_status} (ctid: {input$ctid_select}, interval: {input$interval_select})"))
    if(this_status=="wip") this_status="primary" else if (this_status=="complete") this_status="success" else this_status="danger"
    if(VERBOSE) print(glue("this status: {this_status} (ctid: {input$ctid_select}, interval: {input$interval_select})"))
    selected_rows = dataToDisplay()[ctidint == paste(input$ctid_select, input$interval_select)]
    # add pages
    selected_rows[,page:=rep(1:(nrow(selected_rows) %/% IMG_PER_PAGE + 1), each=IMG_PER_PAGE)[1:nrow(selected_rows)]]
    # restrict to current page
    selected_rows=selected_rows[page==input$pageSelect]
    if(VERBOSE) print(selected_rows)
    checkbox_ids=paste0(file_path_sans_ext(basename(unique(selected_rows$fn))),  "_species")
    # replace parentheses with underscores
    checkbox_ids=gsub("\\(|\\)", "_", checkbox_ids)
      lapply(checkbox_ids, function(checkbox_id) {
        if(VERBOSE) print(glue("refreshing {checkbox_id}"))
          updateCheckboxGroupButtons(session, checkbox_id,label = "",
          choices=setNames(speciesInSequence()$id, speciesInSequence()$friendlyName),
          selected = input[[checkbox_id]],
          status = this_status)
      })
  }

  observeEvent(input$markAttention, {
    if(VERBOSE) print("MARKEDATTENTION TRIGGERED")
    req(dataToDisplay())
    req(eventsStatus())
    changeEventStatus(input$ctid_select, input$interval_select, "attention")
    refreshBtnColors()
  })


  observeEvent(input$markAsComplete, {
    if(VERBOSE) print("MARKASCOMPLETE TRIGGERED")
    selected_ctid=input$ctid_select
    selected_interval=input$interval_select
    changeEventStatus(selected_ctid, selected_interval, "complete")
    refreshBtnColors()
    triggerRedraw()
    
    # because this is a very granular action we can handle a very granular exception
    ## if there are >1 events in the CT, we need to switch to the next
    these_choices=unique(dataToDisplay()[ctid == selected_ctid]$interval)
      updateSelectInput(session, "interval_select", choices = these_choices, selected=these_choices[1])
  })

    speciesInSequence=reactiveVal()

    checkboxes_and_observers = reactiveValues()

    # observer for the interval_select: generate speciesInSequence and trigger init state
    observeEvent(input$interval_select, {
      if(VERBOSE) print("INTERVAL SELECT OBSERVE CALLED")
      req(eventsStatus()) 
      if(VERBOSE) print(" AFTER REQ")
        selected_ctid=input$ctid_select
        selected_interval=input$interval_select
        if(!is.null(selected_interval)){
          if(VERBOSE) print("SPECIES IN SEQUENCE")
            speciesIDs=unique(unlist(merged_dt()[ctidint == paste(input$ctid_select, input$interval_select)]$species))
            speciesNames=species_dt()[id %in% speciesIDs,.(id, `Common Name`, `Lao Name`, `Species Name`)]
            speciesNames[,friendlyName:=paste(`Common Name`, `Lao Name`, `Species Name`, sep=" ")]
            speciesNames=speciesNames[,.(id, friendlyName)]
            speciesInSequence(speciesNames)
            initState(T)
            # update the page select
            updateSelectInput(session, "pageSelect", choices=1:numberOfPages(), selected=1)
        }
    }, ignoreInit=T
    )

    # reactive to store observers for checkboxes
    checkboxes_and_observers = reactiveValues()

    # Display photographs based on selection
    output$photos_display = renderUI({
      req(dataToDisplay())
    selected_ctid = input$ctid_select

    selected_interval = as.integer(input$interval_select)
    if(!is.null(selected_interval) && !is.na(selected_interval)){
        if(VERBOSE) print("DRAW CALLED")
        selected_rows = dataToDisplay()[ctidint == paste(input$ctid_select, input$interval_select)]
        if(VERBOSE) print(glue("selected rows: {nrow(selected_rows)}"))
        selected_rows[,page:=rep(1:(nrow(selected_rows) %/% IMG_PER_PAGE + 1), each=IMG_PER_PAGE)[1:nrow(selected_rows)]]
        if(VERBOSE) print(selected_rows)
        
        selected_rows=selected_rows[page==input$pageSelect]
        if (nrow(selected_rows) > 0) {
        species_options = setNames(as.character(speciesInSequence()$id), speciesInSequence()$friendlyName) # Get the species names for checkboxes
        image_output_list = lapply(unique(selected_rows$fn), function(filename, species_options) {
            image_id = paste0(ns(basename(filename)), "_image")
            checkboxes_id_pure=paste0(file_path_sans_ext(basename(filename)), "_species")
            # replace parentheses with underscores
            checkboxes_id_pure=gsub("\\(|\\)", "_", checkboxes_id_pure)
            checkboxes_id=ns(checkboxes_id_pure)

            # whenever we redraw here, we want to update the status of the checkboxes to reflect the preexisting selections in userSelections and eventsStatus
            pre_existing_selected_species=NULL
            pre_existing_status="primary"
            userSel=userSelections()
            if(nrow(userSel) && nrow(userSel[fn == filename])){
              if(VERBOSE) print("preexisting selected species")
              pre_existing_selected_species = userSel[fn == filename]
              if(nrow(pre_existing_selected_species)) pre_existing_selected_species=as.character(pre_existing_selected_species$species)
              if(VERBOSE) print(pre_existing_selected_species)
            }

            eventSts=isolate(eventsStatus())
            selected_ctid = isolate(input$ctid_select)
            selected_interval = as.integer(isolate(input$interval_select))  
            if(nrow(eventSts) && nrow(eventSts[ctid == selected_ctid & interval == selected_interval])){
              if(VERBOSE) print("preexisting status")
              pre_existing_status = eventSts[ctid == selected_ctid & interval == selected_interval]$status
              if(VERBOSE) print(pre_existing_status)
              if(pre_existing_status=="wip") pre_existing_status="primary" else if (pre_existing_status=="complete") pre_existing_status="success" else pre_existing_status="danger"
            }

            checkboxes=checkboxGroupButtons(
                inputId=checkboxes_id,
                label="",
                choices=species_options,
                status=pre_existing_status,
                selected=pre_existing_selected_species
            )
            # check if an observer for this checkbox has already been created
            if(!(checkboxes_id_pure %in% names(checkboxes_and_observers))){
              if(VERBOSE) print(glue("creating observer for {checkboxes_id_pure}"))
              checkboxes_and_observers[[checkboxes_id_pure]] = observeEvent(input[[checkboxes_id_pure]], {
                safevv=isolate(input[[checkboxes_id_pure]])
                safev=if(is.null(safevv)) "NULL" else safevv
                if(VERBOSE) print(glue("internal checkbox observe called: {checkboxes_id_pure}={safev}"))
                if(initState()){
                  # we are in init state, which means the user has just changed the view
                  # we must return without doing anything and adding the current state to the init state
                  if(checkboxes_id_pure %in% observersAlreadyCalled()) {initState(F); observersAlreadyCalled(NULL); if(VERBOSE) print("init state expired")}
                  else {
                    observersAlreadyCalled(c(observersAlreadyCalled(), checkboxes_id_pure))
                    if(VERBOSE) print("init state, returning")
                    return()
                  }
                }
                # Find or create entries for the current file
                current_selection = isolate(userSelections())
                current_status=isolate(eventsStatus())
                selected_ctid = isolate(input$ctid_select)
                selected_interval = as.integer(isolate(input$interval_select))
                current_data_to_display=isolate(dataToDisplay())
                eventIsMarkedForAttention=current_status[ctid == selected_ctid & interval == selected_interval]$status=="attention"
                # Filter out existing entries for this file
                current_selection <- current_selection[current_selection$fn != filename, ]
                # if there are existing entries, and the checkboxes are NULL, it means we have just loaded the app and there are preexisting selections
                # we need to update the controls to reflect the preexisting selections

                # Add new entries based on current selections
                if (!is.null(safevv)) {
                    new_entries <- data.table(fn = rep(filename, length(safevv)), 
                                            species = safevv, stringsAsFactors = FALSE)
                    current_selection <- rbind(current_selection, new_entries)
                }

                # if all of the images have been tagged, we need to update the status of the event to complete
                if(VERBOSE) print("CHECKING IF ALL TAGGED")
                if(VERBOSE) print(current_data_to_display[ctidint == paste(selected_ctid, selected_interval)]$fn)
                if(VERBOSE) print("CURRENT SELECTION")
                if(VERBOSE) print(current_selection$fn)
                if(all(current_data_to_display[ctidint == paste(selected_ctid, selected_interval)]$fn %in% current_selection$fn) && !eventIsMarkedForAttention){
                  changeEventStatus(selected_ctid, selected_interval, "complete")
                  refreshBtnColors()
                  triggerRedraw()
                }else{
                  statusToChangeTo=ifelse(eventIsMarkedForAttention, "attention", "wip")
                  # if the event is different from the current status, we need to update the status
                  if(current_status[ctid == selected_ctid & interval == selected_interval]$status != statusToChangeTo){
                    if(VERBOSE) print("CHANGING STATUS from {current_status[ctid == selected_ctid & interval == selected_interval]$status} to {statusToChangeTo}")
                    changeEventStatus(selected_ctid, selected_interval, statusToChangeTo)
                    refreshBtnColors()
                  }
                    
                  
                }
                  

                # Update the reactive value with the new selection
                userSelections(current_selection) 
                }, ignoreInit = T, ignoreNULL = FALSE)
            }
            
            # Combine checkboxes and image for each photo
            tagList(
              checkboxes # Dynamically generated checkboxes
              ,
              withSpinner(imageOutput(image_id, inline=T))
              ,
              hr()
            )
        }, species_options)
        image_output_list
        } else {
          appLang$noPhotosInSelection
        }
    }
    })
    
    # actually render the images
    observe({
      req(dataToDisplay())
      selected_rows = dataToDisplay()[ctidint == paste(input$ctid_select, input$interval_select)]
      if (nrow(selected_rows) > 0) {
        selected_rows=selected_rows
        # if there are more than 10 rows and the thumbnails exist and the requested size is <1000, use the thumbnails
        if(nrow(selected_rows) > 10 && input$imgSize < 1000){
          thumbdir=file.path(dataDir(), "thumbnails")
          selected_rows[,thumbnail_path:=file.path(thumbdir, fn)]
          selected_rows[,thumbnail_exists:=file.exists(thumbnail_path)]
          if(all(selected_rows$thumbnail_exists)){
            lapply(selected_rows$thumbnail_path, function(filename) {
              image_id = paste0((basename(filename)), "_image")
              output[[image_id]] = renderImage({
                list(src = normalizePath(filename), height = input$imgSize)
              }, deleteFile = FALSE)
            })
            return()
          }
          # the return above will prevent the code below from running if all thumbnails exist
          lapply(selected_rows$fn, function(filename) {
            image_id = paste0((basename(filename)), "_image")
            output[[image_id]] = renderImage({
              list(src = normalizePath(glue("{rootPath()}/{filename}")), height = input$imgSize)
            }, deleteFile = FALSE)
          })
          
        }
      }
    })

    update_checkboxes_status = function(status) {
      # Generate selected_rows inside the function
      selected_rows = dataToDisplay()[ctidint == paste(input$ctid_select, input$interval_select)]
      if(!nrow(selected_rows)) return()
      # Get the ids of all currently displayed checkboxes
      checkbox_ids = paste0(file_path_sans_ext(basename(unique(selected_rows$fn))),  "_species")
      # replace parentheses with underscores
      checkbox_ids=gsub("\\(|\\)", "_", checkbox_ids)
      species_options = setNames(speciesInSequence()$id, speciesInSequence()$friendlyName)
      # Update the status of each checkbox
      lapply(checkbox_ids, function(checkbox_id) {
        updateCheckboxGroupButtons(session, checkbox_id, choices = species_options, selected = input[[checkbox_id]], status = status)
      })
    }


  output$numOfEventsText=renderUI({
    p(glue("{nrow(eventsStatus())} total events have multiple species. Completed or marked events will show below."))
  })
  output$referenceStatusTable=renderTable({
    tbl=eventsStatus()[status!="wip"]
    if(nrow(tbl)) tbl else NULL
    })

  return(reactive(list(tags=userSelections(), status=eventsStatus())))
  })
}


retagMultiDemo = function(usePreexisting=F) {
  library(shiny)
  library(data.table)
  library(glue)
  library(shinycssloaders)
  library(shinyWidgets)
  library(tools)
  # Example data.tables
  #taggedEvents_dt = fread("/mnt/c/Users/R.\ Tidi\ Victor/Sync/CameraTrapAI/NKD\ 2022/both_tagged.csv")
  taggedEvents_dt = fread("/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/tagging/eventTagging.csv")
  taggedEvents_dt=unique(taggedEvents_dt[,.(ctid, event, speciesID)])  # this is to remove >1 obs of the same sp
  taggedEvents_dt[,ctidint:=paste(ctid, event)]
  taggedEvents_dt=taggedEvents_dt[,list(.N, list(speciesID)), by=c("ctidint")][N>1]
  #intervals_dt = fread("/mnt/c/Users/R.\ Tidi\ Victor/Sync/CameraTrapAI/NKD\ 2022//metadata/intervals.csv")
  intervals_dt = fread("/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/metadata/intervals.csv")
  

  # Root directory for photographs (adjust path as necessary)
  #old_root_dir="/mnt/d/CT\ II/NKD_2022/Data\ processing/raw_images/"
  old_root_dir="/mnt/t/CT_Data/sus_scrofa/renamed"
  #photo_root_dir = "/mnt/d/CT\ II/NKD_2022/Data\ processing/raw_images"
  photo_root_dir = "/mnt/t/CT_Data/sus_scrofa/renamed"
  #species_dt= fread("/mnt/c/Users/R.\ Tidi\ Victor/Sync/species.final.csv")
  species_dt= fread("/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/metadata/species.csv")

  # Merge the data.tables with adjusted image paths
  setnames(taggedEvents_dt, c("V2"), c("species"))
  #intervals_dt[, fn := sub(old_root_dir, photo_root_dir, fn, fixed = TRUE)]
  intervals_dt[,ctidint:=paste(ctid, interval)]
  #intervals_dt[,c("ctid", "interval"):=NULL]
  merged_dt = merge(taggedEvents_dt, intervals_dt, by = 'ctidint')

  appLang=config::get(file="/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/Shiny_Desktop_App/sadpaR/IntervalDetector/lang.yml", config="English")
  ui = fluidPage(
    theme = "mytheme.css",
    retagMultiUI("photoModule", appLang),
    tableOutput("tags"),
    tableOutput("status")
  )
  
  if(usePreexisting){
    #statusFilePath="/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/NKD 2022/tagging/multipleEventStatus.csv"
    statusFilePath="/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/tagging/multipleEventStatus.csv"
    #tagsFilePath="/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/NKD 2022/tagging/multipleEventTags.csv"
    tagsFilePath="/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/tagging/multipleEventTags.csv"
    savedRetag=list(status=fread(statusFilePath), tags=fread(tagsFilePath))
    #savedRetag=
  }else savedRetag=NULL
  data_dir="/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data"

  server = function(input, output, session) {
    merged_dt[,fn:=sub(old_root_dir, "", fn, fixed = TRUE)]
    retag=retagMultiServer("photoModule", reactiveVal(merged_dt), reactiveVal(species_dt), appLang, savedRetag, rootPath = reactiveVal(old_root_dir), dataDir=reactiveVal(data_dir))
    
    output$tags=renderTable(head(retag()$tags))
    output$status=renderTable(head(retag()$status))
  }
  
  shinyApp(ui, server)
}
