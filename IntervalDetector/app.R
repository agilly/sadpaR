library(data.table)
library(chron)
library(DT)
library(tools)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(glue)
#required to allow up to 100M load
options(shiny.maxRequestSize=100*1024^2)

## LANGUAGE SETTINGS
appLang=config::get(file="lang.yml", config="English")

options(error = function() {
  calls <- sys.calls()
  if (length(calls) >= 2L) {
    sink(stderr())
    on.exit(sink(NULL))
    cat("Backtrace:\n")
    calls <- rev(calls[-length(calls)])
    for (i in seq_along(calls)) {
      cat(i, ": ", deparse(calls[[i]], nlines = 1L), "\n", sep = "")
    }
  }
  if (!interactive()) {
    q(status = 1)
  }
})

printv=function(...){
print(...)
}

# printv=function(...){
#   vars=as.list(match.call())[-1];
#   vals=list(...)
#   print(vals)
#   i=0
#   for(v in vars){
#     cat(paste("================\nVariable", v, "=\n"));
#     print(vals[[i]]);cat("==================\n")
#     i=i+1
#     }
#   }



emptyTaggingTable=data.table(id=integer(), individual=character(),common_name=character(), lao_name=character(), scientific_name=character(), group=character(), family=character(), order=character(), sex=character(), age=character())
emptyInternalTaggingTable=data.table(ctid=character(0), event=integer(0), numInd=integer(0), indID=integer(0), speciesID=integer(0), indName=character(0), sex=integer(0), age=integer(0))


basepath=""

source("lib.R")

server <- function(input, output, session) {
  shinyOptions(progress.style="old")
  loadedDataset <-reactiveValues(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL, imagePath=NULL)
  dur <- reactiveValues(durations=NULL)
  currentTagging=reactiveValues(displayTable=emptyTaggingTable, internalTable=emptyInternalTaggingTable)



  output$animation=renderImage({
    fn=paste(appPaths$sequenceDir, sub(" ", ".", input$whichCT), paste0("sequence.", input$sequence, ".gif"), sep="/")
    #print(fn)
    list(
      src=fn, height=input$imgSize,
      nonce=runif(1)
    )
  }, deleteFile=F)


  observeEvent(input$replay, {
    output$animation=renderImage({
      fn=paste(appPaths$sequenceDir, sub(" ", ".", input$whichCT), paste0("sequence.", input$sequence, ".gif"), sep="/")
      #print(fn)
      list(
        src=fn, height=input$imgSize,
        nonce=runif(1)
      )
    },  deleteFile=F)
  })


  observeEvent(input$previous, {
    req(input$sequence)
    choices=unique((loadedDataset$interval_data)[ctid==input$whichCT]$interval)
    if (as.integer(input$sequence) != choices[1]) {
      currId=which(choices==as.integer(input$sequence))
      newSelection <- choices[currId - 1]
      #print("observe previous called")
      updateSelectInput(session, inputId = "sequence", selected = newSelection)
    }
    if (as.integer(input$sequence) == choices[1]) {
      shinyjs::disable("previous")
    }else{
      shinyjs::enable("previous")
    }
  })


  observeEvent(input$nextButton, {
    req(input$sequence)
    choices=unique((loadedDataset$interval_data)[ctid==input$whichCT]$interval)
    if (as.integer(input$sequence) != choices[length(choices)]) {
      currId=which(choices==as.integer(input$sequence))
      newSelection <- choices[currId + 1]
      #print("observe next called")
      print(currId)
      print(newSelection)
      updateSelectInput(session, inputId = "sequence", selected = newSelection)
      updateSelectInput(session, inputId = "tagSequence", selected=newSelection)
    }
    if (as.integer(input$sequence) == choices[length(choices)]) {
      shinyjs::disable("nextButton")
    }else{
      shinyjs::enable("nextButton")
    }
  })


  output$animationContainer=renderUI({imageOutput("animation", height=input$imgSize)})


  output$imgct=renderText({req(input$sequence);
    X=nrow((loadedDataset$interval_data)[ctid==input$whichCT & interval == as.integer(input$sequence)])
    Y=dur$durations$duration[dur$durations$sequence==input$sequence]
    paste(glue(appLang$durationTooltip))
  })

  output$tagInfo=renderText({
    req(input$whichCTSeq)
    req(input$sequence)
    #updateTaggingOnSeqChange(session, input, output, currentTagging, loadedDataset)
    ctidSel=input$whichCTSeq
    interval = as.integer(input$sequence)
    numTags=nrow(currentTagging$internalTable[ctid==ctidSel & event==interval])
    #print(currentTagging$internalTable)
    #print(input$whichCTSeq)
    if(numTags==0){
      return(appLang$tooltipNoTagYet)
    }
    print(numTags)
    print(ctidSel)
    print(interval)
    print(currentTagging$internalTable[ctid==ctidSel & event==interval]$numInd)

    if(numTags==1 & unique(currentTagging$internalTable[ctid==ctidSel & event==interval]$numInd)==0){
      return(appLang$tooltipTaggedEmpty)
    }
  })

output$CTInEditFrame=renderText({
  req(input$whichCT)
  return(input$whichCT)
})

  df=reactive({loadedDataset$interval_data})



  output$speciesSummary=renderDataTable(loadedDataset$species_data)

  output$frame=renderImage({
    #print(basepath)
    fn=paste(loadedDataset$imagePath, input$PicInSequence, sep="/")
    #print(fn)
    list(
      src=fn, height=input$frameSize,
      nonce=runif(1)
    )
  }, deleteFile=F)

  output$restPath=renderText({loadedDataset$imagePath})
  output$editFrame=renderUI({imageOutput("frame", height=input$frameSize)})



  sexes =reactive({sapply(1:nrow(currentTagging$displayTable), function(i) input[[paste0(get_sex_sel_id(), i)]])})
  ages  =reactive({sapply(1:nrow(currentTagging$displayTable), function(i) input[[paste0(get_age_sel_id(), i)]])})
  observe({
    # this means 1 sex has been changed. We do not know which one so we have to update all.
      #a=isolate(input$num)
      #print(sexes())
      if(!is.null(unlist(sexes()))) {
        currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence, Sex:=sexes()]
      }
      if(!is.null(unlist(ages()))) currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence, Age:=ages()]
      if(!is.null(unlist(sexes())) | !is.null(unlist(ages()))){
        currentInternalTable=currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence]
        dispTable=merge(currentInternalTable, loadedDataset$species_data, by.x="speciesID", by.y="id", all.x=T)
        dispTable=dispTable[ctid==input$tagCT & event==input$tagSequence]
        dispTable[,c("ctid", "event", "numInd", "speciesID"):=NULL]
        setcolorder(dispTable, c("indID", "indName", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order", "Sex", "Age"))
        setnames(dispTable, c("id", "individual", "common_name", "lao_name", "scientific_name", "group", "family", "order", "Sex", "Age"))
        currentTagging$displayTable=dispTable
        existingTags=dispTable
      }

    })


  sex_counter <- reactiveVal(0)
  get_sex_sel_id <- reactive({
    input$tagSequence
    currentTagging$displayTable
    isolate(sex_counter(sex_counter() + 1))
    paste0("sex", sex_counter())
  })


  age_counter <- reactiveVal(0)
  get_age_sel_id <- reactive({
    input$tagSequence
    currentTagging$displayTable
    isolate(age_counter(age_counter() + 1))
    paste0("age", age_counter())
  })

  taggingData <- reactive({
    df <- currentTagging$displayTable

    for (i in 1:nrow(df)) {
      df$Sex[i] <- as.character(selectInput(paste0(get_sex_sel_id(), i),
                                                           "",
                                                           choices = c("", "Male", "Female"),
                                                           selected=df$Sex[i],
                                                           width = "100px"))
      df$Age[i] <- as.character(selectInput(paste0(get_age_sel_id(), i),
                                                           "",
                                                           choices = c("", "Juvenile", "Subadult" ,"Adult"),
                                                           selected=df$Age[i],
                                                           width = "100px"))
    }
    df
  })


  output$taggingTable=renderDataTable(taggingData(), options = list(autoWidth = TRUE, dom='t', paging = FALSE, ordering = FALSE), 
    escape = FALSE, server = FALSE,
  editable=list(target="column", disable=list(columns=c(0,2:10))), rownames=F,callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());"))

  output$speciesSelector=renderDataTable(loadedDataset$species_data, options = list(autoWidth = TRUE), filter = list(position = 'top'), rownames=F, selection="single")


  observeEvent(input$addSpeciesButton, {
    isolate(table <- currentTagging$displayTable)
    #print(currentTagging$displayTable)
    iselected=input$speciesSelector_rows_selected
    if(!is.null(iselected)){
      selectedRow=loadedDataset$species_data[iselected]
      #print(selectedRow)
      nextID=nrow(table)
      #print(nextID)
      newRow=data.table(id=nextID, individual="", common_name=selectedRow[,`Common Name`],
      lao_name=selectedRow[,`Lao Name`], scientific_name=selectedRow[,`Species Name`], group=selectedRow$Group,
      family=selectedRow$Family, order=selectedRow$Order, Sex=NA, Age=NA)
      #print(newRow)

      currentInternalTable=currentTagging$internalTable[ctid==input$whichCTSeq & event==input$tagSequence]
      #print("one")
      #print(currentInternalTable)
      #print(nrow(currentInternalTable))
      if(nrow(currentInternalTable)==1 ){#& 
        if(is.na(currentInternalTable$speciesID)){
        currentTagging$internalTable=currentTagging$internalTable[!(ctid==input$whichCTSeq & event==input$tagSequence)]
      }
      }
      #print("two")
      #numInd must be updated all across
      addInternalTable=data.table(ctid=input$whichCTSeq, event=input$tagSequence, numInd=nrow(currentTagging$displayTable)+1, indID=newRow$id, speciesID=selectedRow$id, indName=newRow$individual, Sex=newRow$Sex, Age=newRow$Age)
      #print("three")
      currentTagging$internalTable=rbind(currentTagging$internalTable, addInternalTable)
      #print("four")
      currentTagging$internalTable[ctid==input$whichCTSeq & event==input$tagSequence, numInd:=nrow(currentTagging$displayTable)+1]
      #print("five")
      # this is needed here because of the triggers attached to displaytable
      currentTagging$displayTable=rbind(table, newRow)
      #print("six")
    }
  })

  observeEvent(input$taggingTable_cell_edit, {
    #print(c(input$taggingTable_cell_edit$row,input$taggingTable_cell_edit$col))
    #print(1)
    currentTagging$displayTable[input$taggingTable_cell_edit$row,2] = input$taggingTable_cell_edit$value
    #print(2)
    currentIdBeingModified=currentTagging$displayTable[input$taggingTable_cell_edit$row,1]
    #print(3)
    #print("Current ID being modified")
    #print(currentIdBeingModified)
    #print("^^^^^^^^^^^^^ ")
    #print(currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence & indID==currentIdBeingModified])
    #print(",,,,,,,,,,,,,,,,,,,,,,")
    #print(input$taggingTable_cell_edit$value)
    #currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence & indID==currentIdBeingModified, indName:=input$taggingTable_cell_edit$value]
    currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence, indName:=input$taggingTable_cell_edit$value]
    #print("============\n=================")

    #print(currentTagging$internalTable[ctid==input$tagCT & event==input$tagSequence & indID==currentIdBeingModified,])
  })

  observeEvent(input$rmSpeciesButton, {
    iselected=input$taggingTable_rows_selected
    idtoremove=currentTagging$displayTable[iselected]$id
    idtoremove2=which(currentTagging$internalTable$ctid==input$whichCTSeq & currentTagging$internalTable$event==input$tagSequence & currentTagging$internalTable$indID %in% idtoremove)
    if(length(idtoremove)!=length(idtoremove2) | length(idtoremove)<1){print(idtoremove);print(idtoremove2);stop("problem with idtoremove")}
    currentTagging$internalTable=currentTagging$internalTable[-idtoremove2]
    if(!is.null(iselected)){
      currentTagging$displayTable=currentTagging$displayTable[-iselected]
    }
    # reset numbering in table
    currentTagging$displayTable[,id:=seq(0, nrow(currentTagging$displayTable)-1)]
    currentTagging$internalTable[ctid==input$whichCTSeq & event==input$tagSequence,indID:=seq(0, nrow(currentTagging$displayTable)-1)]
  })

  output$durationSliderUI=renderUI({
    sliderInput('duration', appLang$maxDurationTooltip,
    min=0, max=max(as.numeric(dur$durations$duration)),
    value=max(as.numeric(dur$durations$duration)),
    step=1, round=0)
  })

  output$seqsummary=renderDataTable(dur$durations, options = list(autoWidth = TRUE), filter = list(position = 'top'),
  rownames=F, colnames=c("average date/time", "sequence number", "number of images", "total duration (min)"))
  output$metadataSummary=renderDT(loadedDataset$metadata)
  output$stationsSummary=renderDT(loadedDataset$stations)
  output$ctSummary=renderDT(loadedDataset$ct)

  output$existingTags=renderDT({req(input$sequence)
    if(!is.null(currentTagging$displayTable) & nrow(currentTagging$displayTable) & !(is.na(currentTagging$displayTable$group[1]))){
      return(currentTagging$displayTable)
    }else{
      return(NULL)
    }

})

  source("internalSelectInputCoherenceAndDurationGeneration.R")
  source("dataMgmt.R")
  internalSelectInputCoherenceAndDurationGeneration(session, input, output, loadedDataset, currentTagging, dur, rootDir)

  output$landscape=renderText({loadedDataset$metadata$Landscape})
  output$block=renderText({loadedDataset$metadata$Block})
  output$code=renderText({loadedDataset$metadata$Code})
  output$season=renderText({loadedDataset$metadata$Season})

  getDrives=function(){
    drives=setNames("/", "root filesystem")
    mountedDrives=list.files("/mnt", full.names=T)
    driveLetters=toupper(list.files("/mnt"))
    driveLetters=paste(driveLetters, "drive")
    drives=c(drives, setNames(mountedDrives, driveLetters))
    drives=c(setNames("/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI", "debug"), drives)
    return(drives)

  }


  appPaths=reactiveValues(taggingCSV="", sequenceDir="")



  shinyDirChoose(input, 'inputFolder', session=session, roots=getDrives())
  rootDir=reactive({parseDirPath(getDrives(), input$inputFolder)})

  observeEvent(input$inputFolder, {
    selectedRootDir=rootDir()
    if(length(selectedRootDir)){
      withProgress(message = appLang$loadingDatasetModal, value = 0, {
        if(checkSelectedFolder(session, input, output, rootDir, loadedDataset, currentTagging, dur, appPaths)){
          loadDataset(session, input, output, rootDir, loadedDataset, currentTagging, dur)
        }
      })
    }
  })

  hideTab(inputId="tabs", target="Sequence")
  hideTab(inputId="tabs", target="Edit")
  hideTab(inputId="tabs", target="Tagging")

  observeEvent(input$editSequenceButton, {
    #showTab(inputId="tabs", target="Edit")
    updateNavbarPage(session, "tabs",
    selected = "Edit")
  })

  observeEvent(input$tagSequenceButton, {
    #showTab(inputId="tabs", target="Edit")
    updateNavbarPage(session, "tabs",
    selected = "Tagging")
  })

  session$onSessionEnded(function() {
    print("performing postflight tasks")
    print(isolate(currentTagging$internalTable))
    fwrite(isolate(currentTagging$internalTable), isolate(appPaths$taggingCSV))
  })



  observeEvent(input$emptySequenceButton, {
    ctidSel=input$whichCT
    sequence=input$sequence
    interval = as.integer(input$sequence)
    numTags=nrow(currentTagging$internalTable[ctid==ctidSel & event==interval])
    if(numTags==0){
      # sequence previously untagged
      currentTagging$internalTable=rbind(currentTagging$internalTable, data.table(ctid=ctidSel, event=sequence, numInd=0, indID=NA, speciesID=NA, indName=NA))
    }else{
      # sequence previously tagged
      currentTagging$internalTable[ctid==ctidSel & event==interval]=data.table(ctid=ctidSel, event=sequence, numInd=0, indID=NA, speciesID=NA, indName=NA)
    }

  })

  observeEvent(input$PicInSequence, {
    print(input$PicInSequence)
    })

  observeEvent(input$previousEdit, {
    req(input$PicInSequence)
    req(input$ChooseEdit)
    seq=as.integer(input$ChooseEdit)
    fn=input$PicInSequence
    chosenCT=input$whichCT
    selectedFn=loadedDataset$interval_data[location==strsplit(chosenCT, " ")[[1]][1] & ct == strsplit(chosenCT, " ")[[1]][2],fn]
    choices=tstrsplit(selectedFn, loadedDataset$imagePath)[[2]]
    if (input$PicInSequence != choices[1]) {
      i=(1:length(choices))[choices==fn]
      newSelection = choices[i-1]
      updateSelectInput(session, inputId = "PicInSequence", selected = newSelection)
    }
    if (input$PicInSequence == choices[length(choices)]) {
      shinyjs::disable("nextEdit")
    }else{
      shinyjs::enable("nextEdit")
    }
    if (input$PicInSequence == choices[1]) {
      shinyjs::disable("previousEdit")
    }else{
      shinyjs::enable("previousEdit")
    }
  })

  observeEvent(input$nextEdit, {
    req(input$PicInSequence)
    req(input$ChooseEdit)
    seq=as.integer(input$ChooseEdit)
    fn=input$PicInSequence
    chosenCT=input$whichCT
    selectedFn=loadedDataset$interval_data[location==strsplit(chosenCT, " ")[[1]][1] & ct == strsplit(chosenCT, " ")[[1]][2],fn]
    choices=tstrsplit(selectedFn, loadedDataset$imagePath)[[2]]
    if (input$PicInSequence != choices[length(choices)]) {
      i=(1:length(choices))[choices==fn]
      newSelection = choices[i+1]
      updateSelectInput(session, inputId = "PicInSequence", selected = newSelection)
    }
    if (input$PicInSequence == choices[length(choices)]) {
      shinyjs::disable("nextEdit")
    }else{
      shinyjs::enable("nextEdit")
    }
    if (input$PicInSequence == choices[1]) {
      shinyjs::disable("previousEdit")
    }else{
      print("enabling previousEdit")
      shinyjs::enable("previousEdit")
    }
  })


}


ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
        position:fixed;
        top: calc(50%);
        left: calc(50%);
      }
      "
    )
  )
),
#titlePanel("Interval detector"),
navbarPage(title=div(img(src="wcs.logo.black.png", style="height:30px"), tags$b("sadpaR"),appLang$appName), id="tabs",
tabPanel(title=appLang$summaryTabLabel,value="Summary",
fluidRow(
  column(2,
    #fileInput("inputFN", "Select tagging dataset"),
    #actionButton("debug", "debug")
    shinyDirButton('inputFolder', appLang$loadDatasetButtonLabel, appLang$loadDatasetModalTitle),
    h5(appLang$loadDatasetTooltip)
  ), column(1,
    h4(glue("{appLang$landscapeField}:")), br(), textOutput("landscape")),

    column(1, h4(glue("{appLang$blockField}:")),br(),textOutput("block")
  ), column(1,
    h4(glue("{appLang$codeField}:")),br(),
    textOutput("code")),
    h4(glue("{appLang$seasonField}:")),br(),
    column(1, textOutput("season"),
    #DTOutput("metadataSummary")
  )

),
fluidRow(
  hr(),
  column(6,
    h3(appLang$intervalsTableLabel),
    selectInput("whichCT", appLang$selectCTTooltip, choices=""),
    DTOutput('seqsummary')
  ),
  column(6,
    h3(appLang$speciesTableLabel),
    DTOutput("speciesSummary")
  )
),
fluidRow(
  hr(),
  column(6,
    h3(appLang$stationsTableLabel),
    DTOutput('stationsSummary')
  ),
  column(6,
    h3(appLang$CTTableLabel),
    DTOutput("ctSummary")
  )
),
hr()
),
tabPanel(title=appLang$sequenceTabLabel, value="Sequence",
sidebarLayout(
  sidebarPanel(
    selectInput("whichCTSeq", appLang$selectCTTooltip, choices=""),
    selectInput("sequence", appLang$selectSequenceTooltip, ""),
    sliderInput('imgSize', appLang$pictureSizeTooltip,
    min=100, max=2000,
    value=300,
    step=10, round=0),
    uiOutput("durationSliderUI")
    #,unique(d$d30m.event))
    ,width=2),

    mainPanel(
      tagAppendAttributes(textOutput("imgct"), class="h4"),
      uiOutput("animationContainer"),
      actionButton("previous", appLang$previousButtonLabel),
      actionButton("replay", appLang$replayButtonLabel),
      actionButton("nextButton", appLang$nextButtonLabel),
      actionButton("emptySequenceButton", appLang$tagEmptyButtonLabel, icon=icon("times-circle", lib="font-awesome")),
      actionButton("editSequenceButton", appLang$editSequenceButtonLabel, icon=icon("pen-to-square", lib="font-awesome")),
      actionButton("tagSequenceButton", appLang$tagSequenceButtonLabel, icon=icon("crow", lib="font-awesome")),br(),
      tagAppendAttributes(textOutput("tagInfo"), class="h4"),
      DTOutput("existingTags")
    )
  )

),
tabPanel(title=appLang$editButtonLabel, value="Edit", sidebarLayout(
  sidebarPanel(
    appLang$CTTooltip,
    tagAppendAttributes(textOutput("CTInEditFrame"), class="h4"),
    hr(),
    sliderInput('frameSize',  appLang$pictureSizeTooltip,
    min=100, max=2000,
    value=300,
    step=10, round=0),
    selectInput("ChooseEdit", appLang$selectSequenceToEdit, choices="",#, unique(d$d30m.event)
    , multiple=T, size=8, selectize=F),
    actionButton("mergeEdit", appLang$mergeSequencesTooltip),
    hr(),
    strong(appLang$pathTooltip),
    verbatimTextOutput("restPath"),
    selectInput("PicInSequence", appLang$listPicturesTooltip, "", size=8, selectize=F),
    actionButton("previousEdit", " ", icon=icon("chevron-up", lib="font-awesome")),
    actionButton("nextEdit", " ", icon=icon("chevron-down", lib="font-awesome")),
    actionButton("splitEdit", " ", icon=icon("cut", lib="font-awesome")),
    br(),
    h6(appLang$warningCut),

    width=2),
    mainPanel(
      uiOutput("editFrame")
    )
  )),
  tabPanel(title=appLang$taggingTabLabel, value="Tagging",
  sidebarPanel(
    selectInput("tagCT", appLang$CTTooltip, choices=""),
    selectInput("tagSequence", appLang$selectSequenceToTag, choices=""),# unique(d$d30m.event)),
    h4(appLang$fixedFields),
    #textInput("block", "Block", value="NCNX"),
    textInput("CTNumber", appLang$CTNumber, value="CT01"),
    textInput("camType", appLang$CTType, value="Bushnell"),
    textInput("locationID", appLang$locationLabel, value="T1-01"),
    textInput("researcher", appLang$researcherLabel, value="Lo1"),
    #textInput("season", "Season", value="2020"),
    #textInput("team", "Team", value="Team 1")

    ,width=2),
    mainPanel(
      h3(appLang$taggingTitle),
      DTOutput('taggingTable'),
      actionButton("rmSpeciesButton", appLang$deleteRow, icon=icon("minus", lib="font-awesome"),
      style="color: #fff; background-color: #df4759; border-color: #8b0000"),
      hr(),
      DTOutput('speciesSelector'),
      actionButton("addSpeciesButton", appLang$addRow, icon=icon("plus", lib="font-awesome"),
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")


    ))
  )

)

shinyApp(ui = ui, server = server)
