library(shiny)
library(shinyglide)
library(shinyFiles)
library(shinyjs)
library(glue)
library(shinybusy)
library(shinyWidgets)
library(bslib)
library(DT)
library(data.table)
library(EXIFr)
library(lubridate)
library(shinycssloaders)

Sys.setenv(TZ="America/New_York")
options(tz="America/New_York")
# css = "
# .container-fluid {
#   max-width: 700px;
#   padding: 0 20px;
# }
# .shinyglide {
#   border: 1px solid #888;
#   box-shadow: 0px 0px 20px #888;
# }
# .my-control {
#   display: block;
#   position: absolute;
#   top: 50%;
#   transform: translateY(-50%);
#   line-height: 1;
#   font-size: 2.5em;
#   color: #0055DD;
#   opacity: 0.8;
#   cursor: pointer;
# }
# .my-control:hover {
#   opacity: 1;
# }
# .disabled {
#   opacity: 0.3;
#   cursor: not-allowed;
# }
# .prev-screen {
#   left: 20px;
# }
# .next-screen,
# .last-screen {
#   right: 20px;
# }
# .glide__slides {
#   margin: 0 7em;
# }

# @keyframes hourglass {
#   from {
#     transform: translateY(-50%) rotate(0deg);
#   }
#   50% {
#     transform: translateY(-50%) rotate(359deg);
#   }
#   to {
#     transform: translateY(-50%) rotate(360deg);
#   }
# }
# .loading {
#   font-size: 2em;
#   animation: hourglass 1s linear infinite;
# }
# "

createIntervals=function(dateTimeDF, baseDir, outDir, intervalDuration=30){

    # first we identify location and ct
    #datetime[,c("location", "ct"):=tstrsplit(sub("^/+", "", sub(base, "", fn)), "/")[1:2]]
    # start spinner
    show_modal_spinner(text="Creating intervals...")
    print(glue("baseDir: {baseDir}, outDir: {outDir}"))
    baseDirPlatformIndependent=paste0(gsub("[/\\\\]+", "\\[\\/\\\\\\\\\\]", baseDir), "[/\\\\]")
    dateTimeDF[,c("location", "ct"):=tstrsplit(sub(baseDirPlatformIndependent, "", fn), "[/\\\\]")[1:2]]

    # datetime[,c("date", "time"):=tstrsplit(date, " ")]
    # datetime[,dt:=chron(dates=date, times=time, format=c("y:m:d", "h:m:s"))]
    # datetime[,c("date", "time"):=NULL]

    print(dateTimeDF)

    # if there is a date column, rename it to dt
    if("date" %in% names(dateTimeDF))
        setnames(dateTimeDF, "date", "dt")
    

    # if date is not posixct, convert it
    if(!inherits(dateTimeDF$dt, "POSIXct"))
        dateTimeDF[,date:=as.POSIXct(dt, format="%Y:%m:%d %H:%M:%S")]
    
    print(dateTimeDF)


    dateTimeDF[,interval:=lapply(.SD, function(v){
        vdiff=difftime(v[-1], v[-length(v)], units="sec")
        vdiff=c(0, vdiff)
        vdiff=vdiff<intervalDuration*60
        return(cumsum(!vdiff))
    }),.SDcols="dt",by=c("location", "ct")]
    # create metadata directory if it doesn't exist
    metadataDir=file.path(outDir, "metadata")
    if(!dir.exists(metadataDir)){
        dir.create(metadataDir)
    }
    # write the intervals to metadata/interval.csv
    fwrite(dateTimeDF[,.(location, ct, fn, interval)], file.path(metadataDir, "interval.csv"))
    # stop spinner
    remove_modal_spinner()
    sendSweetAlert(
        title = "Intervals created",
        text = glue("{nrow(unique(dateTimeDF[,.(location, ct, interval)]))} intervals have been processed."),
        type = "success"
    )


}


analyzeDirectory=function(dirPath){
    # checks if the input dir has a maximum directory depth of 2 (dirPath/a/b/)
    # and if the first level only contains directories
    # and if the second level only contains files named jpg or jpeg, case insensitive
    # returns T or F
    
    # error if it doesn't exist
    if(!dir.exists(dirPath)){
        return(list(message=glue("Directory {dirPath} does not exist."), value=F))
    }

    # # error if the directory doesn't only contain directories
    # if(!setequal(list.dirs(dirPath, full.names = F, recursive=F), list.files(dirPath, full.names = F, recursive=F))){
    #     return(list(message=glue("Directory {dirPath} contains files."), value=F))
    # }

    #subdirs=list.dirs(dirPath, recursive=T)
    subdirs=getSubDirectories(dirPath)
    subdirs=sub(paste0(gsub("[/\\\\]", ".", dirPath), "."), "", subdirs)
    # remove leading /
    subdirs=sub("^/", "", subdirs)
    # count the / in subdirs
    slashes=nchar(subdirs)-nchar(gsub("[/\\\\]", "", subdirs))
    # if the max number of slashes is not 1, return F
    if(max(slashes)!=1){
        return(list(message=glue("Directory {dirPath} does not have 2 levels of directories: {paste(subdirs[slashes>1], collapse=', ')}"), value=F))
    }

    # check if the second level only contains jpg or jpeg files
    #filesRecursiveWithoutDirs=setdiff(list.files(dirPath, recursive = T, full.names = F), list.dirs(dirPath, recursive = T, full.names = F))
    filesRecursiveWithoutDirs=getFilesRecursiveWithoutDirs(dirPath)
    if(!all(grepl(".*\\.(jpg|jpeg)$", filesRecursiveWithoutDirs, ignore.case = T))){
        return(list(message=glue("Directory {dirPath} contains files that are not jpg or jpeg: {grep('.*\\.(jpg|jpeg)$', filesRecursiveWithoutDirs, ignore.case = T, value=T, invert=T)}"), value=F))
    }
    return(list(value=T, dirs=subdirs, files=filesRecursiveWithoutDirs))
}

getSubDirectories=function(dirPath){
    if(.Platform$OS.type=="windows")
        dirs=shell(glue('for /R "{dirPath}" /D %i in (*) do @echo %i'), intern=T)
    else
        dirs=system(glue('find {dirPath} -type d'), intern=T)
    return(dirs)
}

getFilesRecursiveWithoutDirs=function(dirPath){
    # if windows
    if(.Platform$OS.type=="windows"){
        # use for /R "C:\path\to\your\directory" /D %i in (*) do @echo %i
        # to get subdirectories
        # use for /R "C:\path\to\your\directory" %i in (*) do @echo %i
        # to get files
        # do the difference of both
        files=shell(glue("for /R \"{dirPath}\" %i in (*) do @echo %i"), intern = TRUE)
    } else{
        files=system(glue('find {dirPath} -type f -iname "*.jpg" -o -type f -iname "*.jpeg"'), intern=T)
    }
    return(files)
}

countFilesPerCam=function(filesRecursiveWithoutDirs){
    # we expect a vector of char with the latest part of the path the image
    # and the two previous are the location and CT
    # we want to count the number of files per location and CT
    # we return a data.table with the counts
    resultTable=data.table(files=filesRecursiveWithoutDirs)
    resultTable[,ct:=basename(dirname(files))]
    resultTable[,location:=basename(dirname(dirname(files)))]
    resultTable=resultTable[,files:=NULL][,.(count=.N), by=.(location, ct)]
    return(resultTable)
}



  modal_controls <- glideControls(
      list(
        prevButton(),
        firstButton(
          class = "btn btn-danger",
          `data-dismiss`="modal",
          "Cancel"
        )
      ),
      list(
        nextButton(),
        lastButton(
          class = "btn btn-success",
          `data-dismiss`="modal",
          "Done"
      )
    )
  )

  glide_modal <- # modalDialog(
    # title = "Create new dataset",
    # easyClose = FALSE,
    # footer = NULL,
    fluidRow(
    glide(
      custom_controls = modal_controls,
      screen(
        next_label = 'Next <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
        next_condition = 'input.datasetName != ""',
        h3("Dataset Properties"),
        p("Please input basic data about your dataset."),
        shiny::textInput("landscape", "Landscape"),
        shiny::textInput("block", "Block"),
        shiny::textInput("code", "Code"),
        shiny::textInput("season", "Season"),
        textInput("datasetName", HTML("Dataset name <font color='red'>*</font> <small><i>(required field)</i></small>"), "myDataset")

      ),
      screen(
        next_condition = 'input.loadedDir != ""',
        h3("Input Directory"),
        p("Select an input directory. The following checks will be performed:"),
        tags$ul(
          tags$li("The directory must exist."),
          tags$li("It must only contain directories."),
          tags$li("It must have only 2 levels: subdirectories with stations and sub-subdirectories with camera traps."),
          tags$li("Each camera trap directory must only contain *.jpg or *.jpeg files.")
        ),hr(),br(),
        fluidRow(
          column(12,
            div(class = "input-group align-items-center", 
              div(class = "input-group-prepend",
                shinyDirButton("loadDir", "Load directory", "Load a directory", class = "btn btn-secondary", multiple = F)
              ),
              div(style = "width: 10px;"),  # spacer
              div(style = "display: inline-block; vertical-align: middle;",
                textInput("loadedDirPathTextInput", "", value = "")
              )
            )
          )
        ),
        uiOutput("analyzeButtonUI"),
        #shinycssloaders::withSpinner(
          DTOutput("CTtable")
        #)
        ,
        br()      ),
      screen(
        next_condition = 'input.timestampsLoaded != ""',
        h3("Output Directory"),
        p("Select the directory in which you would like to create the dataset."),
        p("Please note that the dataset directory will be named after the dataset name, and placed in the selected directory."),
        fluidRow(
          column(12, 
            div(class = "input-group align-items-center", 
              div(class = "input-group-prepend",
                shinyDirButton("outputDir", "Output directory", "Select output directory", class = "btn btn-secondary", multiple = F)
              ),
              div(style = "width: 10px;"),  # spacer
              div(style = "display: inline-block; vertical-align: middle;",
                textInput("outputDirPathTextInput", "", value = "")
              )
            )
          )
        ),
        # hidden text saying the next step is to extract timestamps, and a hidden button to do so
        hidden(
          div(id="extractTimestampsDiv",
          h3("Timestamps"),
            p("The next step is to extract timestamps from the images."),
            actionBttn("extractTimestamps", "Extract timestamps")
          )
        ),
        br(),
        # hidden div to show the plot UI output
        hidden(
          div(id="timeRangeDiv",
            timeRangeUI("timeRange")
          )
        )

      ),
      # a screen with shinyFiles buttons to upload a species.csv file and a stations.csv
      screen(
        next_condition = 'input.datasetComplete != ""',
        h3("Species and Stations"),
        p("Please upload the species.csv and stations.csv files."),
        p("The species.csv file should contain the species names and their tags."),
        p("The stations.csv file should contain three columns: 'Station', 'X' and 'Y', the latter two being latitude and longitude, respectively."),
        p("The ct.csv and metadata.csv will also be written here, completing the metadata directory."),
        fluidRow(
          column(12,
            div(class = "input-group align-items-center", 
              div(class = "input-group-prepend",
                shinyFilesButton("speciesFile", "Species file", "Upload species file", accept = ".csv", multiple = F)
              ),
              div(style = "width: 10px;"),  # spacer
              div(style = "display: inline-block; vertical-align: middle;",
                textInput("speciesFilePathTextInput", "", value = "")
              )
            )
          ),
          column(12,
            div(class = "input-group align-items-center", 
              div(class = "input-group-prepend",
                shinyFilesButton("stationsFile", "Stations file", "Upload stations file", accept = ".csv", multiple = F)
              ),
              div(style = "width: 10px;"),  # spacer
              div(style = "display: inline-block; vertical-align: middle;",
                textInput("stationsFilePathTextInput", "", value = "")
              )
            )
          )
        ),
        hr(),
        br(),
        actionBttn("uploadFiles", "Upload files")
      ),
      screen(
        next_condition = 'input.sequencesMade != ""',
        h3("Intervals"),
        p("The next step is to create intervals from the timestamps. This is a quick process."),
        actionBttn("createIntervals", "Create intervals"),
        br(),
        hr(),
        hidden(div(id="makeSequencesDiv",
          h3("Sequences"),
          p("The next step is to create sequences from the intervals. This can take hours."),
          p("Please do not close this window, and do not let your computer go to sleep."),
          p("We recommend the Caffeine apps and extensions to keep your computer awake."),
          actionBttn("makeSequences", "Make sequences")
        ))
      )
    )
    )



ui <- page_fluid(
  theme = bs_add_rules(bs_theme(), 
    rules = 
      ".shinyglide {
        position: relative;
        max-width: 70%;
        margin-inline: auto;
        }"
      )
  ,
  title="Dataset creator",
  useShinyjs(),
  card(
    glide_modal,
    fill=F
  )
)

# ui = fluidPage(
#   tags$head(
#     tags$style(HTML(css))
#   ),
#     useShinyjs(),
#     glide_modal
# )

server <- function(input, output, session) {

#  showModal(glide_modal)
    
    shinyjs::runjs('Shiny.setInputValue("loadedDir", "")')
    shinyjs::runjs('Shiny.setInputValue("timestampsLoaded", "")')
    # disable the text input
    shinyjs::disable("loadedDirPathTextInput")
getRootsInSystem=function(){
    # if windows, get the drives and also the user directory
    if(.Platform$OS.type=="windows"){
        sysdrivereport = system("wmic logicaldisk get caption", intern = TRUE)
        availableDriveLetters=substr(sysdrivereport[-c(1, length(sysdrivereport))], 1, 1)
        drives = setNames(paste0(availableDriveLetters, ":"), glue("{availableDriveLetters} drive"))
        userDir = setNames(Sys.getenv("USERPROFILE"), Sys.getenv("USERNAME"))
        platformIndependentDebugPath="T:/CT_Data/sus_scrofa"
        return(c(drives, userDir, setNames(platformIndependentDebugPath, "debug")))
    }else{
        # if not windows, return what's in /mnt, /, and the home directory
        roots=setNames(c("/mnt", "/", Sys.getenv("HOME")), c("mnt", "root", "home"))
        platformIndependentDebugPath="/mnt/t/CT_Data/sus_scrofa"
        roots=c(roots, setNames(platformIndependentDebugPath, "debug"))
        return(roots)
    }
}

    shinyDirChoose(input, "loadDir", root=getRootsInSystem(), defaultRoot = "debug")

    loadedDirectory=reactiveVal(NULL)
    loadedSubdirs=reactiveVal(NULL)
    loadedFiles=reactiveVal(NULL)

    userSelectedDir=reactiveVal(NULL)

    observeEvent(input$loadDir, {
        req(input$loadDir)
        selectedPath=parseDirPath(roots=getRootsInSystem(), selection=input$loadDir)
        updateTextInput(session, "loadedDirPathTextInput", value=selectedPath)
        userSelectedDir(selectedPath)
    })

    observeEvent(userSelectedDir(), {
        req(userSelectedDir())
        # display an actionbttn to analyze the directory
        output$analyzeButtonUI=renderUI({
          # add some information that the dir now needs to be analyzed
          div(
          p("The directory has been selected. Please click the button below to analyze it."),
            actionBttn("analyzeDir", "Analyze directory")
          )
        })

    })


    observeEvent(input$analyzeDir, {
        chosenDir=input$loadDir
        if("path" %in% names(chosenDir)) {
            show_modal_spinner(text="Analyzing directory")
            roots=getRootsInSystem()
            print(roots)
            thisRoot=roots[chosenDir$root]
            print(thisRoot)
            pathElements=unlist(chosenDir$path)
            pathElements=pathElements[pathElements!=""]
            print(pathElements)
            selectedPath=file.path(thisRoot, do.call(file.path, as.list(pathElements)))
            print(selectedPath)
            print(1)
            directoryIsValid=analyzeDirectory(selectedPath)
            print(2)
            print(str(directoryIsValid))
            if(directoryIsValid$value){
                print("Validation OK")
                loadedDirectory(selectedPath)
                loadedSubdirs(directoryIsValid$dirs)
                loadedFiles(directoryIsValid$files)
                remove_modal_spinner()
                # activate the next button by setting the input
                shinyjs::runjs(glue('Shiny.setInputValue("loadedDir", "{selectedPath}")')) 
                output$CTtable=DT::renderDT({
                    datatable(
                    as.data.table(countFilesPerCam(loadedFiles()))
                    , colnames=c("Camera Trap"="ct", "Number of Images"="count"),
                    rownames=F,
                    options=list(searching=F, ordering=F)
                    )
                })
                # # success message
                # sendSweetAlert(
                #     title = "Success",
                #     text = "The directory is valid.",
                #     type = "success"
                # )
                # update datasetIsValid to enable the extract timestamps button and fill the table
            }else{
                print("Validation KO")
                remove_modal_spinner()
                sendSweetAlert(
                    title = "Error",
                    text = directoryIsValid$message,
                    type = "error"
                )
            }
        }

    })

  shinyDirChoose(input, "outputDir", root=getRootsInSystem(), defaultRoot = "debug")

    outputDirReactive=reactiveVal(NULL)

  observeEvent(input$outputDir, {
    req(input$outputDir)
    selectedPath=parseDirPath(roots=getRootsInSystem(), selection=input$outputDir)
    selectedPath=file.path(selectedPath, input$datasetName)
    updateTextInput(session, "outputDirPathTextInput", value=selectedPath)
        chosenDir=input$outputDir
        if("path" %in% names(chosenDir)) {
            print("Output directory selected")
            outputDirReactive(gsub("[/\\\\]+", "/", file.path(parseDirPath(roots=getRootsInSystem(), selection=chosenDir), input$datasetName)))
            print(outputDirReactive())
        }
    # show the div with the extract timestamps button
    shinyjs::show("extractTimestampsDiv")
  })

  dateTimeDF=reactiveVal(NULL)

  doExtractTimestamps=reactiveVal(FALSE)


    observeEvent(input$extractTimestamps, {
        # does a datetime.csv.gz file exist in the output directory?
        # if it does, ask user if they want to overwrite it or reuse it with a sweetalert
        # if it doesn't, extract the timestamps
        print("Extracting timestamps")
        print(outputDirReactive())
        if(file.exists(file.path(outputDirReactive(), "datetime.csv.gz"))){

        shinyWidgets::confirmSweetAlert(
            inputId = "overwriteDatetime",
            title = "File exists",
            text = "A datetime.csv.gz file already exists in the output directory. Do you want to overwrite it?",
            type = "warning",
            showCancelButton = TRUE,
            btn_labels = c("Reuse", "Overwrite"),
            btn_colors=c("#00796B", "#ff9822")
            )
        } else {
            print("Extracting timestamps")
            doExtractTimestamps(TRUE)
        }
    })

    observeEvent(input$overwriteDatetime, {
        if(input$overwriteDatetime){
            print("Overwriting datetime.csv.gz")
            doExtractTimestamps(TRUE)
        }else{
            print("Reusing datetime.csv.gz")
            datetimedf=fread(file.path(outputDirReactive(), "datetime.csv.gz"))
            # if location is not a column, add it
            if(!"location" %in% names(datetimedf)){
                datetimedf[,location:=basename(dirname(dirname(fn)))]
            }
            # if ct is not a column, add it
            if(!"ct" %in% names(datetimedf)){
                datetimedf[,ct:=basename(dirname(fn))]
            }
            dateTimeDF(datetimedf)
            doExtractTimestamps(FALSE)
        }
    })

    observeEvent(doExtractTimestamps(), {
        if(doExtractTimestamps()){
            imagesToProcess=loadedFiles()
            sendSweetAlert(
                title = glue("Please wait..."),
                btn_labels = NA,
                text = tags$div(
                    progressBar(
                    id = "myprogress",
                    title = glue("Processing {length(imagesToProcess)} images"),
                    display_pct = TRUE, 
                    value = 0
                    ),
                    tags$div(
                    id = "mytext", 
                    tags$p("")
                    )
                ),
                closeOnClickOutside = FALSE,
                backdrop = TRUE
                )
            # do the extraction
            # for each image in imagesToProcess, extract the timestamp with read_exif_tags
            # and add it to the dateTimeDF, update the progress bar each 100 images
            # save the dateTimeDF to a datetime.csv.gz file in the output directory
            # remove the modal

            datetime=data.table()
            # number of images required to increment the progress bar by 1 percent
            percentIncrement=round(length(imagesToProcess)/100)
            print("percentIncrement")
            print(percentIncrement)
            for(i in 1:length(imagesToProcess)){
                if(i %% percentIncrement == 0){
                    updateProgressBar(id="myprogress", value = round(100*i/length(imagesToProcess)))
                    removeUI(selector = "#mytext p", immediate = TRUE)
                    insertUI(selector = "#mytext", ui=tags$p(glue("Processed {i} images of {length(imagesToProcess)}")), immediate = TRUE)
                }
                this_date=tryCatch({
                    date=read_exif_tags(imagesToProcess[i])$DateTime
                }, error=function(e) {print(e$message); return(NA)})
                datetime=rbind(datetime, data.table(fn=imagesToProcess[i], date=this_date))
            }
            fwrite(datetime, file.path(outputDirReactive(), "datetime.csv.gz"))
            # build location and ct
            datetime[,location:=basename(dirname(dirname(fn)))]
            datetime[,ct:=basename(dirname(fn))]
            dateTimeDF(datetime)
            sendSweetAlert(
                title = "Timestamps extracted",
                text = "Timestamps have been extracted and saved to datetime.csv.gz",
                type = "success"
            )

        }
    })

  observe({
    if(!is.null(dateTimeDF())){
      shinyjs::show("timeRangeDiv")
      shinyjs::runjs(glue('Shiny.setInputValue("timestampsLoaded", "{outputDirReactive()}")'))
    }
  })

  timeRangeServer("timeRange", dateTimeDF)

 shinyjs::runjs('Shiny.setInputValue("datasetComplete", "")')

  shinyFileChoose(input, "speciesFile", roots=getRootsInSystem(), defaultRoot = "debug")
  shinyFileChoose(input, "stationsFile", roots=getRootsInSystem(), defaultRoot = "debug")

  # disable the text inputs
  shinyjs::disable("speciesFilePathTextInput")
  shinyjs::disable("stationsFilePathTextInput")

  # observers for the species and stations files that just fill the text inputs
  observeEvent(input$speciesFile, {
    req(input$speciesFile)
    selectedPath=parseFilePaths(roots=getRootsInSystem(), selection=input$speciesFile)
    print(selectedPath)
    print(selectedPath$datapath[1])
    updateTextInput(session, "speciesFilePathTextInput", value=as.character(selectedPath$datapath[1]))
  })

  observeEvent(input$stationsFile, {
    req(input$stationsFile)
    selectedPath=parseFilePaths(roots=getRootsInSystem(), selection=input$stationsFile)
    print(selectedPath)
    print(selectedPath$datapath[1])
    updateTextInput(session, "stationsFilePathTextInput", value=as.character(selectedPath$datapath[1]))
  })



  # observe the upload button, check that the files exist and have the required columns
  # and if they do, write them to the metadata directory
  # and enable the next button
  observeEvent(input$uploadFiles, {
    req(input$speciesFile)
    req(input$stationsFile)
    speciesFile=input$speciesFile
    stationsFile=input$stationsFile
        speciesFilePath=as.character(parseFilePaths(roots=getRootsInSystem(), selection=speciesFile)$datapath[1])
        stationsFilePath=as.character(parseFilePaths(roots=getRootsInSystem(), selection=stationsFile)$datapath[1])
        # species=fread(speciesFilePath)
        # trycatch the above and sendsweetalert an error if load fails
        tryCatch({
            species=fread(speciesFilePath)
        }, error=function(e){
            sendSweetAlert(
                title = "Error",
                text = glue("Error reading species file: {e$message}"),
                type = "error"
            )
            return()
        })

        tryCatch({
            stations=fread(stationsFilePath)
        }, error=function(e){
            sendSweetAlert(
                title = "Error",
                text = glue("Error reading stations file: {e$message}"),
                type = "error"
            )
            return()
        })
        
        # check columns id	Common Name	Lao Name	Species Name	Group	Family	Order in species. Error with the specific columns
        missingcolumns=setdiff(c("id", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order"), names(species))
        if(length(missingcolumns)>0){
            sendSweetAlert(
                title = "Error",
                text = glue("The species file is missing the following columns: {paste(missingcolumns, collapse=', ')}"),
                type = "error"
            )
            return()
        }

        # check columns Station	X	Y
        missingcolumns=setdiff(c("Station", "X", "Y"), names(stations))
        if(length(missingcolumns)>0){
            sendSweetAlert(
                title = "Error",
                text = glue("The stations file is missing the following columns: {paste(missingcolumns, collapse=', ')}"),
                type = "error"
            )
            return()
        }

        # write only the selected columns to the metadata directory
        metadataDir=file.path(outputDirReactive(), "metadata")
        if(!dir.exists(metadataDir)){
            dir.create(metadataDir)
        }
        tryCatch({
        fwrite(species[,c("id", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order")], file.path(metadataDir, "species.csv"))
        fwrite(stations[,c("Station", "X", "Y")], file.path(metadataDir, "stations.csv"))
        # write ct.csv with header Station	Camera ID
        ctdf=countFilesPerCam(loadedFiles())
        fwrite(ctdf[,.(Station=location, `Camera ID`=ct)], file.path(metadataDir, "ct.csv"))
        # write metadata.csv with header Landscape	Block	Code	Season
        fwrite(data.table(Landscape=input$landscape, Block=input$block, Code=input$code, Season=input$season), file.path(metadataDir, "metadata.csv"))


        }, error=function(e){
            sendSweetAlert(
                title = "Error",
                text = glue("Error writing metadata files: {e$message}"),
                type = "error"
            )
            return()
        })
        # enable the next button
        shinyjs::runjs('Shiny.setInputValue("datasetComplete", "")')
        # send success alert saying the number of species and stations written
        sendSweetAlert(
            title = "Files uploaded",
            text = glue("{nrow(species)} species and {nrow(stations)} stations have been written to the metadata directory."),
            type = "success"
        )
        # enable next button
        shinyjs::runjs('Shiny.setInputValue("datasetComplete", "complete")')
  })

  observeEvent(input$createIntervals, {
    createIntervals(dateTimeDF(), loadedDirectory(), outputDirReactive())
    shinyjs::show("makeSequencesDiv")
  })

  observeEvent(input$makeSequences, {
    # check if some sequences already exist
    pathToIntervals=file.path(outputDirReactive(), "metadata", "interval.csv")
    if(checkIfSomeSequencesExist(intervalFile = pathToIntervals, outputDir = outputDirReactive())){
        shinyWidgets::confirmSweetAlert(
            inputId = "overwriteSequences",
            title = "Sequences exist",
            text = "Some sequences already exist. Do you want to overwrite them? If you click 'Reuse', only sequences that are missing will be created. If you click 'Overwrite', all sequences will be (re)created.",
            type = "warning",
            showCancelButton = TRUE,
            btn_labels = c("Reuse", "Overwrite"),
            btn_colors=c("#00796B", "#ff9822")
            )
    } else {
        createSequences(file.path(outputDirReactive(), "metadata", "interval.csv"), file.path(outputDirReactive(), "sequences"), session=session)
    }
  })

  observeEvent(input$overwriteSequences, {
    # createSequences=function(intervalFile, outputDir, maxImagesBeforeDownsampling=100, session=NULL, verbose=F, overwrite=T)
        createSequences(file.path(outputDirReactive(), "metadata", "interval.csv"), file.path(outputDirReactive(), "sequences"), session=session, overwrite=input$overwriteSequences)
  })
}

shinyApp(ui, server, options = list(launch.browser = F, port=8877))