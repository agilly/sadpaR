library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)
library(shinyFiles)
library(glue)
library(shinybusy)
library(shinyWidgets)
library(data.table)
library(DT)
library(EXIFr)

options(shiny.autoreload = TRUE)

gear = popover(
  bs_icon("folder-plus"),
  shinyDirButton("loadDir", "Load directory", "Load a directory", multiple = F),
  title = "Load a directory"
)

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
    subdirs=sub(dirPath, "", subdirs)
    # remove leading /
    subdirs=sub("^/", "", subdirs)
    # count the / in subdirs
    slashes=nchar(subdirs)-nchar(gsub("/", "", subdirs))
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
        dirs=system(glue('for /R "{dirPath}" /D %i in (*) do @echo %i'), intern=T)
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
        files=system(glue("for /R \"{dirPath}\" %i in (*) do @echo %i"), intern = TRUE)
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

getRootsInSystem=function(){
    # if windows, get the drives and also the user directory
    if(.Platform$OS.type=="windows"){
        sysdrivereport = system("wmic logicaldisk get caption", intern = TRUE)
        availableDriveLetters=substr(sysdrivereport[-c(1, length(sysdrivereport))], 1, 1)
        drives = setNames(paste0(availableDriveLetters, ":"), glue("{availableDriveLetters} drive"))
        userDir = setNames(Sys.getenv("USERPROFILE"), Sys.getenv("USERNAME"))
        return(c(drives, userDir))
    }else{
        # if not windows, return what's in /mnt, /, and the home directory
        roots=setNames(c("/mnt", "/", Sys.getenv("HOME")), c("mnt", "root", "home"))
        roots=c(roots, setNames("/mnt/t/CT_Data/sus_scrofa", "debug"))
        return(roots)
    }
}

createIntervals=function(dateTimeDF, baseDir, outDir, intervalDuration=30){

    # first we identify location and ct
    #datetime[,c("location", "ct"):=tstrsplit(sub("^/+", "", sub(base, "", fn)), "/")[1:2]]
    # start spinner
    show_modal_spinner(text="Creating intervals...")
    dateTimeDF[,c("location", "ct"):=tstrsplit(sub("^/+", "", sub(baseDir, "", fn)), "/")[1:2]]

    # datetime[,c("date", "time"):=tstrsplit(date, " ")]
    # datetime[,dt:=chron(dates=date, times=time, format=c("y:m:d", "h:m:s"))]
    # datetime[,c("date", "time"):=NULL]

    dateTimeDF[,c("date", "time"):=tstrsplit(date, " ")]
    dateTimeDF[,dt:=as.POSIXct(paste0(date, " ", time), format="%Y:%m:%d %H:%M:%S")]
    dateTimeDF[,c("date", "time"):=NULL]

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

ui = page_fillable(
    shinyjs::useShinyjs(),
    title = "sadpaR dataset creator",
  card(
    card_header("Dataset properties", gear,
    class = "d-flex justify-content-between"),
    title="Dataset generator",
    uiOutput("mainUI")
    )
)

server = function(input, output) {
    loadedDirectory=reactiveVal(NULL)
    loadedSubdirs=reactiveVal(NULL)
    loadedFiles=reactiveVal(NULL)
    output$mainUI = renderUI({
        if(is.null(loadedDirectory())){
            return("Please load a directory to get started.")
        }else{
            tagList(
                verbatimTextOutput("loadedDir"),
                DT::dataTableOutput("CTtable"),
                # 4 inputs for Landscape	Block	Code	Season
                shiny::textInput("landscape", "Landscape"),
                shiny::textInput("block", "Block"),
                shiny::textInput("code", "Code"),
                shiny::textInput("season", "Season"),
                textInput("datasetName", HTML("Dataset name <font color='red'>*</font> <small><i>(required field)</i></small>"), "myDataset"),
                # a shinyFiles dirCreator to select the output where to create the dataset
                shinyjs::hidden(shinyDirButton("outputDir", "Select where to create output directory", "Select where to create the output directory", multiple = F)),
                # verbatim text to show the output directory
                verbatimTextOutput("outputDirPrint"),
                # a shiny action button to create the dataset
                shinyjs::hidden(actionBttn("extractTimestamps", "Extract timestamps", icon = bs_icon("clock-history"))),
                # a hidden input specifying the number of minutes to use as interval
                shinyjs::hidden(numericInput("interval", "Interval (minutes)", 30, min=1, max=60)),
                # a hidden button to call intervals
                shinyjs::hidden(actionBttn("callIntervals", "Call intervals", icon = bs_icon("calendar3-range")))
                
            )
        }
    })

    # reactive for the output directory
    outputDirReactive=reactiveVal(NULL)

    # enable the outputDir button only if a dataset name is provided
    observe({
        if(!length(input$datasetName)){
            shinyjs::hide("outputDir")
        }else{
            shinyjs::show("outputDir")
            if(grepl("[^A-Za-z0-9_]", input$datasetName)){
                # replace all those characters with _
                updatedName=gsub("[^A-Za-z0-9_]", "_", input$datasetName)
                updateTextInput(inputId="datasetName", value=updatedName)
            }
        }
    })


    # hide outputDir and createDataset if no output directory is selected
    observe({
        if(is.null(outputDirReactive())){
            print("Hiding outputDir and createDataset")
            shinyjs::hide("outputDir")
            shinyjs::hideElement("extractTimestamps")
        } else {
            print("Showing outputDir and createDataset")
            print(outputDirReactive())
            shinyjs::show("outputDir")
            output$outputDirPrint=renderPrint(outputDirReactive())
            shinyjs::showElement("extractTimestamps")
        }
    })

    observe({
        if(!is.null(loadedDirectory())){
            output$loadedDir=renderPrint(loadedDirectory())
            subdirs=grep( "/",loadedSubdirs(), value=T)
            print(as.data.table(tstrsplit(subdirs, .Platform$file.sep, fixed=T, type.convert=T, fill=T)))
            output$CTtable=DT::renderDataTable({
                datatable(
                as.data.table(countFilesPerCam(loadedFiles()))
                , colnames=c("Camera Trap"="ct", "Number of Images"="count"),
                options=list(dom="t")
                )
            })
        }
    })

    shinyDirChoose(input, "loadDir", root=getRootsInSystem(), defaultRoot = "debug")

    shinyDirChoose(input, "outputDir", root=getRootsInSystem())

    observe({
        chosenDir=input$outputDir
        if("path" %in% names(chosenDir)) {
            print("Output directory selected")
            outputDirReactive(file.path(parseDirPath(roots=getRootsInSystem(), selection=chosenDir), input$datasetName))
            print(outputDirReactive())
        }
    })

    observe({
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

    dateTimeDF=reactiveVal(NULL)

    doExtractTimestamps=reactiveVal(FALSE)

    observeEvent(input$extractTimestamps, {
        #show_modal_spinner(text="Extracting timestamps")
        # ofn=paste0(args$outbase, ".datetime.csv.gz")
        # if(file.exists(ofn)){
        #   inform("Timestamps already extracted, skipping this step.")
        #   datetime=fread(ofn)
        #   inform("Reusing", nrow(datetime), "records.")
        # }else{
        #   inform("Checking directory, please wait...")
        #   fl=list.files(args$inputdir, pattern=".*\\.(jpg|jpeg)$", recursive=T, full.names=T, ignore.case=T)
        #   inform(length(fl), "images detected.")
        #   inform("Extracting timestamps, please wait...")
        #   datetime=rbindlist(lapply(fl, 
        #     function(fn){
        #       this_date=tryCatch({
        #         date=read_exif_tags(fn)
        #         d[['DateTime']]
        #       }, error=function(e) return(NA))
        #   return(data.table(fn=fn,date=date))}))
        #   inform(nrow(datetime), "timestamps extracted. Writing to file.")
        #   fwrite(datetime, ofn)
        # }
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
            confirmButtonText = "Overwrite",
            cancelButtonText = "Reuse",
            confirmButtonColor = "#3085d6",
            cancelButtonColor = "#d33"
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
            dateTimeDF(fread(file.path(outputDirReactive(), "datetime.csv.gz")))
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
            # show the callIntervals button and the interval input
            shinyjs::show("interval")
            shinyjs::show("callIntervals")
        } else{
            shinyjs::hide("interval")
            shinyjs::hide("callIntervals")
        }
    })

    observeEvent(input$callIntervals, {
        createIntervals(dateTimeDF(), loadedDirectory(), outputDirReactive(), input$interval)
    })

}



runApp(shinyApp(ui, server, options = list(launch.browser = F, port=8877)))