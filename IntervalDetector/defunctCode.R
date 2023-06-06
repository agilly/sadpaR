observeEvent(currentTagging$internalTable, {
  ## this means the internal species table has been changed, we must update the display one
  if(!is.null(loadedDataset$species)){
      dt=currentTagging$internalTable
      dt=merge(dt, species, by.x="speciesID", )
  }
})


# checkSelectedFolder=function(){
#   seqFolders=paste(loadedDataset$ct[['Station']], loadedDataset$ct[['Camera ID']], sep=".")
#   tenRandom=sample(seqFolders, 10)
#   tenRandom=paste(sequenceDir(), tenRandom, sep="/")
#   if(any(file.exists(tenRandom))){
#     return(T)
#   }
#   return(F)
# }


# observeEvent(input$inputFN, {
#   req(input$inputFN)
#   print("loadedDataset called")
#     fn=input$inputFN$datapath
#     if(file_ext(fn)!="zip") {
#       showNotification("Please select a zip file.", type="error")
#       return(list())}
#     zipContents=unzip(fn, list = TRUE)$Name
#     if(!any(basename(zipContents) %in% "intervals.csv")){
#       showNotification("No intervals dataset found. Has this file been generated using CTpreprocess?", type="error")
#       return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     if(!any(basename(zipContents) %in% "species.csv")){
#       showNotification("No species dataset found. Please add a species.csv to the file.", type="error")
#       return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     if(!any(basename(zipContents) %in% "ct.csv")){
#       showNotification("No camera trap dataset found. Please add a ct.csv to the file.", type="error")
#       return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     if(!any(basename(zipContents) %in% "metadata.csv")){
#       showNotification("No metadata found in zip file. Please add a metadata.csv to the file.", type="error")
#       return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     if(!any(basename(zipContents) %in% "stations.csv")){
#       showNotification("No stations dataset found. Please add a stations.csv to the file.", type="error")
#       return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     interval_data=fread(cmd=paste("unzip -p", fn, "intervals.csv"))
#     if(!all(file.exists(sample(interval_data$fn, 10)))){
#       showNotification("Some images not found when performing random check. If the images are on a hard drive, please plug it in before loading dataset.", type="error")
#       #return(list(interval_data=NULL, species_data=NULL, metadata=NULL, ct=NULL, stations=NULL))
#     }
#     interval_data[,ctid:=paste(location, ct)]
#     interval_data[,ctid:=paste(location, ct)]
#     choices=unique(interval_data$ctid)
#     print(head(choices))
#     updateSelectInput(session, inputId = "whichCT", choices=choices, selected=choices[1])
#     updateSelectInput(session, inputId = "whichCTSeq", choices=choices, selected=choices[1])
#     updateSelectInput(session, inputId = "sequence", choices=unique(interval_data$interval[interval_data$ctid==choices[1]]))
#     loadedDataset$interval_data=interval_data; #fread(unz(fn, "intervals.csv")),
#     loadedDataset$species_data=fread(cmd=paste("unzip -p", fn, "species.csv")) #fread(unz(fn, "species.csv"))
#     loadedDataset$ct=fread(cmd=paste("unzip -p", fn, "ct.csv"))
#     loadedDataset$stations=fread(cmd=paste("unzip -p", fn, "stations.csv"))
#     loadedDataset$metadata=fread(cmd=paste("unzip -p", fn, "metadata.csv"))
#
#     dat=interval_data
#     #dat[,ctid:=paste(location, ct)]
#     #print(input$whichCT)
#     #print(head(dat))
#     dat=dat[ctid==choices[1]]
#     durations=dat[,list(mean(as.numeric(dt)), .N, difftime(max(as.POSIXct(dt, origin="1970-01-01 00:00:00")), min(as.POSIXct(dt, origin="1970-01-01 00:00:00")), units="min")), by=interval]
#     setnames(durations, c("sequence", "avgdate", "num_images", "duration"))
#     print(head(durations))
#     durations[,avgdate:=as.character(chron(avgdate))]
#     print(head(durations))
#     durations[,duration:=ceiling(as.numeric(duration))]
#     setcolorder(durations, c("avgdate", "sequence", "num_images", "duration"))
#     dur$durations=as.data.table(durations)
#
# })


#species=reactive({loadedDataset$species_data})


# output$restPath=renderText({
#   seq=as.integer(input$ChooseEdit)
#   path=d[d30m.event==seq]$fn[1]
#   path=unlist(strsplit(path, "/"))
#   l=length(path)
#   basepath<<-paste(path[seq(1, (l-3))], collapse="/")
#   basepath
# })


# observeEvent(input$ChooseEdit, {
#   req(input$ChooseEdit)
#     seq=as.integer(input$ChooseEdit)
#     print(input$ChooseEdit)
#     choices=d[d30m.event==seq]$fn
#     choices=paste(basename(dirname(dirname(choices))), basename(dirname(choices)), basename(choices), sep="/")
#     updateSelectInput(session, inputId = "PicInSequence", choices=choices, selected=choices[1])
# })


# observeEvent(input$nextEdit, {
#   req(input$PicInSequence)
#   req(input$ChooseEdit)
#   seq=as.integer(input$ChooseEdit)
#   fn=input$PicInSequence
#   choices=d[d30m.event==seq]$fn
#   choices=paste(basename(dirname(dirname(choices))), basename(dirname(choices)), basename(choices), sep="/")
#   if (input$PicInSequence != choices[length(choices)]) {
#     i=(1:length(choices))[choices==fn]
#     newSelection = choices[i+1]
#     updateSelectInput(session, inputId = "PicInSequence", selected = newSelection)
#   }
#   if (input$PicInSequence == choices[length(choices)]) {
#     shinyjs::disable("nextEdit")
#   }else{
#     shinyjs::enable("nextEdit")
#   }
#   if (input$PicInSequence == choices[1]) {
#     shinyjs::disable("previousEdit")
#   }else{
#     print("enabling previousEdit")
#     shinyjs::enable("previousEdit")
#   }
# })

# observeEvent(input$previousEdit, {
#   req(input$PicInSequence)
#   req(input$ChooseEdit)
#   seq=as.integer(input$ChooseEdit)
#   fn=input$PicInSequence
#   choices=d[d30m.event==seq]$fn
#   choices=paste(basename(dirname(dirname(choices))), basename(dirname(choices)), basename(choices), sep="/")
#   if (input$PicInSequence != choices[1]) {
#     i=(1:length(choices))[choices==fn]
#     newSelection = choices[i-1]
#     updateSelectInput(session, inputId = "PicInSequence", selected = newSelection)
#   }
#   if (input$PicInSequence == choices[length(choices)]) {
#     shinyjs::disable("nextEdit")
#   }else{
#     shinyjs::enable("nextEdit")
#   }
#   if (input$PicInSequence == choices[1]) {
#     shinyjs::disable("previousEdit")
#   }else{
#     shinyjs::enable("previousEdit")
#   }
# })

# output$taggingTable=renderDT({
#   data.table(Individual=c(1, 2), Family=c("Viverridae", "Mustelidae"), Species=c("", "Greater hog badger (Arctonyx collaris)"), Sex=c("M", "F"))
# }, editable=T)


# observe({
#    #  roots=volumes,
#   if(!is.null(input$sequenceFolder)){
#   #selectedFolder=paste(getDrives()[input$sequenceFolder$root], unlist(input$sequenceFolder$path), sep="/")
#   selectedFolder=parseDirPath(getDrives(), input$sequenceFolder)
#   #print(input$sequenceFolder[['root']])
#   #print(unlist(input$sequenceFolder$path))
#   #print(selectedFolder)
#   print("aaaa")
#   print(selectedFolder)
#   print("ssss")
# }
# })

# observe({
#   selectedRootDir=rootDir()
#   if(length(selectedRootDir) & !is.null(loadedDataset$metadata)){
#     showNotification("Datasets fully loaded", type="default")
#     showTab(inputId="tabs", target="Sequence")
#     # metadata is implicitly validated in function
#     # selected Sequence Dir is not
#     if(checkSelectedFolder()){
#       print("selected folder validated")
#     }else{
#       showNotification("Selected folder does not contain camera trap data.", type="error")
#     }
#   }
# })
