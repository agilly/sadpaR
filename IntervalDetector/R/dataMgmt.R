checkSelectedFolder=function(session, input, output, rootDir, loadedDataset, currentTagging, dur, appPaths, appLang){
  #setProgress(0.2, detail = paste("Checking dataset consistency"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=20, title="Checking dataset consistency")
  if(!file.exists(paste(rootDir(), "metadata", sep="/"))){
    showNotification("Selected folder does not contain a metadata directory.", type="error")
    return(F)
  }
  if(!file.exists(paste(rootDir(), "sequences", sep="/"))){
    showNotification("Selected folder does not contain a sequences directory.", type="error")
    return(F)
  }
  if(!file.exists(paste(rootDir(), "tagging", sep="/"))){
    showNotification("Selected folder does not contain a tagging directory, creating.", type="message")
    dir.create(paste(rootDir(), "tagging", sep="/"))
  }else{
    if(file.exists(paste(rootDir(), "tagging", "eventTagging.csv" ,sep="/"))){
    #setProgress(0.25, detail = paste("Reading tagging information"))
    updateProgressBar(session = session, id="loadDatasetPBar", value=25, title="Reading tagging information")
    currentTagging$internalTable=fread(paste(rootDir(), "tagging", "eventTagging.csv" ,sep="/"), colClasses=c(indName="character", Sex="character", Age="character"))
  }
  }
  appPaths$taggingCSV=paste(rootDir(), "tagging", "eventTagging.csv" ,sep="/")
  # at this stage we have all three necessary folders.
  ## checking contents of metadata
  if(! "intervals.csv" %in% list.files(paste(rootDir(), "metadata", sep="/"))){
    showNotification("No intervals dataset found in metadata directory. Please generate using CTpreprocess", type="error")
    return(F)
  }
  if(! "species.csv" %in% list.files(paste(rootDir(), "metadata", sep="/"))){
    showNotification("No species dataset found in metadata directory.", type="error")
    return(F)
  }
  if(! "ct.csv" %in% list.files(paste(rootDir(), "metadata", sep="/"))){
    showNotification("No camera trap dataset found in metadata directory. ", type="error")
    return(F)
  }
  if(! "metadata.csv" %in% list.files(paste(rootDir(), "metadata", sep="/"))){
    showNotification("No study metadata dataset found in metadata directory.", type="error")
    return(F)
  }
  if(! "stations.csv" %in% list.files(paste(rootDir(), "metadata", sep="/"))){
    showNotification("No stations dataset found in metadata directory.", type="error")
    return(F)
  }

  ## checking contents of sequences
  ### requires loading of camera trap data
  #setProgress(0.3, detail = paste("Loading cameras"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=30, title="Loading camera information")
  
  ctdata=fread(paste(rootDir(), "metadata", "ct.csv" ,sep="/"))
  seqFolders=paste(ctdata$Station, ctdata[['Camera ID']], sep=".")
  print(head(seqFolders))
  tenRandom=sample(seqFolders, min(c(10, nrow(ctdata))))
  print("ok")
  tenRandom=paste(rootDir(), "sequences", tenRandom, sep="/")
  #setProgress(0.4, detail = paste("Checking if camera traps can be found in sequences"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=40, title="Checking if camera traps can be found in sequences")

  if(! any(file.exists(tenRandom))){
    # at least one
    showNotification("We tried finding 10 camera traps from ct.csv in the sequence folder and failed.
    Usually this indicates mismatching CT and sequence data.
    If you think this is wrong, please try reloading the dataset.", type="error")
    return(F)
  }
  appPaths$sequenceDir=paste(rootDir(), "sequences" ,sep="/")

  # checking if we can find the pictures
  #setProgress(0.5, detail = paste("Looking for photos"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=50, title="Looking for photos")
  interval_data=fread(paste(rootDir(), "metadata", "intervals.csv" ,sep="/"))
  if(!all(file.exists(sample(interval_data$fn, 10)))){
  # this does not work because sweet alerts override each other, so the progress bar makes this one invisible
  # confirmSweetAlert(
  #   inputId="warningSomeImagesMissing",
  #   session = session,
  #   title = appLang$warning,
  #   text = appLang$warningSomeImagesMissing,
  #   type = "warning"
  # )
  #  notie(
  #     text = appLang$warningSomeImagesMissing,
  #     time=10,
  #     type = "warning",
  #     position = "bottom"
  #   )
  showNotification(appLang$warningSomeImagesMissing, type="warning")
  }
  

    #return(F)
  

  return(T)
}

updateTaggingOnSeqChange=function(session, input, output, currentTagging, loadedDataset, selctid=input$tagCT, selevent=input$tagSequence){
  print("Entered updateTaggingOnSeqChange")
  print(currentTagging$internalTable)
  print(glue("ctid={selctid}, event={selevent}"))
  currentInternalTable=currentTagging$internalTable[ctid==selctid & event==selevent]
  print(currentInternalTable)
  dispTable=merge(currentInternalTable, loadedDataset$species_data, by.x="speciesID", by.y="id", all.x=T)
  dispTable=dispTable[ctid==selctid & event==selevent]
  dispTable[,c("ctid", "event", "numInd", "speciesID"):=NULL]
  setcolorder(dispTable, c("indID", "indName", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order", "Sex", "Age"))
  setnames(dispTable, c("id", "individual", "common_name", "lao_name", "scientific_name", "group", "family", "order", "Sex", "Age"))
  print(currentTagging$displayTable)
  currentTagging$displayTable=dispTable
  print(currentTagging$displayTable)
}

loadDataset=function(session, input, output, rootDir, loadedDataset, currentTagging, dur, retag){
  #showNotification("Loading datasets...", type="message")
  #setProgress(0.6, detail = paste("Reading intervals"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=60, title="Reading intervals")
  interval_data=fread(paste(rootDir(), "metadata", "intervals.csv" ,sep="/"))
  print("this is a debug version. Data will load automatically. Remove line 78 in dataMgmt.R")
  #interval_data[,fn:=sub("/mnt/d/CT II/NKD_2022/Data processing/raw_images/","/mnt/c/Users/R. Tidi Victor/NKD_2022/",fn, fixed=T)]
  interval_data[,ctid:=paste(location, ct)]
  choices=unique(interval_data$ctid)
  #print(head(choices))
  print("general observe called")
  updateSelectInput(session, inputId = "whichCT", choices=choices, selected=choices[1])
  updateSelectInput(session, inputId = "whichCTSeq", choices=choices, selected=choices[1])
  updateSelectInput(session, inputId = "tagCT", choices=choices, selected=choices[1])
  seqChoices=unique(interval_data$interval[interval_data$ctid==choices[1]])
  updateSelectInput(session, inputId = "sequence", choices=seqChoices)
  updateSelectInput(session, inputId = "tagSequence", choices=seqChoices)
  updateSelectInput(session, inputId = "ChooseEdit", choices=seqChoices, selected=seqChoices[1])

  loadedDataset$interval_data=interval_data; #fread(unz(fn, "intervals.csv")),
  #setProgress(0.7, detail = paste("Loading other metadata"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=70, title="Loading other metadata")
  loadedDataset$species_data=fread(paste(rootDir(), "metadata", "species.csv" ,sep="/")) #fread(unz(fn, "species.csv"))
  loadedDataset$ct=fread(paste(rootDir(), "metadata", "ct.csv" ,sep="/"))
  loadedDataset$stations=fread(paste(rootDir(), "metadata", "stations.csv" ,sep="/"))
  loadedDataset$metadata=fread(paste(rootDir(), "metadata", "metadata.csv" ,sep="/"))
  #setProgress(0.8, detail = paste("Understanding directory structure"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=80, title="Understanding directory structure")
  loadedDataset$imagePath=largestCommonPath(interval_data$fn)
  print(paste("imagepath= ", loadedDataset$imagePath))
  #print(loadedDataset$imagePath)
  #print(strsplit(choices[1], " "))

  #emptyTaggingTable=data.table(id=integer(), individual=character(),common_name=character(), lao_name=character(), scientific_name=character(), group=character(), family=character(), order=character())
  #emptyInternalTaggingTable=data.table(ctid=character(0), event=integer(0), numInd=integer(0), indID=integer(0), speciesID=integer(0), indName=character(0))
  #id	Common Name	Lao Name	Species Name	Group	Family	Order
  # print("loaddataset")
  # print(currentTagging$internalTable)
  # print("------------")
  # print(loadedDataset$species_data)
  # print("------------")
  dispTable=merge(currentTagging$internalTable, loadedDataset$species_data, by.x="speciesID", by.y="id", all.x=T)
  # print(dispTable)
  # print("------------")
  # print(input$tagCT)
  # print("------------")
  # print(input$tagSequence)
  
  #printv(input$tagCT, input$tagSequence)
  dispTable=dispTable[ctid==choices[1] & event==seqChoices[1]]
  dispTable[,c("ctid", "event", "numInd", "speciesID"):=NULL]
  print(dispTable)
  print("BYE====\n\n\n")
  setcolorder(dispTable, c("indID", "indName", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order", "Sex", "Age"))
  setnames(dispTable, c("id", "individual", "common_name", "lao_name", "scientific_name", "group", "family", "order", "Sex", "Age"))
  currentTagging$displayTable=dispTable

  #setProgress(0.9, detail = paste("Computing durations"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=90, title="Computing durations")
  dat=interval_data
  dat=dat[ctid==choices[1]]
  durations=dat[,list(mean(as.numeric(dt)), .N, difftime(max(as.POSIXct(dt, origin="1970-01-01 00:00:00")), min(as.POSIXct(dt, origin="1970-01-01 00:00:00")), units="min")), by=interval]
  setnames(durations, c("sequence", "avgdate", "num_images", "duration"))
  #print(head(durations))
  durations[,avgdate:=as.character(chron(avgdate))]
  #print(head(durations))
  durations[,duration:=ceiling(as.numeric(duration))]
  setcolorder(durations, c("avgdate", "sequence", "num_images", "duration"))
  dur$durations=as.data.table(durations)
  showTab(inputId="tabs", target="Sequence")
  showTab(inputId="tabs", target="Edit")
  showTab(inputId="tabs", target="Tagging")
  shinyjs::show("saveButton")
  #setProgress(1, detail = paste("Dataset finished loading"))
  updateProgressBar(session = session, id="loadDatasetPBar", value=90, title="Loading multispecies tags")
  if(file.exists(paste(rootDir(), "tagging", "multipleEventTags.csv" ,sep="/"))){
    retag$tags=fread(paste(rootDir(), "tagging", "multipleEventTags.csv" ,sep="/"))
  }
  if(file.exists(paste(rootDir(), "tagging", "multipleEventStatus.csv" ,sep="/"))){
    retag$status=fread(paste(rootDir(), "tagging", "multipleEventStatus.csv" ,sep="/"))
  }
  updateProgressBar(session = session, id="loadDatasetPBar", value=100, title="Dataset finished loading")
}

saveBackup=function(session, input, output, rootDir, loadedDataset, currentTagging, dur, ctid, seq){
  if(!file.exists(paste(rootDir(), "backup", sep="/"))){
    showNotification("Backup folder does not exist, creating.", type="message")
    dir.create(paste(rootDir(), "backup", sep="/"))
  }
  if(!file.exists(paste(rootDir(), "backup", sep="/"))){
    showNotification("Impossible to create a backup directory. Aborting.", type="error")
    return(-1)
  }
  bkdirname=paste0("backup/", format(Sys.time(), "%d.%m.%Y-%H.%M"))
  dir.create(paste(rootDir(), bkdirname, sep="/"))
  for (extension in c("metadata", "sequences", "tagging")){
    dir.create(paste(rootDir(), bkdirname, extension, sep="/"))
  }
  fwrite(loadedDataset$interval_data, paste(rootDir(), bkdirname, "metadata/intervals.csv" ,sep="/"))
  fwrite(loadedDataset$ct, paste(rootDir(), bkdirname, "metadata/ct.csv" ,sep="/"))
  fwrite(loadedDataset$species_data, paste(rootDir(), bkdirname, "metadata/species.csv" ,sep="/"))
  fwrite(loadedDataset$stations, paste(rootDir(), bkdirname, "metadata/stations.csv" ,sep="/"))
  fwrite(loadedDataset$metadata, paste(rootDir(), bkdirname, "metadata/metadata.csv" ,sep="/"))
  file.copy(paste(rootDir(), "sequences", ctid, paste("sequence", seq, "gif", sep="."), sep="/"), paste(rootDir(), bkdirname, "sequences", sep="/"))
  fwrite(currentTagging$internalTable, paste(rootDir(), bkdirname, "tagging/eventTagging.csv" ,sep="/"))
}

mergeGIFs=function(rootDir, ctid, seq, newseq){
  # generate new gif
  cmdline=paste("gifsicle --merge",
  paste(paste0('"', paste(rootDir(), "sequences", ctid, paste("sequence", seq, "gif", sep="."), sep="/"), '"'), collapse=" "),
  "-o", paste0('"', paste(rootDir(), "sequences", ctid, paste("sequence", newseq, "gif", sep="."), sep="/"), '"'))
  print(cmdline)
  system(cmdline, intern=F)
}

# handlepaths=function(path){
#   return(gsub(" ", "\\ ", path, fixed=T))
# }

handlepaths=function(path){
  return(paste0("\"", path, "\""))
}


generateNewGIFs=function(rootDir, loadedDataset, selectedLoc, selectedCT,seqIDs){
  temp_dir=tempdir(T)
  otemp=paste0(temp_dir, "/new_gifs.csv")
  fwrite(loadedDataset$interval_data[location==selectedLoc & ct==selectedCT & interval %in% seqIDs, c("fn", "interval")], otemp)
  cmdline=paste("./splitGIF.sh", otemp, handlepaths(paste(rootDir(),  "sequences", paste(selectedLoc, selectedCT, sep=".") ,sep="/")))
  print(cmdline)
  system(cmdline, intern=F)
}

saveDataset=function(rootDir, loadedDataset, currentTagging, retag){
  fwrite(loadedDataset$interval_data, paste(rootDir(), "metadata/intervals.csv" ,sep="/"))
  fwrite(loadedDataset$ct, paste(rootDir(), "metadata/ct.csv" ,sep="/"))
  fwrite(loadedDataset$species_data, paste(rootDir(), "metadata/species.csv" ,sep="/"))
  fwrite(loadedDataset$stations, paste(rootDir(),  "metadata/stations.csv" ,sep="/"))
  fwrite(loadedDataset$metadata, paste(rootDir(), "metadata/metadata.csv" ,sep="/"))
  fwrite(currentTagging$internalTable, paste(rootDir(), "tagging/eventTagging.csv" ,sep="/"))
  print("writing multipleEventTags.csv")
  print(retag$tags)
  if(nrow(retag$tags)) fwrite(retag$tags, paste(rootDir(), "tagging/multipleEventTags.csv" ,sep="/"))
  statusToWrite=retag$status[!is.na(ctid)]
  print("writing multipleEventStatus.csv")
  print(statusToWrite)
  if(nrow(statusToWrite)) fwrite(statusToWrite, paste(rootDir(), "tagging/multipleEventStatus.csv" ,sep="/"))
}
