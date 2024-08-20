#source("/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/Shiny_Desktop_App/sadpaR/R/makeRecordTable.R")

library(data.table)
#mulstatus=fread("/mnt/t/CT_Data//sus_scrofa/sus_scrofa.sadpaR_data/tagging//multipleEventStatus.csv")



takeFirstAmongFilenames = function(paths, verbose = FALSE) {
  dt = data.table(original_path = paths, basename = basename(paths))
  
  # if length 1 return the same
  if (nrow(dt) == 1) return(dt$original_path)
  
  # if all paths contain parenthesed numbers
  if (all(grepl("\\(\\d+\\)", dt$basename))) {
    # extract the number from each path
    dt[, number := as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", basename))]
    # take the first path with the smallest number
    if (verbose) cli::cli_inform("Parenthesed: Taking {dt[which.min(number), original_path]} as the earliest among {paste(dt$original_path, collapse=', ')}")
    return(dt[which.min(number), original_path][1])
  }
  
  # iterate over the prefixes
  prefixes = c("DCIM", "IMG", "DSC")
  for (prefix in prefixes) {
    if (any(grepl(prefix, dt$basename))) {
      dt[, number := as.numeric(gsub(paste0(".*", prefix, "(\\d+).*"), "\\1", basename))]
      if (verbose) cli::cli_inform("{prefix}: Taking {dt[which.min(number), original_path]} as the earliest among {paste(dt$original_path, collapse=', ')}")
      return(dt[which.min(number), original_path][1])
    }
  }
  
  # otherwise we don't know, get the first one
  if (verbose) cli::cli_inform("Unknown case: Taking {dt$original_path[1]} as the earliest among {paste(dt$original_path, collapse=', ')}")
  return(dt[1, original_path])
}



makeRecordTable=function(intervals, tags, species, multispecies_tagging, imageRootOriginal, aggregateBy){
  if(VERBOSE) cli::cli_inform("Generating record table")
  if(VERBOSE) print("multispecies tagging")
  if(VERBOSE) print(multispecies_tagging())
    multagging=multispecies_tagging()$tags()
    d=intervals()
    tag=tags()
    if(VERBOSE) print("tags")
    tagsp=merge(tag, species(), by.x="speciesID", by.y="id", all.x=T)
    tagsp=tagsp[!is.na(speciesID)]
    if(VERBOSE) print("tagsp")
    tagsp[,species_name:=paste0(`Common Name`, " - [",`Lao Name`, '] (', `Species Name`, ')')]
    tagsp[,c("Station", "Camera"):=tstrsplit(ctid, " ")]
    if(VERBOSE) print("tagsp 2")
    species_ct=tagsp[,length(unique(species_name)),by=.(ctid, event)]
    species_ct[,ctidevent:=paste(ctid, event)]
    if(VERBOSE) print("species_ct")
    tagsp[,ctidevent:=paste(ctid, event)]
    tagsp[,spct:=length(unique(species_name)), by="ctidevent"]
    #if(VERBOSE) print(tagsp)
    if(is.numeric(d$dt))
      d[,dt:=as.POSIXct(chron::chron(dt))]
    if(VERBOSE) print("tagsp 3")
    singlespevents=tagsp[spct==1]

    mulspevent=tagsp[spct>1]
    if(VERBOSE) print("singlespevents")
    #mulstatus[,ctidevent:=paste(ctid, interval)]

    #unTaggedMulSpEvents = setdiff(unique(mulspevent$ctidevent), unique(mulstatus$ctidevent))
    #noMulSpeciesTagged = setdiff(unique(mulstatus$ctidevent), unique(mulspevent$ctidevent))

    #if(length(unTaggedMulSpEvents))
    #    cli::cli_warn("The following multiple species events are not tagged: {paste(unTaggedMulSpEvents, collapse=', ')}")
    #if(length(noMulSpeciesTagged))
    #    cli::cli_warn("The following multiple species events are tagged but have no status: {paste(noMulSpeciesTagged, collapse=', ')}")
    d[,fn:=sub(imageRootOriginal(), "", fn)]
    if(VERBOSE) print("d")
    if(VERBOSE) print(d)
    if(VERBOSE) print("multagging")
    if(VERBOSE) print(multagging)
    mulsptag=merge(d, multagging, by="fn")
    if(VERBOSE) print("mulsptag")
    if(VERBOSE) print(mulsptag)
    if(VERBOSE) print("species")
    if(VERBOSE) print(species())
    mulsptag=merge(mulsptag, species(), by.x="species", by.y="id", all.x=T)
    # if the merge failed it means that the species is not in the species table
    if(VERBOSE) print("mulsptag A")
    if(VERBOSE) print(mulsptag)
    if(any(is.na(mulsptag$`Common Name`))){
        warningMessage=glue("The following species are not in the species table: {paste(unique(mulsptag[is.na(`Common Name`), species]), collapse=', ')}. {nrow(mulsptag[is.na(`Common Name`), species])} events will be excluded from the record table. Make sure you have the right species.csv")
        cli::cli_warn(warningMessage)
        sendSweetAlert(
            session = session,
            title = "Warning",
            text = warningMessage,
            type = "warning"
        )
    }
    mulsptag[,species_name:=paste0(`Common Name`, " - [",`Lao Name`, '] (', `Species Name`, ')')]
    #mulsptag[,c("first", "last"):=list(min(dt), max(dt)), by=.(ctid, interval, species_name)]
    #setorder(mulsptag, ctid, interval, species_name, first)
    #mulsptag=unique(mulsptag[,c("ctid", "interval", "species_name", "first", "last")])
    #mulsptag[,vdiff:=ifelse(.N-1, c(0, difftime(first[-1], last[-.N], units="secs")), 0), by=.(ctid, interval, species_name)]
    if(VERBOSE) print("mulsptag 2")
    independent_interval_threshold=30
    setorder(mulsptag, ct, species_name, dt)
    # if dt is numeric, use chron
    print(mulsptag)
    mulsptag[,interval2:=lapply(.SD, function(dt) {vdiff=difftime(dt[-1], dt[-length(dt)], units="sec"); vdiff=c(0, vdiff); vdiff=vdiff<independent_interval_threshold*60;return(cumsum(!vdiff))}),.SDcols="dt",by=.(location, ct, interval, species_name)]
    if(VERBOSE) print("mulsptag 3")
    # number each distinct species per ctid, interval
    mulsptag[,species_offset:=match(species_name, unique(species_name)), by=.(ctid, interval)]
    # number each distinct interval per ctid
    mulsptag[,interval_offset:=match(interval, unique(interval)), by=.(ctid)]
    if(VERBOSE) print("mulsptag 4")
    #mulsptag[,interval2:=interval2+species_offset+interval_offset]
    mulsptag=merge(mulsptag, d[,max(interval), by=.(ctid)], by="ctid")
    #mulsptag[,interval2:=interval2+V1]
    if(VERBOSE) print("mulsptag 5")
    setorder(mulsptag, ctid, interval, interval_offset, species_offset, interval2)
    if(VERBOSE) print(mulsptag)
    mulsptag[,c("start", "end"):=list(min(dt), max(dt)), by=.(ctid, interval, species_name, interval2)]
    mulsptag[,startFileName:=takeFirstAmongFilenames(fn[dt==min(dt)]), by=.(ctid, interval, species_name, interval2)]
    if(VERBOSE) print("mulsptag 6")
    eventtable=unique(mulsptag[,.(ctid, species_name, interval, start, end, V1, interval_offset, species_offset, interval2, startFileName)])
    eventtable[,interval3:=V1+1:.N,by=.(ctid)]
    eventtable=eventtable[,.(ctid, species_name, interval3, start, end, startFileName)]
    setnames(eventtable, "interval3", "interval")
    if(VERBOSE) print("mulsptag 7")
    d[,ctidevent:=paste(ctid, interval)]
    singlespeventtable=merge(unique(singlespevents[,.(species_name, ctidevent)]), d, by="ctidevent")
    singlespeventtable[,c("start", "end"):=list(min(dt), max(dt)), by=.(ctidevent)]
    # startfilename
    if(VERBOSE) print("singlespeventtable")
    singlespeventtable[,startFileName:=takeFirstAmongFilenames(fn[dt==start]), by=.(ctidevent)]
    singlespeventtable=unique(singlespeventtable[,.(ctid, species_name, interval, start, end, startFileName)])
    eventtable=rbind(eventtable, singlespeventtable)
    if(VERBOSE) print("mulsptag 8")
    # check that all intervals are unique per ctid
    if(any(eventtable[,anyDuplicated(interval),by=ctid]$V1))
        cli::cli_warn("There are duplicate intervals per ctid")

    eventtable[,c("Station", "Camera"):=tstrsplit(ctid, " ")]
    # if aggregation is by station, then further aggregate events by station and species
    if(VERBOSE) {print("Event table:"); print(eventtable)}
    fwrite(eventtable, "eventtable.csv")
    if(aggregateBy=="byStation"){
      setorder(eventtable, Station, species_name, start, end)

      # Define the buffer in seconds
      buffer = independent_interval_threshold * 60

      # merge events with the same species at the same station that overlap, i.e. start1>=start2 and start1<=end2 or end1>=start2 and end1<=end2
      # oldet=copy(eventtable)
      eventtable[,overlap:=c(0, (start[-1] >= (start[-.N] - buffer) & start[-1]<=(end[-.N] + buffer)) |
                                (end[-1] >= (start[-.N] - buffer) & end[-1] <= (end[-.N] + buffer))), by=.(Station, species_name)]
      eventtable[,interval2:=cumsum(!overlap), by=.(Station, species_name)]
      eventtable[,c("start", "end"):=list(min(start), max(end)), by=.(Station, species_name, interval2)]
      # fwrite(eventtable, "eventtable2.csv")
      # eventtable=oldet
      # eventtable[,overlap:=c(0, (start[-1]>=start[-.N] & start[-1]<=end[-.N]) | (end[-1]>=start[-.N] & end[-1]<=end[-.N])), by=.(Station, species_name)]
      # eventtable[,interval2:=cumsum(!overlap), by=.(Station, species_name)]
      # eventtable[,c("start", "end"):=list(min(start), max(end)), by=.(Station, species_name, interval2)]
      # fwrite(eventtable, "eventtable3.csv")
      eventtable[,startFileName:=takeFirstAmongFilenames(startFileName), by=.(Station, species_name, interval2)]
      eventtable=unique(eventtable[,.(Station, species_name, interval2, start, end, startFileName)])
      setorder(eventtable, Station, species_name, start)
    } else {
      setorder(eventtable, ctid, species_name, start)
    }
        
    setnames(eventtable, "species_name", "Species")
    setnames(eventtable, "start", "DateTimeOriginal")
    eventtable[,c("Date", "Time"):=list(as.Date(DateTimeOriginal), format(DateTimeOriginal, "%H:%M:%S"))]
    if(VERBOSE) print("mulsptag 9")
    # order by ctid, species, interval
    #setorder(eventtable, ctid, Species, DateTimeOriginal)
    # delta is the difference between the start of the event and the end of the previous event of the same species at this station (first is 0)
    if(aggregateBy=="byStation")
      groupByCols=c("Station", "Species")
    else
      groupByCols=c("Station", "Camera", "Species")
    eventtable[,delta:=DateTimeOriginal-shift(end, fill=DateTimeOriginal[1]), by=groupByCols]
    # delta.time.secs, delta.time.mins, delta.time.hours and delta.time.days are the same as delta but in seconds, minutes, hours and days
    eventtable[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days"):=list(as.numeric(delta), as.numeric(delta)/60, as.numeric(delta)/3600, as.numeric(delta)/86400)]
    eventtable[,c("Directory", "FileName"):=list(dirname(startFileName), basename(startFileName))]
    if(aggregateBy=="byStation")
        eventtable=eventtable[,.(Station, Species, DateTimeOriginal, Date, Time, delta.time.secs, delta.time.mins, delta.time.hours, delta.time.days, Directory, FileName)]
    else
        eventtable=eventtable[,.(Station, Camera, Species, DateTimeOriginal, Date, Time, delta.time.secs, delta.time.mins, delta.time.hours, delta.time.days, Directory, FileName)]
    return(eventtable)
}

makeRecordTableUI = function(id, appLang) {
    ns = NS(id)
    tagList(
      fluidRow(
        uiOutput(ns("untaggedEvents")),
        uiOutput(ns("untaggedMulSpEvents"))
      ),
      fluidRow(
        # column(3,
        radioGroupButtons(inputId=ns("aggregateBy"), label=appLang$recordTableAggregateByLabel, choiceNames=appLang$recordTableAggregateByChoices, 
                          choiceValues=c("byCamera", "byStation"), selected="byCamera", justified=F, status = "primary"),
        actionBttn(inputId=ns("generateRecordTableBttn"), label=appLang$generateRecordTableBttn, icon("table"), color="primary", inline=T),
        # ),
        # column(3,
        downloadBttn(outputId=ns("exportRecordTableBttn"), label=appLang$exportRecordTableBttn, icon=icon("download"), color="primary")
        # )
      ),
        uiOutput(ns("recordTable"))
    )
}

makeRecordTableServer = function(id, intervals, tags, species, multispecies_tagging, imageRootOriginal, appLang) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    recordTableReactive=reactiveVal(NULL)

    untaggedEvents=reactive({
      cli::cli_inform("Checking for untagged events")
      currentTags=copy(tags())
      currentTags=currentTags[,ctidint:=paste(ctid, event)]
      currentTags[,tagged:=T]
      currentTags=unique(currentTags[,.(ctidint, tagged)])
      if(VERBOSE) print(currentTags)
      if(VERBOSE) print("====================")
      currentIntervals=unique(copy(intervals())[,ctidint:=paste(ctid, interval)][,.(ctid, interval, ctidint)])
      if(VERBOSE) print(currentIntervals)
      intervalStatus=merge(currentIntervals, currentTags, by="ctidint", all.x=T)
      intervalStatus[is.na(tagged),tagged:=F]
      if(all(intervalStatus$tagged))
        return(NULL)
      else {
        return(intervalStatus[tagged==F])
      }
    })

    output$untaggedEvents=renderUI({
      if(is.null(untaggedEvents()))
        return(NULL)
      else {
        if(VERBOSE) print("untagged events")
        if(VERBOSE) print(nrow(untaggedEvents()))
        if(VERBOSE) print(length(unique(intervals()[,paste(ctid,interval)])))
        percentUntagged=round(100*nrow(untaggedEvents())/length(unique(intervals()[,paste(ctid,interval)])), 2)
        if(VERBOSE) print(percentUntagged)
        return(tagList(
          h3("Events currently untagged"),
          h4(glue("{nrow(untaggedEvents())} ({percentUntagged}%) {appLang$numUntaggedEventsLeftWarning}")),
          tableOutput(ns("untaggedEventsTable"))
        ))
      }
    })

    output$untaggedEventsTable=renderTable({
      req(untaggedEvents())
      tableToDisplay=copy(untaggedEvents())[,paste(interval,collapse=", "), by=ctid][,.(`Camera Trap`=ctid, `Intervals`=V1)]
      return(tableToDisplay)

    })

    untaggedMulSpEvents=reactive({
      cli::cli_inform("Checking for untagged multiple species events")
      currentTags=copy(tags())
      currentTags=currentTags[,ctidint:=paste(ctid, event)]
      currentTags[,tagged:=T]
      currentTags=unique(currentTags[,.(ctidint, tagged)])
      if(VERBOSE) print(currentTags)
      if(VERBOSE) print("====================")
      currentIntervals=unique(copy(intervals())[,ctidint:=paste(ctid, interval)][,.(ctid, interval, ctidint)])
      if(VERBOSE) print(currentIntervals)
      intervalStatus=merge(currentIntervals, currentTags, by="ctidint", all.x=T)
      intervalStatus[is.na(tagged),tagged:=F]
      if(all(intervalStatus$tagged))
        return(NULL)
      else {
        return(intervalStatus[tagged==F])
      }
    })

    observeEvent(input$generateRecordTableBttn, {
      # display a waiter
      shinybusy::show_modal_spinner(text="Generating record table", spin="flower")
      recordTableReactive(makeRecordTable(intervals, tags, species, multispecies_tagging, imageRootOriginal, input$aggregateBy))
      shinybusy::remove_modal_spinner()
    })

    output$recordTable=renderUI({
      if(is.null(recordTableReactive())) return(NULL)
      renderDataTable(recordTableReactive())
    })

    observe({
      if(is.null(recordTableReactive())){
        if(VERBOSE) print("disabling bttn")
        shinyjs::hide("exportRecordTableBttn_bttn")
      }
      else 
        shinyjs::show("exportRecordTableBttn_bttn")
    })

    output$exportRecordTableBttn=downloadHandler(
      filename = function() {
        paste("recordTable", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        fwrite(recordTableReactive(), file, row.names=F)
      }
    )
    

  })
}

makeRecordTableDemo = function(usePreexisting=F) {
    library(shiny)
    library(data.table)
    library(glue)
    library(shinycssloaders)
    library(shinyWidgets)
    library(tools)
    print(1)
    multagging=fread("/mnt/t/CT_Data//sus_scrofa/sus_scrofa.sadpaR_data/tagging/multipleEventTags.csv")
    print(2)
    species=fread("/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/metadata/species.csv")
    print(3)
    tag=fread("/mnt/t/CT_Data//sus_scrofa/sus_scrofa.sadpaR_data/tagging/eventTagging.csv")
    print(4)
    d=fread("/mnt/t/CT_Data//sus_scrofa/sus_scrofa.sadpaR_data/metadata/intervals.csv")
    imageRootOriginal="/mnt/t/CT_Data/sus_scrofa/renamed"

    intervals = reactiveVal(d)
    tags = reactiveVal(tag)
    species = reactiveVal(species)
    multispecies_tagging = reactiveVal(multagging)
    imageRootOriginal = reactiveVal(imageRootOriginal)
    appLang=config::get(file="/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/Shiny_Desktop_App/sadpaR/IntervalDetector/lang.yml", config="English")

    ui = fluidPage(
      shinyjs::useShinyjs(),
        makeRecordTableUI("recordTable", appLang)
    )

    server = function(input, output, session) {
        makeRecordTableServer("recordTable", intervals, tags, species, multispecies_tagging, imageRootOriginal)
    }

    shinyApp(ui = ui, server = server)

}

#source("sadpaR/IntervalDetector/R/module_makeRecordTable.R");
#shiny::runApp(makeRecordTableDemo(), port=5145)
