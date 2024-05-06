#source("/mnt/c/Users/R. Tidi Victor/Sync/CameraTrapAI/Shiny_Desktop_App/sadpaR/R/makeRecordTable.R")

library(data.table)
#mulstatus=fread("/mnt/t/CT_Data//sus_scrofa/sus_scrofa.sadpaR_data/tagging//multipleEventStatus.csv")



makeRecordTable=function(intervals, tags, species, multispecies_tagging, imageRootOriginal){
    multagging=multispecies_tagging
    d=intervals
    tag=tags
    tagsp=merge(tag, species, by.x="speciesID", by.y="id", all.x=T)
    tagsp=tagsp[!is.na(speciesID)]
    tagsp[,species_name:=paste0(`Common Name`, " - [",`Lao Name`, '] (', `Species Name`, ')')]
    tagsp[,c("Station", "Camera"):=tstrsplit(ctid, " ")]
    species_ct=tagsp[,length(unique(species_name)),by=.(ctid, event)]
    species_ct[,ctidevent:=paste(ctid, event)]
    tagsp[,ctidevent:=paste(ctid, event)]
    tagsp[,spct:=length(unique(species_name)), by="ctidevent"]

    singlespevents=tagsp[spct==1]
    mulspevent=tagsp[spct>1]

    #mulstatus[,ctidevent:=paste(ctid, interval)]

    #unTaggedMulSpEvents = setdiff(unique(mulspevent$ctidevent), unique(mulstatus$ctidevent))
    #noMulSpeciesTagged = setdiff(unique(mulstatus$ctidevent), unique(mulspevent$ctidevent))

    #if(length(unTaggedMulSpEvents))
    #    cli::cli_warn("The following multiple species events are not tagged: {paste(unTaggedMulSpEvents, collapse=', ')}")
    #if(length(noMulSpeciesTagged))
    #    cli::cli_warn("The following multiple species events are tagged but have no status: {paste(noMulSpeciesTagged, collapse=', ')}")

    mulsptag=merge(d[,fn:=sub(imageRootOriginal, "", fn)], multagging, by="fn")
    mulsptag=merge(mulsptag, species, by.x="species", by.y="id", all.x=T)
    # if the merge failed it means that the species is not in the species table
    if(any(is.na(mulsptag$`Common Name`)))
        cli::cli_warn("The following species are not in the species table: {paste(unique(mulsptag[is.na(`Common Name`), species], collapse=', ')}")
    mulsptag[,species_name:=paste0(`Common Name`, " - [",`Lao Name`, '] (', `Species Name`, ')')]
    #mulsptag[,c("first", "last"):=list(min(dt), max(dt)), by=.(ctid, interval, species_name)]
    #setorder(mulsptag, ctid, interval, species_name, first)
    #mulsptag=unique(mulsptag[,c("ctid", "interval", "species_name", "first", "last")])
    #mulsptag[,vdiff:=ifelse(.N-1, c(0, difftime(first[-1], last[-.N], units="secs")), 0), by=.(ctid, interval, species_name)]
    independent_interval_threshold=30
    setorder(mulsptag, ct, species_name, dt)
    mulsptag[,interval2:=lapply(.SD, function(dt) {vdiff=difftime(dt[-1], dt[-length(dt)], units="sec"); vdiff=c(0, vdiff); vdiff=vdiff<independent_interval_threshold*60;return(cumsum(!vdiff))}),.SDcols="dt",by=.(location, ct, interval, species_name)]

    # number each distinct species per ctid, interval
    mulsptag[,species_offset:=match(species_name, unique(species_name)), by=.(ctid, interval)]
    # number each distinct interval per ctid
    mulsptag[,interval_offset:=match(interval, unique(interval)), by=.(ctid)]

    #mulsptag[,interval2:=interval2+species_offset+interval_offset]
    mulsptag=merge(mulsptag, d[,max(interval), by=.(ctid)], by="ctid")
    #mulsptag[,interval2:=interval2+V1]
    setorder(mulsptag, ctid, interval, interval_offset, species_offset, interval2)
    mulsptag[,c("start", "end"):=list(min(dt), max(dt)), by=.(ctid, interval, species_name, interval2)]
    mulsptag[,startFileName:=fn[dt==min(dt)], by=.(ctid, interval, species_name, interval2)]
    eventtable=unique(mulsptag[,.(ctid, species_name, interval, start, end, V1, interval_offset, species_offset, interval2, startFileName)])
    eventtable[,interval3:=V1+1:.N,by=.(ctid)]
    eventtable=eventtable[,.(ctid, species_name, interval3, start, end, startFileName)]
    setnames(eventtable, "interval3", "interval")

    d[,ctidevent:=paste(ctid, interval)]
    singlespeventtable=merge(unique(singlespevents[,.(species_name, ctidevent)]), d, by="ctidevent")
    singlespeventtable[,c("start", "end"):=list(min(dt), max(dt)), by=.(ctidevent)]
    # startfilename
    singlespeventtable[,startFileName:=fn[dt==start], by=.(ctidevent)]
    singlespeventtable=unique(singlespeventtable[,.(ctid, species_name, interval, start, end, startFileName)])
    eventtable=rbind(eventtable, singlespeventtable)

    # check that all intervals are unique per ctid
    if(any(eventtable[,anyDuplicated(interval),by=ctid]$V1))
        cli::cli_warn("There are duplicate intervals per ctid")

    eventtable[,c("Station", "Camera"):=tstrsplit(ctid, " ")]
    setnames(eventtable, "species_name", "Species")
    setnames(eventtable, "start", "DateTimeOriginal")
    eventtable[,c("Date", "Time"):=list(as.Date(DateTimeOriginal), format(DateTimeOriginal, "%H:%M:%S"))]
    # order by ctid, species, interval
    setorder(eventtable, ctid, Species, DateTimeOriginal)
    # delta is the difference between the start of the event and the end of the previous event of the same species at this station (first is 0)
    eventtable[,delta:=DateTimeOriginal-shift(end, fill=DateTimeOriginal[1]), by=.(Station, Camera, Species)]
    # delta.time.secs, delta.time.mins, delta.time.hours and delta.time.days are the same as delta but in seconds, minutes, hours and days
    eventtable[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days"):=list(as.numeric(delta), as.numeric(delta)/60, as.numeric(delta)/3600, as.numeric(delta)/86400)]
    eventtable[,c("Directory", "FileName"):=list(dirname(startFileName), basename(startFileName))]
    eventtable=eventtable[,.(Station, Camera, Species, DateTimeOriginal, Date, Time, delta.time.secs, delta.time.mins, delta.time.hours, delta.time.days, Directory, FileName)]
    return(eventtable)
}

makeRecordTableUI = function(id, appLang) {
    ns = NS(id)
    tagList(
      fluidRow(
        # column(3,
        actionBttn(inputId=ns("generateRecordTableBttn"), label=appLang$generateRecordTableBttn, icon("table"), color="primary", inline=T),
        # ),
        # column(3,
        downloadBttn(outputId=ns("exportRecordTableBttn"), label=appLang$exportRecordTableBttn, icon=icon("download"), color="primary")
        # )
      ),
        uiOutput(ns("recordTable"))
    )
}

makeRecordTableServer = function(id, intervals, tags, species, multispecies_tagging, imageRootOriginal) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    recordTableReactive=reactiveVal(NULL)

    observeEvent(input$generateRecordTableBttn, {
      recordTableReactive(makeRecordTable(intervals, tags, species, multispecies_tagging, imageRootOriginal))
    })

    output$recordTable=renderUI({
      if(is.null(recordTableReactive())) return(NULL)
      renderDataTable(recordTableReactive())
    })

    observe({
      if(is.null(recordTableReactive())){
        print("disabling bttn")
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
