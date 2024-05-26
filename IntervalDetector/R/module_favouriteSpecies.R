# UI creation function
favouriteSpeciesUI = function(id) {
    ns = NS(id)
    # UI code goes here
    tagList(
        uiOutput(ns("favouriteSpeciesButtonsUI")),
        DTOutput(ns("existingTags"))
    )
}

# Server function calling moduleServer
favouriteSpeciesServer = function(id, input, output, session, species_df, favouriteSpeciesIds, currentTagging, ctidSelected, eventSelected) {
    # Server code goes here
    moduleServer(id, function(input, output, session) {
        ns = session$ns

        buttonObserverList=reactiveValues()

        # favouriteSpeciesButtonsUI contains the buttons for the favourite species
        output$favouriteSpeciesButtonsUI = renderUI({
            tagList(
                lapply(favouriteSpeciesIds(), function(i) {
                    # if the observer for this button does not exist, create it
                    if(is.null(buttonObserverList[[paste0("favouriteSpeciesBttn_", i)]])) {
                        buttonObserverList[[paste0("favouriteSpeciesBttn_", i)]] = observeEvent(input[[paste0("favouriteSpeciesBttn_", i)]], {
                            print(paste("Button", i, "clicked"))
                            addSpeciesById(currentTagging, species_df, i, ctidSelected(), eventSelected())
                            print(currentTagging$displayTable)
                        })
                    }
                    actionButton(
                        inputId = ns(paste0("favouriteSpeciesBttn_", i)),
                        label = species_df[i, paste(`Common Name`, "(", `Lao Name`, ") -", `Species Name`)],
                        icon = icon("star"),
                        size = "sm",
                        inline=T
                    )
                })
            )
        })

  output$existingTags=renderDT({
    if(!is.null(currentTagging$displayTable) & nrow(currentTagging$displayTable) & !(is.na(currentTagging$displayTable$group[1]))){
      return(currentTagging$displayTable)
    }else{
      return(NULL)
    }
})

    })
    
}

displayTableFromInternal = function(internalTable, selctid, selevent, speciesData) {
  currentInternalTable = internalTable[ctid==selctid & event==selevent]
  dispTable = merge(currentInternalTable, speciesData, by.x="speciesID", by.y="id", all.x=T)
  dispTable = dispTable[ctid==selctid & event==selevent]
  dispTable[,c("ctid", "event", "numInd", "speciesID"):=NULL]
  setcolorder(dispTable, c("indID", "indName", "Common Name", "Lao Name", "Species Name", "Group", "Family", "Order", "Sex", "Age"))
  setnames(dispTable, c("id", "individual", "common_name", "lao_name", "scientific_name", "group", "family", "order", "Sex", "Age"))
  return(dispTable)
}

addSpeciesById=function(currentTagging, speciesData, speciesId, ctidSelected, eventSelected){
    print(paste("Adding species", speciesId, "to ctid", ctidSelected, "event", eventSelected))
    selectedSpecies=speciesData[speciesData$id==speciesId]
    # if the event was previously tagged as empty, the displaytable is empty and the internaltable has a row with numInd=0
    selectedInternalTable=currentTagging$internalTable[ctid==ctidSelected & event==eventSelected]
    isTaggedEmpty=nrow(selectedInternalTable)==1 && selectedInternalTable$numInd==0
    # if that is true, remove that row
    if(isTaggedEmpty)
        currentTagging$internalTable=currentTagging$internalTable[!(ctid==ctidSelected & event==eventSelected)]
    nextIndId=ifelse(nrow(currentTagging$displayTable), max(currentTagging$displayTable$id)+1, 0)
    newDisplayRow=data.table(
        id=nextIndId,
        individual="",
        common_name=selectedSpecies$`Common Name`,
        lao_name=selectedSpecies$`Lao Name`,
        scientific_name=selectedSpecies$`Species Name`,
        group=selectedSpecies$Group,
        family=selectedSpecies$Family,
        order=selectedSpecies$Order,
        Sex="unknown",
        Age="unknown")
    if(isTaggedEmpty)
        currentTagging$displayTable=newDisplayRow
    else 
        currentTagging$displayTable=rbind(currentTagging$displayTable, newDisplayRow)
    
    newInternalRow=data.table(
        ctid=ctidSelected,
        event=eventSelected,
        numInd=nrow(selectedInternalTable)+1,
        indID=nextIndId,
        speciesID=speciesId,
        indName="",
        Sex="unknown",
        Age="unknown")
    currentTagging$internalTable=rbind(currentTagging$internalTable, newInternalRow)
    currentTagging$internalTable[ctid==ctidSelected & event==eventSelected, numInd:=nrow(selectedInternalTable)+1]
}

# Demo function running the module as a standalone app
favouriteSpeciesDemo = function() {
    library(shiny)
    library(data.table)
    library(glue)
    library(shinyWidgets)
    library(bslib)
    species=fread("/mnt/t/CT_Data/sus_scrofa/sus_scrofa.sadpaR_data/metadata/species.csv")
    eventTagging=fread("/mnt/t/CT_data/NKD_2022/NKD 2022/tagging/eventTagging.csv")
    # take 5 random species
    favouriteSpeciesIds = sample(species$id, 5)
    app=shinyApp(
        ui = page_fillable(
            layout_sidebar(
            sidebar=sidebar("controls",
            selectInput("ctidSel", "camera trap", choices=unique(eventTagging$ctid)),
            selectInput("eventSel", "event", choices=unique(eventTagging$event))
            ),
            favouriteSpeciesUI("favouriteSpecies")
        )
        ),
        server = function(input, output, session) {
            currentTagging=reactiveValues()

            observe({
                currentTagging$internalTable=eventTagging
                currentTagging$displayTable=displayTableFromInternal(eventTagging, input$ctidSel, input$eventSel, species)

            })

            favouriteSpeciesServer("favouriteSpecies",input, output, session, species, reactiveVal(favouriteSpeciesIds), currentTagging, ctidSelected=reactiveVal(input$ctidSel), eventSelected=reactiveVal(input$eventSel))
        }
    )
    runApp(app, port=5145, launch.browser=F)
}

