internalSelectInputCoherenceAndDurationGeneration=function(session, input, output, loadedDataset, currentTagging, dur, rootDir){
  observeEvent(input$whichCT, {
    iselected=input$whichCT
    if(iselected !=""){
      dat=loadedDataset$interval_data
      head(dat)
      dat=dat[ctid==iselected]
      durations=dat[,list(mean(as.numeric(dt)), .N, difftime(max(as.POSIXct(dt, origin="1970-01-01 00:00:00")), min(as.POSIXct(dt, origin="1970-01-01 00:00:00")), units="min")), by=interval]
      setnames(durations, c("sequence", "avgdate", "num_images", "duration"))
      #print(head(durations))
      durations[,avgdate:=as.character(chron(avgdate))]
      #print(head(durations))
      durations[,duration:=ceiling(as.numeric(duration))]
      setcolorder(durations, c("avgdate", "sequence", "num_images", "duration"))
      dur$durations=as.data.table(durations)

      chosenCT=input$whichCT
      print(666)
      selectedFn=loadedDataset$interval_data[location==strsplit(chosenCT, " ")[[1]][1] & ct == strsplit(chosenCT, " ")[[1]][2],fn]
      choices=tstrsplit(selectedFn, loadedDataset$imagePath)[[2]]
      updateSelectInput(session, inputId="PicInSequence", choices=choices, selected=choices[1])
    }
    if(iselected!=input$whichCTSeq){
      updateSelectInput(session, inputId="whichCTSeq", selected=iselected)
    }
    if(iselected!=input$tagCT){
      updateSelectInput(session, inputId="tagCT", selected=iselected)
    }

  })

  observeEvent(input$whichCTSeq, {
    iselected=input$whichCTSeq
    if(iselected!=""){
      choices=unique((loadedDataset$interval_data)[ctid==iselected]$interval)
      #print("observe whichCTSeq called.")
      updateSelectInput(session, inputId="sequence", choices=choices)
    }
    if(iselected!=input$whichCT){
      updateSelectInput(session, inputId="whichCT", selected=input$whichCTSeq)
    }
    if(iselected!=input$tagCT){
      updateSelectInput(session, inputId="tagCT", selected=input$whichCTSeq)
    }
  })

  observeEvent(input$tagCT, {
    iselected=input$tagCT
    if(iselected!=""){
      choices=unique((loadedDataset$interval_data)[ctid==iselected]$interval)
      updateSelectInput(session, inputId="tagSequence", choices=choices)
    }
    if(iselected!=input$whichCT){
      updateSelectInput(session, inputId="whichCT", selected=input$whichCTSeq)
    }
    if(iselected!=input$tagCT){
      updateSelectInput(session, inputId="whichCTSeq", selected=input$whichCTSeq)
    }
  })

  observeEvent(input$sequence, {
    iselected=input$sequence
    #print("sequence observe called")
    #print(paste("sequence is ", iselected, ", tagsequence is", input$tagSequence))
    #print(paste("currently selected", input$tabs))
    if(input$tagSequence!=iselected & input$tabs=="Sequence"){
      updateSelectInput(session, inputId="tagSequence", selected=iselected)
    }
    if(!is.null(input$ChooseEdit)){
      if(input$ChooseEdit!=iselected & input$tabs=="Sequence"){
        updateSelectInput(session, inputId="ChooseEdit", selected=iselected)
      }
    }
    #else{
     # print("this did nothing")
    #}
  })

  observeEvent(input$tagSequence, {
    iselected=input$tagSequence
    if(iselected !="") updateTaggingOnSeqChange(session, input, output, currentTagging, loadedDataset, selevent=input$tagSequence)
    #print("tagsequence observe called")
    #print(paste("tagsequence is ", iselected, ", sequence is", input$sequence))
    #print(paste("currently selected", input$tabs))
    if(input$sequence!=iselected & input$tabs=="Tagging"){
      updateSelectInput(session, inputId="sequence", selected=iselected)
    }
    if(!is.null(input$ChooseEdit)){
      if(input$ChooseEdit!=iselected & input$tabs=="Sequence"){
        updateSelectInput(session, inputId="ChooseEdit", selected=iselected)
      }
    }
    #else{
     # print("this did nothing")
     #}
  })

  observeEvent(input$ChooseEdit, {
    chosenCT=input$whichCT
    if(!is.null(chosenCT)){
      #print("going in")
      if(length(input$ChooseEdit)==1){
        shinyjs::disable("mergeEdit")
        print(777)
        print("chosenCT")
        print(strsplit(chosenCT, " "))
        selectedFn=loadedDataset$interval_data[location==strsplit(chosenCT, " ")[[1]][1] & ct == strsplit(chosenCT, " ")[[1]][2] & interval == as.integer(input$ChooseEdit),fn]
        #print("one")
        #print(head(selectedFn))
        choices=tstrsplit(selectedFn, loadedDataset$imagePath)[[2]]
        #print("two")
        updateSelectInput(session, inputId = "PicInSequence", choices=choices, selected=choices[1])
        updateSelectInput(session, inputId = "sequence", selected=input$ChooseEdit)
        updateSelectInput(session, inputId = "tagSequence", selected=input$ChooseEdit)
        #print("end")
      }else{
        shinyjs::enable("mergeEdit")
      }
    }
  })

  observeEvent(input$mergeEdit, {
    #print("observe chooseedit called")
    chosenCT=input$whichCT
    if(!is.null(chosenCT)){
      #print("going in")
      if(length(input$ChooseEdit)>1){
        print(input$ChooseEdit)
        #shinyjs::enable("mergeEdit")
        # the merge action is triggered
        ## get the highest id, increment it
        print(888)
        selectedLoc=strsplit(chosenCT, " ")[[1]]
        selectedCT=selectedLoc[2]
        selectedLoc=selectedLoc[1]
        newSequenceId=max(loadedDataset$interval_data[location==selectedLoc & ct == selectedCT]$interval)+1
        ## save datasets as they are in a backup directory, a function can do this - including the tagging
        #print(rootDir())
        print(head(loadedDataset$ct))
        #withProgress(message = 'Generating new sequence and reloading dataset...', value = 0, {
        progressSweetAlert(session = session, id="loadDatasetPBar", value=0, display_pct=T, title="Generating new sequences and reloading dataset...", status="warning", striped=T, size="l")

        saveBackup(session, input, output, rootDir, loadedDataset, currentTagging, dur, sub(" ", ".", chosenCT), input$ChooseEdit)
        mergeGIFs(rootDir, sub(" ", ".", chosenCT), input$ChooseEdit, newSequenceId)
        ## replace all rows in the interval table with the new ID
        loadedDataset$interval_data[location==selectedLoc & ct==selectedCT & interval %in% as.integer(input$ChooseEdit), interval:=newSequenceId]
        ## destroy tagging info for both IDs
        currentTagging$internalTable=currentTagging$internalTable[!(ctid == chosenCT & event %in% as.integer(input$ChooseEdit))]
        ## trigger a save of the whole dataset
        saveDataset(rootDir, loadedDataset, currentTagging)
        ## reload the dataset ## may be done by modifying
        loadDataset(session, input, output, rootDir, loadedDataset, currentTagging, dur)
      closeSweetAlert(session = session)
      #})
      }
    }

  })

  observeEvent(input$splitEdit, {
    chosenCT=input$whichCT
    print(999)
    selectedLoc=strsplit(chosenCT, " ")[[1]]
    selectedCT=selectedLoc[2]
    selectedLoc=selectedLoc[1]
    if(length(input$PicInSequence)==1){
      newSequenceId=max(loadedDataset$interval_data[location==selectedLoc & ct == selectedCT]$interval)+1
      choices=loadedDataset$interval_data[location==selectedLoc & ct==selectedCT & interval==input$ChooseEdit]$fn
      print(choices)
      imagesBefore=grep(input$PicInSequence, choices)
      imagesAfter=choices[seq(imagesBefore, length(choices))]
      imagesBefore=choices[seq(1, imagesBefore-1)]
      #withProgress(message = 'Generating new sequence and reloading dataset...', value = 0, {
      progressSweetAlert(session = session, id="loadDatasetPBar", value=0, display_pct=T, title="Generating new sequence and reloading dataset...", status="warning", striped=T, size="l")

        saveBackup(session, input, output, rootDir, loadedDataset, currentTagging, dur, sub(" ", ".", chosenCT), input$ChooseEdit)
        loadedDataset$interval_data[location==selectedLoc & ct==selectedCT & interval==input$ChooseEdit & fn %in% imagesBefore, interval:=newSequenceId]
        loadedDataset$interval_data[location==selectedLoc & ct==selectedCT & interval==input$ChooseEdit & fn %in% imagesAfter, interval:=newSequenceId+1]
        generateNewGIFs(rootDir, loadedDataset, selectedLoc, selectedCT,c(newSequenceId, newSequenceId+1))
        ## destroy tagging info for both IDs
        currentTagging$internalTable=currentTagging$internalTable[!(ctid == chosenCT & event %in% as.integer(input$ChooseEdit))]
        ## trigger a save of the whole dataset
        saveDataset(rootDir, loadedDataset, currentTagging)
        ## reload the dataset ## may be done by modifying
        loadDataset(session, input, output, rootDir, loadedDataset, currentTagging, dur)
      closeSweetAlert(session = session)
      #})
    } else{
        print("error, >1 split points selected")
    }   
    })
}
