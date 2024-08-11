library(data.table)

# this file was used to fix the multipleEventTags.csv file when it was broken by a bug in multispecies tagging
# the bug removed most of the "allowed species" per image due to incorrect unlisting of the species list
# the code below reads all the multipleEventTags.csv files and iterately compares them to the previous file, building a new multipleEventTags.csv file
# it also checks that the species in the multipleEventTags.csv file are consistent with the species in the eventTagging file
 
mt=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022//tagging/multipleEventTags.csv")
intervals=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/intervals.csv")
intervals[,fn:=sub("/mnt/d/CT II/NKD_2022/Data processing/raw_images", "", fn, fixed=T)]
m=merge(mt, intervals)
species=fread("/mnt/t/CT_Data/NKD_2022/NKD\ 2022/metadata/species.csv")

taggingFiles=list.files("/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups/", pattern="multipleEventTag", recursive = T, full.names=T)
eventOfInterestTaggedFiles=intervals[ctid=="T1-NK01 IE002" & interval==258]$fn

# read all taggingFiles and store the ones that have tags for these files in fn
foundFiles=c()
for (taggingFile in taggingFiles){
  mt=fread(taggingFile)
  if (any(mt$fn %in% eventOfInterestTaggedFiles)){
    foundFiles=c(foundFiles, taggingFile)
  }
}

# sort all files by the number of rows
filestats=data.table(fn=taggingFiles)
filestats[,date:=sapply(fn, function(x) basename(dirname(dirname(x))))]
filestats[,nrow:=sapply(fn, function(x) nrow(fread(x)))]
filestats[,taggingFile:=sapply(fn, function(x) list.files(dirname(x), pattern="eventTagging", full.names=T))]
# make sure date is converted to date according to "28.07.2024-05.38"
filestats[,date:=as.Date(date, format="%d.%m.%Y-%H.%M")]
setorder(filestats, date)

d=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv")
# starting with fn = /mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv
# read each taggingFile in the dt, and compare it to the previous taggingFile. We are looking for events that have changed.
# specifically, construct ctidevent by pasting ctid and event. Remove the additional ctidevents present in the new file.

# # read the first file
# et=fread(filestats[fn=="/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv"]$taggingFile)
# et[,ctidevent:=paste(ctid, event, sep=" ")]
# i=which(filestats$fn=="/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv")
# # loop through all files and stop when differences are detected
# for (i in (i+1):nrow(filestats)){
#   et2=fread(filestats[i]$taggingFile)
#   et2[,ctidevent:=paste(ctid, event, sep=" ")]
#   # if events are deleted, stop
#     if (any(!(et$ctidevent %in% et2$ctidevent))){
#         print("Error : events deleted")
#         print(setdiff(et$ctidevent, et2$ctidevent))
#         print(i)
#         break
#     }
#   et2=et2[ctidevent %in% et$ctidevent]
#   if(!fsetequal(et, et2)){
#     print("Error : events changed. BEFORE:")
#     diffs=fsetdiff(et, et2)
#     diffs=merge(diffs, species, by.x="speciesID", by.y="id")
#     print(diffs)
#     print("AFTER:")
#     diffs=et2[ctidevent %in% diffs$ctidevent]
#     diffs=merge(diffs, species, by.x="speciesID", by.y="id")
#     print(diffs)
#     print(i)
#     break
#   }
# }

# read the first file
et = fread(filestats[fn == "/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv"]$taggingFile)
et[, ctidevent := paste(ctid, event, sep = " ")]


i = which(filestats$fn == "/mnt/t/CT_Data//NKD_2022/NKD 2022/jay_backups//16.07.2024-00.53/tagging/multipleEventTags.csv")

curMulSpTagging = fread(filestats[i]$fn)
curMulSpTagging = merge(curMulSpTagging, intervals, by = "fn")
curMulSpTagging = curMulSpTagging[, list(list(unique(species))), by = fn]
setnames(curMulSpTagging, "V1", "speciesListOld")
# Outer loop to reset i and continue comparison
while (i < nrow(filestats)) {
    print(glue::glue("i is {i}"))
  # Read the new file for comparison
  et = fread(filestats[i]$taggingFile)
  et[, ctidevent := paste(ctid, event, sep = " ")]
  # Inner loop to compare tagging files
  for (j in (i + 1):nrow(filestats)) {
    print(glue::glue("j is {j}"))
    et2 = fread(filestats[j]$taggingFile)
    et2[, ctidevent := paste(ctid, event, sep = " ")]
    
    # if events are deleted, stop and reset i
    if (any(!(et$ctidevent %in% et2$ctidevent))){
      print("Error : events deleted")
      print(setdiff(et$ctidevent, et2$ctidevent))
      print(j)
      i <<- j
      break
    }
    # print number of new events tagged
    newEventsTagged= length(et2[!ctidevent %in% et$ctidevent, ctidevent])
    if(newEventsTagged){
      print(glue::glue("New events tagged: {newEventsTagged}"))
    }
    et2 = et2[ctidevent %in% et$ctidevent]
    
    # if events are changed, stop and reset i
    if (!fsetequal(et, et2)) {
      print("Error : events changed. BEFORE:")
      diffs = fsetdiff(et, et2)
      diffs = merge(diffs, species, by.x = "speciesID", by.y = "id")
      print(diffs)
      print("AFTER:")
      diffs = et2[ctidevent %in% diffs$ctidevent]
      diffs = merge(diffs, species, by.x = "speciesID", by.y = "id")
      print(diffs)
      print(j)
      i <<- j
      break
    }

    # check if tagging and mulsptagging are coherent, i.e. that all fn x intervals $ species are also present in eventTagging, per ctidevent

    newMulSpTagging=fread(filestats[j]$fn)
    newMulSpTagging=merge(newMulSpTagging, intervals, by="fn")
    newMulSpTagging[,ctidevent:=paste(ctid, interval, sep=" ")]
    spListByEvent=newMulSpTagging[, list(list(unique(species))), by=ctidevent]
    setnames(spListByEvent, "V1", "speciesListMulSP")
    newMulSpTaggingWithEvent=merge(spListByEvent, et2[,list(list(unique(speciesID))), by=ctidevent], by="ctidevent")
    setnames(newMulSpTaggingWithEvent, "V1", "speciesListEvent")
    # create a column isEq that is true for each row if all of the sp in speciesListMulSP are in those for speciesListEvent
    newMulSpTaggingWithEvent[,isEq:=sapply(1:nrow(newMulSpTaggingWithEvent), function(x) all(newMulSpTaggingWithEvent$speciesListMulSP[[x]] %in% newMulSpTaggingWithEvent$speciesListEvent[[x]]))]
    if(nrow(newMulSpTaggingWithEvent[isEq==F])){
      print(glue::glue("Number of discrepancies: {sum(!newMulSpTaggingWithEvent$isEq)}"))
      print(newMulSpTaggingWithEvent[isEq==F])
    }


    # print fsetdiff between the new multiple species tagging file and the current
    newMulSpTagging=fread(filestats[j]$fn)
    newMulSpTagging=merge(newMulSpTagging, intervals, by="fn")
    newMulSpTagging = newMulSpTagging[, list(list(unique(species))), by = fn]
    setnames(newMulSpTagging, "V1", "speciesListNew")
    diffs=merge(curMulSpTagging, newMulSpTagging, by="fn")
    # per line, check if the sets are equal
    diffs[,isEq:=sapply(1:nrow(diffs), function(x) setequal(speciesListOld[x], speciesListNew[x]) )]
    diffs=diffs[isEq==F]
    if(nrow(diffs)){
      print(glue::glue("Discrepancies between current and new multiple species tagging file: {nrow(diffs)}"))
      print(diffs)
    }
    # expand the current multiple species tagging file to include the new species
    newMulSpTagging[,speciesListOld:=speciesListNew][,speciesListNew:=NULL]
    curMulSpTagging<<-rbind(curMulSpTagging[!(fn %in% diffs$fn)], diffs[,.(fn, speciesListOld=speciesListNew)], newMulSpTagging[!fn %in% curMulSpTagging$fn])
  }
  
  # If inner loop completes without discrepancies, exit the outer loop
  if (j == nrow(filestats)) break
  
}

# explode the speciesListOld column into multiple rows
fwrite(curMulSpTagging[, list(species=unlist(speciesListOld)), by=fn], "/mnt/t/CT_Data/NKD_2022//NKD 2022/tagging/multipleEventTags.csv")

mulEventSt=fread("/mnt/t/CT_Data/NKD_2022//NKD 2022/tagging/multipleEventStatus.csv")
mulEventSt[,ctidint:=paste(ctid, interval, sep=" ")]

# are there any empty events?
curMulSpTagging=merge(curMulSpTagging, intervals, by="fn")
curMulSpTagging[,ctidevent:=paste(ctid, interval, sep=" ")]

mulEventSt$ctidint[!mulEventSt$ctidint %in% curMulSpTagging$ctidevent]