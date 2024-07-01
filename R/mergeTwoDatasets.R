library(data.table)
jay=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata//sentByJay.June24/eventTagging_Jay.fixed.csv")
cyang=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata//sentByJay.June24/eventTagging_Chayyang.fixed.csv")

jay[,origin:="Jay"][,ctidevent:=paste(ctid,event)]
cyang[,origin:="Chayyang"][,ctidevent:=paste(ctid,event)]

jay=jay[!is.na(event)]
both=rbind(jay, cyang)

## read the list of camera traps and their taggers
taggers=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/sentByJay.June24/CT_tagger_list.csv")
setnames(taggers, c("ctid", "tagger"))
taggers=taggers[-1]

# "merge" concordance with taggers on ctid, taking into account that ctid can have a wildcard like T2 which matches T2-NK01, T2-NK02, etc.
oldNrowBoth=nrow(both)
both=taggers[, {
  matches = both[grep(paste0("^", ctid), both$ctid)]
  matches
}, by = .(tagger_ctid=ctid, tagger)]
newNrowBoth=nrow(both)
if(oldNrowBoth!=newNrowBoth){
  cli::cli_warn("The number of rows in both has changed from {oldNrowBoth} to {newNrowBoth}")
}

both=both[tagger==origin]

# this should be EMPTY
both[,length(unique(origin)),by=.(ctid,event)][V1>1]

# remove tagger_ctid, tagger, origin
both[,c("tagger_ctid", "tagger", "origin"):=NULL]

# check that we have the same set of ctidevents as before
setequal(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))

# show differences
setdiff(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))

# see what the differences are
jay[ctidevent %in% setdiff(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))]
cyang[ctidevent %in% setdiff(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))]

# For the above, there are no tags in chayyang for these events that are attributed to him, so we use the tags from jay
both=rbind(both[,-c("tagger_ctid", "tagger")], jay[ctidevent %in% setdiff(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))])

# we should recheck
setequal(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))

# this can now be written as the merged tagging dataset
fwrite(both, "/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata//sentByJay.June24/eventTagging_merged.csv")

## Additional code, in case more subtle case by case attribution is needed (does not use the tagger list)
cyang[ctid=="T1-NK01 IE001" & event=="0",]
jay[ctid=="T1-NK01 IE001" & event=="0",]


eventsTaggedByBoth=both[,length(unique(origin)),by=.(ctidevent)][V1>1]$ctidevent
# for each unique ctidevent, check if the two origins agree

concordance=rbindlist(lapply(eventsTaggedByBoth, function(id){
    # if there is only one source return NULL
    if(length(unique(both[ctidevent==id,origin]))==1) return(NULL)
  list(ctidevent=id, isequal=fsetequal(both[ctidevent==id & origin=="Jay",-"origin"], both[ctidevent==id & origin=="Chayyang",-"origin"]))
}))

somedifferences=both[ctidevent %in% concordance[isequal==FALSE][sample(.N, 10)]$ctidevent]
setorder(somedifferences, ctid, event, origin)
somedifferences



print(result)