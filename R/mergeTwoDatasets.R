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
  this.ctid=ctid
  matches = both[grepl(paste0("^", this.ctid), both$ctid)]
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
both=rbind(both, jay[ctidevent %in% setdiff(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent)), -"origin"])

# we should recheck
setequal(unique(c(jay$ctidevent, cyang$ctidevent)), unique(both$ctidevent))

# this can now be written as the merged tagging dataset
fwrite(both[,-c("ctidevent")], "/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata//sentByJay.June24/mergedTagging.csv")

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


### Merging the multispecies tagging
library(data.table)
intervals=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/intervals.csv")
mulEventStatusJay=fread("/mnt/t/CT_Data//NKD_2022/NKD\ 2022/metadata//sentByJay.June24/multipleEventStatus_Jay.csv")
mulEventTagsJay=fread("/mnt/t/CT_Data//NKD_2022/NKD\ 2022/metadata//sentByJay.June24/multipleEventTags_Jay.csv")
# the tag files have the relative path after the root, followed by a species ID, we must find this path in the intervals and merge
rootPathInIntervals="/mnt/d/CT II/NKD_2022/Data processing/raw_images"
intervals[,relPath:=gsub(rootPathInIntervals, "", fn)]
mulEventTagsJay=merge(mulEventTagsJay, intervals[,.(relPath, ctid, interval)], by.x="fn", by.y="relPath")
mulEventStatusChayyang=fread("/mnt/t/CT_Data//NKD_2022/NKD\ 2022/metadata//sentByJay.June24/multipleEventStatus_Chayyang.csv")
mulEventTagsChayyang=fread("/mnt/t/CT_Data//NKD_2022/NKD\ 2022/metadata//sentByJay.June24/multipleEventTags_Chayyang.csv")
# do the same for Chayyang
mulEventTagsChayyang=merge(mulEventTagsChayyang, intervals[,.(relPath, ctid, interval)], by.x="fn", by.y="relPath")

mulEventStatusJay[,c("origin", "ctidevent"):=list("Jay", paste(ctid,interval))]
mulEventStatusChayyang[,c("origin", "ctidevent"):=list("Chayyang", paste(ctid,interval))]

mulEventBoth=merge(mulEventStatusJay[,.(ctidevent, status)], mulEventStatusChayyang[,.(ctidevent, status)], all=TRUE, by="ctidevent", suffixes=c("_Jay", "_Chayyang"))

mulEventBoth[status_Jay!=status_Chayyang]
mulEventBoth[status_Jay==status_Chayyang]
# a lot of complete events in those, which means both of them tagged the images

mulEventTagsJay[,ctidevent:=paste(ctid,interval)]
mulEventTagsChayyang[,ctidevent:=paste(ctid,interval)]

mulEventTagsJay[ctidevent == "T1-NK01 IE001 1"]
mulEventTagsChayyang[ctidevent == "T1-NK01 IE001 1"]

# for those that are complete in both, we have to flag the ctidevents where the tags are different
setnames(mulEventTagsJay, "species", "species_Jay")
setnames(mulEventTagsChayyang, "species", "species_Chayyang")
mulEventMerged=merge(mulEventBoth, mulEventTagsJay[,.(ctidevent, fn, species_Jay)], by="ctidevent", all.x=TRUE)
mulEventMerged=merge(mulEventMerged, mulEventTagsChayyang[,.(ctidevent, fn, species_Chayyang)], by=c("ctidevent", "fn"), all=TRUE)

### Images tagged by both where the tags are different
######################################################
mulEventMerged[status_Jay==status_Chayyang & species_Jay!=species_Chayyang]
# Key: <ctidevent, fn>
#            ctidevent                          fn status_Jay status_Chayyang species_Jay species_Chayyang
#               <char>                      <char>     <char>          <char>       <int>            <int>
# 1: T4-NK20 IE043 195 /T4-NK20/IE043/DCIM3253.JPG   complete        complete         103               47
# 2: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3658.JPG   complete        complete           7              103
# 3:  T4-NK30 IE048 78 /T4-NK30/IE048/DCIM0502.JPG   complete        complete          91               43
taggers[V1 %in% c("T4-NK20 IE043", "T4-NK25 IE051", "T4-NK30 IE048")]
#               V1       V2
#           <char>   <char>
# 1: T4-NK20 IE043 Chayyang
# 2: T4-NK25 IE051 Chayyang
# 3: T4-NK30 IE048 Chayyang
#For these 3, Chayyang is supposed to be the tagger, yet Jay tagged them too and they disagree.
species=fread("/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/species.csv")
species[id %in% c(103, 47, 7, 91, 43)]
#       id          Common Name    Lao Name        Species Name     Group     Family           Order
#    <int>               <char>      <char>              <char>    <char>     <char>          <char>
# 1:     7     Ferret Badger(s)        ໝາລື່ງ        Melogale spp Carnivore Mustelidae       Carnivora
# 2:    43 Red-cheeked Squirrel ກະຮອກແກ້ມແດງ  Dremomys rufigenis    Rodent  Sciuridae        Rodentia
# 3:    47 Northern Red Muntjac      ຟານແດງ Muntiacus vaginalis  Ungulate   Cervidae Cetartiodactyla
# 4:    91      Birds (unknown)      ຕະກູນນົກ                   -      Bird       Bird            Bird
# 5:   103  Unidentified Murids      ປະເພດໜູ                   -    Rodent    Muridae        Rodentia

# For T4-NK20/IE043/DCIM3253.JPG, Jay tagged it as a 103 (unidentified murids) while Chayyang tagged it as 47 (Northern Red Muntjac)
# For T4-NK25/IE051/DCIM3658.JPG, Jay tagged it as 7 (Ferret Badger) while Chayyang tagged it as 103 (unidentified murids)
# For T4-NK30/IE048/DCIM0502.JPG, Jay tagged it as 91 (Birds (unknown)) while Chayyang tagged it as 43 (Red-cheeked Squirrel)
# These animals are quite different
mulEventMerged[fn %in% c("/T4-NK20/IE043/DCIM3253.JPG", "/T4-NK25/IE051/DCIM3658.JPG", "/T4-NK30/IE048/DCIM0502.JPG")]
# Key: <ctidevent, fn>
#            ctidevent                          fn status_Jay status_Chayyang species_Jay species_Chayyang
#               <char>                      <char>     <char>          <char>       <int>            <int>
# 1: T4-NK20 IE043 195 /T4-NK20/IE043/DCIM3253.JPG   complete        complete         103               47
# 2: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3658.JPG   complete        complete           7              103
# 3:  T4-NK30 IE048 78 /T4-NK30/IE048/DCIM0502.JPG   complete        complete          91               43
# 4:  T4-NK30 IE048 78 /T4-NK30/IE048/DCIM0502.JPG   complete        complete          91               91
# For the first one, the muntjac is gone, and the murids are there, so Jay's tag is correct
# For the second one, the ferret badger is there, and the murids are gone, so Jay's tag is correct
# For the last one, both Jay and Chayyang tagged it as 91 (Birds (unknown)) but Chayyang additionally tagged it as 43 (Red-cheeked Squirrel)
# I can confirm on that image there is indeed a squirrel

cat(mulEventMerged[status_Jay=="attention", unique(ctidevent)], sep="\n")
# T2-NK15 IE023 197
# T3-NK34 IE061 23
# T4-NK21 IE055 28
# T4-NK25 IE052 197
# T4-NK30 IE048 41
# T7-NK44 IE139 54
# T7-NK45 IE148 0
# T7-NK53 IE142 41
# T7-NK63 IE146 0
# T7-NK73 IE136 23
# T7-NK73 IE136 54


# images where status is different
unique(mulEventMerged[status_Jay!=status_Chayyang,.(status_Jay, status_Chayyang)])
# All of them are Jay complete and wip chayyang, so we can just take Jay's status
mulEventMerged[status_Jay!=status_Chayyang,c("status_final", "species_final"):=list(status_Jay, species_Jay)]
# for those where the status is the same, and the species are the same, we can just take one of them
mulEventMerged[status_Jay==status_Chayyang & species_Jay==species_Chayyang,c("status_final", "species_final"):=list(status_Jay, species_Jay)]
# The remaining cases, we do one by one
mulEventMerged[ctidevent=="T4-NK20 IE043 195" & fn == "/T4-NK20/IE043/DCIM3253.JPG",c("status_final", "species_final"):=list("complete", 103)]
mulEventMerged[ctidevent=="T4-NK25 IE051 255" & fn == "/T4-NK25/IE051/DCIM3658.JPG",c("status_final", "species_final"):=list("complete", 7)]
mulEventMerged[ctidevent=="T4-NK30 IE048 78" & fn == "/T4-NK30/IE048/DCIM0502.JPG",c("status_final", "species_final"):=list(c("complete", "complete"), c(91, 43))]
mulEventMerged[is.na(status_final)]

# We fix the NAs by manual inspection
# Key: <ctidevent, fn>
#             ctidevent                          fn status_Jay status_Chayyang species_Jay species_Chayyang status_final species_final
#                <char>                      <char>     <char>          <char>       <int>            <int>       <char>         <int>
#  1:  T3-NK34 IE061 23                        <NA>  attention       attention          NA               NA         <NA>            NA
#  2:  T3-NK34 IE062 43 /T3-NK34/IE062/DCIM0340.JPG       <NA>            <NA>          NA               29         <NA>            NA
#  3:  T3-NK34 IE062 43 /T3-NK34/IE062/DCIM0341.JPG       <NA>            <NA>          NA               29         <NA>            NA
#  4:  T3-NK34 IE062 43 /T3-NK34/IE062/DCIM0342.JPG       <NA>            <NA>          NA               29         <NA>            NA
#  5: T4-NK20 IE043 195 /T4-NK20/IE043/DCIM3206.JPG   complete        complete         103               NA         <NA>            NA
#  6: T4-NK20 IE043 195 /T4-NK20/IE043/DCIM3265.JPG   complete        complete         103               NA         <NA>            NA
#  7: T4-NK21 IE050 110                        <NA>   complete        complete          NA               NA         <NA>            NA
#  8: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0569.JPG       <NA>            <NA>          NA              103         <NA>            NA
#  9: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0572.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 10: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0573.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 11: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0574.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 12: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0575.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 13: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0576.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 14: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0577.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 15: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0578.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 16: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0579.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 17: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0580.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 18: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0584.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 19: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0585.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 20: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0586.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 21: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0587.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 22: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0588.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 23: T4-NK21 IE050 110 /T4-NK21/IE050/DCIM0589.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 24:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0419.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 25:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0420.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 26:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0421.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 27:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0422.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 28:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0423.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 29:  T4-NK21 IE055 76 /T4-NK21/IE055/DCIM0424.JPG       <NA>            <NA>          NA               38         <NA>            NA
# 30: T4-NK25 IE051 207 /T4-NK25/IE051/DCIM2995.JPG   complete        complete          43               NA         <NA>            NA
# 31: T4-NK25 IE051 232 /T4-NK25/IE051/DCIM3409.JPG       <NA>            <NA>          NA               39         <NA>            NA
# 32: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3655.JPG       <NA>            <NA>          NA              103         <NA>            NA
# 33: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3656.JPG       <NA>            <NA>          NA              103         <NA>            NA
# 34: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3657.JPG       <NA>            <NA>          NA              103         <NA>            NA
# 35: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3672.JPG       <NA>            <NA>          NA                7         <NA>            NA
# 36: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3673.JPG       <NA>            <NA>          NA                7         <NA>            NA
# 37: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3674.JPG       <NA>            <NA>          NA                7         <NA>            NA
# 38: T4-NK25 IE051 255 /T4-NK25/IE051/DCIM3675.JPG       <NA>            <NA>          NA                7         <NA>            NA
# 39: T4-NK25 IE051 260 /T4-NK25/IE051/DCIM3739.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 40: T4-NK25 IE051 260 /T4-NK25/IE051/DCIM3740.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 41: T4-NK25 IE051 306 /T4-NK25/IE051/DCIM4579.JPG   complete        complete           7               NA         <NA>            NA
# 42: T4-NK25 IE051 306 /T4-NK25/IE051/DCIM4580.JPG   complete        complete           7               NA         <NA>            NA
# 43: T4-NK25 IE051 306 /T4-NK25/IE051/DCIM4581.JPG   complete        complete           7               NA         <NA>            NA
# 44: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7467.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 45: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7468.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 46: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7469.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 47: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7470.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 48: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7471.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 49: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7472.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 50: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7474.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 51: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7475.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 52: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7476.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 53: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7477.JPG       <NA>            <NA>          NA               91         <NA>            NA
# 54: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7497.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 55: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7498.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 56: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7499.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 57: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7500.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 58: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7501.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 59: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7502.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 60: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7503.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 61: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7504.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 62: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7505.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 63: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7506.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 64: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7507.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 65: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7508.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 66: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7509.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 67: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7510.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 68: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7511.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 69: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7512.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 70: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7513.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 71: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7514.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 72: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7515.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 73: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7516.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 74: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7517.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 75: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7518.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 76: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7519.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 77: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7520.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 78: T4-NK25 IE051 366 /T4-NK25/IE051/DCIM7521.JPG       <NA>            <NA>          NA               52         <NA>            NA
# 79: T4-NK25 IE052 197                        <NA>  attention       attention          NA               NA         <NA>            NA
# 80: T4-NK25 IE052 366                        <NA>   complete        complete          NA               NA         <NA>            NA
# 81:  T4-NK26 IE046 25 /T4-NK26/IE046/DCIM0175.JPG       <NA>            <NA>          NA               42         <NA>            NA
# 82:  T4-NK26 IE046 25 /T4-NK26/IE046/DCIM0176.JPG       <NA>            <NA>          NA               42         <NA>            NA
# 83:  T4-NK26 IE046 25 /T4-NK26/IE046/DCIM0187.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 84:  T4-NK26 IE046 25 /T4-NK26/IE046/DCIM0188.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 85:  T4-NK30 IE048 78 /T4-NK30/IE048/DCIM0503.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 86:  T4-NK30 IE048 78 /T4-NK30/IE048/DCIM0504.JPG       <NA>            <NA>          NA               43         <NA>            NA
# 87:  T7-NK53 IE142 41                        <NA>  attention       attention          NA               NA         <NA>            NA
# 88:  T7-NK73 IE136 23                        <NA>  attention       attention          NA               NA         <NA>            NA
# 89:  T7-NK73 IE136 54                        <NA>  attention       attention          NA               NA         <NA>            NA
#             ctidevent                          fn status_Jay status_Chayyang species_Jay species_Chayyang status_final species_final


# T3-NK34 IE062 43 should be complete and tagged as chayyang
mulEventMerged[ctidevent=="T3-NK34 IE062 43" & is.na(status_final),c("status_final", "species_final"):=list("complete", 29)]
# same for T4-NK20 IE043 195
mulEventMerged[ctidevent=="T4-NK20 IE043 195" & is.na(status_final),c("status_final", "species_final"):=list("complete", 103)]
# for T4-NK21 IE050 110, the tagger is Chayyang so we take his
mulEventMerged[ctidevent=="T4-NK21 IE050 110" & is.na(status_final),c("status_final", "species_final"):=list("complete", species_Chayyang)]

eventswithNA=unique(do.call(paste, mulEventMerged[is.na(status_final), tstrsplit(ctidevent, " ")[1:2]]))
taggers[ctid %in% eventswithNA]
#             ctid   tagger
#           <char>   <char>
# 1: T3-NK34 IE061      Jay
# 2: T4-NK21 IE055      Jay
# 3: T4-NK25 IE051 Chayyang
# 4: T4-NK25 IE052      Jay
# 5: T4-NK26 IE046 Chayyang
# 6: T4-NK30 IE048 Chayyang


# T4-NK25 IE051 is also Chayyang
mulEventMerged[grepl("^T4-NK25 IE051 ", ctidevent) & is.na(status_final),c("status_final", "species_final"):=list("complete", species_Chayyang)]
# T4-NK21 IE055 is Jay but he tagged as empty, after examination Chayyang is correct the porcupine is there, we take chayyang
mulEventMerged[ctidevent=="T4-NK21 IE055 76" & is.na(status_final),c("status_final", "species_final"):=list("complete", 38)]
# T4-NK26 IE046 is Chayyang
mulEventMerged[ctidevent=="T4-NK26 IE046 25" & is.na(status_final),c("status_final", "species_final"):=list("complete", species_Chayyang)]
# T4-NK30 IE048 is Chayyang
mulEventMerged[ctidevent=="T4-NK30 IE048 78" & is.na(status_final),c("status_final", "species_final"):=list("complete", species_Chayyang)]
# finally T4-NK25 IE052 366 was tagged by both as complete but has no species, indicating an empty event. Let's tag it as such and confirm later
mulEventMerged[ctidevent=="T4-NK25 IE052 366" & is.na(status_final),c("status_final", "species_final"):=list("complete", NA)]

# the last ones remaining are "attention"
mulEventMerged[is.na(status_final), status_final:="attention"]

# finally we are ready to export the final versions of these files. Status has ctid,interval,status
mulEventMerged[,c("location", "ct", "interval"):=tstrsplit(ctidevent, " ")]
fwrite(unique(mulEventMerged[,.(ctid=paste(location, ct, sep=" "), interval, status=status_final)]), "/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/sentByJay.June24/multipleEventStatus.merged.csv")
# the eventTags just has fn and species, ignoring NA species
fwrite(unique(mulEventMerged[!is.na(species_final),.(fn, species=species_final)]), "/mnt/t/CT_Data//NKD_2022/NKD 2022/metadata/sentByJay.June24/multipleEventTags.merged.csv")
