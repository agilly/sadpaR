# camtrapr=fread("Sync/NKD2022_record_table_30min_deltaT_2023-08-02.csv")
# evts=fread("Sync/CameraTrapAI/NKD 2022/metadata//intervals.csv")
# evts[,dt2:=chron(dt)]
# theirs=camtrapr[Station=="T4-NK31" & Camera=="IE040",.(V1,DateTimeOriginal)]
# ours=evts[ctid=="T4-NK31 IE040",.SD[1], by=interval][,.(interval, dt2)]
# ours[,dt3:=as.POSIXct(dt2)]
# attr(ours$dt3, "tzone")="GMT"
# ours[,dt2:=NULL]
# setnames(ours, c("sadpar.interval", "time"))
# setkey(ours, time)
# setnames(theirs, c("camtrapr.interval", "time"))
# setkey(theirs, "time")
# ours[,sadpar.time:=time]
# theirs[,camtrapr.time:=time]
# ours[theirs, roll="nearest"]
