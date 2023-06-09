#!/usr/bin/env Rscript

library(data.table)
library(exifr)
library(chron)
library(EXIFr)
suppressPackageStartupMessages(library("optparse"))

inform=function(...){
    cat(paste0("[ 🛈 ] ", paste(...), "\n"))
    flush.console()
}

error=function(...){
    cat(paste0("[ ✗ ] ", paste(...), "\n"))
    stop(call.=F)
}


warn=function(...){
    cat(paste0("[ ⚠ ] ", paste(...), "\n"))
    flush.console()
}

# create parser object
parser <- OptionParser(description="CT data processing tool. Author: Arthur Gilly (arthur.gilly@protonmail.ch)")

# specify our desired options
# parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
#     help="Print extra output [default]")
# parser$add_argument("-q", "--quietly", action="store_false",
#     dest="verbose", help="Print little output")
parser = add_option(parser, c("-i", "--inputdir"),
    help = "[REQUIRED] Input directory. This should contain locations, which should contain CT subdirs. Any downstream directory structure will be ignored.")
parser = add_option(parser, c("-o", "--outbase"),
    help="[REQUIRED] Base for naming output files. \"-o test\" will generate test.csv.gz, etc.")
parser = add_option(parser, c("-t", "--interval"), type="integer", default=30,
        help="Maximum time between snapshots to trigger an independent event, in minutes. [default %default min]",
        metavar="number")
parser = add_option(parser, c("-l", "--location"), action="store_true", 
                default=FALSE, help="Consider all cameras to be dependent at a given location [default %default].")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parse_args(parser)

if(is.null(args$inputdir) | is.null(args$outbase)){
  stop("-i and -o are mandatory arguments. See --help.")
}

if(!dir.exists(args$inputdir)){
  write(paste0("Error: supplied directory \"", args$inputdir, "\" does not exist."), stderr())
}
cat("\n")

ofn=paste0(args$outbase, ".datetime.csv.gz")
if(file.exists(ofn)){
  inform("Timestamps already extracted, skipping this step.")
  datetime=fread(ofn)
  inform("Reusing", nrow(datetime), "records.")
}else{
  inform("Checking directory, please wait...")
  fl=list.files(args$inputdir, pattern=".*\\.(jpg|jpeg)$", recursive=T, full.names=T, ignore.case=T)
  inform(length(fl), "images detected.")
  inform("Extracting timestamps, please wait...")
  datetime=rbindlist(lapply(fl, function(fn){date=NA;try({date=read_exif_tags(fn)[["DateTime"]]});return(data.table(fn=fn,date=date))}))
  inform(nrow(datetime), "timestamps extracted. Writing to file.")
  fwrite(datetime, ofn)
}

ofi=paste0(args$outbase, ".ranges.pdf")
inform("Generating CT range plot.")
datetime[,c("date", "time"):=tstrsplit(date, " ")]
datetime[,dt:=chron(dates=date, times=time, format=c("y:m:d", "h:m:s"))]
datetime[,c("date", "time"):=NULL]
# two directories down from the base is the location and CT
base=args$inputdir
base=ifelse(base %like% "/$", base, paste0(base, "/"))
inform("Preparing")
datetime[,c("location", "ct"):=tstrsplit(sub("^/+", "", sub(base, "", fn)), "/")[1:2]]
if(args$location){
    ctrange=datetime[,list(min(dt), max(dt)), by=c("location")]
}else{
    ctrange=datetime[,list(min(dt), max(dt)), by=c("location", "ct")]
}
timerange=as.numeric(c(min(c(ctrange$V1, ctrange$V2), na.rm=T), max(c(ctrange$V1, ctrange$V2), na.rm=T)))
upper=(timerange[2]-timerange[1])*1.2
pdf(ofi, width=10, height=25)

inform("Page 1")
#page 1 : whole plot
plot(x=as.Date(c(ctrange$V1, ctrange$V2)), y=rnorm(nrow(ctrange)*2)*nrow(ctrange), ylim=c(1,nrow(ctrange)), xaxt="n",type="n", xlab="time ranges",
     ylab="", xlim=c(min(c(ctrange$V1, ctrange$V2), na.rm=T), upper),
    yaxt="n")
segments(x0=as.Date(ctrange$V1), x1=as.Date(ctrange$V2), y0=1:nrow(ctrange))
axis(1, as.Date(c(ctrange$V1, ctrange$V2)), format(as.Date(c(ctrange$V1, ctrange$V2)), "%b %Y"), cex.axis = 1, las=0)
text(x=rep(max(ctrange$V2, na.rm=T), nrow(ctrange)), y=1:nrow(ctrange), paste(ctrange$location, ctrange$ct), pos=4)

inform("Page 2")
# page 2: outliers in range
par(mfrow=c(5,1))
ctrange[,rdiff:=V2-V1]
ctrange.full=ctrange
u=mean(ctrange$rdiff)
s=sd(ctrange$rdiff)
m=median(ctrange$rdiff)
mad=mad(ctrange$rdiff)
ctrange=ctrange[rdiff>m+10*mad]
if(nrow(ctrange)){
  warn("The following CTs have abnormally large ranges:", paste(ctrange$location, ctrange$ct, collapse=", "))
  timerange=as.numeric(c(min(c(ctrange$V1, ctrange$V2), na.rm=T), max(c(ctrange$V1, ctrange$V2), na.rm=T)))
  upper=(timerange[2]-timerange[1])*1.2
  plot(x=as.Date(c(ctrange$V1, ctrange$V2)), y=rnorm(nrow(ctrange)*2)*nrow(ctrange), ylim=c(1,nrow(ctrange)), xaxt="n",type="n", xlab="time ranges",
       ylab="", xlim=c(min(c(ctrange$V1, ctrange$V2), na.rm=T), upper),
      yaxt="n", main="Abnormally large ranges")
  segments(x0=as.Date(ctrange$V1), x1=as.Date(ctrange$V2), y0=1:nrow(ctrange))
  axis(1, as.Date(c(ctrange$V1, ctrange$V2)), format(as.Date(c(ctrange$V1, ctrange$V2)), "%b %Y"), cex.axis = 1, las=0)
  text(x=rep(max(ctrange$V2, na.rm=T), nrow(ctrange)), y=1:nrow(ctrange), paste(ctrange$location, ctrange$ct), pos=4)
  fail1=paste(ctrange$location, ctrange$ct)
}

inform("Page 3")
# page 3: outliers in start
ctrange=ctrange.full
m=median(as.numeric(ctrange$V1))
mad=mad(as.numeric(ctrange$V1))
ctrange=ctrange[V1<m-10*mad]
if(nrow(ctrange)){
  warn("The following CTs have abnormally early start dates:", paste(ctrange$location, ctrange$ct, collapse=", "))
  timerange=as.numeric(c(min(c(ctrange$V1, ctrange$V2), na.rm=T), max(c(ctrange$V1, ctrange$V2), na.rm=T)))
  upper=(timerange[2]-timerange[1])*1.2
  plot(x=as.Date(c(ctrange$V1, ctrange$V2)), y=rnorm(nrow(ctrange)*2)*nrow(ctrange), ylim=c(1,nrow(ctrange)), xaxt="n",type="n", xlab="time ranges",
       ylab="", xlim=c(min(c(ctrange$V1, ctrange$V2), na.rm=T), upper),
      yaxt="n", main="Abnormally early start dates")
  segments(x0=as.Date(ctrange$V1), x1=as.Date(ctrange$V2), y0=1:nrow(ctrange))
  axis(1, as.Date(c(ctrange$V1, ctrange$V2)), format(as.Date(c(ctrange$V1, ctrange$V2)), "%b %Y"), cex.axis = 1, las=0)
  text(x=rep(max(ctrange$V2, na.rm=T), nrow(ctrange)), y=1:nrow(ctrange), paste(ctrange$location, ctrange$ct), pos=4)
  fail2=paste(ctrange$location, ctrange$ct)
}
invisible(dev.off())

fail=c(fail1, fail2)
ctrange.full[,failQC:=FALSE]
if(args$location) ctrange.full[location %in% fail, failQC:=FALSE] else ctrange.full[paste(location, ct) %in% fail, failQC:=FALSE]
setnames(ctrange.full, c("V1", "V2"), c("start", "end"))
oft=paste0(args$outbase, ".ranges.csv")
fwrite(ctrange.full, oft)
inform("Wrote CT ranges to", oft, ".")

inform("Applying interval of", args$interval, "minutes.")
setorder(datetime, location, ct, dt)

if(args$location){
datetime[,interval:=lapply(.SD, function(v){
    vdiff=difftime(v[-1], v[-length(v)], units="sec")
    vdiff=c(0, vdiff)
    vdiff=vdiff<args$interval*60
    return(cumsum(!vdiff))
}),.SDcols="dt",by=c("location")]
    }else{
    datetime[,interval:=lapply(.SD, function(v){
        vdiff=difftime(v[-1], v[-length(v)], units="sec")
        vdiff=c(0, vdiff)
        vdiff=vdiff<args$interval*60
        return(cumsum(!vdiff))
    }),.SDcols="dt",by=c("location", "ct")]
    }
ofint=paste0(args$outbase, ".intervals.csv")
fwrite(datetime, ofint)
inform("Wrote", sum(datetime[,max(interval), by=c("location", "ct")]$V1), "independent event data to", ofint)
