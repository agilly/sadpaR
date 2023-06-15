grayfactor=function(imagepath){
	image=load.image(imagepath)
	return(data.table(image=imagepath,
	value=mean(abs(R(image)-B(image))+abs(R(image)-G(image))+abs(G(image)-B(image)))))
}
dn="/mnt/d/CT II/NKD_2022/Data processing/raw_images/T1-NK12/IE015"
tofs=list.files(dn, pattern="JPG|jpg")
isgray=rbindlist(lapply(paste(dn, tofs, sep="/"), grayfactor))
isgray[,cluster:=pam(value, k=2)$cluster]
medoids=isgray[pam(isgray$value, k=2)$id.med]$value
switch_values=function(x){
	# vector of 1 and 2
	x[x==1]=3
	x[x==2]=1
	x[x==3]=2
	return(x)
}
if(medoids[2]<medoids[1]) isgray[,cluster:=switch_values(cluster)]
d=merge(isgray, intervals, by.x="image", by.y="fn")
if(mean(d[hr<5]$cluster-1) < mean(d[hr>11 & hr<16]$cluster-1)){
	print("correctly aligned")
}else{
	print("wrongly aligned")
}


intervals=fread("/mnt/k/Sync/CameraTrapAI//NKD.rawImages.datetime.csv.gz")
intervals[,date:=as.POSIXct(date, format="%Y:%m:%d %H:%M:%S")]
intervals[,hr:=as.integer(format(date, "%H"))]
#intervals[,fn:=sub("//", "/", fn)]
intervals[,dn:=dirname(fn)]

check_directory_AMPM=function(dir.name, d, subsample=NULL, night=c(0,5), day=c(9, 16)){
	dmin=d[dn==dir.name]
	print(dir.name)
	tofs=list.files(dir.name, pattern="JPG|jpg")
	if(!is.null(subsample)){
		spl=sample(nrow(tofs), subsample)
		tofs=tofs[spl]
	}
	if(!length(tofs)) return(data.table(dn=dn, isCorrect=NA))
	isgray=rbindlist(lapply(paste(dir.name, tofs, sep="/"), grayfactor))
	isgray[,cluster:=pam(value, k=2)$cluster]
	medoids=isgray[pam(isgray$value, k=2)$id.med]$value
	if(medoids[2]<medoids[1]) isgray[,cluster:=switch_values(cluster)]
	#print(head(dmin))
	#print(head(isgray))
	dmin=merge(dmin, isgray, by.x="fn", by.y="image")
	dmin[,day:=(hr>=day[1] & hr<=day[2])]
	dmin[,night:=(hr>=night[1] & hr<=night[2])]
	dmin=dmin[day|night]
	if(!nrow(dmin[day])){print("No daytime images");return(data.table(dn=dn, isCorrect=NA))}
	if(!nrow(dmin[night])){print("No nighttime images");return(data.table(dn=dn, isCorrect=NA))}
	#print(head(dmin))
	if(mean(dmin[hr<5]$cluster-1) < mean(dmin[hr>9 & hr<16]$cluster-1)){
		return(data.table(dn=dn, isCorrect=TRUE))
	}else{
		return(data.table(dn=dn, isCorrect=FALSE))
	}
}
# check_directory_AMPM(dn, intervals)

dlist=unique(intervals$dn)
ampm=rbindlist(lapply(dlist, function(x) check_directory_AMPM(x, intervals)))
