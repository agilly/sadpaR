resizeImage=function(path, height, newpath){
im=imager::load.image(path)
old_width=width(im)
# scale width to get height
new_width=old_width*height/height(im)
im=imager::resize(im, new_width, height)
imager::save.image(im, newpath)
}

generateThumbnails=function(data_dir, old_image_root, new_image_root, session=NULL, verbose=F){
    # read interval data
    interval_path=file.path(normalizePath(data_dir), "metadata", "intervals.csv")
    intervals=data.table::fread(interval_path)
    # replace old image root with new image root
    intervals[, fn:=gsub(old_image_root, new_image_root, fn)]
    # create path for image thumbnails
    intervals[, thumbnail_path:=gsub(new_image_root, file.path(data_dir, "thumbnails"), fn)]
    # create directory for thumbnails if it does not exist
    if(!dir.exists(file.path(data_dir, "thumbnails")))
        dir.create(file.path(data_dir, "thumbnails"))
    # subset to events with more than 10 images
    intervals[, N:=.N, by=.(ctid, interval)]
    intervals=intervals[N>10]
    # read tagging data
    tagging_path=file.path(normalizePath(data_dir), "tagging", "eventTagging.csv")
    tags=data.table::fread(tagging_path)
    # identify intervals with more than 1 species (distinct species ID >1)
    setnames(tags, "event", "interval")
    tags[, intID:=paste(ctid, interval, sep="_")]
    tags[,Nspecies:=length(unique(speciesID)), by=intID]
    tags=tags[Nspecies>1]
    # merge tagging data with interval data
    intervals[, intID:=paste(ctid, interval, sep="_")]
    intervals=merge(intervals, tags, by="intID")
    intervals=unique(intervals[, .(fn, thumbnail_path)])
    # restrict to intervals where more than 1 species is present

    print(intervals)
    # generate thumbnails if they do not exist
    for(i in 1:nrow(intervals)){
        if(i %% 10 == 0 && !is.null(session)){
            # we are in a reactive context, update the progress bar
            updateProgressBar(session=session, id="generateThumbnailsPBar", title="Generating thumbnails",value=round(i*100/nrow(intervals)))
        }
        if(!file.exists(intervals$thumbnail_path[i])){
            thumbdir=dirname(intervals$thumbnail_path[i])
            if(!dir.exists(thumbdir))
                dir.create(thumbdir, recursive=T)
            resizeImage(intervals$fn[i], 1000, intervals$thumbnail_path[i])
        }else{
            if(verbose)
                print(paste("Thumbnail already exists for", intervals$fn[i]))}
    }
}
