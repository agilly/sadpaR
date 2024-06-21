guessFsep=function(){
    if(.Platform$OS.type=="windows")
        return("\\")
    else
        return("/")
}

checkIfSomeSequencesExist=function(intervalFile, outputDir){
    if(!file.exists(intervalFile))
        return(FALSE)
    intervals=data.table::fread(intervalFile)
    if(!all(c("location", "ct", "fn", "interval") %in% names(intervals)))
        return(FALSE)
    intervals[,location_ct:=paste(location, ct, sep=".")]
    locations_cts=unique(intervals$location_ct)
    for(loc_ct in locations_cts){
        loc_ct_dir=file.path(outputDir, loc_ct, fsep = guessFsep())
        if(!dir.exists(loc_ct_dir))
            return(FALSE)
        intervalsInLocCt=unique(intervals[location_ct==loc_ct]$interval)
        for(interval in intervalsInLocCt){
            if(any(file.exists(file.path(loc_ct_dir, paste0("sequence.", interval, ".gif"), fsep = guessFsep()))))
                return(TRUE)
        }
    }
    return(FALSE)
}

createSingleSequence = function(interval, intervals_loc_ct, tempDir, loc_ct_dir, maxImagesBeforeDownsampling, verbose, session) {
    fsep=guessFsep()
    # if the tempdir has files, remove them
    if(verbose) cli::cli_inform("tempDir: {tempDir}")
    if(verbose) cli::cli_inform("Number of files in temp dir: {length(list.files(tempDir))}")
    if(length(list.files(tempDir)))
        unlink(list.files(tempDir, full.names = T), recursive=T)
    if(verbose) cli::cli_inform("Number of files in temp dir after unlink: {length(list.files(tempDir))}")
    # copy all the files for this interval to the temp dir, renaming them i.jpg where i is the index
    filesInInterval=intervals_loc_ct[interval==interval]$fn
    isDownsampled=F
    if(length(filesInInterval)>maxImagesBeforeDownsampling){
        # equally space the files so that the total number of files is approximately maxImagesBeforeDownsampling
        filesInInterval=filesInInterval[unique(round(seq(1, length(filesInInterval), length.out=maxImagesBeforeDownsampling)))]
        isDownsampled=T
    }
    for(i in 1:length(filesInInterval)){
        if(verbose) cli::cli_inform("Copying file {filesInInterval[i]} to {file.path(tempDir, paste0(i, '.jpg'), fsep=fsep)}")
        file.copy(filesInInterval[i], file.path(tempDir, paste0(i, ".jpg"), fsep=fsep))
    }
    # create a sequence from the files in the temp dir
    # using ffmpeg -framerate 2 -i tmp/%d.jpg -loop -1 -vf scale=-1:650 $on.sequences/$ctnm/sequence.$i.gif
    # vfarg is -vf "drawtext=fontfile=Lato-Regular.ttf:text='downsampled video, one in every "$downs" image shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10,scale=-1:650"
    # if isDownsampled is true and scale=-1:650 if isDownsampled is false
    if(isDownsampled)
        vfarg="-vf \"drawtext=fontfile='/Windows/fonts/arial.ttf':text='downsampled video,some images not shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10,scale=-1:650\""
    else
        vfarg="-vf scale=-1:650"
    # build command using glue
    command=glue::glue("ffmpeg -y -framerate 2 -i {file.path(tempDir, '%d.jpg', fsep=fsep)} -loop -1 {vfarg} {file.path(loc_ct_dir, paste0('sequence.', interval, '.gif'), fsep=fsep)}")
    # run command, error if it fails
    if(system(command, intern=F)){
        if(!is.null(session))
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "Error creating sequence",
                type = "error"
            )
        else
            cli::cli_abort("Error creating sequence")
    }
}

createSequences=function(intervalFile, outputDir, maxImagesBeforeDownsampling=100, session=NULL, verbose=F, overwrite=T){
    if(!file.exists(intervalFile))
        cli::cli_abort("The interval file does not exist.")
    if(!dir.exists(outputDir))
        dir.create(outputDir)
    intervals=data.table::fread(intervalFile)
    # make sure location,ct,fn,interval are all present
    if(!all(c("location", "ct", "fn", "interval") %in% names(intervals)))
        cli::cli_abort("The interval file must contain columns 'location', 'ct', 'fn', 'interval'")
    # create a temp dir
    # On windows, use outputDir, otherwise use tempdir()
    tempDir=if(.Platform$OS.type=="windows") glue::glue("{outputDir}\\tmp") else tempdir()
    # if windows, set fsep to \\, otherwise /
    fsep=guessFsep()
    tempDir=file.path(tempDir, "sadpaR_sequences", fsep=fsep)
    if(!dir.exists(tempDir))
        dir.create(tempDir, recursive=T)
    intervals[,location_ct:=paste(location, ct, sep=".")]
    locations_cts=unique(intervals$location_ct)
    totalNumberOfSequences=nrow(unique(intervals[,.(location, ct, interval)]))
    numberOfSequencesNeededToIncreaseByOnePercent=round(totalNumberOfSequences/100)
    if(!is.null(session)){
        sendSweetAlert(
            session = session,
            title = "",
            btn_labels = NA,
            text = tags$div(
            progressBar(
                id = "progressSequences",
                title = "Generating sequence GIFs...",
                display_pct = TRUE, 
                value = 0
            ),
            tags$div(
                id = "progressText", 
                tags$p(glue::glue("0/{totalNumberOfSequences} sequences generated"))
            )
            ),
            closeOnClickOutside = FALSE,
            backdrop = TRUE
        )
    }

    done_intervals=0
    for(loc_ct in locations_cts){
        # get the intervals for this location, ct
        intervals_loc_ct=intervals[location_ct==loc_ct]
        # create a subdir for the location_ct if it doesn't exist
        loc_ct_dir=file.path(outputDir, loc_ct, fsep = fsep)
        if(!dir.exists(loc_ct_dir))
            dir.create(loc_ct_dir)
        # for each unique interval
        intervalsInLocCt=unique(intervals_loc_ct$interval)
        for(interval in intervalsInLocCt){
            done_intervals=done_intervals+1
            if(overwrite || !file.exists(file.path(loc_ct_dir, paste0("sequence.", interval, ".gif"), fsep = fsep)))
                createSingleSequence(interval, intervals_loc_ct, tempDir, loc_ct_dir, maxImagesBeforeDownsampling, verbose, session)
            if(!is.null(session)){
                if(done_intervals %% numberOfSequencesNeededToIncreaseByOnePercent == 0)
                    updateProgressBar(
                        session = session,
                        id = "progressSequences",
                        title = "Generating sequence GIFs...",
                        value = done_intervals/totalNumberOfSequences*100
                    )
                removeUI("#progressText p", immediate = TRUE)
                insertUI(
                    selector = "#progressText",
                    ui = tags$p(glue::glue("{done_intervals}/{totalNumberOfSequences} sequences generated")),
                    immediate = TRUE
                )
            }
        }
    }

    if(!is.null(session))
        sendSweetAlert(
            session = session,
            title = "Success",
            text = "All sequences have been generated",
            type = "success"
        )
}

# createSequences=function(intervalFile, outputDir, maxImagesBeforeDownsampling=100, session=NULL, verbose=F, overwrite=T){
#     if(!file.exists(intervalFile))
#         cli::cli_abort("The interval file does not exist.")
#     if(!dir.exists(outputDir))
#         dir.create(outputDir)
#     intervals=data.table::fread(intervalFile)
#     # make sure location,ct,fn,interval are all present
#     if(!all(c("location", "ct", "fn", "interval") %in% names(intervals)))
#         cli::cli_abort("The interval file must contain columns 'location', 'ct', 'fn', 'interval'")
#     # create a temp dir
#     # On windows, use outputDir, otherwise use tempdir()
#     tempDir=if(.Platform$OS.type=="windows") glue::glue("{outputDir}\\tmp") else tempdir()
#     # if windows, set fsep to \\, otherwise /
#     if(.Platform$OS.type=="windows")
#         fsep="\\"
#     else
#         fsep="/"
#     tempDir=file.path(tempDir, "sadpaR_sequences", fsep=fsep)
#     if(!dir.exists(tempDir))
#         dir.create(tempDir, recursive=T)
#     intervals[,location_ct:=paste(location, ct, sep=".")]
#     locations_cts=unique(intervals$location_ct)
#     totalNumberOfSequences=nrow(unique(intervals[,.(location, ct, interval)]))
#     numberOfSequencesNeededToIncreaseByOnePercent=round(totalNumberOfSequences/100)
#     if(!is.null(session)){
#         sendSweetAlert(
#             session = session,
#             title = "",
#             btn_labels = NA,
#             text = tags$div(
#             progressBar(
#                 id = "progressSequences",
#                 title = "Generating sequence GIFs...",
#                 display_pct = TRUE, 
#                 value = 0
#             ),
#             tags$div(
#                 id = "progressText", 
#                 tags$p(glue::glue("0/{totalNumberOfSequences} sequences generated"))
#             )
#             ),
#             closeOnClickOutside = FALSE,
#             backdrop = TRUE
#         )
#     }

#     done_intervals=0
#     for(loc_ct in locations_cts){
#         # get the intervals for this location, ct
#         intervals_loc_ct=intervals[location_ct==loc_ct]
#         # create a subdir for the location_ct if it doesn't exist
#         loc_ct_dir=file.path(outputDir, loc_ct, fsep = fsep)
#         if(!dir.exists(loc_ct_dir))
#             dir.create(loc_ct_dir)
#         # for each unique interval
#         intervalsInLocCt=unique(intervals_loc_ct$interval)
#         for(interval in intervalsInLocCt){
#             # if the tempdir has files, remove them
#             if(verbose) cli::cli_inform("tempDir: {tempDir}")
#             if(verbose) cli::cli_inform("Number of files in temp dir: {length(list.files(tempDir))}")
#             if(length(list.files(tempDir)))
#                 unlink(list.files(tempDir, full.names = T), recursive=T)
#             if(verbose) cli::cli_inform("Number of files in temp dir after unlink: {length(list.files(tempDir))}")
#             # copy all the files for this interval to the temp dir, renaming them i.jpg where i is the index
#             filesInInterval=intervals_loc_ct[interval==interval]$fn
#             isDownsampled=F
#             if(length(filesInInterval)>maxImagesBeforeDownsampling){
#                 # equally space the files so that the total number of files is approximately maxImagesBeforeDownsampling
#                 filesInInterval=filesInInterval[unique(round(seq(1, length(filesInInterval), length.out=maxImagesBeforeDownsampling)))]
#                 isDownsampled=T
#             }
#             for(i in 1:length(filesInInterval)){
#                 if(verbose) cli::cli_inform("Copying file {filesInInterval[i]} to {file.path(tempDir, paste0(i, '.jpg'), fsep=fsep)}")
#                 file.copy(filesInInterval[i], file.path(tempDir, paste0(i, ".jpg"), fsep=fsep))
#             }
#             # create a sequence from the files in the temp dir
#             # using ffmpeg -framerate 2 -i tmp/%d.jpg -loop -1 -vf scale=-1:650 $on.sequences/$ctnm/sequence.$i.gif
#             # vfarg is -vf "drawtext=fontfile=Lato-Regular.ttf:text='downsampled video, one in every "$downs" image shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10,scale=-1:650"
#             # if isDownsampled is true and scale=-1:650 if isDownsampled is false
#             if(isDownsampled)
#                 vfarg="-vf \"drawtext=fontfile='/Windows/fonts/arial.ttf':text='downsampled video,some images not shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10,scale=-1:650\""
#             else
#                 vfarg="-vf scale=-1:650"
#             # build command using glue
#             command=glue::glue("ffmpeg -y -framerate 2 -i {file.path(tempDir, '%d.jpg', fsep=fsep)} -loop -1 {vfarg} {file.path(loc_ct_dir, paste0('sequence.', interval, '.gif'), fsep=fsep)}")
#             # run command, error if it fails
            
#             if(system(command, intern=F)){
#                 if(!is.null(session))
#                     sendSweetAlert(
#                         session = session,
#                         title = "Error",
#                         text = "Error creating sequence",
#                         type = "error"
#                     )
#                 else
#                     cli::cli_abort("Error creating sequence")
#             }

#             done_intervals=done_intervals+1
#             if(!is.null(session)){
#                 if(done_intervals %% numberOfSequencesNeededToIncreaseByOnePercent == 0)
#                     updateProgressBar(
#                         session = session,
#                         id = "progressSequences",
#                         title = "Generating sequence GIFs...",
#                         value = done_intervals/totalNumberOfSequences*100
#                     )
#                 removeUI("#progressText p", immediate = TRUE)
#                 insertUI(
#                     selector = "#progressText",
#                     ui = tags$p(glue::glue("{done_intervals}/{totalNumberOfSequences} sequences generated")),
#                     immediate = TRUE
#                 )
#             }
#         }
#     }

#     # send success when done
#     if(!is.null(session))
#         sendSweetAlert(
#             session = session,
#             title = "Success",
#             text = "All sequences have been generated",
#             type = "success"
#         )
# }


convertPathsToWindows=function(paths){
    # takes in /mnt/DRIVE/PATH/TO/FILE and converts it to DRIVE:\PATH\TO\FILE
    # replace drive letter /mnt/c with c:
    paths=gsub("/mnt/([a-z])/", "\\1:\\\\", paths)
    # replace / with \\
    paths=gsub("/", "\\\\", paths)
    return(paths)
}