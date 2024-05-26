info=cli::cli_alert_info
warn=cli::cli_alert_warning
error=cli::cli_abort
success=cli::cli_alert_success

#' fixCorruptTagging
#'
#' This function reads a CSV file, identifies and corrects three types of errors in the data, and writes the corrected data to a new CSV file.
#' It also creates backup CSV files for each type of error it finds.
#'
#' @param filename A string specifying the path to the CSV file to be corrected.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads the CSV file into a data.table.
#' 2. Creates a backup of the original file.
#' 3. Identifies and corrects three types of errors:
#'    - Empty events: These are events where numInd is 0. The function checks if there is more than one row for these events, if indID is not 0, and if speciesID is not NA. If any of these conditions are not met, the function creates a backup of the erroneous rows and corrects the errors.
#'    - Non-empty events: These are events where numInd is greater than 0. The function checks if speciesID is NA, if numInd is not equal to N (the number of rows for each ctid and event), and if indID does not start at 0 or is not sequential. If any of these conditions are not met, the function creates a backup of the erroneous rows and corrects the errors.
#'    - Sexes and Age: The function checks if Sex is one of "Male", "Female", or "Unknown", and if Age is one of "Adult", "Subadult", "Juvenile", or "Unknown". If any of these conditions are not met, the function creates a backup of the erroneous rows and corrects the errors.
#' 4. Combines the corrected data.tables for empty and non-empty events.
#' 5. Writes the corrected data to a new CSV file.
#'
#' @return This function does not return a value. It writes the corrected data to a new CSV file and creates backup CSV files for each type of error it finds.
#'
#' @examples
#' fixCorruptTagging("path/to/your/file.csv")
#'
#' @export
fixCorruptTagging=function(filename){
    # fread the file
    d=data.table::fread(filename)
    # remove the extension and form the base for the output files
    base=tools::file_path_sans_ext(filename)
    # backup the file
    info("Backing up the original file to {base}.bak.csv")
    data.table::fwrite(d, file=glue::glue("{base}.bak.csv"))
    d[,N:=.N, by=.(ctid, event)]
    # N should be equal to numInd, except when numInd is 0, in which case indID should be 0
    # extract the cases where indID is 0
    d0=d[numInd==0]
    drest=d[numInd>0]
    # make sure there is at most one per ctid+event, and that indID is 0 for all of them
    # create slice of those that have >1 rows
    moreThanOneRow=d0[N>1]
    
    ### ERROR TYPE 1 : Empty events
    flagError=FALSE
    if(!nrow(moreThanOneRow))
        success("No empty event has more than one row")
    else {
        warn("{nrow(moreThanOneRow)} empty events have more than one row. These will be collapsed to one record.")
        warn("Backing up rows to {base}.dupEmptyRows.csv")
        data.table::fwrite(moreThanOneRow, file=glue::glue("{base}.dupEmptyRows.csv"))
        flagError=TRUE
    }
    # report if there are any non-zero indID in the d0
    if(nrow(d0[indID>0])){
        warn("There are {nrow(d0[indID>0])} rows with non-zero indID in the empty events")
        warn("Backing up rows to {base}.nonZeroIndID.csv")
        data.table::fwrite(d0[indID>0], file=glue::glue("{base}.nonZeroIndID.csv"))
        flagError=TRUE
    }else{
        success("All empty events have indID=0")
    }
    # report if there are any rows where speciesID is not NA
    if(nrow(d0[!is.na(speciesID)])){
        warn("There are {nrow(d0[!is.na(speciesID)])} rows with non-NA speciesID in the empty events")
        warn("Backing up rows to {base}.nonNAspeciesID.csv")
        data.table::fwrite(d0[!is.na(speciesID)], file=glue::glue("{base}.nonNAspeciesID.csv"))
        flagError=TRUE
    }else{
        success("All empty events have speciesID=NA")
    }
    # create a new data.table with the corrected empty events
    d0new=unique(d0[,.(ctid, event, indID=0, numInd=0, speciesID=NA_integer_)])
    # if flag is true set d0 to d0new
    if(flagError)
        d0=d0new

    ### ERROR TYPE 2 : Non-empty events
    # check if there are non-empty events (numInd>0) with speciesID=NA
    if(nrow(drest[is.na(speciesID)])){
        warn("There are {nrow(drest[numInd>0 & is.na(speciesID)])} rows with numInd>0 and speciesID=NA")
        warn("Backing up rows to {base}.nonNAspeciesID.csv")
        data.table::fwrite(drest[numInd>0 & is.na(speciesID)], file=glue::glue("{base}.nonNAspeciesID.csv"))
        # remove these rows
        drest=drest[!is.na(speciesID)]
    }else{
        success("All non-empty events have speciesID!=NA")
    }
    
    # for the remaining events, numInd should be = N
    if(nrow(drest[numInd!=N])){
        warn("There are {nrow(drest[numInd!=N])} rows where numInd!=N")
        warn("Backing up rows to {base}.numIndNotEqualN.csv")
        data.table::fwrite(drest[numInd!=N], file=glue::glue("{base}.numIndNotEqualN.csv"))
        # force numInd to be equal to N
        drest[,numInd:=N]
    }else{
        success("All non-empty events have numInd=N")
    }

    # check that for the remaining events, indID starts at 0 and is sequential
    data.table::setorder(drest, ctid, event, indID)
    eventsViolateOrder=drest[,setequal(indID,seq(0,(.N-1))),by=.(ctid,event)]
    if(nrow(eventsViolateOrder[V1==F])){
        warn("There are {nrow(eventsViolateOrder[V1==F])} events where indID does not start at 0 or is not sequential")
        warn("Backing up rows to {base}.indIDNotSequential.csv")
        drest[,identifier:=paste(ctid, event, sep="_")]
        eventsViolateOrder[,identifier:=paste(ctid, event, sep="_")]
        data.table::fwrite(drest[identifier %in% eventsViolateOrder[V1==F,identifier]], file=glue::glue("{base}.indIDNotSequential.csv"))
        # fix the indID
        drest[identifier %in% eventsViolateOrder[V1==F,identifier], indID:=seq(0,.N-1), by=.(ctid,event)]
        # remove identifier
        drest[,identifier:=NULL]
    }else{
        success("All non-empty events have indID starting at 0 and being sequential")
    }

    ### ERROR TYPE 3 : Sexes and Age
    # sex has to be "Male", "Female" or "Unknown"
    acceptedSexes=c("Male", "Female", "Unknown")
    if(nrow(drest[!(Sex %in% acceptedSexes)])){
        warn("There are {nrow(drest[!(Sex %in% acceptedSexes)])} rows with wrong sexes")
        warn("Backing up rows to {base}.wrongSexes.csv")
        data.table::fwrite(drest[!(Sex %in% acceptedSexes)], file=glue::glue("{base}.wrongSexes.csv"))
        # set ambiguous sexes to "Unknown"
        drest[!(Sex %in% acceptedSexes), Sex:="Unknown"]
    }else{
        success("No issues with sexes")
    }
    # age has to be "Adult", "Subadult" ,"Juvenile" or "Unknown"
    acceptedAges=c("Adult", "Subadult", "Juvenile", "Unknown")
    if(nrow(drest[!(Age %in% acceptedAges)])){
        warn("There are {nrow(drest[!(Age %in% acceptedAges)])} rows with wrong ages")
        warn("Backing up rows to {base}.wrongAges.csv")
        data.table::fwrite(drest[!(Age %in% acceptedAges)], file=glue::glue("{base}.wrongAges.csv"))
        # set ambiguous ages to "Unknown"
        drest[!(Age %in% acceptedAges), Age:="Unknown"]
    }else{
        success("No issues with ages")
    }

    # combine the two data.tables
    dnew=rbind(d0, drest)
    #remove N
    dnew[,N:=NULL]
    # resort
    data.table::setorder(dnew, ctid, event, indID)
    # write the new file
    info("Writing the corrected file to {base}.fixed.csv")
    data.table::fwrite(dnew, file=glue::glue("{base}.fixed.csv"))
    success("All done!")
}