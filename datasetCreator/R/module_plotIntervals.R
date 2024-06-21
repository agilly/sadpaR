timeRangeUI = function(id){
    ns=NS(id)
    tagList(
        # shinywidgets toggle for by station or by camera trap
        radioGroupButtons(
          inputId = ns("byCtOrStationToggle"),
          label = "Show ranges by",
          choices = c("Station" = "station", "Camera trap" = "ct"),
          selected = "station",
          checkIcon = list(
              yes = tags$i(class = "fa fa-check-square", 
            style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o", 
            style = "color: steelblue"))
        ),
        plotOutput(ns("timeRangePlot"))
    )
}

timeRangeServer = function(id, timeRangeData) {
  moduleServer(id, function(input, output, session) {
    output$timeRangePlot=renderPlot({
      if(!nrow(timeRangeData())){
        return()
      }
      # if date is not a POSIXct, convert it
        if(!inherits(timeRangeData()$date, "POSIXct"))
            timeRangeData()[,date:=as.POSIXct(date, format="%Y:%m:%d %H:%M:%S")]
    # Common code
    trdata = copy(timeRangeData())
    if(input$byCtOrStationToggle=="station"){
        trdata[,minbyst:=min(date), by=.(location)]
        trdata[,maxbyst:=max(date), by=.(location)]
        trdata=unique(trdata[,.(location, minbyst, maxbyst)])
        setorder(trdata, location, minbyst)
    } else {
        trdata[,minbyst:=min(date), by=.(location, ct)]
        trdata[,maxbyst:=max(date), by=.(location, ct)]
        trdata=unique(trdata[,.(location, ct, minbyst, maxbyst)])
        setorder(trdata, location, ct, minbyst)
    }

    # Empty plot large enough to contain the segments for all stations
    ylabtext = if(input$byCtOrStationToggle=="station") "Station" else "Camera trap"
    par(mar = c(5, 10, 4, 2) + 0.1)
    # write ylab on the left side of the plot as far as possible to the left
    plot(1, type="n", ylim=c(0, nrow(trdata)), xlim=c(min(trdata$minbyst), max(trdata$maxbyst)), xlab="Time range", ylab="", xaxt="n", yaxt="n")
    mtext(text=ylabtext, side=2, line=8)

    # Draw a solid light gray grid on the first day of every month
    monthlegends=seq(floor_date(min(trdata$minbyst), "month"), max(trdata$maxbyst), by = "month")
    abline(v = monthlegends, col = "lightgray")

    # Draw a dotted light gray grid on the first day of every week
    weeklegends=seq(floor_date(min(trdata$minbyst), "week"), max(trdata$maxbyst), by = "week")
    abline(v = weeklegends, col = "lightgray", lty=2)

    # Draw segments for each station
    for(i in 1:nrow(trdata)){
        lines(c(trdata$minbyst[i], trdata$maxbyst[i]), c(i, i), col="#313131", lwd=2)
    }

    # Draw large green and red points for min and max, respectively
    points(trdata$minbyst, 1:nrow(trdata), bg="#095e09", pch=21, col="#313131", cex=1.3, lwd=2)  # Lime green
    points(trdata$maxbyst, 1:nrow(trdata), bg="#FF4500", pch=21, col="#313131", cex=1.3, lwd=2)  # Orange red

    # Add station names in axis
    labels = if(input$byCtOrStationToggle=="station") trdata$location else paste(trdata$location, trdata$ct, sep=" - ")
    axis(2, at=1:nrow(trdata), labels=labels, las=1)

    # Draw x axis of time, months and weeks
    xaxticks=c(weeklegends, monthlegends)
    xaxlabels=format(xaxticks, "%d %b %y")
    axis(1, at=xaxticks, labels=xaxlabels)
    })
  })
}


timeRangeDemo=function(){
    library(shiny)
    library(data.table)
    library(shinyWidgets)
    library(lubridate)
  timeRangeData=data.table::fread("/mnt/t/myDataset/datetime.csv.gz")
  print(timeRangeData)
  timeRangeData[,date:=as.POSIXct(date, format="%Y:%m:%d %H:%M:%S")]

    timeRangeData[,location:=basename(dirname(dirname(fn)))]
    timeRangeData[,ct:=basename(dirname(fn))]
  ui=fluidPage(timeRangeUI("timeRangeDemo"))
  server = function(input, output, session) {
    timeRangeServer("timeRangeDemo", reactive(timeRangeData))
  }
    shiny::shinyApp(ui=ui, server=server, options=list(launch.browser=F, port=8877))
}