# ------------------------------------------------------------------------------
# VERSION: SIMPLE (NO TABS) - v02
# ------------------------------------------------------------------------------
# 01.README comments
#
# Author:
# # Adriano Axel Pliopas Pereira
# E-mail: adriano.axel@gmail.com
#
# This application was developed as solution for the home assignment proposed by
# Appsilon as part of the recruitment process for Shiny developer. It cannot be
# used 
#
# File organization:
# 01.README comments
# 02.Loading libraries and loading of processed data
# 03.Custom modules
#    03.1 - DROPDOWN MODULE
#    03.2 - MAP MODULE
# 04.Main UI/Server function and call to shiny app
# ------------------------------------------------------------------------------

# Libraries for the application:
library(shiny)
library(shiny.semantic)
# library(shinydashboard)
# library(shinyjs)
library(leaflet)
# Libraries for the data loading and manipulation:
library(data.table)
library(bit64)
library(lubridate)
library(dplyr)
library(ggplot2)

# setwd("D:/R/Appsilon Task/Semantic_Tests/Submission Version/")
dataIn <- readRDS("ShipDataProcessed.RDS")
dfShipsClean <- dataIn$dfShipsClean
statShips <- dataIn$statShips


# ==============================================================================
# DROPDOWN MODULE
# This module uses the dfShipsClean dataset to load ship types and names into
# dropdown button and return these choices to the map module. This module
# also shows basic statistics for the vessel chosen, as well as a plot with
# the histogram of the parked points for all the vessels in the type chosen,
# with a different color for the bin of the chosen ship name. (in other words, 
# the histogram changes for each ship type, and the color of the highlighted bar
# indicates to which bin the chosen ship time belongs.)
dropButton <- function(id, label = "DropVesselChoice") {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),
      includeCSS("Styles.css")
    ),
    
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 450, height = "auto",
                  
                  hr(),
                  
                  # The ship types are sorted to make it easier for the user
                  h2(class = "ui header", icon("ship"), div(class = "content",
                                                            "Select a particular ship:")),
                  h3(class = "ui header", "Select Ship Type:"),
                  dropdown_input(ns("dropVesselType"),
                                 choices=sort(unique(dfShipsClean$ship_type)),
                                 value=sort(unique(dfShipsClean$ship_type))[1]),
                  
                  # The ship names/IDs are shown in sorted order to make it easier for the user.
                  h3(class = "ui header", "Select Ship Name:"),
                  dropdown_input(ns("dropVesselName"),
                                 choices=sort(unique(dfShipsClean$IDENTIFICATION[dfShipsClean$ship_type==dfShipsClean$ship_type[1]])),
                                 value=sort(unique(dfShipsClean$IDENTIFICATION[dfShipsClean$ship_type==sort(unique(dfShipsClean$ship_type))[1]]))[1]),
                  
                  hr(),
                  #tags$head( tags$style(HTML(".fa{font-size: 10px;}"))),
                  h2(class="ui header",icon("chart line"),div(class="content",
                                                              "Ship Statistics:")),
                  htmlOutput(ns("shipInfo")),
                  plotOutput(ns("histParkedByType"), height = 200)
    )
  )
}

dropServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$dropVesselType,{
        
        # The ship names/IDs are shown in sorted order to make it easier for the user.
        update_dropdown_input(session,
                              "dropVesselName", 
                              choices=sort(unique(dfShipsClean$IDENTIFICATION[dfShipsClean$ship_type==input[["dropVesselType"]] ]))
        )
        
      })
      
      
      output$shipInfo <- renderText({
        
        info_dwt<-dfShipsClean$DWT[dfShipsClean$IDENTIFICATION==input[["dropVesselName"]]][1]
        if (is.na(info_dwt)) info_dwt <- " "
        
        paste0('ABOUT THE SHIP:<br>',
               '<b>SHIP FLAG:</b> ',       dfShipsClean$FLAG[dfShipsClean$IDENTIFICATION==input[["dropVesselName"]]][1],
               '<br><b>SHIP LENGTH:</b> ', dfShipsClean$LENGTH[dfShipsClean$IDENTIFICATION==input[["dropVesselName"]]][1],' [m]',
               '<br><b>SHIP WIDTH:</b> ',  dfShipsClean$WIDTH[dfShipsClean$IDENTIFICATION==input[["dropVesselName"]]][1],' [m]',
               '<br><b>SHIP DWT:</b> ',    info_dwt,' [ton]',
               '<br><br>ABOUT THE OBSERVED DATA:<br>',
               'NUMBER OF OBSERVATIONS:</b> ', statShips$nPoints[statShips$IDENTIFICATION==input[["dropVesselName"]]][1],
               '<br><b>TIME FROM FIRST TO LAST OBSERVATION:</b> ', round(100*statShips$timeLastFirst[statShips$IDENTIFICATION==input[["dropVesselName"]]][1])/100, ' [hours]',
               '<br><b>FRACTION OF OBSERVATIONS PARKED:</b> ', round(10000*statShips$fractionParked[statShips$IDENTIFICATION==input[["dropVesselName"]]][1])/100,' %',
               '<br><b>NUMBER OF DISCONTINUITIES:</b> ', statShips$nLegs[statShips$IDENTIFICATION==input[["dropVesselName"]]][1]-1,
               '<br><b>MAX.DIST. BETWEEN CONSEC. OBSERVATIONS:</b> ', round(100*max(dfShipsClean$DISTANCE[dfShipsClean$IDENTIFICATION==input[["dropVesselName"]]]))/100,' [m]'
               
               
        )
      })
      
      # Histogram of the proportion of parked time
      # for each SHIP_ID in the original dataser, 
      output$histParkedByType <- renderPlot({
        
        hout<-hist(statShips$fractionParked[statShips$shipType==input[["dropVesselType"]]])
        
        # Fraction of points with status "parked" for the selected ship
        fp<-statShips$fractionParked[statShips$IDENTIFICATION==input[["dropVesselName"]]]
        
        breaks=hout$breaks
        breaks<-breaks[1:length(breaks)-1]
        
        binID<-breaks[breaks<=fp]
        
        binID=binID[length(binID)]
        
        # Create legend for the points of the histogram, identifying differently
        # the bin to which the selected ship belongs
        selected<-rep(paste0("All ",input[["dropVesselType"]], " ships"),length(hout$counts))
        selected[breaks==binID]<-input[["dropVesselName"]]
        
        df<-data.frame(mids=hout$mids,counts=hout$counts,legend=selected)
        
        ggplot(df,aes(fill=legend,x=mids,y=counts))+geom_bar(position="dodge",stat="identity")+
          xlab("Fraction of points as 'Parked'") +
          ylab("Occurrences")
        
      })
      
      return(list(vType=shiny::reactive(input[["dropVesselType"]]),
                  vName=shiny::reactive(input[["dropVesselName"]])
      ))
      
    }
  )
}
# ==============================================================================



# ==============================================================================
# MAP MODULE 
mapModule <- function(id, label = "mapWindow") {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"),width="75%",height="80%")
  )
}

mapModuleServer <- function(id,vName) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$map <- renderLeaflet({
        
        subPlot <- dfShipsClean[dfShipsClean$IDENTIFICATION==vName(),]
        
        LON <- subPlot$LON
        LAT <- subPlot$LAT
        
        df <- sp::SpatialPointsDataFrame(
          #Lon, Lat
          cbind(LON,LAT),
          data.frame(type = factor(rep(subPlot$ship_type)))
        )
        
        # Preparing legend for popup for each point
        subPlot <- subPlot %>% mutate(
          popup_data=paste0(
            '<strong>Ship Name:</strong> ',SHIPNAME,
            '<br><strong>Ship ID:</strong> ', as.numeric(SHIP_ID),
            '<br><strong>Heading:</strong> ', HEADING,'°',
            '<br><strong>Destination:</strong> ',DESTINATION,
            '<br><strong>Port reported:</strong> ',PORT,
            '<br><strong>Port assigned:</strong> ',port,
            '<br><strong>Is Parked indicator value:</strong> ',is_parked,
            '<br><strong>Observation date/time stamp:</strong> ',DATETIME,
            '<br><strong>Distance from previous observed point:</strong> ',round(DISTANCE),' meters'
          )
        )
        
        dLO<-abs(mean(diff(LON)))+5/60
        dLA<-abs(mean(diff(LAT)))+5/60
        
        if (is.na(dLO)) dLO=0.01
        if (is.na(dLA)) dLA=0.01
        
        lonView<-c(min(LON)-dLO/2,max(LON)+dLO/2)
        latView<-c(min(LAT)-dLA/2,max(LAT)+dLA/2)
        
        leaflet(subPlot) %>% addCircles(lng = ~LON, lat = ~LAT) %>%
          addTiles() %>% addCircleMarkers(popup=subPlot$popup_data) %>%
          fitBounds(lonView[1],latView[1],lonView[2],latView[2])
        
      })
      # outputOptions(output, "map", suspendWhenHidden = FALSE)
      
    }
  )
}
# ==============================================================================








# ==============================================================================
# MAIN UI FUNCTION
ui <- function() {
  shinyUI(
    semanticPage(
      
      h1(class = "ui header", "Ship Data Visualization (by Adriano Axel - adriano.axel@gmail.com)"),
      h3(class = "ui header", "(Click on the points in the map for details)"),
      
      mapModule("map1"),
      
      dropButton("dropServer1"),
      
      h4(class = "ui header", paste0("NOTES: (1) The speed data is not shown",
                                     " because it is unreliable in the original data set and recauculating ",
                                     "it properly should take into account the discontinuities (long periods ",
                                     "without observation) for some ships. It is not hard to do, but I ",
                                     "considered it out of the scope of this project. (2) Consecutive observations ",
                                     "were considered those with two consecutive time-stamps, for the same ship ID. ",
                                     "(3) There are 11 cases of ships with more than 1 name for the same ID. These cases ",
                                     "(for IDs 315731, 315950, 316404, 316482, 345254, 347195, 364937, 406999, 757619, ",
                                     "3653787 and 4666609) were treated without distinction of ship name. Only ship ID was ",
                                     "used to distinguish between ships. In a real life work on this data, a verification on the ",
                                     "LAT/LON data and also checking with the data provier could take place to clarify this point.")),
      
    ))
}

# ==============================================================================
# MAIN SERVER FUNCTION
server <- shinyServer(function(input, output,session) {
  
  drop_choice<-dropServer("dropServer1")
  
  mapModuleServer(id="map1",vName=drop_choice$vName)
  
})

shinyApp(ui = ui(), server = server)
