library(dataRetrieval)  #will need to install if you havnt already
library(dplyr)
library(ggplot2)
library(leaflet)

##########

# User interface ----
ui <- fluidPage(
  leafletOutput("mymap"),
  fluidRow(verbatimTextOutput("mymap_marker_click")),
  fluidRow(textOutput("coords")),
  
  fluidRow(column(8,
                  sliderInput("date_slider", "", 
                              min = as.Date("2020-01-01","%Y-%m-%d"), 
                              max = as.Date("2020-12-31","%Y-%m-%d"), 
                              value = c(as.Date("2020-01-01","%Y-%m-%d"), as.Date("2020-12-31","%Y-%m-%d"))),
                  plotOutput("flowSeries")
  ))
  
)

server <- function(input, output, session) {

  sites <- whatNWISsites(bBox=c(-83.0,36.5,-81.0,38.5), 
                         parameterCd=c('00060','00010'),
                         hasDataTypeCd="dv")
  
  site_nos = sites['site_no']
  
  ### First is taking daily data (average for the day since this site collect measurements every 15 mins)
  
  Gage_daily <- readNWISdv(siteNumbers = c(site_nos[1])$site_no,  #could input the site number that you want
                           parameterCd = c('00060','00010'), #Spc, Discharge, Gage height, temp
                           startDate = '2020-01-01',  #I currently dont have an end date here which means it is going to keep looking for data
                           endDate = '2020-12-31')  %>%
    renameNWISColumns()
  
  newGage <- merge(sites, Gage_daily, by="site_no")
  
  newGage <- newGage[c("site_no", "station_nm", "dec_lat_va", "dec_long_va", "Date", "Flow", "Wtemp")]
  
  # Clean Data
  
  newGage <- na.omit(newGage)
  
  newCoords <- cbind(newGage['dec_long_va'], newGage['dec_lat_va'])
  
  points <- eventReactive(input$recalc, {
    data.matrix(newCoords)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(data = newGage) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      
      # Centre the map
      setView(lng = -82.0, 
              lat = 37.5, 
              zoom = 4) %>% 
      
      addMarkers(data = points())
  })
  
  observeEvent(input$mymap_marker_click, { 
      p <- input$mymap_marker_click
      print(p)
      output$coords <- renderText(paste("Showing data for", round(p$lat, 2), "N, ", round(p$lng, 2), "E"))
      pLat <- p$lat
      plotGage <- subset(newGage, dec_lat_va == pLat)
      output$flowSeries=renderPlot({
        # Get date Range
        date_range <- input$date_slider
        ggplot(plotGage, aes(x=Date, y=Flow, size=Wtemp)) +
          geom_point(colour = 'blue') +
          xlim(date_range[1], date_range[2])
      })
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)

