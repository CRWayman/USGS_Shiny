library(dataRetrieval)
library(dplyr)
library(leaflet)
library(plotly)
library(shinythemes)
library(shinycustomloader)

##########

# User interface ----
ui <- bootstrapPage(
  theme= shinytheme("spacelab"),
  
  titlePanel(title=div(img(src="nasa.jpg"), img(src="ssai.png"), "Exploring USGS Water Data",
                       style="z-index:1000;padding:0px 0px 0px 10px;"),
             tags$head(tags$link(rel="shortcut icon", href="ssai.png"))),
  
  navbarPage("Dashboard", id="nav", 
             
             tabPanel("Interactive map",
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = "25%", left = 20, right = "auto", bottom = "auto",
                                        width = 350, height = "auto", style = "z-index: 1;", ## z-index modification,
                                        h3("USGS Hydro Data"),
                                        
                                        tags$div(class = "header", checked = NA,
                                                 tags$b("Select Dates"),
                                                 br()
                                        ),
                                        #textOutput("site"),
                                        #uiOutput("Generate"),
                                        sliderInput("date_slider", "", 
                                                    min = as.Date("2020-01-01","%Y-%m-%d"), 
                                                    max = as.Date("2020-12-31","%Y-%m-%d"), 
                                                    value = c(as.Date("2020-01-01","%Y-%m-%d"), as.Date("2020-12-31","%Y-%m-%d"))),
                                        
                                        #plotlyOutput("flowSeries", height = 200)
                          )),
                      
                      
                      fluidRow(tags$style(type = "text/css", "#map {height: calc(100vh) !important;}"),
                               withLoader(leafletOutput("mymap", width = "100%", height = "500px"), type="html", loader="loader1")),
                      br(),
                      fluidRow(plotlyOutput("flowSeries", height = 300))
             ),
             
             tabPanel("Additional Information",
                      fluidRow(column(12,
                                      h3('Placeholder Tab',  style = "padding: 5px 10px 5px"),
                                      br())))
  )
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
  
  
  # Create custom icons
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "darkblue")
  
  clicked <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "lightred")
  
  # Render map
  output$mymap <- renderLeaflet({
    leaflet(data = newGage) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Centre the map
      setView(lng = -82.0, 
              lat = 37.5, 
              zoom = 4) %>% 
      
      addAwesomeMarkers(data=points(), icon = icons)
    #addMarkers(data = points())
  })
  
  observeEvent(input$mymap_marker_click, { 
    
    # Click point
    p <- input$mymap_marker_click
    
    # Create proxy map based on clicked point
    proxy = leafletProxy("mymap") %>%
      removeMarker(layerId="clicked") %>%
      setView(lng = isolate(p$lng),
              lat = isolate(p$lat),
              zoom = 8) %>%
      addAwesomeMarkers(lng = isolate(p$lng),
                        lat = isolate(p$lat),
                        layerId = "clicked",
                        icon = clicked)
    
    # Subset Data Frame based on clicked point
    pLat <- p$lat
    plotGage <- subset(newGage, dec_lat_va == pLat)
    
    # Find site name
    site_name = unique(plotGage$station_nm)
    
    # Produce Plotly figure
    output$flowSeries=renderPlotly({
      
      # Date Range
      date_range <- input$date_slider
      
      # Subset based on date range
      plotGage <- subset(plotGage, Date>date_range[[1]] & Date < date_range[[2]]) %>%
        arrange(desc(Date))
      
      # Plotting
      plot_ly(plotGage,
              x=~Date, 
              y=~Flow, 
              size=~Wtemp,
              name = "Water Temperature (C)",
              mode = "lines+markers") %>%
        layout(title=paste("Hydrological data for ", site_name),
               yaxis = list(title = "Streamflow (cfs)"),
               xaxis = list(title = "Date"),
               legend = list(x = 0.75, y = 0.9),
               showlegend=TRUE)
      
    })
    
    # Site Name
    output$site <- renderText(paste("Showing data for", unique(plotGage$station_nm)))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)