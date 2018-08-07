library(shiny)
library(leaflet)
library(RColorBrewer)
library(readxl)
library(magrittr)

kiva_mpi <- read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/data/kiva_mpi_region_locations.xlsx")
kiva_mpi<- kiva_mpi[complete.cases(kiva_mpi), ]


labels <- sprintf(
  "<strong>%s</strong><br/>MPI: %g",
  kiva_mpi$LocationName, kiva_mpi$MPI
) %>% lapply(htmltools::HTML)


function(input, output, session){
  filteredData <- reactive({
    kiva_mpi[kiva_mpi$MPI >= input$range[1] & kiva_mpi$MPI <= input$range[2],]
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, kiva_mpi$MPI)
  })
  
  output$map <- renderLeaflet({
    leaflet(kiva_mpi) %>% 
      addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~1000^MPI, weight = 6.5, color = "#777777",
                 fillColor = ~pal(MPI), fillOpacity = 0.7, popup = ~paste(MPI),
                 label = labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto")
      )
  })
  
  observe({
    proxy <- leafletProxy("map", data = kiva_mpi)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~MPI
      )
    }
  })
}