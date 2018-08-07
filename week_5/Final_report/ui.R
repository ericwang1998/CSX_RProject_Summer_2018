library(shiny)
library(leaflet)
library(RColorBrewer)
library(readxl)
library(magrittr)

navbarPage("Kiva",
  tabPanel("MPI interactive map",
    
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("map", width = "900", height = "900"),
      
      absolutePanel(top = 10, right = 10,
                    sliderInput("range", "MPI", min(kiva_mpi$MPI), max(kiva_mpi$MPI),
                                value = range(kiva_mpi$MPI), step = 0.05),
                    selectInput("colors", "Color Scheme",
                                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                    checkboxInput("legend", "Show legend", TRUE))),
  
  tabPanel("Data Table")
)
