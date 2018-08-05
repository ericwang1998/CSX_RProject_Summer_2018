#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(leaflet)

# Choices for drop-downs
vars <- c(
  
  "Grouped by Sector" = "superzip",
  
  "Centile score" = "centile",
  
  "College education" = "college",
  
  "Median income" = "income",
  
  "Population" = "adultpop"
  
)





navbarPage("KIVA", id="nav",
           
           
           
           tabPanel("Interactive map",
                    
                    div(class="outer",
                        
                        
                        
                        tags$head(
                          
                          # Include our custom CSS
                          
                          includeCSS("styles.css"),
                          
                          includeScript("gomap.js")
                          
                        ),
                        
                        
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      
                                      width = 330, height = "auto",
                                      
                                      
                                      
                                      h2("Human conditions explorer"),
                                      
                                      
                                      
                                      selectInput("Grouped by", "Grouped by", vars),
                                      
                                      selectInput("size", "Size", vars, selected = "count"),
                                      
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                                       
                                      ),
                                      
                                      
                                      
                                      plotOutput("histCentile", height = 200),
                                      
                                      plotOutput("scatterCollegeIncome", height = 250)
                                      
                        ),
                        
                        
                        
                        tags$div(id="cite",
                                 
                                 'A interactive map for CSX_SummerProject_107'), ' from shiny samples.'
                                 
                        )
                        
                    )
                    
           ),
           
           
           
           tabPanel("Data explorer",
                    
                    fluidRow(
                      
                      column(3,
                             
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                             
                      ),
                      
                      column(3,
                             
                             conditionalPanel("input.states",
                                              
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                              
                             )
                             
                      ),
                      
                      column(3,
                             
                             conditionalPanel("input.states",
                                              
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                              
                             )
                             
                      )
                      
                    ),
                    
                    fluidRow(
                      
                      column(1,
                             
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                             
                      ),
                      
                      column(1,
                             
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                             
                      )
                      
                    ),
                    
                    hr(),
                    
                    DT::dataTableOutput("ziptable")
                    
           ),
           
           
           
           conditionalPanel("false", icon("crosshair"))
           
)