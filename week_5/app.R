#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("binsBar",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         sliderInput("intervals",
                      "Intervals:",
                      min = 0, max = 6,
                      value = c(1,2))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("distPlot2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      binsBar <- seq(min(x), max(x), length.out = input$binsBar + 1)
      # draw the histogram with the specified number of bins
      hist(x, breaks = binsBar, col = 'darkgray', border = 'white')
   })
  
  output$distPlot2 <- renderPlot({
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
      geom_point() + xlim(input$intervals)
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

