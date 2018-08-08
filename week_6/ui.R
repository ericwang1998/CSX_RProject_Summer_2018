library(shiny)
library(leaflet)
library(RColorBrewer)
library(readxl)
library(magrittr)
library(tidyverse)
library(rsconnect)

loan <- readRDS("./cleaned_loans.rds")
loan$term_in_months <- as.factor(loan$term_in_months)

kiva_mpi <- readRDS("./cleaned_mpi.rds")

freq <- as.data.frame(table(loan$country))
freq_f <- freq[freq$Freq > 6000,]
freq_f <- freq_f[!(freq_f$Var1=="Ecuador"|freq_f$Var1=="El Salvador"|freq_f$Var1=="India"|
                     freq_f$Var1=="Nicaragua"|freq_f$Var1=="Pakistan"|freq_f$Var1=="Peru"|
                     freq_f$Var1=="Tajikistan"|freq_f$Var1=="Bolivia"|freq_f$Var1=="Guatemala"|
                     freq_f$Var1=="Honduras"|freq_f$Var1=="Palestine"|freq_f$Var1=="Philippines"|
                     freq_f$Var1=="Samoa"|freq_f$Var1=="United States"),]
temp1 <- as.character(freq_f$Var1)
temp1 <- as.list(temp1)

navbarPage("Kiva Crowdfunding Analysis",
           tabPanel("Interactive map of MPI",
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    leafletOutput("map", width = "900", height = "900"),
                    absolutePanel(top = 200, right = 10,
                                  sliderInput("range", "MPI", min(kiva_mpi$MPI), max(kiva_mpi$MPI),
                                              value = range(kiva_mpi$MPI), step = 0.05),
                                  selectInput("colors", "Color Scheme",
                                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                  checkboxInput("legend", "Show legend", TRUE),
                                  plotOutput("hist_regions", height = 400))),
           tabPanel("Top MPI countries",
                    fluidRow(
                      plotOutput(outputId = "plot10",height = "600px")
                    )
                    ),
           tabPanel("Data Table 1",
                    fluidRow(
                      column(4, selectInput("con1","country:",c("All",unique(as.character(loan$country) ) ) )),
                      column(4, selectInput("reg1","region:",c("All",unique(as.character(loan$region) ) ) )),
                      column(4, selectInput("act","activity:",c("All",unique(as.character(loan$activity) ) ) )),
                      column(4, selectInput("sec","sector:",c("All",unique(as.character(loan$sector) ) ) ))
                    ),
                    fluidRow(
                      pageWithSidebar(
                        headerPanel('kiva loan'),
                        sidebarPanel(
                          checkboxGroupInput('show_vars1', 'Columns in kiva loans to show:', names(loan),
                                             selected = names(loan)),
                          helpText("You can only select one column for a specific vallue in at a time.
                                   Otherwise, error will happen.")
                        ),
                        mainPanel(
                            tabPanel('loan',dataTableOutput("mytable1")) 
                        )
                      )
                    )
           ),
           tabPanel("Data Table 2",
                    #
                    fluidRow(
                      column(4, selectInput("con2","country:",c("All",unique(as.character(kiva_mpi$country) ) ) )),
                      column(4, selectInput("reg2","region:",c("All",unique(as.character(kiva_mpi$region) ) ) ))
                      ),
                    # Create a new row for the table.
                    fluidRow(
                      pageWithSidebar(
                        headerPanel('Kiva mpi region locations'),
                        sidebarPanel(
                          checkboxGroupInput('show_vars2', 'Columns in Kiva mpi region locations to show:', names(kiva_mpi),
                                             selected = names(kiva_mpi)),
                          helpText("You can only select one column for a specific vallue in at a time.
                                   Otherwise, error will happen.")
                        ),
                        mainPanel(
                            tabPanel('Kiva mpi region locations',dataTableOutput("mytable2")) 
                        )
                      )
                    )
           ),
           tabPanel("Loan Amount by Country",
                    numericInput("n", "Number of column:", min = 0, max = 30, value = 10),
                    plotOutput(outputId = "plot",height = "600px")
                    
           ),
           tabPanel("Loan Amount by Activity",
                    numericInput("n2", "Number of column:", min = 0, max = 30, value = 10),
                    plotOutput(outputId = "plot4",height = "600px")
           ),
           tabPanel("Loan Amount by Sector",
                    plotOutput(outputId = "plot2",height = "600px")
           ),
          tabPanel("Loan Amount Distribution by Sector",
                    plotOutput(outputId = "plot3",height = "600px")
           ),
           tabPanel("Loan Amount Range Distribution",
                    plotOutput(outputId = "plot5",height = "600px")
           ),
           tabPanel("Loan Amount Range Distribution by Country",
                    fluidRow(
                      column(4, selectInput("con3","country:",temp1 ))
                    ),
                    fluidRow(
                      plotOutput(outputId = "plot8",height = "600px")
                    )
           ),
           tabPanel("Loan duration",
                    plotOutput(outputId = "plot7",height = "600px")
           ),
           tabPanel("Loan Duration by Activity",
                    plotOutput(outputId = "plot6",height = "600px")
           ),
           tabPanel("Repayment interval",
                    plotOutput(outputId = "plot9",height = "600px")
           )
)