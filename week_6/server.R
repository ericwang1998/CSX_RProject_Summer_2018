library(shiny)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(dplyr) 
library(readxl)
library(rsconnect)

gdp_per_cap<- read.csv("./GDP_per_cap.csv", header = FALSE)
colnames(gdp_per_cap) <- c("country", "gdp_per_cap")

total_loan <- loan %>%
  group_by(country) %>%
  summarise(total = sum(loan_amount))

gdp_info<- inner_join(total_loan, gdp_per_cap, by = c("country"))

loan <- readRDS("./cleaned_loans.rds")
kiva_mpi <- readRDS("./cleaned_mpi.rds")

loan$repayment_interval <- as.factor(loan$repayment_interval)
loan$term_in_months <- as.numeric(loan$term_in_months)
loan$activity <- as.factor(loan$activity)
loan$sector <- as.factor(loan$sector)
loan$term_in_months <- as.factor(loan$term_in_months)

kiva_better <- filter(loan,loan$term_in_months < 20)


tbl_term_in_months <-table(loan$term_in_months)
tbl_term_in_months <- as.data.frame(tbl_term_in_months)

amtbreaks <- c(0,0.25,0.5,1,1.5,2,2.5,3.5,5,10,15,25,50,100,150,200,400,600,800,1000,1200,1400,1600,1800,2000,2200)
amtlabels <- c("0-0.25","0.25-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3.5","3.5-5","5-10","10-15","15-25","25-50","50-100","100-150","150-200","200-400","400-600","600-800","800-1000","1000-1200","1200-1400","1400-1600",
               "1600-1800","1800-2000","2000-2200")
loan$loan_amount_group <- cut(loan$loan_amount, 
                              breaks = amtbreaks, 
                              right = FALSE, 
                              labels = amtlabels)
loan$loan_amount_group <- as.numeric(loan$loan_amount_group)

loan$term_in_months <- as.numeric(loan$term_in_months)
amtbreaks1 <- c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160)
amtlabels1 <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110","110-120","120-130","130-140"
                ,"140-150","150-160")
loan$term_in_months_group <- cut(loan$term_in_months, 
                                     breaks = amtbreaks1, 
                                     right = FALSE, 
                                     labels = amtlabels1)

loan$activity <- as.factor(loan$activity)
freq_activity <- as.data.frame(table(loan$activity))
freq_activity <- freq_activity[freq_activity$Freq > 10000,]
kiva_new <- loan[(loan$activity == "Agriculture"|loan$activity == "Clothing Sales"|loan$activity == "Farming"|
                    loan$activity == "Fish Selling"|loan$activity == "Food Production/Sales"|loan$activity== "Fruits & Vegetables"|
                    loan$activity == "General Store"|loan$activity == "Grocery Store"|loan$activity == "Higher education costs"|
                    loan$activity == "Home Appliances"|loan$activity == "Livestock"|loan$activity == "Personal Housing Expenses"|
                    loan$activity == "Pigs"|loan$activity == "Retail"),]


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

  
  output$hist_regions = renderPlot({
    mpi_clean <- as.data.frame(kiva_mpi$country) %>%
      cbind(kiva_mpi$region) %>%
      cbind(kiva_mpi$MPI) %>%
      cbind(kiva_mpi$world_region)
    colnames(mpi_clean) <- c("country", "region", "mpi", "world_region")
    mpi_clean %>%
      ggplot(aes(x = world_region, y = mpi)) +
      geom_boxplot()+theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  
  output$mytable1 = renderDataTable({
    if (input$con1 != "All") {
      loan <- loan[loan$country == input$con1, input$show_vars1, drop = FALSE]
    }
    if (input$reg1 != "All") {
      loan <- loan[loan$region == input$reg1,input$show_vars1, drop = FALSE]
    }
    if (input$act != "All") {
      loan <- loan[loan$activity == input$act,input$show_vars1, drop = FALSE]
    }
    if (input$sec != "All") {
      loan <- loan[loan$sector == input$sec,input$show_vars1, drop = FALSE]
    }
    loan[,input$show_vars1, drop = FALSE]
  })
  
  output$mytable2 = renderDataTable({
    if (input$con2 != "All") {
      kiva_mpi <- kiva_mpi[kiva_mpi$country == input$con2,input$show_vars2, drop = FALSE]
    }
    if (input$reg2 != "All") {
      kiva_mpi <- kiva_mpi[kiva_mpi$region == input$reg2,input$show_vars2, drop = FALSE]
    }
    kiva_mpi[,input$show_vars2, drop = FALSE]
  })
 
  output$plot <- renderPlot({
    gdp_info%>%
      top_n(as.numeric(input$n),wt=total)%>%
      arrange(desc(total))%>%
      ggplot(aes(x = reorder(country,-total), y = total,fill = gdp_per_cap))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
      labs(x="Country",y="Loan Amount Sum")
  })
  
  
  output$plot2 <- renderPlot({
    loan %>% 
      filter(!is.na(sector)) %>%
      group_by(sector)%>%
      ggplot(aes(sector,loan_amount))+
      geom_bar(stat="identity",fill = "skyblue")+
      theme_minimal()+
      theme(axis.text.x = element_text(size = 12,angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 12,angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
      labs(x="Sectors",
           y="Loan amount")
  })
  
  output$plot3 <- renderPlot({
    loan_sector <- loan$loan_amount
    #loan$sector <- as.numeric(loan$sector)
    summary(loan_sector)
    sd(loan_sector)
    boxplot(log(loan$loan_amount) ~loan$sector,las=2,cex=0.5,font=10,font.lab=10,cex.lab=10,cex.axis=0.7,
            col ="skyblue", border = "steelblue")
  })
  
  output$plot4 <- renderPlot({
    loan %>% 
      filter(!is.na(activity)) %>%
      group_by(activity) %>%
      summarise( total = sum(loan_amount))%>%
      top_n(as.numeric(input$n2),wt=total)%>%
      arrange(desc(total))%>%
      ggplot(aes(x =  reorder(activity,-total), y = total),family ="english")+
      geom_bar(stat="identity",fill = "skyblue")+
      theme_minimal()+
      theme(
            axis.text.x = element_text(size = 12,angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 12,angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
      labs(x="Activity",y="Loan Sum")
  }) 
  
  output$plot5 <- renderPlot({
    loan$loan_amount_group <- as.numeric(loan$loan_amount_group)
    loan_amt_group <- loan %>%
      filter (!is.na(loan_amount_group))%>%
      group_by(loan_amount_group)%>%
      summarise(count_grp = n())%>%
      arrange(desc(count_grp))%>%
      ggplot(aes(x= loan_amount_group,y = count_grp))+
      geom_bar(stat="identity",fill = "skyblue")+
      coord_flip()+
      geom_label(aes(label = round(count_grp,2)), size = 4, y= 30000, col = "darkblue")+
      theme_minimal()+
      theme(axis.text = element_text(size = 18),
            axis.text.x = element_text(size = 15 ,angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 15 ,angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
      labs(x="Loan amount groups",y="Count")
    loan_amt_group
  })
  
  output$plot6 <- renderPlot({
    kiva_new$term_in_months <- as.numeric(kiva_new$term_in_months)
    kiva_new %>% 
      group_by(activity)%>%
      ggplot(aes(x= activity, y = term_in_months),family ="english")+
      geom_bar(stat="identity",fill = "skyblue")+
      theme_minimal()+
      theme(axis.text.x = element_text(size = 12 ,angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 12 ,angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
      labs(x="Activity",y="Loan duration(month)")
  })
  
  output$plot7 <- renderPlot({
    loan %>%
      ggplot(aes(term_in_months_group))+
      geom_bar(fill= "skyblue")+
      theme_minimal()+
      theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 22,angle = 0, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(size = 15, face = "bold"),
            axis.title.y = element_text(size = 15, face = "bold",angle=0))+
      labs(x="Loan Duration(months)",y="count")
  })
  
 output$plot8 <- renderPlot({
  
   amtbreaks <- c(0,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75)
   amtlabels <- c("0-0.15","0.15-0.2","0.2-0.25","0.25-0.3","0.3-0.35","0.35-0.4","0.4-0.45","0.45-0.5",
                  "0.5-0.55","0.55-0.6","0.6-0.65","0.65-0.7","0.7-0.75")
   loan$loan_amount_group <- cut(loan$loan_amount, 
                                 breaks = amtbreaks, 
                                 right = FALSE, 
                                 labels = amtlabels)
    
   loan %>% 
     group_by(loan_amount_group,country)%>%
     filter (!is.na(loan_amount_group) &
               country ==input$con3 )%>%
     summarise(sum_grp = sum(loan_amount))%>%
     arrange(desc(sum_grp))%>%
     ggplot(aes(x= loan_amount_group,y = sum_grp,group=country))+
     geom_bar(stat="identity",fill = "skyblue")+
     coord_flip()+
     facet_wrap(~country)+
     theme_minimal()+
     theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 1),
           axis.text.y = element_text(size = 12,angle = 0, vjust = 0.5, hjust = 1),
           axis.title.x = element_text(size = 15, face = "bold"),
           axis.title.y = element_text(size = 15, face = "bold",angle=0))+
     labs(x="Loan amount groups", y="Frequency")
   
 }) 
 
 output$plot9 <- renderPlot({
   
   loan %>%
     ggplot(aes(repayment_interval))+
     #scale_y_log10()+
     geom_bar(fill= "skyblue")+
     theme_minimal()+
     theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 1),
           axis.text.y = element_text(size = 12,angle = 0, vjust = 0.5, hjust = 1),
           axis.title.x = element_text(size = 15, face = "bold"),
           axis.title.y = element_text(size = 15, face = "bold",angle=0))+
     labs(x="Payment Interval",y="count")
 })
 
 output$plot10 <- renderPlot({
   mpi_new<- kiva_mpi %>%
     group_by(country) %>%
     summarise(mean_MPI= mean(MPI))
   
   mpi_new <- mpi_new[order(-mpi_new$mean_MPI),]
   
   mpi_new %>%
     top_n(n = 10, wt = mean_MPI) %>%
     ggplot(aes(x = reorder(country,-mean_MPI), y = mean_MPI)) + 
     geom_bar(stat="identity",fill = "skyblue")+
     theme_minimal()+
     theme(axis.text = element_text(size = 12),
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
           axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
           axis.title.x = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5),
           axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
     labs(x="Country",y="Mean MPI values")
 })
}