library(leaflet)
library(magrittr)
library(maps)


## Testing the leaflet package
m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
  addTiles() %>%
  addMarkers(lng=103.758795, lat=1.376922, popup = "Ryan was here" )

m

#Plotting GDP per cap on map

### Load data
gdp_raw<- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/data/world_bank/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_10051784.csv", skip = 4)
### Clean the data
gdp_world <-as.data.frame(gdp_raw$Country.Name) %>%
  cbind(., gdp_raw$X2017)
colnames(gdp_world) <- c("country", "pcap")
gdp_world <- subset(gdp_world, country != "World")
gdp_world<- gdp_world[complete.cases(gdp_world), ]

##Adding the bins
bins <- c(0, 1000, 3000, 6000, 10000, 15000, 21000, Inf)
pal <- colorBin("YlOrRd", domain = gdp_world$pcap, bins = bins)

## Some HTML to show the labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  gdp_world$country, gdp_world$pcap
) %>% lapply(htmltools::HTML)

## Drawing the map
mapCountries <- map(database = "world", regions = gdp_world$Country.Name, fill = TRUE, plot = FALSE)
leaflet(data = mapCountries) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(gdp_world$pcap),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))%>%
  addLegend(pal = pal, values = ~gdp_world$pcap, opacity = 0.7, title = NULL,
            position = "bottomright")
