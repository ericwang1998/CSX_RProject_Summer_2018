library(magrittr)
library(dplyr)

#Doing Preliminary EDA to take a look at the data
zomato <- read.csv("~/Github/CSX_RProject_Summer_2018/week_4/zomato-restaurants-data/zomato.csv")
summaty(zomato)

# We will just shift our focus to Delhi
zomato$City <- as.character(zomato$City)
zomato$Has.Table.booking <- as.character(zomato$Has.Table.booking)
Delhi_food <- zomato %>%
  ungroup()
  filter(., City == "New Delhi")

#ggmap to look at the distribution of the data within the city
library(ggmap)

#only in New Delhi

CountriesMap <- qmap("New Delhi", color = "bw", legend = "topleft")
CountriesMap +
  geom_point(aes(x = Longitude, y = Latitude, color = Has.Table.booking), 
             data = Delhi_food)

summary(Delhi_food)
# First: To see if the availability of online booking affects the ratings? (T-test)
## H0: Whether online booking or not, the rating is unaffected/customers are not bothered by it
cor

# Second: H0: To see if the 
## H0: The ratings are not different amongst the price range. More expensive meals != better experience


# Third: Doing linear modeling to predict the ratings of the restaurant. 