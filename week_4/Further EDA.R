library(tm)
library(ggplot2)
library(dplyr)
library(broom)

## Looking at the data
wine_complete <- read.csv('./wine_data/winemag-data_first150k.csv')
wine_complete <- wine_complete[complete.cases(wine_complete),]
summary(wine_complete)


ggplot(wine_complete, aes(x = price)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of price")
summary(wine_complete$price)
## We can see that the distribution is highly skewed, it is due to the outlier with a price of 2300


## So does expensive wine taste better? We will use the score as a proxy of how well a wine tastes. 

### We will first do a general scatterplot
ggplot(wine_complete, aes(x = price, y = points)) +
  geom_point()
### There doesn't appear to have a good linear relationship. Let us take a look with correlation
wine_complete %>%
  summarize(N = n(), r = cor(price, points))
### There appears to be a weak to moderate correlation of r = 0.4598634

## Top producing countries tend to have higher scores?
### Try to identify the top 5 most reviewed countries to narrow our scope
ggplot(wine_complete, aes(x = country)) + geom_bar() + ggtitle("Country frequencies") +coord_flip()
### So we can see that some countries tend to be reviewed more than the rest.

countries <- transform(as.data.frame(table(wine_complete$country)))
countries <- countries[order(countries$Freq, decreasing = TRUE),]

countries %>%
  head(.,5) %>%
  ggplot(aes(x = Var1, y = Freq)) + 
  geom_col() +
  ggtitle("Top 5 most reviewed countries")

top_5_countries <- c("Chile", "France", "Italy", "Spain", "US")
wine_complete %>%
  filter(country == top_5_countries) %>%
  ggplot(aes(x = points)) + 
  geom_histogram() + 
  facet_wrap(~country) + 
  ggtitle("Distribution of scores for top 5 countries")

wine_complete %>%
  filter(country == top_5_countries) %>%
  ggplot(aes(x = price)) + 
  geom_histogram() + 
  facet_wrap(~country) + 
  ggtitle("Distribution of scores for top 5 countries")

Mean_prices <- aggregate(wine_complete$price, by= list(Country = wine_complete$country), FUN = mean)
Mean_prices$Country <- as.character(Mean_prices$Country)
Mean_prices_T5 <- filter(Mean_prices, Country == top_5_countries)

Mean_scores <- aggregate(wine_complete$points, by= list(Country = wine_complete$country), FUN = mean) %>%
Mean_scores <- Mean_scores[filter(Mean_scores$Country == top_5_countries),]
Median_prices <- aggregate(wine_complete$price, by= list(Country = wine_complete$country), FUN = median)
Median_scores <- aggregate(wine_complete$points, by= list(Country = wine_complete$country), FUN = median)

ggplot(wine_complete, aes(x = points)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Distribution of points") +
  geom_vline(xintercept = Mean_scores$x, show.legend = Mean_scores$Country)

# ANOVA for countries and score
