library(magrittr)
library(dplyr)
require(ggpubr)
#Doing Preliminary EDA to take a look at the data
zomato <- read.csv("~/Github/CSX_RProject_Summer_2018/week_4/zomato-restaurants-data/zomato.csv")
summaty(zomato)

# We will just shift our focus to Delhi
zomato$City <- as.character(zomato$City)
zomato$Has.Online.delivery <- as.character(zomato$Has.Online.delivery)

Delhi_food <- zomato %>%
  ungroup() %>%
  filter(., City == "New Delhi")

#ggmap to look at the distribution of the data within the city
library(ggmap)

#only in New Delhi

CountriesMap <- qmap("New Delhi", color = "bw", legend = "topleft")
CountriesMap +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = Delhi_food)

summary(Delhi_food)
# First: To see if the availability of online booking affects the ratings? (T-test)
## H0: Whether online booking or not, the rating is unaffected/customers are not bothered by it
## As the independent variable online booking yes no, is a bivariate variable, a scatterplot is not suitable. We will simply explore with histograms


ggplot(Delhi_food, aes(x = Aggregate.rating)) + 
  geom_histogram(fill = "turquoise3") +
  facet_wrap(~Has.Online.delivery) +
  geom_vline(aes(xintercept = mean(Delhi_food$Aggregate.rating), linetype = "Mean")) +
  geom_vline(aes(xintercept = median(Delhi_food$Aggregate.rating), linetype = "Median"))

# I believe that correlation analysis will not be helpful in this case. Due to the bivariate variable again.
# Time for t test.

t.test(Aggregate.rating ~ Has.Online.delivery, data = Delhi_food)

### Since p value is less than 0.05, we reject the null hypothesis as the difference in ratings between No and Yes is statistically significant.

# Second: H0: To see if the 
## H0: The ratings are not different amongst the price range. More expensive meals != better experience
ggplot(Delhi_food, aes(x = Average.Cost.for.two, y = Aggregate.rating)) +
  geom_point(colour = "red")

##As the distribution is not normal for both variable, we will use spearman correlation instead.
Delhi_food %>%
summarise(N = n(), r = cor(Aggregate.rating, Average.Cost.for.two, use = "pairwise.complete.obs", method = "spearman"))

##Number of votes one has might be a confounding factor. 
ggplot(Delhi_food, aes(x = Votes, y = Aggregate.rating)) +
  geom_point(colour = "red")

Delhi_food %>%
  summarise(N = n(), r = cor(Aggregate.rating, Votes, use = "pairwise.complete.obs", method = "spearman"))
## A very strong correlation. 

## NOw we will look at Price range since it is a categorical, ordinal factor
Delhi_food$Price.range <- factor(Delhi_food$Price.range,
                                 levels = c(1, 2, 3, 4))

#Looking at the average ratings for each price range
ggplot(data = Delhi_food, 
       aes(x = Price.range, y = Aggregate.rating)) + 
  geom_jitter(fill = "red") +
  geom_hline(yintercept = mean(Delhi_food$Aggregate.rating) , 
             linetype = 1, colour = "red") +
  labs(x = 'Price Range', y = 'Aggregate Rating') +
  annotate(geom="text", label="mean", x=0.5, y=mean(Delhi_food$Aggregate.rating), vjust=-1, colour = "red")

tapply(Delhi_food$Aggregate.rating, Delhi_food$Price.range, mean)
ggline(Delhi_food, x = "Price.range", y = "Aggregate.rating",
  add = c("mean_se"),
  order = c(1, 2, 3, 4),
  ylab = "Aggregate Ratings", xlab = "Price Range")
## Seems like there are some minor differences. 
price_anova <- aov(Aggregate.rating ~ Price.range, data = Delhi_food)
summary(price_anova)

## So we can reject null hypothesis that aggregate ratings are same amongst the different price ranges.
TukeyHSD(price_anova)
## we can see from the result of the tukey test that all difference between
## the groups are different. Except between 3-4.

# Drawing ANOVA?
price_mod1 <- lm(Aggregate.rating ~ Price.range, data = Delhi_food)
summary(price_mod1)

anova(price_mod1)
confint(price_mod1)

price_mod = data.frame(Fitted = fitted(price_mod1),
                       Residuals = resid(price_mod1),
                       Treatment = Delhi_food$Price.range)

ggplot(price_mod, aes(Fitted, Residuals, colour = Treatment)) +
  geom_point()
## we can see that the residuals and fitted values for the first 3 price
## range are quite different. However, between price range 3
## and 4, the values are rather similar and close.

