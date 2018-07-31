# So previously we looked at the following factors:
#   1) Price Range
#   2) Average cost for 2
#   3) Whether there is online delivery
#   4) Number of votes

# So we shall build our model based on those 4 factors.
# Starting with votes, as there was a high correlation

# So let us load in the data from the previous exercise. 
library(magrittr)
library(dplyr)
require(ggpubr)
require(tidyr)

zomato <- read.csv("~/Github/CSX_RProject_Summer_2018/week_4/zomato-restaurants-data/zomato.csv")
Delhi_food <- zomato %>%
  ungroup() %>%
  filter(., City == "New Delhi")

# housekeeping
rm(zomato)
Delhi_food$Price.range <- factor(Delhi_food$Price.range,
                                 levels = c(1, 2, 3, 4))
Delhi_food$Average.Cost.for.two <- as.numeric(Delhi_food$Average.Cost.for.two)
Delhi_food$Votes <- as.numeric(Delhi_food$Votes)

# Scatterplot for the interested varaibles to look at correlations
Delhi_ratings<- Delhi_food[, c("Aggregate.rating", "Average.Cost.for.two", "Votes")]

Delhi_ratings %>%
  gather(-Aggregate.rating, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Aggregate.rating)) + 
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()
# All variabes are not normally distributed, haha Gaussian is an idealist.

# Plotting correlations
corrplot(cor(Delhi_ratings), method = "number", type = "upper")
corrplot(cor(Delhi_ratings), method = 'ellipse', order = 'hclust', addrect = 4,
         type = 'upper', tl.pos = 'tp')
corrplot(cor(Delhi_ratings), add = TRUE, type = 'lower', method = 'number',
         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

# Add in the anova linear model from the sample code
anova(m1 <- lm(Aggregate.rating ~ Price.range, data = Delhi_food))
anova(m2 <- update(m1, . ~ . + Average.Cost.for.two, data = Delhi_food))
anova(m3 <- update(m2, . ~ . - Price.range, data = Delhi_food))
m2 <- lm(Aggregate.rating ~ Price.range + Average.Cost.for.two- 1, data = Delhi_food)

summary(m4 <- lm(Aggregate.rating ~ Average.Cost.for.two + Votes, data = Delhi_ratings))
coefplot(m4, predictors = c('Average.Cost.for.two', 'Votes'),
         xlab = 'Estimated Value', ylab = 'Regression Coefficient(Remove residuals)', title = 'Response variable is Aggregate rating')
require(effects)
plot(allEffects(m4), main = '', ylim = c(0, 5), grid = T)

# Use the lm.beta package to calculate the regression estimates
library(lm.beta)
summary(lm.beta(m4))

# We will see if just average cost for two will affect the ratings
summary(m5 <- update(m4, . ~ . - Votes , data = Delhi_ratings))
anova(m5, m4)





# Time for some force fitting strategies
#m5 <- everything
m5 <- lm(Aggregate.rating ~ Average.Cost.for.two + Has.Online.delivery + Votes + Price.range, data = Delhi_food)
fit_m5 <- data.frame(Delhi_food[, c(11, 14, 17, 18, 21)], fitted = fitted(m5), resid = resid(m5), infl = influence(m5)$hat)

ggplot(data = fit_m5, aes(x = Aggregate.rating, group = Price.range )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(Delhi_food, tapply(Aggregate.rating, Price.range, mean))), linetype = 'dotted')+
  facet_grid(Price.range ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = 'Aggregate rating', y = 'Probability density')
summary(lm.beta(m5))

m6 <- lm(Aggregate.rating ~ Has.Online.delivery + Price.range, data = Delhi_food)
fit_m6 <- data.frame(Delhi_food[, c(14, 17, 18)], fitted = fitted(m6), resid = resid(m6), infl = influence(m6)$hat)

ggplot(data = fit_m6, aes(x = Aggregate.rating, group = Price.range )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(Delhi_food, tapply(Aggregate.rating, Price.range, mean))), linetype = 'dotted')+
  facet_grid(Price.range ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = 'Aggregate rating', y = 'Probability density')
summary(lm.beta(m6))