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

zomato <- read.csv("~/Github/CSX_RProject_Summer_2018/week_4/zomato-restaurants-data/zomato.csv")
Delhi_food <- zomato %>%
  ungroup() %>%
  filter(., City == "New Delhi")

# Check for normality
Delhi_food$Price.range <- factor(Delhi_food$Price.range,
                                 levels = c(1, 2, 3, 4))
anova(m1 <- lm(Aggregate.rating ~ Price.range, data = Delhi_food))
summary(m1)$r.squared

# adding in average cost for two
anova(m2 <- update(m1, . ~ . + 
                     Average.Cost.for.two, data = Delhi_food))

# removing price range
anova(m3 <- update(m2, . ~ . -
                     Price.range, data = Delhi_food))

# include the results in a list
res_lm <- lapply(list(m1, m2, m3), summary)

# comparing controlling for price for 2, price range's effect
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)

# controlling for price range, effect of cost for 2.
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)

require(coefplot)
m2 <- lm(Aggregate.rating ~ Price.range + Average.Cost.for.two- 1,
         data = Delhi_food)
coefplot(m2, xlab = "Estimated Value", ylab = "Regression coefficient", title = "Response Variable = Aggregate Rating")

fit_m2 <- data.frame(Delhi_food[, c(2, 12, 13)], fitted = fitted(m2))