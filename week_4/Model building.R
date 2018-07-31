

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

# shows that price range 4 has the greatest variation when it comes to affecting the aggregate rating?
# and that price range 3 will have the greatest effect on the aggregate rating.

fit_m2 <- data.frame(Delhi_food[, c(14, 17, 18)], fitted = fitted(m2), resid = resid(m2),
          infl = influence(m2)$hat )

# Checking the model fit
ggplot(data = fit_m2, aes(x = Aggregate.rating, group = Price.range )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(Delhi_food, tapply(Aggregate.rating,Price.range, mean))), linetype = 'dotted')+
  facet_grid(Price.range ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = 'Aggregate Rating', y = 'Probability Density')


# Checking the normal distribution assumption, which is false. hahaha.
require(lattice)
qqmath(~ scale(resid) | Price.range, data = fit_m2, type = c('p', 'g', 'r'),
       xlab = 'Normal theoretical quantiles', ylab = 'Standard error', layout = c(2, 3),
       pch = '.', cex = 2)


# Drawing standard eror and predictor value plot. Also, checking for linear and equal distribution, which is false again.
require(MASS)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = Price.range )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(Price.range ~ .) +
  labs(x = 'Predictor value', y = 'Standard error')

summary(influence(m2)$hat)

theme_set(old)
