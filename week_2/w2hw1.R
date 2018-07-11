#install ggplot 2 package
install.packages("ggplot2")
install.packages("magrittr")
library(magrittr)
library(ggplot2)
data("iris")
data("trees")

## Scatter and bar chart
#Explore trees data set
print(trees)
summary(trees)
str(trees)
complete.cases(trees)

#Explore iris data set
print(iris)
summary(iris)
str(iris)
complete.cases(iris)

#preliminary data visualisation

ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_col()
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_point()
# so we can kinda see that the petal length and sepal length can be affected by species. There is a clear pattern here.

ggplot(iris, aes(x = Species, y = Sepal.Width)) + geom_col()
ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Species)) + geom_point()
# So we can also see the species has a clear influence of the size of the flowers.

remove(iris, trees)

## Line graph
data("economics")
str(economics)
complete.cases(economics)

##now plotting time!
ggplot(economics, aes(x = date, y = pce)) + geom_point(color = "blue", size = 1)
ggplot(economics, aes(x = pop, y = unemploy)) + geom_line(color = "pink", size = 2)
remove(economics)

##multiple lines
data("txhousing")
str(txhousing)
ggplot(txhousing, aes(x = date, y = listings, group = city, color = city)) + geom_line()

## Box plot
  
ggplot(txhousing, aes(x = year, y = sales, group = year)) + geom_boxplot()

## Stacked barplot
ggplot(txhousing, aes(x = year, fill = city)) + geom_bar(position = "fill")
                                                               