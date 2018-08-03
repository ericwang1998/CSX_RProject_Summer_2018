library(dplyr)
library(ggplot2)
library(Amelia)

kiva <- readxl::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/kiva_loans.xlsx")
str(kiva)
kiva$repayment_interval["male, male, female, male, female, female, male, female, male, female, male, female, male, female"]
kiva<- kiva %>%
  filter(repayment_interval == "male, male, female, male, female, female, male, female, male, female, male, female, male, female")

missmap(kiva, col = c("grey", "black"), legend = FALSE, main = "Missing values map")

