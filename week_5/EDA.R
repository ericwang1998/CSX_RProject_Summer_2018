library(dplyr)
library(ggplot2)

kiva <- readxl::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/kiva_loans.xlsx")
str(kiva)
kiva$repayment_interval["male, male, female, male, female, female, male, female, male, female, male, female, male, female"]
kiva<- kiva %>%
  filter(repayment_interval == "male, male, female, male, female, female, male, female, male, female, male, female, male, female") %>%
  sub
