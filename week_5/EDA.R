library(dplyr)
library(ggplot2)
library(Amelia)

kiva_loan <- readxl::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/data/kiva_loans.xlsx")
kiva_mpi <- readxl ::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/data/kiva_mpi_region_locations.xlsx")
kiva_theme <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/data/loan_theme_ids.csv")
kiva_region <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/data/loan_themes_by_region.csv")


missmap(kiva_loan, col = c("grey", "black"), legend = FALSE, main = "Missing values map")

kiva_loan <- kiva_loan[complete.cases]

kiva_loan <- kiva_loan[!(kiva_loan$currency == "2014-05-26 05:25:33+00:00"|
                           kiva_loan$currency == "2015-11-23 04:52:48+00:00"|
                           kiva_loan$currency == "Webuye"|
                           kiva_loan$currency == "#Woman owned Biz"|
                           kiva_loan$currency == "user_favorite\""|
                           kiva_loan$currency == "121"|
                           kiva_loan$currency == "irregular"|
                           kiva_loan$currency == "monthly"|
                           kiva_loan$currency == "female"|
                           kiva_loan$currency == "42587"|
                           kiva_loan$currency == "42695"),]

kiva_loan <- kiva_loan[!(kiva_loan$country == "#Single Parent"),]
str(kiva_loan)
kiva_loan <- kiva_loan[,2:21]
kiva_loan$currency <- as.factor(kiva_loan$currency)

if (kiva_loan$currency == ALL) {
  kiva_loan$funded_amount * 0.0092
  kiva_loan$loan_amount * 0.0092 }
  if (kiva_loan$currency == AMD) {
    kiva_loan$funded_amount * 0.0021
    kiva_loan$funded_amount * 0.0021}
    if(kiva_loan$currency == AZN) {
      kiva_loan$funded_amount * 0.59
      kiva_loan$funded_amount * 0.59}
      if(kiva_loan$currency == BIF) {
        kiva_loan$funded_amount * 0.000568
        kiva_loan$funded_amount * 0.000568}
    
  
