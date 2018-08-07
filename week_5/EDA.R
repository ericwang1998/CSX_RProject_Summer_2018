library(dplyr)
library(ggplot2)
library(Amelia)

kiva_loan <- readxl::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/data/kiva_loans.xlsx")
kiva_mpi <- readxl ::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/data/kiva_mpi_region_locations.xlsx")
kiva_theme <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/data/loan_theme_ids.csv")
kiva_region <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/data/loan_themes_by_region.csv")


missmap(kiva_loan, col = c("grey", "black"), legend = FALSE, main = "Missing values map")

kiva_loan <- kiva_loan[complete.cases]
# remove errors in data entry
kiva_better <- kiva_loan[!(kiva_loan$currency == "2014-05-26 05:25:33+00:00"|
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

kiva_better <- kiva_better[!(kiva_better$country == "#Single Parent"),]
str(kiva_better)
kiva_better <- kiva_better[,2:21]
kiva_better$currency <- as.factor(kiva_better$currency)

if (kiva_better$currency == "ALL") {
  kiva_better$funded_amount * 0.0092
  kiva_better$loan_amount * 0.0092
} else if (kiva_better$currency == "AMD") {
    kiva_better$funded_amount * 0.0021
    kiva_better$loan_amount * 0.0021
} else if(kiva_better$currency == "AZN") {
    kiva_better$funded_amount * 0.59
    kiva_better$loan_amount * 0.59
} else if(kiva_better$currency == "BIF") {
    kiva_better$funded_amount * 0.000568
    kiva_better$loan_amount * 0.000568
} else if (kiva_better$currency == "PK") {
    kiva_better$funded_amount * 0.0081
    kiva_better$loan_amount * 0.0081
} else {
    kiva_better$funded_amount * 1
    kiva_better$funded_amount * 1
}
