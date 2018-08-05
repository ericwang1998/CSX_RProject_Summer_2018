library(dplyr)
library(ggplot2)
library(Amelia)

kiva_loan <- readxl::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/kiva_loans.xlsx")
kiva_mpi <- readxl ::read_xlsx("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/kiva_mpi_region_locations.xlsx")
kiva_theme <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/loan_theme_ids.csv")
kiva_region <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_5/kiva/loan_themes_by_region.csv")

str(kiva_loan)
missmap(kiva_loan, col = c("grey", "black"), legend = FALSE, main = "Missing values map")


kiva_loan <- kiva_loan[!(kiva_loan$currency == "2014-05-26 05:25:33+00:00"|
                           kiva_loan$currency == "2015-11-23 04:52:48+00:00"|
                           kiva_loan$currency == "Webuye"|
                           kiva_loan$currency == "#Woman owned Biz"|
                           kiva_loan$currency == "user_favorite\""|
                         kiva_loan$currency == "121"),]
kiva_loan <- kiva_loan[!(kiva_loan$country == "#Single Parent"),]
