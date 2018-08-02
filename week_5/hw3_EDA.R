
library(readxl)
theme_type <- read.csv("./kiva/loan_theme_ids.csv")
reg_sec_amo <- read.csv("./kiva/loan_themes_by_region.csv")
reg1 <- read_excel("./kiva/kiva_loans.xlsx")
reg2 <- read_excel("./kiva/kiva_mpi_region_locations.xlsx")