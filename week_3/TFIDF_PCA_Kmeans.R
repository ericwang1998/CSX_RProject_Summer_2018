#Analyzing the change in music over 20 + years.

#LOAD THE LIBRARIES
library(tidyverse)
library(dplyr)

#IMPORT DATA
##Obtain the list of file locations
setwd("~/GitHub/CSX_RProject_Summer_2018/week_3/songs_data")
files_list <- list.files(path = ".", recursive = TRUE,
                         pattern = "*.txt$", full.names = TRUE)
##Obtain the names of files
Names <- read.csv("~/GitHub/CSX_RProject_Summer_2018/week_3/songs_data/billboardwinners.csv", sep = ".")
Names$X = NULL

##Clean the data
Names <- str_split_fixed(Names$ï..song_name..artist..year, ",",3)
colnames(Names)<-c("Song", "Artist", "Year")

##Obtain the list of songs and merging
songs <- lapply(files_list, read_file) %>%
  data_frame(.) %>%
  cbind(., Names)
  
##housekeeping
rm(f, files_list)

##