library(reactable)
library(data.table)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(leaflet)
library(echarts4r)
library(leafgl)
library(tidycensus)

load("D:/PASSION_PROJECTS/cta/CTA/base_Data-shapes-tuesday-trips.Rdata")

setwd("D:\\PASSION_PROJECTS\\cta\\CTA")
# setwd("~/Documents/CTA/CTA")
#  
# files_paths <- list.files("~/Documents/CTA/CTA/google_transit", full.names = T)
# files_names <- list.files("~/Documents/CTA/CTA/google_transit")



# files_paths <- list.files("C:\\Users\\pgupta\\CTA\\google_transit", full.names = T)
# files_names <- list.files("C:\\Users\\pgupta\\CTA\\google_transit")

files_paths <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit", full.names = T)
files_names <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(files_names, pattern = "\\.txt$", replacement = "")

stop_times <- files[["stop_times"]]
trips <- files[["trips"]]
calendar <- files[["calendar"]]
routes <- files[["routes"]]
stops <- files[["stops"]]

stops_tues <- stop_times[trip_id %in% trips_tuesday$trip_id,,]

#filtering trips for tuesday and after June 11th end trips
trips_tuesday <- setDT(left_join(trips,calendar, by = "service_id"))
trips_tuesday <- trips_tuesday[ tuesday == 1 & end_date != "20220611", , ]












