library(sf)
library(data.table)
library(lehdr)
library(tidycensus)
library(stringr)
library(dplyr)
library(leaflet)
library(tidyr)
library(htmltools)
library(leaflet)
library(rvest)
library(lubridate)
library(purrr)
library(leafgl)
library(tigris)
library(deckgl)
#library(rdeck)
library(sfheaders)




files_paths <- list.files("C:\\Users\\pgupta\\CTA\\google_transit", full.names = T)
files_names <- list.files("C:\\Users\\pgupta\\CTA\\google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(pattern = "\\.txt$", replacement = "", x = files_names)
cta_data <- files



# assigning all the value to the variables
routes <- cta_data[["routes"]]
trips <- cta_data[["trips"]]
calendar <- cta_data[["calendar"]]
stops <- cta_data[["stops"]]
stop_times <- cta_data[["stop_times"]]
shapes <- cta_data[["shapes"]]


#getting one master file 

master_file <- left_join(
                left_join(
               left_join(stop_times, 
                trips[ , c(1,2,3,6,7)], 
                  by = "trip_id"), stops[,c(1,3,5,6)], 
                    by = "stop_id"), calendar[,c(1:8)], by = "service_id")


#calculating time range per route

master_file[ , `:=`(startTime = hms(arrival_time),
                    endTime = hms(departure_time)) , ][   , 
                    `:=`(start_time = as.numeric(str_c(hour(startTime), ".",minute(startTime), second(startTime))),
                         end_time = as.numeric(str_c(hour(endTime ), ".",minute(endTime), second(endTime)))),
                    ]


return_routes_time <- function(master_file){
  
  
}

tuesday <-   master_file[tuesday == 1,  `:=`(min_hour = min(start_Hour),
                                max_hour = max(end_Hour),), 
            by = list(route_id )]














