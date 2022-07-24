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

#total trips, routes, frequency, first-trip, last-trip, tod
#stops : trips by TOD and routes, routs x-axis, trips by tod Y axis 

cmap_grid <- st_read("C:\\Users\\gupta\\Downloads\\books\\CMAPONTO2050ForecastByLAZ",
                     layer = "CMAP_ONTO2050_ForecastByLAZ")
cmap_grid_f <-st_transform(cmap_grid[ cmap_grid$COUNTY == "031", ],4326)

bg <-  st_transform(get_acs(variables = "B02001_001", geography = "block group",
               state = "17", county = "031",output = "wide",
               geometry = T, year = 2020),4326)
colnames(bg)[3] <- "Total"

return_tod <- function(day_one, type_of_day){
  if(type_of_day != "saturday" & type_of_day != "sunday"){
    day_tod <- day_one %>%
      mutate(TOD = if_else(start_local_hour >= 4 &
                             start_local_hour < 6, "Early-AM",
                           if_else(start_local_hour >= 6 &
                                     start_local_hour < 9, "AM-Peak",
                                   if_else(start_local_hour >= 9 &
                                             start_local_hour < 15, "Midday",
                                           if_else(start_local_hour >= 15 &
                                                     start_local_hour < 19, "PM-Peak",
                                                   if_else(start_local_hour >= 19 &
                                                             start_local_hour < 23, "Evening", "Late-Night"
                                                   )
                                           )
                                   )
                           )
      )
      )
  } else if(type_of_day == "saturday"){
    day_tod <- day_one %>%
      mutate(TOD = if_else(start_local_hour >= 4 &
                             start_local_hour < 8, "SAT-AM",
                           if_else(start_local_hour >= 8 &
                                     start_local_hour < 20, "SAT-MD",
                                   if_else(start_local_hour >= 20 , "SAT-PM", "SAT-Late-Night"
                                   )
                           )
      )
      )
  } else if(type_of_day == "sunday"){
    day_tod <- day_one %>%
      mutate(TOD = if_else(start_local_hour >= 0 &
                             start_local_hour < 5, "SUN-EA",
                           if_else(start_local_hour >= 5 &
                                     start_local_hour < 9, "SUN-AM",
                                   if_else(start_local_hour >= 9 &
                                             start_local_hour < 18, "SUN-MD", "SUN-PM"
                                   )
                           )
      )
      )
    
  }
  
  return(day_tod)
  
}

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

load("D:/PASSION_PROJECTS/cta/CTA/base_Data-shapes-tuesday-trips.Rdata")


#filtering trips for tuesday and after June 11th end trips
trips_tuesday <- setDT(left_join(trips,calendar, by = "service_id"))
trips_tuesday <- trips_tuesday[ tuesday == 1 & end_date != "20220611", , ]

#making Columns for TOD classification
stops_tuesday <- stop_times[trip_id %in% 
                              trips_tuesday$trip_id , , ][, 
                      `:=`(start_local_hour = as.numeric(str_sub(arrival_time,1,2)),
                          arrival = as.numeric(gsub(arrival_time, pattern = ":", replacement = "")),
                          departure = as.numeric(gsub(departure_time, pattern = ":", 
                                                      replacement = ""))), ] 

#adding TOD information 
stops_tuesday <- return_tod(setDT(left_join(stops_tuesday, 
                                            trips_tuesday[ , c(1,3)], 
                                            by = "trip_id")), 
                                       type_of_day = "tuesday")

#adding stops meta info like lon,lat,stop_name and stop_id to join
stops_tuesday <- left_join(stops_tuesday, 
                           stops[,c(1,3,5,6)], 
                           by = "stop_id")
#adding tuesday trips info
stops_tues_trips <- left_join(stops_tuesday, 
                              tuesday[, c(1:10,14,15)], 
                              by = "trip_id")
daily_stops_stats <- stops_tues_trips[ , .(Ttrips_lastRow = .N,
                                           Ttrips_uniq = length(unique(trip_id)),
                                           routes = paste0(unique(route_id.x), collapse = ", "),
                                           stop_seq = paste0(unique(stop_sequence), collapse = ", "),
                                           head_sign = paste0(unique(str_c(route_id.x,"-",stop_headsign,
                                           "-",stop_sequence)), collapse = ", "),
                                           distance = sum(shape_dist_traveled)), 
                                       by = list(stop_id,TOD,stop_name, stop_lat,stop_lon )]
setorder(daily_stops_stats, -Ttrips_lastRow)
daily_stops_stats_sf <- st_as_sf(daily_stops_stats, 
                                 coords = c("stop_lon","stop_lat"),
                                 crs = 4326 )


by_bg <- st_join(daily_stops_stats_sf, 
                 bg[,c(1,3)], join = st_within)
by_laz <- st_join(daily_stops_stats_sf, 
                  cmap_grid_f[,c(7)], join = st_within)

#Visualize here


