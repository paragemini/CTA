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
library(echarts4r)

#functions
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





# load("D:/PASSION_PROJECTS/cta/CTA/base_Data-shapes-tuesday-trips.Rdata")
load("C:\\Users\\pgupta\\CTA/base_Data-shapes-tuesday-trips.Rdata")


#setwd("D:\\PASSION_PROJECTS\\cta\\CTA")
# setwd("~/Documents/CTA/CTA")
#  
# files_paths <- list.files("~/Documents/CTA/CTA/google_transit", full.names = T)
# files_names <- list.files("~/Documents/CTA/CTA/google_transit")



 files_paths <- list.files("C:\\Users\\pgupta\\CTA\\google_transit", full.names = T)
 files_names <- list.files("C:\\Users\\pgupta\\CTA\\google_transit")




#files_paths <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit", full.names = T)
#files_names <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(files_names, pattern = "\\.txt$", replacement = "")

stop_times <- files[["stop_times"]]
trips <- files[["trips"]]
calendar <- files[["calendar"]]
routes <- files[["routes"]]
stops <- files[["stops"]]

#filtering trips for Tuesday and after June 11th end trips
trips_tuesday <- setDT(left_join(trips,calendar, by = "service_id"))
trips_tuesday <- trips_tuesday[ tuesday == 1 & end_date != "20220611", , ]

#Stops for Tuesday
stops_tues_f <- stop_times[trip_id %in% tuesday$trip_id,]    
stops_tues <- stop_times[trip_id %in% tuesday$trip_id,
                         .(all_stops = list(stop_id)),
                                 by = list(trip_id)]

tuesday <- setDT(left_join(return_tod(tuesday,"Tuesday"), stops_tues, by = "trip_id"))


#final stops with TOD, without TOD
final_stops <- tuesday[ , .(Trips = .N,
               total_stops = Total_Stops[.N],
               all_stops = all_stops[.N]) , 
               by = list(route_id,direction,shape_id)]

final_stops_tod <- tuesday[ , .(Trips = .N,
                            total_stops = Total_Stops[.N],
                            all_stops = all_stops[.N]) , 
                        by = list(TOD,route_id,direction,shape_id)]
#joining stops to shapes
final_stops$all_stops <- lapply(final_stops$all_stops, function(x){
         left_join(setDT(list(x)), stops[,c(1,6,5,3)], by = c("V1" = "stop_id"))
})

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = final_stops$all_stops[final_stops$shape_id == "306800007"], 
             lat = final_stops$all_stops[final_stops$shape_id == "306800007"][[1]]$stop_lat ,
             lng = final_stops$all_stops[final_stops$shape_id == "306800007"][[1]]$stop_lon,
             popup = paste0("<strong>Stop Name : </strong>", 
        final_stops$all_stops[final_stops$shape_id == "306800007"][[1]]$stop_name)) %>%
  addPolylines( data = trip_shapes[trip_shapes$shape_id == "306800007",], 
                weight = 4, color = "black") %>%
  addCircleMarkers(data = final_stops$all_stops[final_stops$shape_id == "306800084"], 
                   lat = final_stops$all_stops[final_stops$shape_id == "306800084"][[1]]$stop_lat ,
                   lng = final_stops$all_stops[final_stops$shape_id == "306800084"][[1]]$stop_lon,
                   popup = paste0("<strong>Stop Name : </strong>", 
                                  final_stops$all_stops[final_stops$shape_id == "306800084"][[1]]$stop_name)) %>%
  addPolylines( data = trip_shapes[trip_shapes$shape_id == "306800084",], 
                weight = 4, color = "black")

#### creating chart for all trips by Route 

blue_trips_stops <- stops_tues_f[trip_id %in% tuesday$trip_id[tuesday$route_id == "Blue"], ]
blue_trips_stops <- left_join(blue_trips_stops, stops[,c("stop_id","stop_name")], by = "stop_id")
max_trip_id <- blue_trips_stops[ which(blue_trips_stops$stop_sequence == max(blue_trips_stops$stop_sequence))[c(1,2)]]$trip_id


bstops_time <- blue_trips_stops %>%
  #filter(trip_id %in% max_trip_id) %>%
  arrange(trip_id,arrival_time) %>%
  select(stop_name,arrival_time,trip_id) %>%
  arrange(arrival_time,trip_id)%>%
  pivot_wider(names_from = "trip_id", values_from = "arrival_time")



time_conv <- setDT(lapply(bstops_time[,2:ncol(bstops_time)], function(x){
  h <- strsplit(x, ":")
  t <- unlist(lapply(h, function(k){

       t_hour <- as.numeric(k[1]) * 3600
       t_min  <- as.numeric(k[2]) * 60
       t_s    <- as.numeric(k[3])
       return(t_hour + t_min + t_s)
  }))

  return(t)

}))

time_conv <- cbind(bstops_time[,1], time_conv) %>%
  arrange(`68227751051`)

colnames(time_conv)[c(2:ncol(time_conv))] <- unlist(lapply(2:ncol(time_conv), function(x){
                                                            str_c("trip_", x)})) 

 time_conv %>%
  e_charts(stop_name) %>%
  e_line(trip_2, legend = F) %>%
   e_line(trip_3) %>%
   e_line(trip_4) %>%
   e_line(trip_5) %>%
   e_line(trip_6) %>%
   e_line(trip_7) %>%
   e_line(trip_8) %>%
   e_line(trip_9) %>%
   e_tooltip(trigger = "axis") %>%
   e_y_axis(inverse = T) %>%
   #e_x_axis(alignTicks = T) %>%
   e_x_axis(alignTicks = T, axisLabel = list(interval = 3)) %>%
   e_title("Line and area charts")

  convert_secs <- function(col_vector){
    h <- strsplit(col_vector, ":")
      t <- unlist(lapply(h, function(k){

           t_hour <- as.numeric(k[1]) * 3600
           t_min  <- as.numeric(k[2]) * 60
           t_s    <- as.numeric(k[3])
           return(t_hour + t_min + t_s)
      }))

      return(t)
  }
  
  