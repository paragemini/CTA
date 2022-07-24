library(sf)
library(data.table)
library(lehdr)
library(tidycensus)
library(stringr)
library(dplyr)
library(tidyr)
library(htmltools)
library(leaflet)
library(rvest)
library(lubridate)
library(purrr)
library(leafgl)
library(tigris)
library(sfheaders)
library(tigris)
library(mapdeck)



setwd("D:\\PASSION_PROJECTS\\cta\\CTA")
# files_paths <- list.files("C:\\Users\\pgupta\\CTA\\google_transit", full.names = T)
# files_names <- list.files("C:\\Users\\pgupta\\CTA\\google_transit")

files_paths <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit", full.names = T)
files_names <- list.files("D:\\PASSION_PROJECTS\\cta\\CTA\\google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(pattern = "\\.txt$", replacement = "", x = files_names)
cta_data <- files
tod_hours <- read.csv("tod-hours.csv")
tod_hours$PERIOD <- gsub(tod_hours$PERIOD, pattern = "_", replacement = "-")



# assigning all the value to the variables
routes <- cta_data[["routes"]]
trips <- cta_data[["trips"]]
calendar <- cta_data[["calendar"]]
stops <- cta_data[["stops"]]
stop_times <- cta_data[["stop_times"]]
shapes <- cta_data[["shapes"]]


#
#functions for analysis 

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

return_routes_time <- function( master_f){
  
  message("Getting all the trips and stop sequences by day")
  days <- c("monday","tuesday","wednesday","thursday",           
            "friday","saturday","sunday")    
  days_split <- lapply(days, function(x){
    day_one <-   master_f[master_f[[x]] == 1,]
    day_one <- return_tod(day_one = day_one, type_of_day = x)
    day_one$Day <- x
    return(day_one)
  })
  
  message("Got all the trips")
  names(days_split) <- days
  
  return(days_split)
  
  message("Conveting now int to double")
  days_split <- lapply(days_split, function(y){
    setDT(lapply(y, 
                 function(x){  
                   if(is.integer(x)){
                     return(as.double(x))
                   } else {
                     return(x)
                   }
                 }))
  })
  
  message("Converted everything int to double")
  
  message("Calculate stats")
  i <- 0
  expt_2 <- lapply(days_split, function(x){
    i <<- i + 1
    return(
      x[ , .(Min_Stop_Sequence = min(stop_sequence),
             Max_Stop_Sequence = max(stop_sequence),
             Min_Arrival_Time = min(start_time),
             Max_Departure_Time = max(end_time),
             trips =  length(trip_id[stop_sequence == 1]),
             dist_travelled = sum(Distance[stop_sequence == 1]),
             max_dist_travelled = max(Distance[stop_sequence == 1]),
             min_dist_travelled = min(Distance[stop_sequence == 1]),
             median_dist_travelled = median(Distance[stop_sequence == 1]),
             trp_ids = trip_id[which.max(stop_sequence)],
             max_shape_id = shape_id[which.max(stop_sequence)],
             Day = days[i]
      ), 
      by = list(route_id, TOD, direction)][ ,
      `:=`(Min_Arrival_Time = str_pad(Min_Arrival_Time,6,side = "left",pad = "0"),
         Max_Departure_Time = str_pad(Max_Departure_Time,6,side = "left",pad = "0")),] 
    )    
  })
  message("Finished Stats and now Rbinding")
  routes_time <- do.call(rbind.data.frame, expt_2)
  return(routes_time)
}




#getting trips stats from stop_times file

insert_comma <- function(st_vector){
  new_st_vector <-  str_c(str_sub(st_vector,1,2),":", 
                          str_sub(st_vector,3,4), ":",
                           str_sub(st_vector,5,6))
  return(new_st_vector)
}


stop_times[ , `:=`(start_time = gsub(arrival_time, pattern = ":", replacement = ""),
                   end_time = gsub(departure_time, pattern = ":", replacement = "")) , ]

trips_stats <- stop_times[ , .(Distance = shape_dist_traveled[which.max(stop_sequence)] ,
                Total_Stops = stop_sequence[which.max(stop_sequence)],
                Min_Arrival_Time = min(start_time),
                Max_Departure_Time = max(end_time)), 
                by = list(trip_id)][,
                  `:=`(arrival =hms(insert_comma(Min_Arrival_Time)),
                       departure = hms(insert_comma(Max_Departure_Time))),
                ][ ,`:=`(duration= seconds(departure - arrival),
                         start_local_hour = hour(arrival)), ][ , duration:= as.numeric(duration), ]


trips_stats_meta<- setDT(left_join(
  left_join(trips[,c(1,2,3,6,7)],
            trips_stats, 
            by = "trip_id"), calendar[,c(1:10)], 
             by = "service_id"))

trips_stats_meta_f <- as.data.frame(trips_stats_meta[end_date != 20220611,])

trips_day <- return_routes_time(trips_stats_meta_f)
trips_day_df <- do.call(rbind.data.frame, trips_day)

tuesday <- setDT(list(  trips_day_df$trip_id[trips_day_df$Day == "tuesday"]))



tuesday <- left_join(  tuesday,
                       trips_stats_meta_f[,-c(10,11,14:22)], 
                       by = c("V1" = "trip_id"))
colnames(tuesday)[1] <- "trip_id"
tuesday[ , `:=`(start_local_mins = as.numeric(str_sub(Min_Arrival_Time,3,4)),
                start_local_secs = as.numeric(str_sub(Min_Arrival_Time,5,6))), ][ , 
                  `:=`(start_secs = start_local_hour * 60 * 60 +
                                    start_local_mins * 60 + 
                                    start_local_secs ), ][ , 
                      end_secs:= start_secs + duration, ]

rm(list = setdiff(ls(),c("tuesday","trip_shapes")))

gc()

# Shape IDs
tuesday_sf <- left_join(tuesday, 
                        trip_shapes, 
                        by = "shape_id")
tuesday_trips <-  lapply(1:nrow(tuesday_sf), sum)

for( i in 1:length(1:nrow(tuesday_sf))){ #:
  print(i)
   paths <- data.frame(Z = rep(i, nrow(tuesday_sf$geometry[[i]][ , c(1,2)])),
             start_secs = tuesday_sf$start_secs[i],
             ends_secs = tuesday_sf$end_secs[i],
             duration = tuesday_sf$duration[i])
  
  x <-  cbind(tuesday_sf$geometry[[i]][ , c(1,2)],paths)
  x$interval <-  x$duration/nrow(x)
  x$sequence <- 1:nrow(x)
  colnames(x)[1:2] <- c("X","Y")
  x$M <- x$start_secs + x$interval * x$sequence
  x_f <- setDT(x[ , c(1,2,3,9)])
  tuesday_trips[[i]] <- st_linestring(as.matrix(x_f))
}

names(tueday_trips) <- tuesday_sf$V1
  
all_tues_trip <- data.frame(ID = tuesday_sf$V1)        
all_tues_trip$geometry <- tuesday_trips
all_tues_trip <- st_as_sf(all_tues_trip, sf_column_name = "geometry",
                       crs = 4326)
#[route_color == "565a5c" , route_color:= "ffffff" ,]
routes[ , route_color:= str_c("#", route_color), ]
all_tues_trip <- left_join(left_join(all_tues_trip, 
                           tuesday[,c(1,2)],
                           by = c("ID" = "V1")), 
                           routes[,c(1,6)],
                           by = "route_id")

all_tues_trip_one <- split(all_tues_trip,
                           all_tues_trip$route_color)

trips_map <- mapdeck(
  location = c( -87.62963419064182,41.88243038220215) , 
  zoom = 10, 
  style = mapdeck_style("light")
)


# trips_map <- trips_map %>%
#   add_trips(
#     layer_id = str_c("5"),
#     data = all_tues_trip_one[[5]][1:12800,],
#     stroke_colour = "#ffffff",
#     start_time = 0,
#     end_time = 90300,
#     trail_length = 400,
#     animation_speed = 700,
#     stroke_width = 10
#   )
trips_map <- trips_map %>% add_trips(
  layer_id = str_c("5"),
  data = all_tues_trip_one[[5]][1:12500,],
  stroke_colour = "#6E85B7",
  start_time = 0,
  end_time = 90300,
  trail_length = 400,
  animation_speed = 400,
  stroke_width = 100,
  opacity = 0.2
)





for( i in c(11:1)){
  if(i != 5) {
    trips_map <- trips_map %>%
      add_trips(
        layer_id = str_c(i),
        data = all_tues_trip_one[[i]],
        stroke_colour = names(all_tues_trip_one)[i],
        start_time = 0,
        end_time = 90300,
        trail_length = 500,
        animation_speed = 400,
        stroke_width = 100
      )
  }
}
trips_map

 





weekly_stats <- trips_day_df[ ,.(total_trips = .N,
                total_distance = sum(Distance),
                total_stops = sum(Total_Stops),
                total_time = sum(duration)), by = list(Day,TOD)]
weekly_stats_f <-  setDT(lapply(weekly_stats[ , 3:ncol(weekly_stats)], function(x){
  return(sum(x))
}))
weekly_stats_f$distance_miles <- weekly_stats_f$total_distance * 0.000189394
weekly_stats_f$times_hours <- weekly_stats_f$total_time /3600


trips_day_df[ , .(total_trips = n(),
                  distance = sum(Distance),
                  total_stops = sum(total_stops)), 
              by = list(TOD , Day, route_id)]

#Distance, trips, stops, times :  weekly 

#top 5 routes with most trips and least trips
#top5 routes by distance,stops and least by distance

#total trips by day
# total trips by time of day by transit type like buses and subway
# 5 longest trips with stops by time
# 5 shortest trips with stops by time

# 5 longest trips with stops by distance
# 5 shortest trips with stops by time






#getting one master file 

master_file <- left_join(
  left_join(
    left_join(stop_times, 
              trips[ , c(1,2,3,6,7,10)], 
              by = "trip_id"), stops[,c(1,3,5,6)], 
    by = "stop_id"), calendar[,c(1:10)], by = "service_id")






#calculating time range per route

master_f <- master_file[ end_date != 20220611  , , ][ , `:=`(
  start_time = as.numeric(gsub(arrival_time,pattern = ":",replacement = "")),
  end_time =  as.numeric(gsub(departure_time,pattern = ":",replacement = "")),
  start_local_hour = as.numeric(str_sub( arrival_time,1,2))
)]







routes_time <-  return_routes_time(master_f = master_f)

routes_daily_stats <- left_join(routes_daily_stats, tod_hours, by = c( "TOD" =  "PERIOD"))
routes_daily_stats[ , Day_Type := if_else(Day != "saturday" & 
                          Day != "sunday" , "Wkdy","Wknd"), ]
routes_daily_stats <-  routes_daily_stats[ , c(1:3,13,14,16,8,9, 6,7,10,11,12,5,4,15)]
frequency <- routes_daily_stats[, .(total_trips = sum(trips),
                                    Min_Arrival_Time = Min_Arrival_Time,
                                    Max_Departure_Time = Max_Departure_Time) ,
                                by = list( TOD, Day,Day_Type, route_id ) ]



shapes_routes <- setDT(list(unique(routes_daily_stats$max_shape_id)))
routes_daily_stats_shapes <-st_as_sf(left_join(shapes_routes, trip_shapes, 
                                               by = c("V1"= "shape_id")),
                                     crs = 4326, sf_column_name = "geometry")

stops_trips <- setDT(list(unique(stop_times$stop_id[(stop_times$trip_id %in% routes_daily_stats$trp_ids)])))

stops_trips <- left_join(stops_trips,
                         stops[ , c(1,3,5,6)],  
                         by =c("V1" = "stop_id"))


stops
b_17031 <- blocks(state = "17", county = "031", year = 2020)

leaflet() %>% addProviderTiles(providers[[113]]) %>%
  addPolylines(data = routes_daily_stats_shapes, weight = 2, opacity = 0.5,
               color = "yellow") %>%
  addCircleMarkers(data = stops_trips, 
                   lat = stops_trips$stop_lat, lng = stops_trips$stop_lon,
                   radius = 4, weight = 1, color = "white") %>%
  addPolygons(data = st_transform(b_17031, 4326), weight = 1,
              fillOpacity = 0)

# weekly trips, VMT, total stops, time 

#top 5 routes by distance, 















