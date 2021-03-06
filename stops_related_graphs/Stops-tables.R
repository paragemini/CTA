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

# cmap_grid <- st_read("C:\\Users\\gupta\\Downloads\\books\\CMAPONTO2050ForecastByLAZ", 
#                      layer = "CMAP_ONTO2050_ForecastByLAZ")
# cmap_grid_f <- cmap_grid[ cmap_grid$COUNTY == "031", ]



# 
# leaflet() %>%
#   addTiles() %>%
#   addGlPoints(data = stops_tues_sf , weight = 1, radius = 10,
#               color = "red", fillColor = "red", 
#               popup = paste0( "<b> Stop Name : ", 
#                               stops_tues_sf$stop_name,"</b>")) %>%
#   addPolygons(data = st_transform(cmap_grid_f,4326), weight = 1)
# 

# leaflet() %>%
#   addProviderTiles(providers[[113]]) %>%
#   addCircleMarkers(data = final_stops_25, lat = final_stops_25$stop_lat,
#                    lng = final_stops_25$stop_lon, 
#                    popup  = paste0("<b>Stop Rank : </b>", final_stops_25$Rank,
#                                    "<br><b>Name : </b>", final_stops_25$stop_name,
#                                    "<br><b>Total Trips : </b>", final_stops_25$total_trips,
#                                    "<br><b> Routes : </b>", final_stops_25$route_ids,
#                                    "<br><b> Total Routes : </b>", final_stops_25$total_routes),
#                    radius = (final_stops_25$total_trips/100) * 2, 
#                    weight = 1, fillColor = "yellow", color = "yellow")




# county <-  get_acs(variables = "B02001_001", geography = "county",
#                    state = "17", county = "031",output = "wide", 
#                    geometry = T, year = 2020)

bg <-  get_acs(variables = "B02001_001", geography = "block group",
                   state = "17", county = "031",output = "wide",
                   geometry = T, year = 2020)
# 
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = st_transform(bg, 4326),
#               weight = 1) %>%
#   addPolygons(data = county, weight = 1, color = "red")

# setwd("~/Documents/CTA/CTA")
#  
# files_paths <- list.files("~/Documents/CTA/CTA/google_transit", full.names = T)
# files_names <- list.files("~/Documents/CTA/CTA/google_transit")


setwd("D:\\PASSION_PROJECTS\\cta\\CTA")
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

trips_cal <- setDT(left_join(trips,calendar, by = "service_id"))
trips_tuesday <- trips_cal[ tuesday == 1 & end_date != "20220611", , ]

stops_tuesday <- stop_times[trip_id %in% trips_tuesday$trip_id , , ][, 
                          `:=`(start_local_hour = as.numeric(str_sub(arrival_time,1,2)),
                               arrival = as.numeric(gsub(arrival_time, 
                                      pattern = ":", replacement = "")),
                               departure = as.numeric(gsub(departure_time, 
                                      pattern = ":", replacement = ""))), ] 


stops_tuesday <- return_tod(setDT(left_join(stops_tuesday, 
                    trips_cal[ , c(1,3)], by = "trip_id")), type_of_day = "tuesday")

stops_tuesday <- left_join(stops_tuesday, 
                           stops[,c(1,3,5,6)], 
                           by = "stop_id")
stops_tues_trips <- left_join(stops_tuesday, 
                    tuesday[, c(1:10,14,15)], 
                    by = "trip_id")
daily_stops_stats <- stops_tues_trips[ , .(Ttrips_lastRow = .N,
                      Ttrips_uniq = length(unique(trip_id)),
                      routes = paste0(unique(route_id.x), collapse = ", "),
                      stop_seq = paste0(unique(stop_sequence), collapse = ", "),
                      head_sign = paste0(unique(str_c(route_id.x,"-",stop_headsign,
                                                      "-",stop_sequence)), 
                                         collapse = ", "),
                      distance = sum(shape_dist_traveled)), 
                  by = list(stop_id,TOD,stop_name, stop_lat,stop_lon )]
setorder(daily_stops_stats, -Ttrips_lastRow) 












stops_tues_sf <- st_as_sf(stops_tuesday, 
                          coords = c("stop_lon", "stop_lat"),
                          crs = 4326)






stops_a <- stops_tuesday %>% group_by(stop_id) %>%
              arrange(arrival_time) %>% 
                filter(row_number() == 1) %>%
                  select(arrival_time)
stops_d <- stops_tuesday %>% group_by(stop_id) %>%
              arrange(desc(departure_time)) %>% 
                   filter(row_number() == 1) %>%
                      select(departure_time)

stop_trips <-  stops_tuesday[ , .(total_trips = length(unique(trip_id)),
                                   total_routes = length(unique(route_id)),
                                  route_ids = paste0(unique(route_id),collapse = ",")),
                                   by = list(stop_id)]
stop_trips_tod <- stops_tuesday[ ,  .(total_trips = length(unique(trip_id)))   , 
                                 by = list(TOD,stop_id)] %>%
                                    pivot_wider(names_from = TOD, 
                                            values_from = total_trips)
stop_trips_strat <- setDT(stops_tuesday %>% 
  group_by(stop_id, TOD, route_id) %>%
  summarise(total_trips_count = n()) %>%
  ungroup())


#final table

final_stops <- left_join(left_join(
   left_join(
  left_join(stops_a,stops_d,
          by = "stop_id"), stop_trips_tod,
            by = "stop_id"),stop_trips, 
             by = "stop_id"), 
             stops[, c(1,3,5,6)], 
               by = "stop_id") %>%
   arrange(desc(total_trips))
final_stops$Rank <- 1:nrow(final_stops)


final_stops_25 <- final_stops[1:25,]




return_e_chart <- function(id = "1106"){
  id <- as.numeric(id)
  stop_times_tod <-  stop_trips_strat[ stop_id == id, , ]  %>%
   pivot_wider(names_from = TOD, values_from = total_trips_count)
  stop_times_tod$Total <- rowSums(stop_times_tod[,3:8], na.rm = T)
  print(stop_times_tod)
  bar_chart <- stop_times_tod %>%
    arrange(desc(Total)) %>%
    e_charts(route_id) %>%
    e_bar(`AM-Peak`, stack = "1", emphasis = list(focus = 'series')) %>%
    e_bar(`PM-Peak`, stack = "1", emphasis = list(focus = 'series')) %>%
    e_bar(`Midday`, stack = "1", emphasis = list(focus = 'series')) %>%
    e_bar(`Early-AM`, stack = "1", emphasis = list(focus = 'series')) %>%
    e_bar(`Evening`, stack = "1", emphasis = list(focus = 'series')) %>%
    e_bar(`Late-Night`, stack = "1", emphasis = list(focus = 'series')) %>% 
    e_tooltip(trigger = 'axis', axisPointer = list(type = 'shadow')) %>%
    e_line(Total) %>%
    e_flip_coords() %>%
    e_theme("vintage")
  return(bar_chart)
} 
 
   
return_e_chart("18396")



