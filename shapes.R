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
#library(deckgl)
library(rdeck)
library(sfheaders)


#unique shapes 1658

setwd("D:\\PASSION_PROJECTS\\cta")
#setwd("/Users/parag.geminigmail.com/Documents/CTA")

### main statsistics file #############
#tripsByRoute,
#tripsByUniqueStopsRoutes ,
#tripsByTotalStopsRoutes
# View(tripsByRoute)
# View(tripsByUniqueStopsRoutes) 
# View(tripsByTotalStopsRoutes)



######## writting own summary function #####

sum_detail <- function(d_frame){
  message(str_c("Total Columns : ", ncol(d_frame),
                " ; Total Rows : ", nrow(d_frame)))
  
  message(str_c(  "Column Names : ", paste0(colnames(d_frame), collapse = " , ")))
  
  
  i <- 1
  x <- lapply(d_frame, function(x){
    
    if(length(unique(x)) >= 3 ) {
      message( str_c( "Name " , i," : ", colnames(d_frame)[i],
                      " ; Length Unique : ",length(unique(x)),
                      " ; Values : ", paste0(unique(x)[1:3], collapse = ",") ))  
    } else {
      message( str_c( "Name " , i," : ", colnames(d_frame)[i],
                      " ; Length Unique  : ",length(unique(x)),
                      " ; Values : ", paste0(unique(x), collapse = ",")))
    }
    i <<- i + 1
  })
}
scrape_stops <- function(){
  routes_f <- routes[ route_type == 3, ]
  stops_list <- lapply(1:nrow(routes_f), sum)
  
  stops_list <-   map(1:nrow(routes_f),possibly(
    function(x){
      print(  str_c(x , " : ", routes_f$route_long_name[x]))
      l <- html_table(read_html(str_c("https://www.transitchicago.com/assets/1/6/stoplist_",
                                      routes_f$route_short_name[x],".htm")), header = T)
      l[[1]]$route_long_name <- routes_f$route_long_name[x]
      l[[1]]$route_short_name <- routes_f$route_short_name[x]
      stops_list[[x]] <- l[[1]]
    }, 
    otherwise = NA_real_))
  
  names(stops_list) <- routes_f$route_short_name
  flat_stops <-  setDT(do.call(rbind.data.frame, stops_list))
  return(flat_stops)
}



###### reading files #########
#transfers <- read.csv("transfers.csv")
files_paths <- list.files("./google_transit", full.names = T)
files_names <- list.files("./google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(pattern = "\\.txt$", replacement = "", x = files_names)
cta_data <- files

### getting GTFS feed separately 
routes <- cta_data[["routes"]]
trips <- cta_data[["trips"]]
calendar <- cta_data[["calendar"]]


for( i in 1:nrow(calendar)){
  print(i)
  days <-  setDT(list(colnames(calendar[,2:8])[which(calendar[i,2:8] == 1)]))
  nickDays <- data.frame(V1 = colnames(calendar[1,2:8]),
                         V2 = c("M","T","W","R","F","SA","SU"))
  days <- left_join(days,nickDays, by = "V1" )
  calendar$Days[i] <- paste0(days$V2, collapse = ",")
}

stops <- cta_data[["stops"]]
stop_times <- cta_data[["stop_times"]]

shapes <- cta_data[["shapes"]]

#### creating all shapes from shapes.txt by shape_id ############
shape_split <- split(shapes, shapes$shape_id)
shape_geom <- lapply(1:length(shape_split), sum)

for( i in 1:length(shape_split)){
  print(i)
  shape_geom[[i]] <- st_linestring(as.matrix(shape_split[[i]][ , c(3,2)]))
}

trip_shapes <-st_sf(data.frame(shape_id = names(shape_split), 
                               geometry = st_sfc(shape_geom)), sf_column_name = "geometry",
                    crs = 4326)



trip_shapes <- left_join(trip_shapes, shapes[ , .(total_distance_ft = shape_dist_traveled[.N]) ,
                                              by = list(shape_id)], by = "shape_id")
trip_shapes$length_mi <- st_length(trip_shapes)/1609.34


###########################################################################

######################## creating shapes for trips by stop ids #####################

#stop times with stops lat,long
stop_times_updated <- left_join(stop_times,  stops[,c(1,3,5,6)],  by = "stop_id")

trip_stops <-  left_join(left_join(stop_times, trips, by = "trip_id"),
                         calendar[ , c(1,9:11)], by = "service_id")

totalTripsByTotStops <- left_join(
                        trip_stops[, .(total_stops = .N), #total stops by tripID
                        by = list(route_id, shape_id,
                                  trip_id,direction,service_id)][ ,
                        .(totalTrips = .N, trip_id = trip_id[.N]) ,
                        by = list( route_id,total_stops,direction,
                                   service_id,shape_id)],
                        calendar[,c(1,9,10,11 )], by = "service_id")

split_stop_times <- split(stop_times_updated, stop_times_updated$trip_id)
stop_times_geom <- lapply(1:length(split_stop_times), sum)

stops_vector <- NULL
for( i in 1:length(split_stop_times)){
  print(i)
  stops_vector <- c(stops_vector, 
                    paste0( split_stop_times[[i]]$stop_id, 
                    collapse = "," ))
  stop_times_geom[[i]] <- st_linestring(as.matrix(
                      split_stop_times[[i]][ , c(11,10)]))
  }

stop_trip_shapes <-st_sf(data.frame(trip_id = names(split_stop_times), 
                                    stops = stops_vector,
                               geometry = st_sfc(stop_times_geom)), 
                         sf_column_name = "geometry",
                    crs = 4326)


 stop_trip_shapes <-  left_join(left_join(stop_trip_shapes, 
            trips[ , c(1,2,3,4,7,6)], 
            by= "trip_id"),calendar[,c(1,9:11)], by = "service_id")
 
 
 
 
 
 
master_file <- setDT(left_join(totalTripsByTotStops[ ,-c(4,8:10)],
                         st_drop_geometry(stop_trip_shapes[, c(1,2)]), 
                         by = "trip_id"))
 
 #without stops as by : 1658 , with stops as by 1761
master_file <- master_file[ ,.(totalTrips = sum(totalTrips), trip_id = trip_id[.N]) ,
                        by = list( route_id,total_stops,direction,shape_id,stops)]




routesByMaxStops <- left_join(left_join(master_file[ , .SD[which.max(total_stops)] ,
             by = list(route_id, direction)], routes[,c(1,3,4)], by = "route_id"),
             trip_shapes, by = "shape_id")
routesByMaxStops <- st_as_sf(routesByMaxStops, sf_column_name = "geometry", crs = 4326)



 stops_sf <- st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = 4326)

#duplicate stops
# 41.97767
# -87.90422, 41.97767
# -87.90422

leaflet() %>%
  addTiles() %>%
  addGlPolylines(data = routesByMaxStops, 
              weight = 1, popup = paste0("Name : " , routesByMaxStops$route_long_name, 
              "</br> Total Stops : ", routesByMaxStops$total_stops,
              "</br> Direction : ", routesByMaxStops$direction,
              "</br> Total Trips : ", routesByMaxStops$totalTrips,
              "</br> Total Length : ", round(routesByMaxStops$length_mi,1), " miles",
              "</br> Shape ID : ", routesByMaxStops$shape_id)) %>%
  # addGlPoints(data = stops_sf,
  #             popup = paste0(stops_sf$stop_name, " ", stops_sf$stop_id),
  #             fillColor = "black" ) %>%
  addCircleMarkers(data = ,
                   lat = stops[stops$stop_id == "30076",]$stop_lat,
                   lng = stops[stops$stop_id == "30076",]$stop_lon,
                    weight = 4, color = "red")


# leaflet() %>%
#   addTiles() %>%
#   addPolylines(data = routesByMaxStops[ routesByMaxStops$shape_id == "306800001"  , ], 
#                  weight = 3,
#                popup = paste0("Name : " , routesByMaxStops$route_long_name[ routesByMaxStops$shape_id == "306800001"   ], 
#                        "</br> Total Stops : ", routesByMaxStops$total_stops[ routesByMaxStops$shape_id == "306800001"   ],
#                        "</br> Direction : ", routesByMaxStops$direction[ routesByMaxStops$shape_id == "306800001"   ],
#                         "</br> Total Trips : ", routesByMaxStops$totalTrips[ routesByMaxStops$shape_id == "306800001"  ],
#                        "</br> Total Length : ", round(routesByMaxStops$length_mi[ routesByMaxStops$shape_id == "306800001"   ],1), " miles",
#                         "</br> Shape ID : ", routesByMaxStops$shape_id[ routesByMaxStops$shape_id == "306800001"   ]),group = "1") %>%
#   addPolylines(data = routesByMaxStops[ routesByMaxStops$shape_id == "306800002"  , ], 
#               weight = 3, 
#               popup = paste0("Name : " , routesByMaxStops$route_long_name[ routesByMaxStops$shape_id == "306800002"   ], 
#                              "</br> Total Stops : ", routesByMaxStops$total_stops[ routesByMaxStops$shape_id == "306800002"   ],
#                              "</br> Direction : ", routesByMaxStops$direction[ routesByMaxStops$shape_id == "306800002"   ],
#                              "</br> Total Trips : ", routesByMaxStops$totalTrips[ routesByMaxStops$shape_id == "306800002"   ],
#                              "</br> Total Length : ", round(routesByMaxStops$length_mi[ routesByMaxStops$shape_id == "306800002"   ],1), " miles",
#                              "</br> Shape ID : ", routesByMaxStops$shape_id[ routesByMaxStops$shape_id == "306800002"   ]), group = "2") %>%
#   addLayersControl(overlayGroups = c("1","2"))
 
one_sf <- routesByMaxStops[ routesByMaxStops$shape_id == "306800001"  , ] 
plot(st_buffer(one_sf[,1], dist = 10), reset = FALSE)
plot(one_sf,col='blue',add=TRUE)

leaflet() %>%
  addProviderTiles(providers[[110]]) %>%
  addGlPolylines(data = stop_trip_shapes, 
                 weight = 1, popup = stop_trip_shapes$trip_id) %>%
  addGlPoints(data = stops_sf,
              popup = paste0(stops_sf$stop_name, " ", stops_sf$stop_id),
              fillColor = "black" )








