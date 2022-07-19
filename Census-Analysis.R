library(tidycensus)
library(sf)
library(stringr)
library(data.table)
library(dplyr)
library(lehdr)

#Functions to calculate census-data

#API calls

# it returns clean column names automatically
get_data <- function(table_id, geo, st = "00", co = "000", 
                     geometry_logical = 1, y = 2020){
  if(geo == "us"){
    data_got <- get_acs(geography= "us", table = table_id, 
                        year = y,geometry = geometry_logical, output = "wide")
  } else if (geo == "state"){
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = y, state = st, output = "wide",
                        geometry = geometry_logical)
  } else {
    #county,tract, block group
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = y, state = st, county = co,
                        output = "wide",geometry = geometry_logical)
  }
  message("Got Data")
  print(class(data_got))
  if(table_id == "B01001"){
    data_got <- calc_percentage(analyse_age(clean_col_names(data_got)), cols = 52:57 , total = 3)
  } else {
    data_got <- clean_col_names(data_got)
  }
  
  return(data_got)
}

#Cleaning column names
clean_col_names <- function(sf_frame, y = 2019){
  
  acs_2019 <- load_variables(year = y, dataset = "acs5")
  sf_frame_f <- sf_frame[,!grepl(colnames(sf_frame), pattern = "M$")]
  
  logi_vector <- acs_2019$name %in% gsub(colnames(sf_frame_f)[3:ncol(sf_frame_f)], 
                                         pattern = "E$", replacement = "") # getting into final format of B01001_001 from B01001_001E
  
  col_names_dirty <- acs_2019$label[logi_vector]
  
  
  clean_names <- mgsub::mgsub(col_names_dirty, 
                              pattern = c("Estimate!!Total:!!"," ", ":!!", "Estimate!!",":"), 
                              replacement = c('', '_','_','',''))
  print(clean_names)
  
  colnames(sf_frame_f)[3:ncol(sf_frame_f)] <- c(clean_names, "geometry")
  message("renaming")
  return(sf_frame_f[!st_is_empty(sf_frame_f),])
}

###Analysing Data

#general calculations
calc_percentage <- function(data_frame, cols , total) {
  y <- as.data.frame(st_drop_geometry(data_frame))
  percentage_matrix <- apply(y[ , cols] , 2, function(x) { 
    round( x  * 100/ y [ , total] ,2)  })
  colnames(percentage_matrix) <- str_c("P_" , colnames(percentage_matrix))
  final_data <- cbind(data_frame, percentage_matrix)
  return(final_data)
}
calc_stat <- function(census_data, col_name) {
  census_data <- st_drop_geometry(census_data)
  min_row <- which(census_data[, col_name] == min(census_data[, col_name]))
  median_row <- which(census_data[, col_name] == median(census_data[, col_name]))
  max_row <- which(census_data[, col_name] == max(census_data[, col_name]))
  
  final_list <- data.frame( ID = c(census_data[ min_row, "GEOID"], census_data[ median_row, "GEOID"], census_data[ max_row, "GEOID"]),
                            Values = c(census_data[ min_row, col_name], census_data[ median_row, col_name],census_data[ max_row, col_name]))
  
  # return(census_data[ c(min_row, median_row, max_row), c("GEOID", "Estimate_Total:") ])
  
}

#specific calculation
analyse_age <- function(sf_frame){
  df <- setDT(st_drop_geometry(sf_frame))
  y <- df[ ,`:=`(GEOID = GEOID,Total = Total, Male = Male, Female = Female, 
                 Under_5 = Male_Under_5_years + Female_Under_5_years,
                 bw_5_17 = Male_5_to_9_years + Male_10_to_14_years+ Male_15_to_17_years+
                   Female_5_to_9_years + Female_10_to_14_years+Female_15_to_17_years,
                 
                 bw_18_24 = Female_18_and_19_years + Female_20_years +Female_21_years +Female_22_to_24_years+
                   Male_18_and_19_years +Male_20_years+Male_21_years  + Male_22_to_24_years , 
                 
                 bw_25_34 =Male_25_to_29_years+Male_30_to_34_years +
                   Female_25_to_29_years+Female_30_to_34_years,
                 
                 bw_35_64 = Male_35_to_39_years+Male_40_to_44_years+ Male_45_to_49_years+Male_50_to_54_years+
                   Male_55_to_59_years+Male_60_and_61_years+Male_62_to_64_years +
                   Female_35_to_39_years+Female_40_to_44_years+ Female_45_to_49_years+Female_50_to_54_years+
                   Female_55_to_59_years+Female_60_and_61_years+Female_62_to_64_years,
                 
                 bw_65_P = Female_65_and_66_years+Female_67_to_69_years+Female_70_to_74_years+
                   Female_75_to_79_years+Female_80_to_84_years+Female_85_years_and_over + Male_65_and_66_years+
                   Male_67_to_69_years+Male_70_to_74_years+Male_75_to_79_years+Male_80_to_84_years+Male_85_years_and_over
  ) ,]
  y_geom <- st_sf(left_join(y, sf_frame[,1], by = "GEOID"))
  
  return(y_geom)
}
analyse_minority <- function(sf_frame){
  df <- setDT(st_drop_geometry(sf_frame))
  df[ , Minority := (Black_or_African_American_alone +
        American_Indian_and_Alaska_Native_alone + 
          Asian_alone + Native_Hawaiian_and_Other_Pacific_Islander_alone +
          Some_other_race_alone + Two_or_more_races)
      , ]
  y_geom <- st_sf(left_join(df, sf_frame[,1], by = "GEOID"))
  return(y_geom)
}

age <- get_data( table_id = "B01001", st = "17", co = "031",
                                 geo = "block group")
race <- analyse_minority(get_data( table_id = "B02001", st = "17", co = "031",
                  geo = "block group"))


jobs <- setDT(grab_lodes(state = 'il', year = 2019, lodes_type = "wac" ))
chicago_jobs <- jobs[,county := str_sub(w_geocode,1,5), ][ county == "17031", c(1,2,6,7,30:34) ]
colnames(chicago_jobs) <- c( "GEOID", "tJobs", "L1250","Bw1250-3333", "BORAM",
                            "AIORANA","AA","NHOROPIA","TORMRG")

#median income, vehicles per capita, zero-vehicle households, rental units












