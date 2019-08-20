
load_function = function() {
  source('lib/functions.R')
  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/evaluate_staypoint_estimates_helper.R')
}

load_library = function() {
  library(tidyverse)
  library(lubridate)
  library(drake)
  library(sp)
  library(sf)
  library(osmdata)
  library(geohash)

}



load_function()
load_library()

drakeplan <- drake_plan( 
  # load in the individual locations information
  df_location = get_df_location(),
#
  # get survey data
  df_all = get_df_all(),
#
  # get survey timestamps
  df_all_ts = get_df_all_ts( df_all ),
#
  df_filenames =   get_staypoint_filenames(),
#
  df_all_staypoints_multi =   get_all_staypoints_multiprocessor ( df_filenames),
#
  df_4sq_locations_filtered =  get_df_4sq_locations_filtered(), 
#
  df_osm_amenity  = get_df_osm_locations_amenity() ,
  df_osm_leisure  = get_df_osm_locations_leisure() ,
  df_target_locations_combined =get_df_target_locations_combined  (df_osm_amenity, df_4sq_locations_filtered)
#
)



make(drakeplan)

readd( df_all_staypoints_multi) %>%
    head(100) %>% 
    { . } -> a


  readd( df_target_locations_combined ) %>%
    analyse_staypoint_full_4sq()
  

