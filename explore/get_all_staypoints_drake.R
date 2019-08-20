
load_function = function() {
  source('lib/functions.R')
  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/evaluate_staypoint_estimates_helper.R')
  source('lib/keys.R')
}

load_library = function() {
  library(tidyverse)
  library(lubridate)
  library(drake)
  library(sp)
  library(sf)
  library(osmdata)
  library(geohash)
  library(revgeo)
}



load_function()
load_library()

drakeplan <- drake_plan( 
  # load in the individual locations information
  df_location = get_df_location(),
#
    df_filenames =   get_staypoint_filenames(),
#
  df_all_staypoints_multi =   get_all_staypoints_multiprocessor ( df_filenames),
#
  # TODO
  # add row to multi, get rid of grouping
#
# get target locations
  df_4sq_locations_filtered =  get_df_4sq_locations_filtered(), 
  df_osm_amenity  = get_df_osm_locations_amenity() ,
  df_osm_leisure  = get_df_osm_locations_leisure() ,
  df_target_locations_combined =get_df_target_locations_combined  (df_osm_amenity, df_4sq_locations_filtered),
#
#  a=head( df_all_staypoints_multi, 100),
  df_sp_rounded_location = get_df_sp_round_location( df_all_staypoints_multi ),
  df_sp_joined_geography = get_df_sp_joined_geography( df_sp_rounded_location, df_target_locations_combined),
  # TODO
  df_geocoded_addresses = get_df_revgeo_addresses( df_sp_joined_geography %>% head(250) ),
#
  # get target timestamps
  # get survey data
  df_all = get_df_all(),
  # get survey timestamps
  df_all_ts = get_df_all_ts( df_all ),
  df_survey_nested  = get_df_survey_nested( df_all_ts),
  df_matching_survey = get_matching_survey ( df_all_staypoints_multi,  df_all_ts_nested )
#
)

make(drakeplan)

#options(error = recover) # setting the error option
#options(error = dump.frames) # setting the error option

