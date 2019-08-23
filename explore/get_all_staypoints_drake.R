
load_function = function() {
  #source('lib/functions.R')
  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/evaluate_staypoint_estimates_helper.R')
  source('lib/keys.R')
}

load_library = function() {
#
  library(tidyverse)
  library(lubridate)
  library(drake)
#  library(sp)
#  library(sf)
  library(osmdata)
  library(revgeo)
  library(tsibble)
  library(magrittr)
  library(stringr)
  library(knitr)
  library(wrapr )   # for the qc function
  library( fuzzyjoin)
  library(IRanges)
  library(multidplyr)
  library(geohash)
  library(geosphere)
  library(zoo)
  library(glue)
  library(tibbletime)
}




drakeplan <- drake::drake_plan( 
  max_expand=3, 
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
  #
  # get target timestamps
  # get survey data
  df_all = get_df_all(),
  # get survey timestamps
  df_all_ts = get_df_all_ts( df_all ),
  df_survey_nested  = get_df_survey_nested( df_all_ts),
  df_matching_survey = get_matching_survey ( df_all_staypoints_multi,  df_survey_nested ),
  #
#  a=head( df_all_staypoints_multi, 100),
  df_sp_joined_geography = get_df_sp_joined_geography( df_all_staypoints_multi , df_target_locations_combined)
  # TODO
#  df_geocoded_addresses = get_df_revgeo_addresses( df_sp_joined_geography %>% head(250) )
)

load_library()
load_function()
make(drakeplan)

drake_plan(max_expand = SMALL_NUMBER)
#options(error = recover) # setting the error option
#options(error = dump.frames) # setting the error option

