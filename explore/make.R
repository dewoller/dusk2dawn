
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
  library(sf)
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


logFileName = 'staypoint_estimation.log'
sp_min_staypoint_time_range=c(5,10, 15)*60
sp_max_jump_time_range=c(2,5,10)*60
sp_max_staypoint_distance_range= c(10,20,40)
sigma_range=c(.5, 1, 100)
gh_precision_range=7:9
gh_minpoints_range=0:2*6+3

accuracy_range = c(100,50,30,20,10)

drakeplan <- drake::drake_plan(
  trace=TRUE,
#
  # load in the GPS individual locations information
  #df_location = get_df_single_location() ,
  df_location = get_df_location() ,
  # get target locations, osm and 4sq
  df_4sq_locations_filtered =  get_df_4sq_locations_filtered(), 
  df_osm_amenity  = get_df_osm_locations_amenity() ,
  df_osm_leisure  = get_df_osm_locations_leisure() ,
  df_target_locations_combined =get_df_target_locations_combined  (df_osm_amenity, df_4sq_locations_filtered),
#
  # get surveys
  df_all = get_df_all(),
  # get survey timestamps
  df_all_ts = get_df_all_ts( df_all ),
  # nest surveys
  df_survey_nested  = get_df_survey_nested( df_all_ts),
 ##
  #
  filtered_accuracy = target(
                            prune_gps_accuracy (df_location, accuracy),
                            transform = map( accuracy = !!accuracy_range )
  )
  ,
#
  filtered_geohash = target(
                          prune_gps_geohash (df_location, precision, minpoints),
                          transform = cross( precision  = !!gh_precision_range,
                                              minpoints = !!gh_minpoints_range)
  )
  ,
   filtered_sigma = target(
                    prune_gps_outliers (df_location, .sigma = sigma),
                    transform = map( sigma = !!sigma_range)
  )
  ,
  staypoints_accuracy = target(
                            find_staypoint_distance( filtered_accuracy,  max_jump_time, min_staypoint_time, max_staypoint_distance ),
                            transform=cross( filtered_accuracy, 
                                            max_jump_time = !!sp_max_jump_time_range, 
                                            min_staypoint_time = !!sp_min_staypoint_time_range,
                                            max_staypoint_distance  = !!sp_max_staypoint_distance_range  )
  )
  ,
  staypoints_geohash = target(
                            find_staypoint_distance( filtered_geohash,  max_jump_time, min_staypoint_time, max_staypoint_distance ),
                            transform=cross( filtered_geohash, 
                                            max_jump_time = !!sp_max_jump_time_range, 
                                            min_staypoint_time = !!sp_min_staypoint_time_range,
                                            max_staypoint_distance  = !!sp_max_staypoint_distance_range  )
  )
  ,
  staypoints_sigma = target(
    find_staypoint_distance( filtered_sigma,  max_jump_time, min_staypoint_time, max_staypoint_distance ),
    transform=cross( filtered_sigma, 
                    max_jump_time = !!sp_max_jump_time_range, 
                    min_staypoint_time = !!sp_min_staypoint_time_range,
                    max_staypoint_distance  = !!sp_max_staypoint_distance_range  )
  )
#
#
  # get target timestamps
  # get survey data
# df_matching_survey = get_matching_survey ( df_all_staypoints_multi,  df_survey_nested ),
  #
#  a=head( df_all_staypoints_multi, 100),
 # df_sp_joined_geography = get_df_sp_joined_geography( df_all_staypoints_multi , df_target_locations_combined),
  # TODO
#
 # df_sp_no_bar =  get_df_sp_no_bar(df_all_staypoints_multi , df_sp_joined_geography  ) ,
#
 # df_geocoded_addresses = get_df_revgeo_addresses( df_sp_no_bar %>% head(250) ), 
 # df_all_staypoints_matched = df_summarise_staypoint_algorithms( df_all_staypoints_multi, df_matching_survey,df_sp_joined_geography),
 # wflow_publish(knitr_in("analysis/evaluate_staypoint_estimates.Rmd"), view = FALSE)
)

load_library()
load_function()

drakeplan %>%
  drake_config( ) %>%
  vis_drake_graph( )

options(clustermq.scheduler = "multicore")
make(drakeplan, parallelism="clustermq", jobs= parallel::detectCores())


#options(error = recover) # setting the error option
#options(error = dump.frames) # setting the error option
