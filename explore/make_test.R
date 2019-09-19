

load_function = function() {
  #source('lib/functions.R')
  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/evaluate_staypoint_estimates_helper.R')
  source('lib/keys.R')
  source('lib/kernel_density_functions.R')
}

load_library = function() {
#
  library(raster)
 library(RPostgreSQL)
#  library(sf)
  library(tidyverse)
  library(lubridate)
  library(drake)
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

load_library()
load_function()


logFileName = 'staypoint_estimation.log'
sp_min_staypoint_time_range=c(5)*60
sp_max_jump_time_range=c(2)*60
sp_max_staypoint_distance_range= c(10)
sigma_range=c(.5, 1, 2, 3, 100)
gh_precision_range=7:9
gh_minpoints_range=0:2*6+3
accuracy_range = c(100)
interpolation_delay_range = c(120)

    max_expand_setting=9999999
#    df_location_initial = get_df_single_location() 
#    max_expand_setting=2

df_location_initial = get_df_location()  

drakeplan <- drake::drake_plan(
  max_expand = max_expand_setting,
  df_location = df_location_initial,
##
  #
  filtered_accuracy = target(
                            prune_gps_accuracy (df_location, accuracy),
                            transform = map( accuracy = !!accuracy_range , .tag_out=filtered_data)
  ) ,
  interpolated_locations = target(
                                   interpolate_locations (filtered_accuracy, max_delay=max_delay, period=30),
                                  transform = map( filtered_accuracy, max_delay = !!interpolation_delay_range, .tag_out=filtered_data)

  ),
  staypoints_distance= target(
                            find_staypoint_distance( filtered_data,  max_jump_time, min_staypoint_time, max_staypoint_distance ),
                            transform=cross( filtered_data, 
                                            max_jump_time = !!sp_max_jump_time_range, 
                                            min_staypoint_time = !!sp_min_staypoint_time_range,
                                            max_staypoint_distance  = !!sp_max_staypoint_distance_range  )
  )
  ,
  #
  #####################################
  # Evaluation data prep
  #####################################
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
  #  # nest surveys
  df_survey_nested  = get_df_survey_nested( df_all_ts),

  #####################################
  # Evaluate 
  #####################################
  # get target timestamps
  # get survey data
  df_matching_survey = target( 
                              get_matching_survey ( staypoints_distance,  df_survey_nested ),
                              transform = map( staypoints_distance )),
                               #
, trace=TRUE
)




if(startsWith(Sys.info()['nodename'], 'lims')) {
  library(future.batchtools)
  future::plan(batchtools_slurm, template = "/home/group/wollersheimlab/slurm_batchtools.tmpl")
  make(drakeplan, parallelism="future", jobs= 100, caching='worker', elapsed = Inf, retries = 3)
} else {

  drakeplan %>%
    drake_config( ) %>%
    vis_drake_graph( )

#  drake_plan_source(drakeplan)

}



