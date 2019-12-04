
if (startsWith(Sys.info()['nodename'], 'lims')) { 
  currentMachine = "lims"
  max_jobs=32
} else (
  currentMachine = Sys.info()['nodename']
)



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
sp_min_staypoint_time_range=c(5,10, 15)*60
sp_max_jump_time_range=c(30,240)*60
sp_max_staypoint_distance_range= c(20,100)
sigma_range=c(.5, 1, 2, 3, 100)
#gh_precision_range=7:9
#gh_minpoints_range=0:2*6+3
accuracy_range = c(100,10)
interpolation_delay_range = c(120, 300, 600)
#iterations_range=c(10,100,1000)
#hm_range =  c( 1, 10, 100)
#ht_range = c(6, 60, 600 )

if (currentMachine == "dewlap") {
  max_expand_setting=1
  df_location_initial = get_df_single_location() 

} else {
  max_expand_setting=99999999
  df_location_initial = get_df_location()  
}

drakeplan <- drake::drake_plan(
  max_expand = max_expand_setting,
#
  # load in the GPS individual locations information
  df_location = df_location_initial,
##
  #
  filtered_accuracy = target(
                            prune_gps_accuracy (df_location, accuracy),
                            transform = map( accuracy = !!accuracy_range , .tag_out=filtered_data)
  ) ,
#  filtered_geohash = target(
#                          prune_gps_geohash (df_location, precision, minpoints),
#                          transform = cross( precision  = !!gh_precision_range,
#                                              minpoints = !!gh_minpoints_range,
#                                              .tag_out=filtered_data)
#  ) ,
  filtered_sigma = target(
                          prune_gps_outliers (df_location, .sigma = sigma),
                          transform = map( sigma = !!sigma_range, .tag_out=filtered_data)
  ),
  filtered_sigma.v2 = target(
                    prune_gps_outliers.v2 (df_location, .sigma = sigma),
                    transform = map( sigma = !!sigma_range, .tag_out=filtered_data)
  ) ,
  interpolated_locations = target(
                                   interpolate_locations (filtered_accuracy, max_delay=max_delay, period=30),
                                  transform = cross( filtered_accuracy, max_delay = !!interpolation_delay_range, .tag_out=filtered_data)

  ),
  staypoints_distance= target(
                              find_staypoint_distance( filtered_data,  max_jump_time, min_staypoint_time, max_staypoint_distance ),
                              transform=cross( filtered_data, 
                                              max_jump_time = !!sp_max_jump_time_range, 
                                              min_staypoint_time = !!sp_min_staypoint_time_range,
                                              max_staypoint_distance  = !!sp_max_staypoint_distance_range  )
  )
  ,
#  meanshift_mode = target(
#                            find_meanshift_mode ( filtered_data,  min_staypoint_time, max_staypoint_distance, iterations, hm, ht ),
#                            transform=cross( interpolated_locations, 
#                                            min_staypoint_time = !!sp_min_staypoint_time_range,
#                                            max_staypoint_distance  = !!sp_max_staypoint_distance_range ,
#                                            iterations = !!iterations_range ,
#                                            hm = !!hm_range ,
#                                            ht = !!ht_range )
#  )
#  ,
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

  # get valid surveys;  eliminate pre, tom and load categories, and surveys without gps points
 df_all_ts_valid = get_df_all_ts_valid( df_all_ts, df_location ),

#  # nest surveys
  df_survey_nested  = get_df_survey_nested( df_all_ts_valid ),
 
#####################################
# Evaluate 
#####################################

 # summarise staypoints for every userid, night.  One line per staypoint / algorithm
 df_summarise_staypoints = target( summarise_staypoints ( staypoints_distance),
                                  transform = map( staypoints_distance)),

 # summarise staypoints for every userid, night.  One line per staypoint / algorithm
 df_all_summarise_staypoints = target( my_combine(df_summarise_staypoints   ) , 
                                      transform = combine( df_summarise_staypoints )),


 # count staypoints for every userid, night.  One dataset per algorithm
 df_count_staypoints = target( 
                              count_staypoints ( staypoints_distance),
                              transform = map( staypoints_distance)),

 # total staypoints for each algorithm
 df_count_staypoints_per_algorithm  = target( 
                            count_staypoints_per_algorithm ( df_count_staypoints ),
                             transform = map( df_count_staypoints )),

  df_all_count_staypoints_per_algorithm = target( 
                                my_combine( df_count_staypoints_per_algorithm) , 
                                transform = combine( df_count_staypoints_per_algorithm )),


  # match survey data with staypoints
  df_matching_survey = target( 
                              get_matching_survey ( staypoints_distance,  df_survey_nested ),
                              transform = map( staypoints_distance )),
#

# df_matching_survey_mode = target( 
#                             get_matching_survey ( meanshift_mode,  df_survey_nested ),
#                             transform = map( meanshift_mode ), .tag_out = matching_survey),
  #
#  a=head( df_all_staypoints_multi, 100),
  
# df_matching_geography = target( 
#                             calculate_sp_match_geography( staypoints_distance, df_target_locations_combined),
#                             transform = map( staypoints_distance )
  #),
#
#

# consolidate surveys per staypoint
df_matching_survey_per_staypoint = target( 
                                       get_matching_survey_per_staypoint( df_matching_survey),
                                       transform = map( df_matching_survey )),

# 
df_matching_survey_summarised = target( 
                                   summarise_matching_surveys( df_matching_survey_per_staypoint),
                                      transform = map( df_matching_survey_per_staypoint)),
#
#df_matching_survey_summarised_mode = target( 
#                                       summarise_matching_surveys( df_matching_survey_mode ),
#                                       transform = cross( df_matching_survey_mode )),
#
#  df_matching_geography_summarised = target( 
#                              summarise_matching_geography( df_matching_geography),
#                              transform = map( df_matching_geography)),
#
# dq_geocoded_addresses = get_df_revgeo_addresses( df_sp_no_bar %>% head(250) ), 
#

# Each survey, along with all the staypoint points that it matches
# unused
df_all_matching_survey = target( 
                                my_combine( df_matching_survey) , 
                                transform = combine(df_matching_survey)),

# Each survey, along with the single staypoint points that it matches
# unused
df_all_matching_survey_per_staypoint = target( 
                              my_combine( df_matching_survey_per_staypoint) , 
                              transform = combine(df_matching_survey_per_staypoint)),

# number of surveys for each staypoint, summarised
df_all_sp_match_survey = target( 
                                my_combine( df_matching_survey_summarised) , 
                                transform = combine(df_matching_survey_summarised )),

# combine the number of found surveys and the number of found staypoints
df_all_sp_match_survey_combined = target( df_all_sp_match_survey   %>%
                                         mutate( source = str_replace( source, '.*_staypoints_', 'staypoints_' )) %>%
                                         inner_join( 
                                                    df_all_count_staypoints_per_algorithm  %>%
                                                      mutate( source = str_replace( source, '.*_staypoints_', 'staypoints_' )), by='source'
                                                    )),

# df_all_sp_match_survey_mode = target( 
#                      my_combine( df_matching_survey_summarised_mode) , 
#                      #gdata::combine( df_matching_survey_summarised) %>% rename( original_target=source), 
#                      transform = combine(df_matching_survey_summarised_mode)),


  #
  #wflow_publish(knitr_in("analysis/evaluate_staypoint_estimates.Rmd"), view = FALSE),
trace=TRUE
)



my_combine <- function(...) {
  arg_symbols <- match.call(expand.dots = FALSE)$...
  arg_names <- as.character(arg_symbols)
  #browser()
  out <- NULL
  for (arg_name in arg_names) {
    print( arg_name )
    dataset <- readd(arg_name, character_only = TRUE) %>% mutate( source=arg_name )
    out <- bind_rows(out, dataset)
    #    gc() # Run garbage collection.
  }
  out
}


if (currentMachine == "lims") {

  library(future.batchtools)
  future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl")
  make(drakeplan, parallelism="future", jobs= max_jobs, caching='worker', elapsed = Inf, retries = 1)

} else if (currentMachine == "dewlap") {

  drakeplan %>%
    drake_config( ) %>%
    vis_drake_graph( )
  #  drake_plan_source(drakeplan)

} else if( currentMachine == "hermoine") {
  library(sf)

  options(clustermq.scheduler = "multicore")
  make(drakeplan, parallelism="clustermq", jobs= parallel::detectCores() ,  memory_strategy = "autoclean"  )

}

#make(drakeplan)


