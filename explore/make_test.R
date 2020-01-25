

load_function = function() {
  #source('lib/functions.R')
  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/functions.R')
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

drakeplan <-
  drake_plan (
  df_loc_test = get_df_single_location() ,
  ##
  #
  filtered_accuracy_test = target(
                              prune_gps_accuracy (df_location, accuracy),
                              transform = map( accuracy = !!accuracy_range , .tag_out=filtered_data)
                                ),

  staypoints_distance_test =
    target(
           do_something_with_file_name(filtered_data, print_2),
           transform = map(filtered_data )
           ),
              trace = TRUE
)
