
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
 
}

load_function()
load_library()

drakeplan <- drake_plan( 

                        # load in the individual locations information
                        df_location = get_df_location(),

                        # get survey data
                        df_all = get_df_all(),

                        # get survey timestamps
                        df_all_ts = get_df_all_ts( df_all ),

                        df_filenames =   get_staypoint_filenames(),

                        df_all_staypoints_multi =   get_all_staypoints_multiprocessor ( df_filenames)
)



make(drakeplan)

readd( df_all_staypoints_multi) %>%
distinct( filename)


