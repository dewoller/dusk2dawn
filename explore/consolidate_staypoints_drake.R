

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

df_all_staypoints =   get_all_staypoints( df_filenames)
)

config <- drake_config(drakeplan)
vis_drake_graph(config)

readd(df_all_stayppoints)


make(drakeplan, jobs=4)

predict_runtime

library(clustermq)
fx = function(x) x * 2

# queue the function call on your scheduler
Q(fx, x=1:3, n_jobs=1)
# list(2,4,6)
