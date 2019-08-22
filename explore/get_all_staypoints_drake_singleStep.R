
load_function = function() {
  #source('lib/functions.R')
#  source('lib/get_data.R')
  source('lib/location_prep.R')
  source('lib/gps_functions.R')
  source('lib/evaluate_staypoint_estimates_helper.R')
  source('lib/keys.R')
}

load_library = function() {

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
#  library(multidplyr)
  library(geosphere)
  library(zoo)
  library(glue)
  library(tibbletime)

}



load_function()
load_library()

  df_sp_joined_geography = get_df_sp_joined_geography( readd(df_sp_rounded_location), readd(df_target_locations_combined))

saveRDS(df_sp_joined_geography, 'try.rds')



options(error = recover) # setting the error option
#options(error = dump.frames) # setting the error option

