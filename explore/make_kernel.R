

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


#max_expand_setting=99999999
#df_location_initial = get_df_location()  
df_location_initial = get_df_single_location() 
max_expand_setting=2


drakeplan <- drake::drake_plan
(
  max_expand = max_expand_setting,
#
  # load in the GPS individual locations information
  #df_location = get_df_single_location() ,
  df_location = df_location_initial,

##
  #
  filtered_accuracy = target(
                            prune_gps_accuracy (df_location, accuracy),
                            transform = map( accuracy = !!accuracy_range , .tag_out=filtered_data)
  ) ,
df_extrapolated_locations = target( 
                                   extrapolate_locations( filtered_accuracy )),
  kernel = target( 
                  find_staypoint_kernel( df_extrapolated_locations, k
  #
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


drakeplan %>%
  drake_config( ) %>%
  vis_drake_graph( )

if(Sys.info()['nodename'] == 'lims') {
  library(future.batchtools)
  future::plan(batchtools_slurm, template = "/home/group/wollersheimlab/slurm_batchtools.tmpl")
  make(drakeplan, parallelism="future", jobs= 100, caching='worker', elapsed = Inf, retries = 3)
} else {
  options(clustermq.scheduler = "multicore")
  make(drakeplan, parallelism="clustermq", jobs= parallel::detectCores() ,  memory_strategy = "autoclean"  )
}

make(drakeplan)


