# Staypoint estimation

#We are trying determine what is the set of GPS points that participate in a staypoint

## preliminary data cleaning
#1) eliminate duplicates - many duplicate locations at a time stamp. Keep most accurate location at a time stamp
#2) many locations at north pole, markedly wrong.  Eliminate them.
#3) calculate interval and distance between timestamps

## Determine staypoints

#The staypoint determination algorithm uses 4 variables;
# - min_staypoint_time  - minimum time, in minutes, that must stay within max_staypoint_distance
# * max_jump_time - maximum time, in minutes, between readings
# * max_staypoint_distance - maximum distance for readings to be counted as a single staypoint
# * sigma - smoothing parameter.  Smaller is more smooth



options(warn=-1)
library( knitr )
opts_chunk$set(cache=TRUE, autodep=TRUE)

source('lib/functions.R')
source('lib/get_data.R')
source('lib/location_prep.R')
library(tidyverse)
library(lubridate)

library(multidplyr)
library(parallel)
library(ParallelLogger) 

# load in the individual locations information
df_location = get_df_location()


logFileName = 'staypoint_estimation.log'
min_staypoint_time_range=c(5,10, 15)*60
max_jump_time_range=c(2,5)*60
max_staypoint_distance_range= c(5,10,20)
sigma_range=c(.25, .5, 1, 2, 100)
gh_precision_range=7:9
gh_minpoints_range=0:2*6+3

expand.grid(min_staypoint_time_range,max_jump_time_range,max_staypoint_distance_range, sigma_range ) %>% 
  setNames( qc(i_min_staypoint_time, i_max_jump_time, i_max_staypoint_distance, i_sigma )) %>%
  as_tibble() %>% 
  { . } -> grid_search 

expand.grid(min_staypoint_time_range,max_jump_time_range,max_staypoint_distance_range, gh_precision_range, gh_minpoints_range ) %>% 
  setNames( qc(i_min_staypoint_time, i_max_jump_time, i_max_staypoint_distance, i_precision, i_minpoints )) %>%
  as_tibble() %>% 
  { . } -> grid_search 




#do_one_prune(1)



#prune_gps_geohash<- function( df, gh_precision = 7, minpoints=3 )  {
#options(error = traceback)

#get_geohashed_filename( 1,2)

#do_one_geohash( 10,3 )


# generate a range of staypoint estimates, for a range of parameters


#```{r generate_staypoints, eval=TRUE }

if( FALSE) {
  # create pruned location sets
  cluster <- makeCluster(7)
  clusterExport(cluster,  qc(df_location, distanceBetween, findStayPoint, distance2centroid, prune_gps_outliers, eliminate_sigma, get_pruned_filename))
  clusterExport(cluster,  qc( prune_gps_outliers, eliminate_sigma, get_pruned_filename))
  clusterEvalQ(cluster, {
                library(glue)
                library(geosphere)
                library(tidyverse)
                NULL
      })
  dummy <- clusterApply(cluster, sigma_range , do_one_prune)
  stopCluster(cluster)
}

cluster <- makeCluster(4)
clearLoggers() # Clean up the loggers from the previous example
createLogger(name = "PARALLEL",
             threshold = "INFO",
             appenders = list(createFileAppender(layout = layoutParallel,
                                                 fileName = logFileName))) %>%
registerLogger()


# create geohashed location sets
cluster <- makeCluster(8)
clusterExport(cluster,  qc(df_location, prune_gps_geohash, get_geohashed_filename))

clusterEvalQ(cluster, {
               library(glue)
               library(geohash)
               library(tidyverse)
               NULL
             })

grid_search  %>% 
  distinct( i_precision, i_minpoints ) %>%
  mutate( row = row_number()) %>%
  nest( -row ) %>% 
  { . } -> gs

dummy <- clusterApply(cluster, gs$data, do_one_geohash)
stopCluster(cluster)

#

# create pruned location sets
cluster <- makeCluster(5)
clusterExport(cluster,  qc(df_location, distanceBetween, findStayPoint, distance2centroid, prune_gps_outliers, eliminate_sigma, get_pruned_filename))
clusterExport(cluster,  qc( prune_gps_outliers, eliminate_sigma, get_pruned_filename))
clusterEvalQ(cluster, {
               library(glue)
               library(geosphere)
               library(tidyverse)
               NULL
     })
dummy <- clusterApply(cluster, sigma_range , do_one_prune)
stopCluster(cluster)

# process pruned locationsets
clearLoggers() # Clean up the loggers from the previous example
createLogger(name = "PARALLEL",
             threshold = "INFO",
             appenders = list(createFileAppender(layout = layoutParallel,
                                                 fileName = logFileName))) %>%
registerLogger()

cluster <- makeCluster(12)

clusterExport(cluster,  qc(distanceBetween, findStayPoint, distance2centroid, get_pruned_filename))
clusterEvalQ(cluster, {
               library(glue)
               library(geosphere)
               library(tidyverse)
               library(RAppArmor)
               NULL
     })
grid_search  %>% 
  mutate( row = row_number()) %>%
  nest( -row ) %>% 
  { . } -> gs
dummy <- clusterApply(cluster, gs$data , do_one_search)


# single threaded
needs( purrrlyr)
by_row( grid_search, do_one_search)

debug(do_one_search)
undebug(do_one_search)
stopCluster(cluster)




while(TRUE) {
  tryCatch( {
    # process pruned locationsets

    clearLoggers() # Clean up the loggers from the previous example
    createLogger(name = "PARALLEL",
                threshold = "INFO",
                appenders = list(createFileAppender(layout = layoutParallel,
                                                    fileName = logFileName))) %>%
    registerLogger()

    cluster <- parallel::makeForkCluster(12)

    clusterExport(cluster,  qc(distanceBetween, findStayPoint, distance2centroid, get_geohashed_filename))
    clusterEvalQ(cluster, {
                  library(glue)
                  library(geosphere)
                  library(tidyverse)
                  library(RAppArmor)
                  NULL
                })

    grid_search  %>% 
      mutate( row = row_number()) %>%
      nest( -row ) %>% 
      { . } -> gs
    dummy <- parallel::clusterApply(cluster, gs$data , do_one_search_geohash )

  })
}


options(error = recover)
by_row( grid_search %>% tail(1), do_one_search_geohash)


