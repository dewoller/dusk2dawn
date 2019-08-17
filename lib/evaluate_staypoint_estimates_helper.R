
library(tidyverse)

row= tribble( ~filename,'data/save_v1_maxspeed_1200_1200_10_20_df.rds')

#********************************************************************************
#analyse_staypoint_base_information_detail
# just get summary information for each actual staypoint, return one line per staypoint, summarised
#********************************************************************************
analyse_staypoint_base_information_detail <- function( row ) {


  readRDS(row$filename)  %>%
    filter( n_staypoint > 0 )  %>%
    group_by( userid, night, n_staypoint) %>%
    dplyr::summarise( latitude = mean( latitude), 
              longitude  = mean( longitude),
              min_latitude = min( latitude), min_longitude  = min( longitude),
              max_latitude = max( latitude), max_longitude  = max( longitude), 
              start=min(timestamp), end=max(timestamp), duration=end-start ) %>% 
    ungroup() 

}
#********************************************************************************
#analyse_staypoint_base_information_summary
# summarise ALL staypoints for this user and night
#********************************************************************************
analyse_staypoint_base_information_summary <- function( row ) {
  print(paste(row, collapse=','))

  analyse_staypoint_base_information_detail ( row ) %>%
    { . } -> a

  a %>% 
    group_by( userid, night) %>%
    summarise( n=n()) %>%
    ungroup() %>%
    summarise( n_staypoint = nrow(a), mean_sp_night=mean(n), max_sp_night=max(n), sd_sp_night=sd(n)) %>% 
    bind_cols( row ) 

}

#********************************************************************************
#analyse_staypoint_set_geography_detail
# see if this staypoint intersects any of the geography staypoints
#********************************************************************************
analyse_staypoint_set_geography_detail <- function( row ) {
  min_overlap_distance = 20 / 1000 # 20 m


  analyse_staypoint_base_information_detail( row ) %>%
    mutate(dist=0) %>% 
    geo_inner_join(df_4sq_locations_filtered, max_dist = min_overlap_distance, distance_col='dist') %>% 
    mutate( dist = round( dist * 1000, 0)) %>%
    group_by( userid, night, n_staypoint ) %>% 
    arrange( dist , type) %>%
    do( head(., 1)) %>%
    ungroup() 

}


#********************************************************************************
#analyse_staypoint_set_geography
# see if this stay
#********************************************************************************
analyse_staypoint_set_geography_summary <- function( row ) {

  cat( row$filename )
  cat("\n")

#= glue( "data/save_{.df$i_min_staypoint_time}_{.df$i_max_jump_time}_{.df$i_max_staypoint_distance}_{.df$i_max_speed_filter}_df.rds")
#  row= tribble( ~filename,'data//save_1200_60_5_10_df.rds')

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned

  df_intersect_geo_cleaned %>%
    count( type, sort=TRUE) %>%
    bind_rows( tribble( ~type, ~n, 'junk', 0)) %>%
    spread(type, n) %>%
    bind_cols( row ) %>%
    bind_cols (enframe( nrow(df_intersect_geo_cleaned), value='nhits')) %>%
    select(  nhits, everything() ) %>%
    select( -name )

}


#********************************************************************************
#analyse_staypoint_set_time_detail 
# match the staypoints to the survey times
#********************************************************************************
analyse_staypoint_set_time_detail <- function( row ) {

  analyse_staypoint_base_information_detail( row ) %>%
    group_by( userid, night ) %>%
    nest( .key='staypoints') %>%
    ungroup() %>%
    inner_join( df_all_ts_nested, by=c('userid', 'night')) %>% 
    group_by( userid, night) %>%
    do( joined = interval_inner_join( data.frame(.$surveys), 
                                     data.frame(.$staypoints), 
                                     by=c('start','end'),
                                     maxgap=300))  %>%
    ungroup() %>%
    unnest() %>% 
    group_by( userid, night, n_staypoint ) %>% 
    arrange(  duration, which) %>%  # take the smallest duration staypoint
    do( head(., 1)) %>%
    ungroup() 

}

#********************************************************************************
#analyse_staypoint_set_time_summary 
#********************************************************************************
analyse_staypoint_set_time_summary <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
  { . } -> df_intersect_time_cleaned

  df_intersect_time_cleaned %>% 
    count( which, sort=TRUE) %>%
    spread(which, n) %>%
    bind_cols(row) %>%
    bind_cols (enframe( nrow(df_intersect_time_cleaned), value='nsurvey_hits')) %>%
    select( -name )

}

row= tribble( ~filename,'data//save_v3_geohash_300_120_5_precision_7_minpoints_3_df.rds')
#********************************************************************************
# analyse_staypoint_set_time_and_geography_summary 
#********************************************************************************
analyse_staypoint_set_time_and_geography_summary <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
    { . } -> df_intersect_time_cleaned

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned


  df_intersect_time_cleaned %>%
    full_join( df_intersect_geo_cleaned, by = qc( userid, night, n_staypoint )) %>% 
    { . } -> df_intersect_both

  df_intersect_both %>%
    count( is.na( start.x), is.na( latitude.x)) %>% 
    { . } -> a

  if (nrow(a)==3) {
    a %>%
      mutate( hit_count = qc( both_hits, survey_only_hits, geo_only_hits)) %>%
      select( n, hit_count ) %>%
      spread( hit_count, n ) %>%
      bind_cols( row )
  } else {
    row %>%
      bind_cols( tribble( ~both_hits, ~survey_only_hits, ~geo_only_hits, NA ,NA ,NA )) 
  }

}


#********************************************************************************
# analyse_staypoint_set_time_and_geography_detail 
#********************************************************************************
analyse_staypoint_set_time_and_geography_detail <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
    { . } -> df_intersect_time_cleaned

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned

  df_intersect_time_cleaned %>%
    full_join( df_intersect_geo_cleaned, by = qc( userid, night, n_staypoint )) %>% 
    { . } -> df_intersect_both

}

#********************************************************************************
# get_staypoint_filenames
#********************************************************************************
get_staypoint_filenames = function() {

list.files( path='data/', pattern='save_v[123].*rds', full.names=TRUE ) %>%
  enframe(value = 'filename' ) %>%
  select(-name) %>%
  #  head(1) %>%
  separate( col=filename, 
           into=c(NA, NA, qc(type, min_staypoint_time, max_jump_time, max_staypoint_distance, rest)), 
           sep='_', 
           convert=TRUE, 
           extra='merge',
           remove=FALSE)  %>%
  mutate( rest = str_replace( rest, '_df.rds', '') ) 

}
 

#********************************************************************************
#consolidate_staypoints
#********************************************************************************
consolidate_staypoints <- function( row ) {

  readRDS(row$filename)  %>%
    filter( n_staypoint > 0 ) %>%
    group_by( userid, night, n_staypoint) %>%
    summarise( longitude = mean(longitude), latitude = mean(latitude)) %>%
    ungroup %>%
    crossing(row)

}
#


#********************************************************************************
#get_all_staypoints
#********************************************************************************
get_all_staypoints = function( filenames)  {
  filenames %>%
    rowwise() %>%
    do( consolidate_staypoints(.) ) 

}


#********************************************************************************
#get_all_staypoints
#********************************************************************************
get_all_staypoints_multiprocessor = function( filenames)  {
  library(multidplyr)

  create_cluster(11) %>%
	cluster_assign_value('consolidate_staypoints', consolidate_staypoints) %>%
	cluster_library( "tidyverse" ) %>%
	{.} -> cluster

  filenames %>%
    rowwise() %>%
    partition(filename, cluster=cluster) %>%

    do( consolidate_staypoints(.) )  %>%
    collect()


}


