
library(tidyverse)
row= tribble( ~filename,'data/save_1200_1200_10_20_.rds')

#********************************************************************************
#analyse_staypoint_base_information
#********************************************************************************
analyse_staypoint_base_information_detail <- function( row ) {

  readRDS(row$filename)  %>%
    filter( n_staypoint > 0 ) 

}
#********************************************************************************
#analyse_staypoint_base_information
#********************************************************************************
analyse_staypoint_base_information_summary <- function( row ) {

  analyse_staypoint_base_information_detail ( row ) %>%
    group_by( userid, night, n_staypoint) %>%
    summarise( latitude = mean( latitude), longitude  = mean( longitude)) %>% 
    ungroup() %>%
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
#********************************************************************************
analyse_staypoint_set_geography_detail <- function( row ) {
  min_overlap_distance = 20 / 1000 # 20 m


  analyse_staypoint_base_information_detail( row ) %>%
    group_by( userid, night, n_staypoint) %>%
    summarise( latitude = mean( latitude), longitude  = mean( longitude)) %>% 
    ungroup() %>%
    geo_inner_join(df_4sq_locations_filtered, max_dist = min_overlap_distance, distance_col='dist') %>% 
    mutate( dist = round( dist * 1000, 0)) %>%
    group_by( userid, night, n_staypoint ) %>% 
    arrange( dist , type) %>%
    do( head(., 1)) %>%
    ungroup() 

}


#********************************************************************************
#analyse_staypoint_set_geography 
#********************************************************************************
analyse_staypoint_set_geography_summary <- function( row ) {
  min_overlap_distance = 20 / 1000 # 20 m


  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned

  df_intersect_geo_cleaned %>%
    count( type, sort=TRUE) %>%
    spread(type, n) %>%
    bind_cols( row ) %>%
    bind_cols (enframe( nrow(df_intersect_geo_cleaned), value='nhits')) %>%
    select(  nhits, everything() ) %>%
    select( -name )

}


#********************************************************************************
#analyse_staypoint_set_time_detail 
#********************************************************************************
analyse_staypoint_set_time_detail <- function( row ) {


  analyse_staypoint_base_information_detail( row ) %>%
    filter( n_staypoint > 0 ) %>%
    group_by( userid, night, n_staypoint) %>%
    summarise( start=min(time_stamp), end=max(time_stamp), duration=end-start ) %>% 
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
      mutate( hit_count = qc( both_hits, survey_only_hits, geo_only_hits)) %>%
      select( n, hit_count ) %>%
      spread( hit_count, n ) %>%
      bind_cols( row )
}



