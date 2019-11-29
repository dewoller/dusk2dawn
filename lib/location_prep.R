
get_df_best_location <- function( df_location ) {

  df_location %>%
  arrange( userid, night, timestamp ) %>%
  #
  # clean gps noise, take the most accurate point for a timestamp
  filter( longitude > 0 &   longitude <10 & latitude > 40) %>%
  group_by( userid, night, local_time ) %>%
  filter( accuracy == min(accuracy)) %>%
  #
  # take the mean location for accuracy ties
  group_by( userid, night, local_time, timestamp, accuracy ) %>%
  summarise( longitude=mean(longitude), latitude=mean(latitude) ) %>%
  group_by( userid, night) %>%
  #
  # we want people who had at least 1 reading/night
  filter( n() > 1 ) %>%  
  #
  # find distance, speed between successive gps locationa on a night
  mutate( interval = difference( timestamp, 1 ), 
      dist = calc_interval_distance(longitude, latitude),
      speed = dist/interval * 1000) %>%  # in m/sec
  select( interval, dist, speed, accuracy, everything())  %>% 
  ungroup() 

}

# get survey data
get_df_all <- function( ) {

  read.csv('data/EveningMasterFullAnonym.csv') %>% 
    as_tibble %>% 
    dplyr::rename( userid=user ) %>%
    mutate( night =  ymd(sprintf('2014%04.0f', day ))) 

}


# get timestamp information from survey data
get_df_all_ts <- function( df_all ) {

  df_all %>% 
    dplyr::select( id, ends_with('timestamp'))  %>% 
    tidyr::gather( which, timestamp , -id ) %>%
    mutate( which = str_replace( which, "_.*","")) %>%
    { . } -> ts

  df_all %>% 
    dplyr::select( id, ends_with('timezone_id'))  %>% 
    tidyr::gather( which, timezone , -id ) %>%
    mutate( which = str_replace( which, "_.*","")) %>%
    { . } -> tz


  inner_join( ts, tz, by = c("id", "which")    ) %>% 
    filter( timestamp != '') %>%
    group_by( timezone ) %>%
    mutate( ts = ymd_hms( timestamp, tz=min( timezone))) %>%   # TODO: maybe igmore timezone?
    mutate( timestamp= seconds( ts )) %>% 
    inner_join( dplyr::select( df_all, id, userid, night), by='id') %>%
    ungroup() %>%
    group_by( which, timestamp, userid, night ) %>%
    summarise( id = min(id)) %>%
    { . } -> df_all_ts

   df_all_ts
}



