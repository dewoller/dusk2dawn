
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
    mutate( night =  ymd(sprintf('2014%04.0f', day )))  %>%
    mutate_if( is.factor, as.character)

}


# get timestamp information from survey data
get_df_all_ts <- function( df_all ) {


  #survey_type='pre'
  get_one_survey_type = function( df_all, survey_type)   {
    df_all %>%
      dplyr::select( userid, night, starts_with( paste0( survey_type, '_'))) %>%
      dplyr::select( userid, night, ends_with( '_timestamp'), ends_with('_timezone_id')) %>%
      distinct()  %>%
      dplyr::rename( timestamp=3, timezone=4) %>%
      mutate(which=survey_type ) %>%
      filter( timestamp != "")
  }

  bind_rows(
            get_one_survey_type( df_all, 'dq'),
            get_one_survey_type( df_all, 'env'),
            get_one_survey_type( df_all, 'forg'),
            get_one_survey_type( df_all, 'video'),
            get_one_survey_type( df_all, 'load'),
            get_one_survey_type( df_all, 'pre'),
            get_one_survey_type( df_all, 'tom')
            ) %>%
    group_by( timezone ) %>% # group together all the same timezones so  that calculation is faster
    mutate( ts = ymd_hms( timestamp, tz=min( timezone))) %>%   # TODO: maybe igmore timezone?
    ungroup() %>%
    mutate( timestamp= seconds( ts )) %>% 
    { . } -> df_all_ts

   df_all_ts
}



