################################################################################
# plot_one_person_survey_source
# plot the timestamp duration ranges of a user's staypoints, and their corresponding survey points
# using defaults
################################################################################

plot_one_person_survey_source = function(df_user_night, source, min_accuracy=100 ){

  source %>%
    str_c('df_matching_survey_df_merged_staypoints_', .) %>%
    readd(character_only = TRUE) %>%
    dplyr::rename( timestamp=timestamp_survey ) %>%
    { . } -> df_survey

  source %>%
    readd(character_only = TRUE) %>%
    { . } -> df_staypoint

plot_one_person_survey(df_user_night, readd(df_location), df_staypoint, df_survey, min_accuracy)

}

################################################################################
# plot_one_person_survey
# plot the timestamp duration ranges of a user's staypoints, and their corresponding survey points
# df_usernight
################################################################################

plot_one_person_survey = function(df_user_night, df_location, df_staypoint, df_survey, min_accuracy=10){
  a=RColorBrewer::brewer.pal(10,'Spectral')

  df_user_night %>%
    dplyr::select(userid,night) %>%
    inner_join( df_location, by=c('userid', 'night')) %>%
    filter( accuracy <= min_accuracy) %>%
    { . } -> df_1_loc

  df_1_loc %>%
    summarise( start_ts  = min(timestamp)) %>%
    pluck('start_ts') %>%
    { . } -> start_ts

  df_1_loc %>%
    mutate(timestamp = (timestamp - start_ts) / 3600 ) %>%
    { . } -> df_1_loc

  df_user_night %>%
    dplyr::select(userid,night) %>%
    inner_join( df_staypoint, by = c("userid", "night") ) %>%
    mutate_at( vars(ends_with('ts')), function(ts) { (ts - start_ts) / 3600  })  %>%
    { . } -> df_1_sp

 df_user_night %>%
   dplyr::select(userid,night) %>%
    inner_join( df_survey , by=c('userid', 'night')) %>%
    mutate_at( vars(ends_with('timestamp')), function(ts) { (ts - start_ts) / 3600  })  %>%
    { . } -> df_1_surveys


  title = paste( df_user_night$userid, df_user_night$night  )

  df_1_sp %>%
    mutate( which_sp = paste('sp', n_staypoint)) %>%
    mutate( ts = (max_ts - min_ts)/2 + min_ts ) %>%
    ggplot( aes( ts, which_sp, color=which_sp  )) +
    geom_errorbarh( aes( xmax = max_ts, xmin=min_ts)) +
    geom_point( aes( timestamp, 'surveys'), color='black', shape=3, data=df_1_surveys) +
    geom_point( aes( timestamp, 'gps'), color='green', shape=3, data=df_1_loc)  +
    ggtitle( title ) %>%
    { . } -> p

p- sadgg
print(p)


}


################################################################################
# map_one_user_test
################################################################################

map_one_user_test = function() {

  df_location %>%
    filter( accuracy <= min_accuracy) %>%
    inner_join( df_user_night, by=c('userid', 'night')) %>%
    distinct(n_staypoint) %>%
    left_join( df_staypoint %>% dplyr::select(userid, night, timestamp, n_staypoint), by=c('userid', 'night', 'timestamp')) %>%
    distinct(n_staypoint)


}
################################################################################
# map_one_user_source
################################################################################
map_one_user_source  = function( df_user_night, source, min_accuracy=100) {

  source %>%
    str_c('df_matching_survey_df_merged_staypoints_', .) %>%
    readd(character_only = TRUE) %>%
    { . } -> df_survey

  source %>%
    readd(character_only = TRUE) %>%
    { . } -> df_staypoint

  map_one_user( df_user_night, readd( df_location ), df_survey, df_staypoint, min_accuracy )
}
################################################################################
# map_one_user
# user_night -  one row, containing user and night
# location = all locations
# survey  - the location of ALL the surveys
# df_staypoint - if this location point is a staypoint

################################################################################

map_one_user = function( df_user_night, df_location, df_survey, df_staypoint, min_accuracy ) {
  title = paste( df_user_night$userid, df_user_night$night  )


  df_location %>%
    filter( accuracy <= min_accuracy) %>%
    inner_join( df_user_night %>% dplyr::select(userid,night), by=c('userid', 'night')) %>%
    left_join( df_staypoint %>% dplyr::select(userid, night, timestamp, n_staypoint) %>%
              distinct(),
            by=c('userid', 'night', 'timestamp')) %>%
    dplyr::mutate( start = timestamp, end = timestamp ) %>%
    { . } -> df_user_night_locations

  min_lat = min( df_user_night_locations$latitude)
  min_long = min( df_user_night_locations$longitude)

  # find latitude and longitude for a survey

  df_survey %>%
    inner_join( df_user_night %>% dplyr::select(userid,night), by=c('userid', 'night')) %>%
    dplyr::mutate( start = timestamp_survey, end = timestamp_survey) %>%
    dplyr::select(-longitude, -latitude) %>%
    interval_inner_join( df_user_night_locations, by=c('start','end'), maxgap=3600) %>%
    group_by( id ) %>%
    # find the GPS point that is the closest to this staypoint
    filter( abs( start.x - start.y) == min( abs( start.x- start.y)  ) ) %>%
    ungroup() %>%
    mutate(
           m_lat = ll2m( latitude, min_lat, m_per_latitude),
           m_lon = ll2m( longitude, min_long, m_per_longitude)
           ) %>%
    select( m_lat, m_lon, id ) %>%
    { . } -> df_closest_survey


  df_user_night_locations %>%
      mutate(
             m_lat = ll2m( latitude, min_lat, m_per_latitude),
             m_lon = ll2m( longitude, min_long, m_per_longitude),
            alpha = ifelse( is.na( n_staypoint), .1, 1),
            n_staypoint = ifelse(is.na( n_staypoint), 0, n_staypoint) %>% as.factor()
           ) %>%
    arrange( timestamp) %>%
    mutate( ts_diff = (log(timestamp-lag(timestamp))/3 )) %>%
    ggplot( aes(m_lat, m_lon, color=n_staypoint, alpha = alpha) )  +
    geom_point() +
    geom_path( )  +
    geom_point( aes(m_lat, m_lon ), shape=5, size=10, color='black', alpha=1, data= df_closest_survey) +
    ggtitle( title ) %>%
    { . } -> p

  p - sadgg
  print(p)

}
################################################################################


map_one_user_osm_source_test = function( ) {

  source='optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100'
  source %>%
    str_c('df_matching_survey_df_merged_staypoints_', .) %>%
    readd(character_only = TRUE) %>%
    dplyr::select(userid,night) %>%
    head(2) %>%
    tail(1) %>%
    map_one_user_osm_source( source)





}
################################################################################
# map_one_user_source
################################################################################
map_one_user_osm_source  = function( df_user_night, source, min_accuracy=100) {

  source %>%
    str_c('df_matching_survey_df_merged_staypoints_', .) %>%
    readd(character_only = TRUE) %>%
    { . } -> df_survey

  source %>%
    readd(character_only = TRUE) %>%
    { . } -> df_staypoint

  map_one_user_osm( df_user_night, readd( df_location ), df_survey, df_staypoint, min_accuracy )
}

################################################################################
# map_one_user
# user_night -  one row, containing user and night
# location = all locations
# survey  - the location of ALL the surveys
# df_staypoint - if this location point is a staypoint

################################################################################

map_one_user_osm = function( df_user_night, df_location, df_survey, df_staypoint, min_accuracy ) {
  title = paste( df_user_night$userid, df_user_night$night  )

  library(sf)
  library(tmap)
  library(tmaptools)

  browser()

  df_location %>%
    filter( accuracy <= min_accuracy) %>%
    inner_join( df_user_night %>% dplyr::select(userid,night), by=c('userid', 'night')) %>%
    left_join( df_staypoint %>% dplyr::select(userid, night, timestamp, n_staypoint) %>%
              distinct(),
            by=c('userid', 'night', 'timestamp')) %>%
    dplyr::mutate( start = timestamp, end = timestamp,
           alpha = ifelse( is.na( n_staypoint), .1, 1),
           n_staypoint = ifelse(is.na( n_staypoint), 0, n_staypoint) 
           ) %>%
    arrange( timestamp) %>%
    mutate( ts_diff = (log(timestamp-lag(timestamp))/3 )) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
    filter( !st_is_empty(geometry)) %>%
    st_transform(crs = 27700) %>%
    { . } -> df_user_night_locations

  select(longitude, latitude) %>%
    as.matrix() %>%
    st_linestring() %>%
    filter( !st_is_empty(geometry)) %>%

 
  osm = read_osm(df_user_night_locations, type="osm")

  # find latitude and longitude for a survey

  df_survey %>%
    inner_join( df_user_night %>% dplyr::select(userid,night), by=c('userid', 'night')) %>%
    dplyr::mutate( start = timestamp_survey, end = timestamp_survey) %>%
    dplyr::select(-longitude, -latitude) %>%
    interval_inner_join( df_user_night_locations, by=c('start','end'), maxgap=3600) %>%
    group_by( id ) %>%
    # find the GPS point that is the closest to this staypoint
    filter( abs( start.x - start.y) == min( abs( start.x- start.y)  ) ) %>%
    ungroup() %>%
    st_transform(crs = 27700) %>%
    { . } -> df_closest_survey


  df_user_night_locations %>%
#    group_by(n_staypoint) %>%
        arrange( timestamp ) %>%
    summarize( ) %>%
    st_cast("LINESTRING") %>%
#    ungroup() %>%
    st_transform(crs = 27700) %>%
#    mutate( display_colors= RColorBrewer::brewer.pal( n() , "RdBu"))%>%
    { . } -> path

  df_user_night_locations %>%
    select(longitude, latitude) %>%

    st_coordinates() %>%
    st_linestring() %>%
    st_sf() %>%
    { . } -> path

  qtm(osm) + qtm(df_user_night_locations) + qtm(path, )

  df_user_night_locations %>% 
    filter( n_staypoint >0 ) %>% 
    mutate( n_staypoint = as.character(n_staypoint)) %>% 
    { . } -> a


  tm_shape( osm) +
    tm_rgb( alpha=.5 )  + 
  tm_shape(a) +
    tm_symbols(col='n_staypoint', size=.1) +
  tm_shape(path) +
    tm_lines( alpha=.5,  col='black')  





  tm_shape() +
    tm_rgb() +

    qtm(df_user_night_locations) +

    tm_shape(path %>% head(2)) +
    tm_lines( col='display_colors') +
    tm_legend()


  route = sf::read_sf("https://git.io/fhnA2")



  CBS_bb <- bb("La Trobe University, Bundoora, Victoria, Australia", width=.003, height=.002)
CBS_osm1 <- read_osm(CBS_bb, type="osm")
tm_shape(CBS_osm1) + tm_rgb()

  ggplot( aes(m_lat, m_lon, color=n_staypoint, alpha = alpha) )  +
  geom_point() +
  geom_path( )  +
  geom_point( aes(m_lat, m_lon ), shape=5, size=10, color='black', alpha=1, data= df_closest_survey) +
  ggtitle( title ) %>%
  { . } -> p

p - sadgg
print(p)

}
################################################################################



