################################################################################
# get_nights_with_gps
################################################################################
get_nights_with_gps = function( min_accuracy = 10) {
  readd(df_location) %>%
    filter( accuracy <= min_accuracy ) %>%
    group_by( userid, night ) %>%
    summarise( n_gps_points=n(),
              last_gps_timestamp = max(timestamp),
              first_gps_timestamp = min(timestamp),
              duration = last_gps_timestamp - first_gps_timestamp  )

}

################################################################################
# get_surveys_to_match
################################################################################
get_surveys_to_match = function( ) {
  # zoom into the surveys that we are supposed to have found

  readd( df_all_ts  ) %>%
    group_by( id, userid, night) %>%
    filter( which %in% c('dq') ) %>%
    dplyr::summarise( timestamp = max(timestamp)) %>%
    ungroup() %>%
    { . } ->  df_surveys

  get_df_florian_locations() %>%
    filter(category =='sp') %>%
    inner_join( df_surveys, by='id') %>%
    { . } -> df_surveys_to_match
  df_surveys_to_match
}


################################################################################
# get_df_surveys_cleaned
################################################################################
get_predictable_surveys = function( ) {

  # zoom into the surveys that we are supposed to have found
  df_surveys_to_match = get_surveys_to_match()
  df_nights_with_gps = get_nights_with_gps()
  desert_endpoint_offset = 5

  # nights with surveys, but missing GPS entirely

  df_surveys_to_match %>%
    inner_join( df_nights_with_gps %>% dplyr::select(userid, night) , by=c('userid','night')) %>%
    { . } -> df_surveys_to_match_2

    # poor quality GPS signals
  # which survey nights had less than 20 minutes of GPS in a night

  df_surveys_to_match_2 %>%
    anti_join( df_nights_with_gps %>% filter( duration < 1200 ), by=c('userid','night') ) %>%
    { . } -> df_surveys_to_match_3

 # delete all surveys that happen in the deserts

df_location_deserts = get_location_deserts( df_location )

df_surveys_to_match_3 %>%
  match_surveys_to_deserts( df_location_deserts) %>%
  { . } -> df_surveys_in_desert


df_surveys_to_match_3 %>%
  anti_join( df_surveys_in_desert, by='id') %>%
    inner_join( df_nights_with_gps, by=c('userid', 'night')) %>%
    filter( timestamp >= first_gps_timestamp - desert_endpoint_offset  &
           timestamp <= last_gps_timestamp + desert_endpoint_offset ) %>%
    { . } -> df_surveys_to_match_final

df_surveys_to_match_final

}


################################################################################
# match_surveys_to_deserts
################################################################################
match_surveys_to_deserts = function( df_surveys_to_match, df_location_deserts ) {


  #
  # get_df_surveys_cleaned () %>%
  #   { . } -> df_surveys_to_match_5

  df_surveys_to_match %>%
    group_by(userid, night) %>%
    mutate( timestamp_start = timestamp, timestamp_end = timestamp) %>%
    dplyr::select(userid, night, starts_with('timestamp_'), id) %>%
    nest( surveys=c(starts_with('timestamp_'), id)) %>%
    ungroup() %>%
    { . } -> df_surveys_to_match_nested


  # keep track of ALL staypoints found so we don't lose any staypoints
  # when we join them to the surveys in the next step
  maximum_seconds_distant =  0

  # which staypoints match survey timestamps
  df_location_deserts %>%
    inner_join( df_surveys_to_match_nested, by=c('userid', 'night')) %>%
    group_by( userid, night ) %>%
    do( joined = interval_inner_join( .$surveys[[1]], .$deserts[[1]], by=c('timestamp_start','timestamp_end'),
                                     maxgap=maximum_seconds_distant ))  %>%
    unnest( joined ) %>%
    ungroup() %>%
    { . } -> df_surveys_in_desert
  df_surveys_in_desert
}

################################################################################
# get_location_deserts
################################################################################
get_location_deserts = function (  df_location, desert_length_seconds = 5*60, desert_offset_seconds = desert_length_seconds /2 ) {


  desert_length_seconds=10
  desert_offset_seconds = 5*60
  min_accuracy = 10

  readd(df_location) %>%
    { . } -> df_location

  df_location %>%
    filter( accuracy < min_accuracy ) %>%
    group_by(userid, night) %>%
    dplyr::arrange(timestamp, .by_group=TRUE) %>%
    mutate( timestamp_start = lag(timestamp) + desert_offset_seconds,
           timestamp_end = timestamp - desert_offset_seconds,
           diff = (timestamp_end - timestamp_start)) %>%
    filter(!is.na(timestamp_start)) %>%
    filter( diff > desert_length_seconds )  %>%
    dplyr::select(userid, night, starts_with('timestamp_'), diff) %>%
    nest( deserts=c(starts_with('timestamp_'), diff)) %>%
    ungroup() %>%
    { . } -> df_location_deserts
  df_location_deserts


}






################################################################################
# bad_surveys
################################################################################
bad_surveys = function() {



  get_df_florian_locations() %>%
    anti_join( readd( df_all_ts  ) %>%
              filter( which %in% c('dq', 'video', 'env') )) %>%
  dplyr::select('id') %>%
  { . } -> df_incorrect_ids

readd( df_all_ts  ) %>%
  filter( which %in% c('dq', 'video', 'env') ) %>%
  group_by( id, userid, night) %>%
  summarise( a=max(timestamp), b=min(timestamp), diff=a-b) %>%
  ungroup() %>%
  arrange(desc(diff)) %>%
  filter( diff > 600) %>%
  dplyr::select(id) %>% clipr::write_clip()


df_all %>%
  filter(id==3592) %>%
  dplyr::select( ends_with('timestamp'))  %>%

  readd(df_location) %>%
  group_by( userid, night) %>%
  summarise( first_gps_timestamp = min(timestamp)) %>%
  ungroup() %>%
  { . } -> df_first_gps

readd( df_all_ts  ) %>%
  filter( which %in% c('dq', 'video', 'env') ) %>%
  group_by( id, userid, night) %>%
  summarise( timestamp=max(timestamp)) %>%
  inner_join( get_df_florian_locations(), by = 'id') %>%
  inner_join( df_first_gps, by=c('userid','night')) %>%
  { . } -> df_joined

df_joined %>% filter( timestamp < first_gps_timestamp)


}

bad_surveys = function() {



  get_df_florian_locations() %>%
    anti_join( readd( df_all_ts  ) %>%
              filter( which %in% c('dq', 'video', 'env') )) %>%
  dplyr::select('id') %>%
  { . } -> df_incorrect_ids

readd( df_all_ts  ) %>%
  filter( which %in% c('dq', 'video', 'env') ) %>%
  group_by( id, userid, night) %>%
  summarise( a=max(timestamp), b=min(timestamp), diff=a-b) %>%
  ungroup() %>%
  arrange(desc(diff)) %>%
  filter( diff > 600) %>%
  dplyr::select(id) %>% clipr::write_clip()


df_all %>%
  filter(id==3592) %>%
  dplyr::select( ends_with('timestamp'))  %>%

  readd(df_location) %>%
  group_by( userid, night) %>%
  summarise( first_gps_timestamp = min(timestamp)) %>%
  ungroup() %>%
  { . } -> df_first_gps

readd( df_all_ts  ) %>%
  filter( which %in% c('dq', 'video', 'env') ) %>%
  group_by( id, userid, night) %>%
  summarise( timestamp=max(timestamp)) %>%
  inner_join( get_df_florian_locations(), by = 'id') %>%
  inner_join( df_first_gps, by=c('userid','night')) %>%
  { . } -> df_joined

df_joined %>% filter( timestamp < first_gps_timestamp)


}

