
################################################################################
# merge_staypoints_helper_test
# optics_distance_1800_300_100_interpolated_locations_120_filtered_accuracy_100
################################################################################
merge_staypoints_helper_test = function( ) {

 merge_staypoints_helper(df,'')
  file = 'optics_distance_1800_300_100_interpolated_locations_120_filtered_accuracy_100'

    filename = 'optics_distance_1800_300_10_interpolated_locations_120_filtered_accuracy_10'

filename = "optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100"

df = readd(filename, character_only=TRUE) %>% merge_staypoints_helper( filename )

readd(filename, character_only=TRUE) %>%
  count(userid, night)

df_location  %>%
count(userid, night)

readd(filename, character_only=TRUE) %>% merge_staypoints_helper( filename )

df_results %>%
  ggplot( aes( min_sp_duration, sp_total, color=algorithm)) +
  geom_jitter()


df %>%
  count( n_staypoint, sort=TRUE) %>% summarise(sum(n))

    df %>% distinct(userid, night, n_staypoint) %>% count()
    df1 = df %>% merge_staypoints_helper( file)
    df1 %>% distinct(userid, night, n_staypoint) %>% count()

    df2 = df1 %>% merge_staypoints_helper( file)
    df2 %>% distinct(userid, night, n_staypoint) %>% count()

}

################################################################################
# get_parameter_name
################################################################################
get_parameter_name = function( df ) {

  match.call(expand.dots=FALSE)$df %>%
    as.character() %>%
    { . } -> filename


}
################################################################################
# merge_staypoints_helper
# transllate from drake to merger, need parameters from filename
# filename example:
# optics_distance_1800_300_100_interpolated_locations_120_filtered_accuracy_100
################################################################################
merge_staypoints_helper = function( df, filename = NA ) {


  if (is.na(filename)) {
    match.call(expand.dots=FALSE)$df %>%
      as.character() %>%
      { . } -> filename
  }

  # extract max_staypoint_distance, min_staypoint_time
  #browser()
  merge_staypoints( df,
      extract_nth_chunk( filename, 5),
      extract_nth_chunk( filename, 4)
      )

}

################################################################################
# merge_staypoints_test_
################################################################################
merge_staypoints_test = function( ) {
  filename = 'optics_distance_1800_300_100_interpolated_locations_120_filtered_accuracy_100'

    filename = 'optics_distance_1800_300_10_interpolated_locations_120_filtered_accuracy_10'

    df = readd(filename, character_only=TRUE)
    max_staypoint_distance = extract_nth_chunk( filename, 5)
    min_staypoint_time = extract_nth_chunk( filename, 4)



    readd(df_location) %>% count()

    readd( interpolated_locations_120_filtered_accuracy_100) %>% count()
}


################################################################################
# merge_staypoints
# merge staypoints if they are close enough in space and time
################################################################################
merge_staypoints = function( df, max_staypoint_distance, min_staypoint_time ) {

  #browser()

  # calculate the total latitude and longitude (for later calcuation of centroid, and accrual of staypoints)
  df %>%
    group_by( userid, night, n_staypoint ) %>%
    summarise(
        sum_latitude = sum( latitude),
        sum_longitude = sum( longitude),
        min_ts = min(timestamp),
        max_ts = max(timestamp),
        n=n(),
        ) %>%
    filter(n()>1) %>%
    group_by( userid, night) %>%
    arrange( min_ts, .by_group=TRUE ) %>%
    mutate(
        l_sum_latitude = lag(sum_latitude, default=0) ,
        l_sum_longitude = lag(sum_longitude, default=0) ,
        l_max_ts = lag(max_ts, default=0) ,
        l_n_staypoint = lag(n_staypoint, default=0) ,
        l_n = lag(n, default=0),
        new_n_staypoint = n_staypoint
        ) %>%
    ungroup() %>%
    { . } -> df_summary

  prev_n=0
  prev_sum_lat = 0
  prev_sum_long = 0
  prev_n_staypoint = 0
  did_merge=FALSE
  i=1
  for( i in 1:nrow( df_summary )) {
    #print(i)

    row =  df_summary[ i, ]
    if( ! did_merge ) {
      # we only want to merge if there was a lag staypoint for this usernight
        prev_n=0
        prev_sum_lat = 0
        prev_sum_long = 0
        prev_n_staypoint = -1
    }
#
    prev_sum_lat = (row$l_sum_latitude   + prev_sum_lat )
    prev_sum_long = (row$l_sum_longitude   + prev_sum_long )
    prev_n = prev_n + row$l_n

    #browser()

    if( row$l_n > 0  &&
       should_merge_with_previous_staypoint
        ( row$sum_latitude / row$n , row$sum_longitude / row$n ,
          prev_sum_lat / prev_n, prev_sum_long / prev_n,
          row$min_ts , row$l_max_ts ,
          max_staypoint_distance, min_staypoint_time
        )
      ) {
      if (!did_merge) {
        # lock in first staypoint id of the series
        prev_n_staypoint = row$l_n_staypoint
      }
      did_merge=TRUE
    } else  {
      did_merge=FALSE
    }
    if( did_merge) {
#      #browser()
      df_summary[ i,]$new_n_staypoint = prev_n_staypoint   # reset to the first of this series
    }
  }
  #browser()
  df_summary %>%
    dplyr::select( userid, night, ends_with('n_staypoint')) %>%
    right_join( df, by=c( 'userid', 'night', 'n_staypoint' )) %>%
    mutate(n_staypoint_pre_merge = n_staypoint ) %>%
    mutate(n_staypoint = ifelse( is.na(new_n_staypoint),n_staypoint,new_n_staypoint )) %>%
    dplyr::select( -new_n_staypoint)

}

################################################################################
# should_merge_with_previous_staypoint
################################################################################
should_merge_with_previous_staypoint = function( centroid_latitude, centroid_longitude,
    l_centroid_latitude, l_centroid_longitude,
    min_ts, l_max_ts,
    max_staypoint_distance, min_staypoint_time
    ) {
#browser()

  if (l_centroid_latitude == 0) return(FALSE)

  if( distm(c(l_centroid_longitude,l_centroid_latitude),
            c(centroid_longitude,centroid_latitude),
            fun=distHaversine) > max_staypoint_distance )
        return(FALSE)

  if( (min_ts - l_max_ts ) > min_staypoint_time ) return(FALSE)

  # we only get here if we are close enough in space and time, and not the first point
  return( TRUE )



}

