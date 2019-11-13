loadd(meanshift_mode_300_10_10_100_6_interpolated_locations_300_filtered_accuracy_100)
loadd(df_survey_nested )

df_survey_nested  = df_all_ts_nested


df_staypoints  = readd(df_matching_survey_staypoints_distance_120_600_10_filtered_accuracy_20)

get_matching_survey ( df_staypoints  ,  df_survey_nested )

df_survey_nested = get_df_survey_nested ( df_all_ts ) 

debug(test)
undebug(test)

test = function( surveys, staypoints, by, maxgap ) {
  #print(surveys)
a=1
interval_inner_join( surveys, staypoints, by=c('timestamp_start','timestamp_end'), maxgap=maximum_seconds_distant )

}

interval_inner_join( surveys, staypoints, by=c('timestamp_start','timestamp_end'), maxgap=maximum_seconds_distant )
interval_inner_join( surveys[[1]], staypoints[[1]], by=c('timestamp_start','timestamp_end'), maxgap=maximum_seconds_distant )



get_matching_survey = function( df_staypoints,  df_survey_nested ) {
  # match all staypoints up to surveys, return df with single line per userid, night and staypoint 
  # with MATCH FOUND, matches in /which_survey/

  maximum_seconds_distant = 5*60
  df_staypoints  %>%
    group_by( userid, night) %>% 
    summarise( sp_total = max( n_staypoint)) %>% 
    { . } -> df_sp_total

  df_staypoints  %>%
    group_by( userid, night, n_staypoint ) %>%
    summarise( longitude = mean(longitude), latitude = mean(latitude), 
              timestamp_start = min(timestamp), 
              timestamp_end = max(timestamp)) %>%
    group_by( userid, night ) %>%
    nest(  staypoints = c( n_staypoint, starts_with('timestamp'), ends_with('itude'))) %>%
    inner_join( df_survey_nested , by=c('userid', 'night')) %>% 
    group_by( userid, night ) %>%
    do( joined = interval_inner_join( .$surveys[[1]], .$staypoints[[1]], by=c('timestamp_start','timestamp_end'),
                                     maxgap=maximum_seconds_distant ))  %>%
    unnest( joined ) %>%
    ungroup() %>% 
    { . } -> df 

  if(nrow(df) != 0 ) {

    df %>%
      group_by( userid, night, n_staypoint ) %>%
      mutate( minutes_since_arrival = round(( timestamp_start.x - min( timestamp_start.y))/60,2)) %>%
      arrange( timestamp_start.x) %>%
      summarise( which_survey = paste(which, minutes_since_arrival, collapse=',')) %>%
      ungroup() %>% 
      { . } -> df

  }

  df %>%
    right_join( df_sp_total, by=qc(userid, night))

}

