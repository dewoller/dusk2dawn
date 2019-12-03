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



