
filename = "optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100"

df = readd(filename, character_only=TRUE) %>% merge_staypoints_helper( filename )

cached() %>%
  enframe() %>%
  { . } -> cache


cache %>%
  filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100$' )) %>%





cache %>%
filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100$' )) %>%
filter( str_detect( value, '^df_summarise_staypoints_df_merged' )) %>%
pluck('value') %>%
readd(character_only = TRUE) %>% 
{ . } -> df



calculate_sp_match_geography(df, df_target_locations_combined)




