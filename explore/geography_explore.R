
filename = "optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100"

df = readd(filename, character_only=TRUE) %>% merge_staypoints_helper( filename )

cached() %>%
  enframe() %>%
  { . } -> cache


cache %>%
  filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100$' )) %>%

  cache %>%
  filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100$' )) %>%
  df_matching_survey_per_sp_summarised



cache %>%
filter( str_detect( value, 'optics_distance_14400_300_100_interpolated_locations_300_filtered_accuracy_100$' )) %>%
filter( str_detect( value, '^df_matching_geography' )) %>%
head(2) %>%
tail(1) %>%
pluck('value') %>%
readd(character_only = TRUE) %>%
{ . } -> df


readd(df_matching_geography_df_summarise_staypoints_df_merged_staypoints_optics_distance_14400_300_100_interpolated_locations_300_filtered_accuracy_100) %>% 
{ . } -> df

df %>%
  filter( dist<100 ) %>%
  ggplot( aes( dist)) + geom_histogram()





[1] ""

cache %>%
  filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_10$' )) %>%
  filter( str_detect( value, '^df_summarise_staypoints_df_merged' )) %>%
  pluck('value') %>%
  readd(character_only = TRUE) %>%
  { . } -> df


calculate_sp_match_geography(df, df_target_locations_combined)

cache %>%
  filter( str_detect( value, '^df_matching_geography_summarised')) %>%
  head(2) %>%
  tail(1) %>%
  pluck('value') %>%
  { . } -> a

  readd() %>%
  { . } -> df


readd("df_matching_geography_summarised_df_matching_geography_df_merged_staypoints_optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_10")

readd("df_matching_geography_summarised_df_matching_geography_df_merged_staypoints_ optics_distance_14400_300_100_interpolated_locations_300_filtered_accuracy_100") %>%
