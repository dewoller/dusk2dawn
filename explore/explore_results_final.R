source('lib/load_data_post_drake.R')

loadd(df_all_summarise_staypoints )

loadd(df_all_count_staypoints_per_algorithm)



loadd(df_all_matching_survey)

loadd(df_all_matching_survey_per_staypoint)

loadd(df_all_sp_match_survey_combined)



cached() %>% enframe() -> cache


df_results %>%
  filter( str_detect(  source, 'optics'))


df_results %>%
  distinct( filter_type)



df_results %>%
  filter( str_detect(  base_file, 'optics'))


cache %>%
  filter( str_detect(  value, 'optics_distance')) %>%
  filter( !str_detect(  value, 'df_count')) %>%
  filter( !str_detect(  value, 'df_matching_geography')) %>%
  filter( !str_detect(  value, 'df_matching_survey')) %>%
  filter( !str_detect(  value, 'df_summarise_staypoints')) %>%
  filter( !str_detect(  value, '^optics_distance')) %>%

  readd(df_count_staypoints_optics_distance_600_100_interpolated_locations_120_filtered_accuracy_10) %>%
  arrange( desc( total_sp_duration)) %>%
  pluck( 'sp_total') %>% sum()

readd(df_all_sp_match_survey ) %>%
mutate( source = str_replace( source, '.*_df_matching_survey_(staypoints|optics)_', '\\1_' )) %>%
  filter( str_detect(  source, 'optics')) %>%


df_all_matching_survey_per_staypoint %>%
  filter( str_detect(  source, 'optics')) %>%

df_all_sp_match_survey_combined  %>%
    filter( str_detect(  source, 'optics')) %>%



    df1 %>% 
    filter( str_detect(  source, 'optics')) %>%


  df_results %>%
  count( algorithm)

df_results %>%
  count( filter_type)

df_results %>%
  count( interpol, algorithm)


df_results %>%
  filter( str_detect(  base_file, 'optics')) %>%
  arrange( desc( sp_total)) %>%
  ggplot( aes( sp_total, surveys_total, color=filter_type)) +
