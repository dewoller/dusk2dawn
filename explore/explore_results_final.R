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
  filter( str_detect(  value, 'optics')) %>%
  filter( !str_detect(  value, 'df_count')) %>%
  filter( !str_detect(  value, 'df_matching_geography')) %>%
  filter( !str_detect(  value, 'df_matching_survey')) %>%
  filter( !str_detect(  value, 'df_summarise_staypoints')) %>%
  filter( !str_detect(  value, '^optics_distance')) %>%


readd(df_all_sp_match_survey ) %>%
  filter( str_detect(  source, 'optics')) %>%


df_all_matching_survey_per_staypoint %
  filter( str_detect(  source, 'optics')) %>%

