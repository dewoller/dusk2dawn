options(warn=-1)
library( knitr )
opts_chunk$set(cache=TRUE, autodep=TRUE, eval=TRUE)

source('lib/gps_functions.R')

library(tidyverse)
library(drake)
library(wrapr)
library(skimr)
library(treemap)
library(data.tree)

source('lib/load_data_post_drake.R')


loadd(df_all_ts_valid)

df_all_ts_valid %>%
  ungroup() %>%
  count( which ) %>% 
  { . } -> df_all_ts_frequencies


# which userid has the most surveys survey exist
df_all_ts_valid %>%
  ungroup() %>%
  filter( which != 'tom' ) %>%
  distinct( which, ts, userid, night ) %>%
  count( userid, night, sort=TRUE ) %>%
  head(1) %>% 
  { . } -> df_numerous_surveys


#which algorithm found the most staypoint / survey matches
loadd(df_location)

df_location %>%
  group_by( userid, night) %>%
  mutate( 
         m_lat = ll2m( latitude, min(latitude), m_per_latitude),
         m_lon = ll2m( longitude, min(longitude), m_per_longitude)
         ) %>% 
  ungroup() %>%
  { . } -> df_location
 
df_results %>% 
  top_n(1, surveys_total) %>%
  pluck( 'base_file' ) %>% 
  { . } -> best_algorithm

  readd( best_algorithm, character_only=TRUE ) %>% 
  { . } -> df_staypoints

cached() %>% enframe() -> cache

cache %>%
  filter( endsWith( value, best_algorithm))


# counting staypoints
df_count_staypoints_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100

# consolidated matching surveys, by userid and night
df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_summarised_df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_summarised_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100

# find the person with the MOST surveys who had NO matches in the best staypoint discovery algorithm

df_all_ts_valid %>%
  count(userid, night, sort=TRUE, name='number_of_valid_surveys') %>% 
  { . } -> df_survey_frequencies

df_survey_frequencies %>%
  anti_join( readd( df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100 )) %>% 
  { . } -> df_surveys_with_no_corresponding_staypoints

# we have dodgy userid/nights, with no corresponding staypoints.  What does their journey look like?

# jouney with most surveys only has 9 locations
df_surveys_with_no_corresponding_staypoints %>%
  head(1) %>%
  inner_join( df_location )

# jouney with most surveys only has 0 locations after filtering / interpolating
df_surveys_with_no_corresponding_staypoints %>%
  head(1) %>%
  inner_join( df_staypoints)


# all empty trips  and a count of their original locations
# poor accuracy 
df_surveys_with_no_corresponding_staypoints %>%
  inner_join( df_location ) %>%
  group_by( userid, night, number_of_valid_surveys ) %>%
  summarise( n=n(), ave_accuracy = mean(accuracy)) %>%
  ungroup() %>%
  arrange( desc(n))

# all empty trips and a count of their filtered and interpolated locations
df_surveys_with_no_corresponding_staypoints %>%
  inner_join( df_staypoints ) %>%
  group_by( userid, night, number_of_valid_surveys ) %>%
  summarise( n=n(), rate = n()*min(number_of_valid_surveys)) %>%
  ungroup() %>%
  arrange( desc(rate)) %>% 
  { . } -> df_poor_performers

df_poor_performers

#map the user paths of the people who have the poorest survey matching results

df_poor_performers %>%
  head(1) %>%
  inner_join( df_location ) %>%
  arrange( timestamp ) %>%
 ggplot( aes( latitude, longitude, color=accuracy)) +
  geom_path()

df_poor_performers %>%
  head(1) %>%
  inner_join( df_staypoints ) %>%
  arrange( timestamp ) %>%
  ggplot( aes( latitude, longitude)) +
  geom_path()

df_poor_performers %>%
  head(2) %>% 
  tail(1) %>%
  inner_join( df_location ) %>%
  arrange( timestamp ) %>%
  ggplot( aes( m_lat, m_lon)) +
  geom_path()

# TODO - do some basic error checking on survey matching algorithm
# Dataset description




df %>%
  right_join( df_sp_total, by=qc(userid, night))



df %>%
  group_by( userid, night, n_staypoint ) %>%
  mutate( minutes_since_arrival = round(( timestamp_start.x - min( timestamp_start.y))/60,2)) %>%
  arrange( timestamp_start.x) %>%

df %>% 
  count( which, sort=TRUE, name='n_found') %>%
  inner_join( df_all_ts_frequencies, by='which') %>%
  mutate( pct = round( n_found/n * 100, 2)) %>%
  dplyr::rename( n_surveys = n, percent_found = pct) %>%
  kableExtra::kable() %>% clipr::write_clip()


