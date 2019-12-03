options(warn=-1)
library( knitr )
opts_chunk$set(cache=TRUE, autodep=TRUE, eval=TRUE)

library(tidyverse)
library(drake)
library(wrapr)
library(skimr)
library(treemap)
library(data.tree)

source('lib/load_data_post_drake.R')


df_all_ts %>%
  ungroup() %>%
  count( which ) %>% 
  { . } -> df_all_ts_frequencies


# which userid has the most surveys survey exist
df_all_ts %>%
  ungroup() %>%
  filter( which != 'tom' ) %>%
  distinct( which, ts, userid, night ) %>%
  count( userid, night, sort=TRUE ) %>%
  head(1) %>% 
  { . } -> df_numerous_surveys


#which algorithm found the most staypoint / survey matches

df_results %>% 
  top_n(1, surveys_total) %>%
  pluck( 'base_file' )

df_staypoints = readd( staypoints_distance_120_300_40_interpolated_locations_120_filtered_accuracy_100)

loadd( df_matching_survey_summarised_df_matching_survey_staypoints_distance_120_300_40_interpolated_locations_120_filtered_accuracy_100)

a = readd(df_matching_survey_staypoints_distance_120_300_40_interpolated_locations_120_filtered_accuracy_100)

a %>% 
  inner_join( df_numerous_surveys ) %>%

df_all_ts %>%
  inner_join( df_numerous_surveys ) %>%


a=cached()

a %>% 
  enframe() %>%
  filter( startsWith(value, df



# TODO - do some basic error checking on survey matching algorithm
# Dataset description

map the user paths of the people who have the poorest survey matching results



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


