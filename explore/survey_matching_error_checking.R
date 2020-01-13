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
  filter( str_detect( value, '^df_matching_survey')) %>%
  filter( !str_detect( value, 'mode')) %>%

  readd(df_matching_survey_staypoints_distance_1800_600_20_interpolated_locations_120_filtered_accuracy_10)


readd(df_matching_survey_optics_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_10) %>%
left_join( get_df_florian_locations() %>% dplyr::select( id, ptype_id_long) )  %>%
mutate( ptype_id_long = ifelse(is.na( ptype_id_long), 0, ptype_id_long  )) %>% 
{ . } -> a

a %>%
distinct( userid, night, id, ptype_id_long ,) %>%
inner_join( ptype_long(), by='ptype_id_long') %>%
count(userid, night, id, sort=TRUE)  %>% 
filter( n>1 ) %>%
inner_join(a)  %>%
mutate( d = round( (timestamp_survey - timestamp_start_location ) / 60 , 2) ) %>%


readd( staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100 ) %>%
    get_matching_survey( df_survey_nested) %>%
    { . } -> df_test


# counting staypoints
df_count_staypoints_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100

# consolidated matching surveys, by userid and night
df_matching_survey_per_staypoint_

df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_summarised_df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
df_matching_survey_summarised_df_matching_survey_staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100
staypoints_distance_14400_300_20_interpolated_locations_120_filtered_accuracy_100

# find the user and nights with NO matches in any of the staypoint discovery algorithms

df_all_ts_valid %>%
  count(userid, night, sort=TRUE, name='number_of_valid_surveys') %>%
  { . } -> df_survey_frequencies

df_all_ts_valid %>%
  count(userid,  sort=TRUE, name='number_of_valid_surveys') %>%


# all the surveys that were matched
loadd(df_all_matching_survey_per_staypoint )

df_survey_frequencies %>%
  anti_join( df_all_matching_survey_per_staypoint, by=c('userid', 'night')) %>%
  { . } -> df_surveys_with_no_corresponding_staypoints

# we have dodgy userid/nights, with no corresponding staypoints.  What does their journey look like?

# jouney with most surveys only has 9 locations
readd(df_location)

df_surveys_with_no_corresponding_staypoints %>%
  ggplot( aes( number_of_valid_surveys)  ) + geom_histogram()

df_surveys_with_no_corresponding_staypoints %>%
  inner_join( df_all_ts_valid ) %>%
  group_by( userid, night ) %>%
  summarise(
            s_mints = min(timestamp),
            s_maxts = max(timestamp),
            number_of_valid_surveys = min(number_of_valid_surveys)) %>%
  inner_join( df_staypoints) %>%
  group_by( userid, night ) %>%
  summarise(
            sp_mints = min(timestamp),
            sp_maxts = max(timestamp),
            number_of_valid_surveys = min(number_of_valid_surveys),
            s_mints = max(s_mints ),
            s_maxts = max(s_maxts)
            ) %>%
  ungroup() %>%
  mutate(
         before = floor((sp_mints  - s_maxts)/3600+1),
         after = floor((s_mints  - sp_maxts)/3600+1),
         before=ifelse( before < 0, 0, before),
         after=ifelse( after < 0, 0, after)
         ) %>%
  filter( before==1) %>%
  count( night, sort=TRUE) %>%
  filter( before==0 & after==0) %>%
  count(userid) %>%
  inner_join( df_survey_frequencies ) %>%

  count( before, sort=TRUE) %>%
  count( after, sort=TRUE) %>%
  summarise( sum(before, na.rm=TRUE), sum(after, na.rm=TRUE))



1205 userid / nights

df_surveys_with_no_corresponding_staypoints %>%
  anti_join( df_staypoints) %>%


readd( df_staypoints)

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
  inner_join( df_location ) %>%
  group_by( userid, night, number_of_valid_surveys ) %>%
  summarise( n=n(), rate = n()*min(number_of_valid_surveys)) %>%
  ungroup() %>%
  arrange( desc(rate)) %>%
  { . } -> df_poor_performers

df_poor_performers

df_poor_performers %>%
  count( userid, sort=TRUE)


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

df_poor_performers %>%
  head(28) %>%
  tail(1) %>%
  { . } -> df
df %>%
  plotit()

# all the staypoint of all the algorithms

loadd(df_all_summarise_staypoints)

  # plot all the  points for this dataset
plotit = function ( df ) {

  a=RColorBrewer::brewer.pal(10,'Spectral')
  df %>%
    inner_join( df_location ) %>%
    { . } -> df_1_loc
  df %>%
    inner_join( df_all_summarise_staypoints)  %>%
    inner_join(
               df_results %>% mutate(source=paste0('df_summarise_staypoints_',base_file))
               ) %>%
    filter(match3 < 100) %>%
    { . } -> df_1_sp
   df %>%
    inner_join( df_all_ts_valid)  %>%
  { . } -> df_1_surveys

print( df_1_loc %>% count(userid, night, timezone, n))
print( df_1_surveys%>% distinct(userid, night, timezone, number_of_valid_surveys, which, ts-min(ts)))


  # df %>%
  #   inner_join( df_all_summarise_staypoints) %>%
  #   ggplot( aes( ts_min )) +
  #   geom_histogram()
  #
  # df %>%
  #   inner_join( df_1_surveys) %>%
  #   ggplot( aes( timestamp )) +
  #   geom_histogram()
  #
df_1_sp %>%
    mutate( ts = (ts_max - ts_min)/2 + ts_min ) %>%
    ggplot( aes( ts, source, color=as.factor(filter_type)  )) +
    geom_errorbarh( aes( xmax = ts_max, xmin=ts_min)) +
    geom_point( aes( timestamp, '1'), color='black', shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp-3600*1, '2'), color=a[1], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp-3600*2, '3'), color=a[2], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp-3600*3, '4'), color=a[3], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp-3600*4, '5'), color=a[4], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp+3600*1, '2'), color=a[1], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp+3600*2, '3'), color=a[2], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp+3600*3, '4'), color=a[3], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp+3600*4, '5'), color=a[4], shape=3, data=df_1_surveys)+
    geom_point( aes( timestamp+3600*5, '6'), color=a[5], shape=3, data=df_1_surveys)


}



# problem with timezones;  look at all unique timezones per person and night

df_location %>%
  count( userid, night, timezone, name='n_location_tz' ) %>%
  rename( location_timezone = timezone) %>%
  { . } -> df_location_tz

df_all_ts_valid  %>%
  count( userid, night, timezone, name='n_survey_tz' ) %>%
  rename( survey_timezone = timezone) %>%
  { . } -> df_survey_tz


inner_join( df_location_tz, df_survey_tz) %>%
 count( location_timezone, survey_timezone, sort=TRUE ) %>%
 kableExtra::kable() %>% clipr::write_clip()




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



