
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



cached() %>%
  enframe() %>%
  { . } -> cache


cache %>%
  filter( str_detect( value, 'optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100$' )) %>%

  df_pre = readd(optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100)

df_post = readd(df_merged_staypoints_optics_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100)


df_pre %>%
  count( userid, night, n_staypoint)

df_post %>%
  count( userid, night, n_staypoint)

df_pre = readd(staypoints_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100)

df_post = readd(df_merged_staypoints_staypoints_distance_14400_300_10_interpolated_locations_120_filtered_accuracy_100)


df_pre %>%
  count( userid, night, n_staypoint)

df_post %>%
  count( userid, night, n_staypoint)


df_pre %>% 
  ungroup() %>%
  inner_join( df_post, by=qc(userid, night, timestamp)) %>%


  df_location %>%
  filter( accuracy<=100) %>%
  group_by( provider ) %>%
  summarise( mean( accuracy)) %>%

  df_location %>%
  filter( accuracy<=10) %>%
  count( userid, night, timestamp, sort=TRUE) %>%
filter( n>1) %>%
head(1) %>%

