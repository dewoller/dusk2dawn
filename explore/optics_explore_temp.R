cached() %>% enframe() -> cache

cache %>%
  filter( endsWith( value, 'best_algorithm'))


cache %>%
  filter( str_detect(value, 'df_count' ))




df %>%
  mutate( cluster = calculate_clusters_optics(., eps_cl, min_staypoint_time ), eps_cl = eps_cl) %>%
  group_by(cluster) %>%
  arrange( id, .by_group = TRUE) %>%
  # do we have a break in the cluster of sufficient size
  mutate( newcluster = cumsum( id != lag( id, default=0 ) + 1 &
                              (timestamp - lag(timestamp, default=0)) / 60  > max_discontinuity
                            )) %>%
  ungroup() %>%
  mutate( cluster = paste0( cluster, '.', newcluster)) %>%
  { . } ->  df

  count( cluster, newcluster, sort=TRUE) %>%
  dplyr::select(-newcluster)


d %>%
  mutate( diff = timestamp - lag(timestamp,1) ) %>%
  filter( startsWith( cluster, '1.') ) %>% arrange(id) %>% View


assess_cluster_optics( min_staypoint_time = min_staypoint_time, max_staypoint_distance = max_staypoint_distance )


df_matching_survey_per_staypoint_df_matching_survey_optics_distance_600_20_interpolated_locations_300_filtered_accuracy_100 )

df = readd(df_matching_survey_optics_distance_600_20_interpolated_locations_300_filtered_accuracy_100 )
df = readd(optics_distance_600_20_interpolated_locations_300_filtered_accuracy_100 )
df = readd(interpolated_locations_300_filtered_accuracy_100 )


readd(df_matching_survey_per_staypoint_df_matching_survey_optics_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)
readd(df_matching_survey_optics_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)
readd(df_matching_survey_staypoints_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)
readd(df_matching_survey_per_staypoint_df_matching_survey_optics_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)
