cached() %>% enframe() -> cache



2020-01-23

cache %>%
  filter( str_detect( value, 'staypoints_distance_14400_300_20_filtered_sigma.v2_0.5$'))


1   512 df_count_staypoints_per_algorithm_df_count_staypoints_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
2  1161 df_count_staypoints_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
3  1829 df_matching_geography_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
4  2298 df_matching_geography_summarised_df_matching_geography_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5

5  2941 df_matching_survey_categories_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5

6  3566 df_matching_survey_categories_summary_df_matching_survey_categories_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
7  8162 df_matching_survey_per_sp_summarised_df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
8  8822 df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
9  9822 df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
10 11263 df_matching_survey_summarised_df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
11 12047 df_matching_survey_summarised_df_matching_survey_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
12 16858 df_summarise_staypoints_staypoints_distance_14400_300_20_filtered_sigma.v2_0.5
1



match to 4sq

match with semantic location - staypoints


4sq venu


userid, night, sp, semantic label, 4sq label, time start, time end, gps center




if endofsp close enough to start of next staypoint and  this staypoing and the same place (???  Center within same radius, same time )


m_distm = function (a,b, fun) {
browser()
distm(a,b, fun=distHaversine)

}

df_missing_summary %>%
  head(6) %>%
  tail(1) %>%
  inner_join(df_best, by =c("userid", "night") ) %>%
  group_by( n_staypoint)  %>%
  summarise( latitude = mean(latitude), longitude = mean(longitude), min_ts = min(min_ts), max_ts = max(max_ts)) %>%
  ungroup() %>%
  arrange(min_ts) %>%
  mutate( llat = lead(latitude),
         llon = lead( longitude),
        lts = lead(min_ts)) %>%
  rowwise() %>%
  mutate( dist_offset = distm(c(llon, llat), c(longitude, latitude), fun=distHaversine) ,
         time_offset = (lts - max_ts)/60
         ) 

df_missing_summary %>%
  head(10) %>%
  tail(1) %>%
  inner_join(df_best, by =c("userid", "night") ) %>%
  group_by( userid, night, n_staypoint)  %>%
  summarise( latitude = mean(latitude), longitude = mean(longitude), min_ts = min(min_ts), max_ts = max(max_ts)) %>%
  ungroup() %>%
  inner_join(df_surveys_missed) %>%
  select(userid, id, night, n_staypoint, ends_with('itude'), ends_with('_ts'), timestamp) %>%
  group_by(id) %>%
  arrange(min_ts, .by_group=TRUE) %>%
  mutate(nts = lead(min_ts)) %>%
  filter(  timestamp > max_ts & timestamp < nts ) %>%
  mutate( n1 = (timestamp - max_ts)/60, n2 = (nts -timestamp )/60) %>%
  select(n1,n2,everything()) %>%
  ungroup()





  distinct( n_staypoint) %>%
  count()

