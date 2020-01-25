################################################################################
# plot_one_person_survey
# plot the timestamp duration ranges of a user's staypoints, and their corresponding survey points
################################################################################
plot_one_person_survey = function(df, df_location, df_staypoints, df_surveys, min_accuracy){
#browser()
  a=RColorBrewer::brewer.pal(10,'Spectral')

  df %>%
    inner_join( df_location ) %>%
    filter( accuracy <= min_accuracy) %>%
    { . } -> df_1_loc

  df_1_loc %>%
    summarise( start_ts  = min(timestamp)) %>%
    pluck('start_ts') %>%
    { . } -> start_ts

  df_1_loc %>%
    mutate(timestamp = (timestamp - start_ts) / 3600 ) %>%
    { . } -> df_1_loc

  df %>%
    inner_join( df_staypoints, by = c("userid", "night") ) %>%
    mutate_at( vars(ends_with('ts')), function(ts) { (ts - start_ts) / 3600  })  %>%
    { . } -> df_1_sp

  df %>%
    inner_join( df_surveys )  %>%
    mutate_at( vars(ends_with('timestamp')), function(ts) { (ts - start_ts) / 3600  })  %>%
    { . } -> df_1_surveys


  title = paste( df$userid, df$night  )

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
    mutate( which_sp = paste('sp', n_staypoint)) %>%
    mutate( ts = (max_ts - min_ts)/2 + min_ts ) %>%
    ggplot( aes( ts, which_sp, color=which_sp  )) +
    geom_errorbarh( aes( xmax = max_ts, xmin=min_ts)) +
    geom_point( aes( timestamp, 'surveys'), color='black', shape=3, data=df_1_surveys) +
    geom_point( aes( timestamp, 'gps'), color='green', shape=3, data=df_1_loc)  +
    ggtitle( title ) %>%
    { . } -> p

p- sadgg
print(p)


}


