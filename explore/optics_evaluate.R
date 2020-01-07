
if(FALSE) {

  df_location %>%
    head(1) %>%
    select(userid, night) %>%
    inner_join( df_location) %>%
    select( longitude, latitude, timestamp) %>%
    { . } -> traj

  options(error=recover)
    options(error=stop)

    stdbscan( traj, 50/111320, 3600/2, 100) -> a

    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }


  traj %>%
    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }

  df_location %>%
    distinct( userid, night ) %>%
    head(2) %>%
    tail(1) %>%

    readd( interpolated_locations_120_filtered_accuracy_100 ) %>%
    inner_join( df_location %>%
        distinct( userid, night ) %>%
        head(4) %>%
#tail(1) %>%
        select(userid, night) ) %>%
    { . } -> b

  cache %>%
    filter(endsWith(value, '300_100_interpolated_locations_120_filtered_accuracy_100')) %>%
    filter(!str_detect(value, 'count')) %>%
    filter(!str_detect(value, 'geography')) %>%
    filter(!str_detect(value, 'mode')) %>%
    filter(str_detect(value, 'df_matching_survey_summarised')) %>%
    pluck('value') %>%



# find the 2 surveys that have the most disparity
readd( df_matching_survey_per_staypoint_df_matching_survey_optics_distance_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
mutate(algo='optics') %>%
bind_rows( readd(df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
          mutate(algo='sp')) %>%
count( userid, night, algo) %>%
spread(algo, n, fill=0) %>%
arrange( optics-sp) %>%
group_by( userid, night) %>%


find the actual points of the surveys that have disparity


  df = readd( interpolated_locations_120_filtered_accuracy_100)

find_cluster_optics_all(df) -> df_all_optics

df %>%
  filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
  arrange( timestamp, .by_group = TRUE) %>%
  mutate( id = row_number()) %>%
  find_cluster_optics_single(min_staypoint_time = 10 , max_staypoint_radius = 100 )  %>%
  {.} -> df_optics

  df_optics %>%
    count( cluster)

  df %>%
    filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    find_staypoint_distance_night(60, min_staypoint_time = 10 , max_staypoint_distance=200 )  %>%
    
    {.} -> df_optics

readd( df_matching_survey_per_staypoint_df_matching_survey_optics_distance_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
mutate(algo='optics') %>%
bind_rows( readd(df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
mutate(algo='sp')) %>%
count( userid, night, algo) %>%
spread(algo, n, fill=0) %>%
arrange( optics-sp) %>%
group_by( userid, night) %>%


    b %>%
#  arrange(timestamp) %>%
    select( longitude, latitude) %>%
    mutate_all(normalize) %>%
    { . } -> x
  x %>%
    optics( ) %>%
    { . } -> a

  extractXi(a, .5) ->a; a$clusters_xi
    hullplot(x, a)


    res <- extractDBSCAN(a, eps_cl = .065)
    res
    plot(res)  ## black is noise
    hullplot(x, res)

### re-cut at a higher eps threshold
    res <- extractDBSCAN(res, eps_cl = .01)
    res
    plot(res)
    hullplot(x, res$cluster)


### extract hierarchical clustering of varying density using the Xi method
    res <- extractXi(res, xi = 0.2)
    res
    plot(res)
    hullplot(x, res)



    b %>%
    mutate( cluster = res$cluster ) %>%
    { . } -> d

  a

    df1 %>%
    filter( cluster > 0 ) %>%
    ggplot( aes( latitude, longitude, color=as.factor(cluster))) +
    geom_point()

    df %>%
    filter( cluster > 0 ) %>%
    mutate(
        m_lat = ll2m( latitude, min(latitude), m_per_latitude),
        m_lon = ll2m( longitude, min(longitude), m_per_longitude)) %>%
    ggplot( aes( m_lat, m_lon, color=as.factor(cluster))) +
    geom_point()


# greedy algorithm;  extract the longest group of points that fits within the constrained area.  Repeat

    b
    do1( b, .0001, 5,1000)

    a=tibble()
    for ( eps_cl in rev(1:100/1000)) {
      a %>% bind_rows( do1( b, eps_cl, min_staypoint_time = 10, max_staypoint_distance = 500 )   )  %>%
      { . } -> a
    }
}


