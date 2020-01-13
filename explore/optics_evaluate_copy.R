

if(FALSE) {

  cached() %>%
    enframe() %>%
    { . } -> cache

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


#find the actual points of the surveys that have disparity
# optics_distance_600_20_interpolated_locations_300_filtered_accuracy_100

df = readd( interpolated_locations_300_filtered_accuracy_100 )

df = readd( interpolated_locations_120_filtered_accuracy_100)

find_cluster_optics_all(df, 3600, 600, 200) -> df_all_optics

find_staypoint_distance_night(3600, min_staypoint_time = 600 , max_staypoint_distance=200 )  %>%

df_all_optics %>%
#filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
 count( n_staypoint,sort=TRUE) %>%

df %>%
  filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
  arrange( timestamp, .by_group = TRUE) %>%
  mutate( id = row_number()) %>%
  find_cluster_optics_single(min_staypoint_time = 10 , max_staypoint_radius = 100 )  %>%
  {.} -> df_optics


df %>%
  distinct( userid, night ) %>t
  count( night) %>%
  filter( n<10) %>%
    arrange(desc(n))

df %>%
  filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
  find_cluster_optics_all(min_staypoint_time = 10 , max_staypoint_radius = 100 ) %>%
  { . } -> eee

eee %>% count( n_staypoint,sort=TRUE)

df %>%
  filter( night=='2014-12-05') %>%
find_cluster_optics_all() %>%
{ . } -> eee

  find_cluster_optics_single(min_staypoint_time = 10 , max_staypoint_radius = 100 )  %>%


df %>%
  group_by( userid, night ) %>%
  arrange( timestamp, .by_group = TRUE) %>%
  mutate( id = row_number()) %>%
  { . } -> dfaaa

df_optics %>%
  mutate( n_staypoint = row_number()) %>%
  unnest(ids) %>%
  inner_join(
             df %>%
               filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
               arrange( timestamp, .by_group = TRUE) %>%
               mutate( id = row_number()) ,
             by='id'
             ) %>%
{.} -> df_optics_n

  df_optics %>%
    count( cluster)

  df %>%
    filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    find_staypoint_distance_night(3600, min_staypoint_time = 600 , max_staypoint_distance=200 )  %>%
    {.} -> df_staypoint

  df_staypoint %>%
    filter(n_staypoint > 0 ) %>%
    { . } -> df_staypoint_n

  df_staypoint_n %>% count( n_staypoint,sort=TRUE)
  df_optics_n %>% count( n_staypoint,sort=TRUE)

  df_staypoint_n %>%
    group_by( n_staypoint )
    count( n_staypoint,sort=TRUE)

  df_optics_n %>%
    ggplot( aes( latitude, longitude, color=as.factor(n_staypoint))) +
    geom_point()

  df_staypoint_n %>%
    ggplot( aes( latitude, longitude, color=as.factor(n_staypoint))) +
    geom_point()



# match optics and staypoint, comparing found surveys
readd( df_matching_survey_per_staypoint_df_matching_survey_optics_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
mutate(algo='optics') %>%
bind_rows( readd(df_matching_survey_per_staypoint_df_matching_survey_staypoints_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
    mutate(algo='sp')) %>%
count( userid, night, algo) %>%
spread(algo, n, fill=0) %>%
arrange( optics-sp) %>%
arrange( sp - optics) %>%
summarise( sum(optics), sum(sp)) %>%
group_by( userid, night) %>%


#cache %>% filter( str_detect( value, 'df_all_summarise_staypoints')) %>%
# match optics and staypoint methods, comparing found staypoints

readd(df_all_summarise_staypoints) %>%
filter( str_detect( source, 'distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100$') ) %>%
mutate( algo = ifelse( str_detect( source, 'optics_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100$'), 'optics','sp' )) %>%
count( userid, night, algo) %>%
spread(algo, n, fill=0) %>%
arrange( optics-sp) %>%
filter( userid=='7907f345-ef4b-412a-9340-b56ebb589cca' & night=='2014-09-19') %>%


readd( df_matching_survey_optics_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
mutate(algo='optics') %>%
bind_rows( readd(df_matching_survey_staypoints_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100) %>%
          mutate(algo='sp')) %>%
count( userid, night, algo) %>%
spread(algo, n, fill=0) %>%
arrange( optics-sp) %>%
arrange( sp - optics) %>%
summarise( sum(optics), sum(sp)) %>%
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


