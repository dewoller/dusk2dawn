
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
  filter(endsWith(value, 'interpolated_locations_120_filtered_accuracy_100'))


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

d %>%
  filter( cluster > 0 ) %>%
  ggplot( aes( latitude, longitude, color=as.factor(cluster))) +
  geom_point()

d %>%
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
  a %>% bind_rows( do1( b, eps_cl, min_staypoint_time = 600, max_staypoint_distance = 500 )   )  %>%
  { . } -> a
        }
}


# testing codde starts here

if(FALSE) {


  df=d%>% arrange(timestamp) %>% mutate( id = row_number())
  spos = 1
  epos = nrow(df)
  min_staypoint_time = 600
  max_staypoint_distance = 500
  eps_cl=80


  #tessting code

  d = bind_rows( b, b%>% mutate(timestamp = timestamp + 36000))
flog.threshold(DEBUG)
find_cluster_optics_single(b %>% arrange(timestamp) %>% mutate( id = row_number()) , eps_cl_index=28)

find_cluster_optics_single(d %>% arrange(timestamp) %>% mutate( id = row_number()) , eps_cl_index=1)

d %>% arrange(timestamp) %>% mutate( id = row_number())  %>% optics_plot( eps_cl=100)

find_cluster_optics_all(b) -> a

count(a, userid, night, cluster)




do( joined = interval_inner_join( data.frame(.$surveys),
                                 data.frame(.$staypoints),
                                 by=c('start','end'),
                                 maxgap=600))  %>%
ungroup()


readd(interpolated_locations_300_filtered_accuracy_10) %>%
{ . } -> df


    find_cluster_optics_all ( df, 14400, 900, 20) %>%
    { . } -> df_optics

max_jump_time = 14400
min_staypoint_time = 900
max_staypoint_distance = 20
}

# global variables
eps_seq = function( x ) rev(1:10*x )
eps_cl_levels = c( eps_seq(100), eps_seq(10), eps_seq(1), eps_seq(.1), eps_seq(.01), eps_seq(.001)  )
max_discontinuity = 60

#################################################################################
# find_cluster_optics_all_test
#################################################################################
find_cluster_optics_all_test = function( df)   {
  options(error=recover)
  options(error=stop)

  flog.threshold(DEBUG)
  flog.threshold(WARN)

  min_staypoint_time = 600
 max_staypoint_distance = 500

  df = readd(interpolated_locations_120_filtered_accuracy_10)

 readd(df_matching_survey_optics_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)

 readd(df_matching_survey_optics_distance_14400_600_20_interpolated_locations_600_filtered_accuracy_10)

 find_cluster_optics_all(df, 14400,600,20) %>%
   { . } -> clusters

 df = readd(interpolated_locations_600_filtered_accuracy_10)
df_summarise_staypoints_optics_distance_900_100_interpolated_locations_
 cached() %>% enframe() -> cache

 cache %>%
   filter( startsWith( value,  'optics_distance_900_100_interpolated_locations_120'))


readd('optics_distance_900_100_interpolated_locations_120_filtered_accuracy_10')

readd("optics_distance_14400_300_100_interpolated_locations_120_filtered_accuracy_100") %>% 
{ . } -> df

readd("interpolated_locations_120_filtered_accuracy_100") %>% 
  { . } -> df


find_cluster_optics_all(df, 14400, 300, 100) %>%
{ . } -> clusters


# winnow out first 4 user nights
  df %>%
    distinct( userid, night) %>%
    head(4) %>%
    tail(1) %>%
    inner_join( df ) %>%
    { . } -> df

  df = readd( interpolated_locations_120_filtered_accuracy_100)

  df %>%
    filter( userid=='6abb3992-29f1-4d36-a9dd-1c67b258a8da' & night=='2014-10-24') %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    { . } -> df1


find_cluster_optics_single(df1 )  %>%
{.} -> d



  flog.threshold(DEBUG)

}
#################################################################################
# find_cluster_optics_all
#################################################################################

find_cluster_optics_all = function( df, max_jump_time = 3600, min_staypoint_time = 600, max_staypoint_distance = 500)   {
  flog.threshold(INFO)

#  TRACE, DEBUG, INFO, WARN, ERROR, FATAL
  df %>%
    arrange( userid, night,  timestamp) %>%
    mutate( id = row_number()) %>%
    { . } -> df_id

df_id %>%
    group_by( userid, night ) %>%
    group_modify( ~find_cluster_optics_single( .x ,
                                              max_jump_time = max_jump_time,
                                              min_staypoint_time = min_staypoint_time ,
                                              max_staypoint_distance = max_staypoint_distance ),
                 keep=TRUE) %>%
    group_by( userid, night ) %>%
    mutate( n_staypoint = row_number()) %>%
    ungroup() %>%
    { . } -> df_test


  if (nrow( df_test) > 0 ) {
    df_test %>%
      unnest( cols=c(ids)) %>%
      { . } -> df_test
  } else {
      df_test %>%
        select(-ids) %>%
        mutate(id=0) %>%
        { . } -> df_test
  }

  df_test %>%
    inner_join( df_id, by=c('userid','night','id')) %>%
    { . } -> df_clusters

df_clusters
}


#################################################################################
# find_cluster_optics_single_test
#################################################################################
find_cluster_optics_single_test = function( ) {

  readd(interpolated_locations_120_filtered_accuracy_10) ->a
  a %>% inner_join( a %>% distinct( userid, night ) ) %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    { . } -> df

 spos = 1
 epos = nrow(df)
 min_staypoint_time = 600
 max_staypoint_distance = 600
 eps_cl_index = 1

}


#################################################################################
# find_cluster_optics_single
#################################################################################
find_cluster_optics_single = function( df,
                                        spos = 1,
                                        epos = nrow(df),
                                        max_jump_time = 3600,
                                        min_staypoint_time = 600,
                                        max_staypoint_distance = 500,
                                        eps_cl_index = 1 )   {

  rv=tibble()
  # return if epos or spos messed up
  if (epos <= spos | epos > nrow(df) | spos <=0 | nrow(df) == 0 ) {
    return( rv )
  }

  # return if end - start could not be long enough for a staypoint
  if ((pluck(df, 'timestamp', epos ) - pluck(df, 'timestamp', spos ) ) < min_staypoint_time) {
    return( rv )
  }

  found=FALSE
  flog.info("Starting find_cluster_optics_single userid %s night %s spos %s epos %s  esp_cl %s ", pluck(df, 'userid', 1), pluck(df, 'night', 1), spos, epos, eps_cl_levels[ eps_cl_index])
  while(!found & eps_cl_index <= length( eps_cl_levels)) {
    flog.debug("inside find chunks loop, spos %s epos %s  esp_cl %s ", spos, epos, eps_cl_levels[ eps_cl_index])

    # find all the clusters at this level, and choose the the best cluster
    df %>%
      assess_eps_level_optics( spos, epos, eps_cl_levels[ eps_cl_index], min_staypoint_time = min_staypoint_time ) %>%
      assess_cluster_optics( min_staypoint_time = min_staypoint_time, max_staypoint_distance = max_staypoint_distance ) %>%
      {.} -> rv


    # if we have good clusters, otherwise, just keep refining the eps_cl_index
    if (nrow(rv) > 0) {
      flog.trace("we found %s good clusters, dive into remainders ", nrow(rv))
      found=TRUE
      downstream_chunks = tibble()
      #browser()

      # we only ever have a single cluster
      cluster_this_min = pluck(rv, 'cluster_min_id', 1)
      cluster_this_max = pluck(rv, 'cluster_max_id', 1)
      cluster_spos = which( df$id == cluster_this_min) - 1
      cluster_epos = which( df$id == cluster_this_max) + 1
      flog.trace("looking at the bits before the cluster, running from ID %s to %s", cluster_this_min, cluster_this_max )
      flog.trace("first, the bit before: running from positin %s to %s", spos, cluster_spos )
      flog.trace("and, the bit after: running from positin %s to %s",  cluster_epos, epos )

      rv %>%
        bind_rows( find_cluster_optics_single( df, spos=spos,
                                          epos = cluster_spos,
                                          max_jump_time,
                                          min_staypoint_time,
                                          max_staypoint_distance, eps_cl_index) ) %>%
        bind_rows( find_cluster_optics_single( df, spos=cluster_epos,
                                            epos = epos ,
                                            max_jump_time,
                                            min_staypoint_time,
                                            max_staypoint_distance, eps_cl_index) ) %>%
        { . } -> rv
    }
    eps_cl_index = eps_cl_index + 1
  }
  rv
}



#################################################################################
# optics_plot
#################################################################################


optics_plot = function( df,
                       eps_cl = 1000
                       ,
                       min_staypoint_time = 600
                       )   {

  df1 %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    assess_eps_level_optics(eps_cl = eps_cl, min_staypoint_time=min_staypoint_time) %>%
    mutate(
           m_lat = ll2m( latitude, min(latitude), m_per_latitude),
           m_lon = ll2m( longitude, min(longitude), m_per_longitude)) %>%
    ggplot( aes( m_lat, m_lon, color=as.factor(cluster))) +
    geom_point()

}


#################################################################################
# assess_eps_level_optics
#################################################################################


assess_eps_level_optics = function( df,
                                   spos = 1,
                                   epos = nrow(df),
                                   eps_cl,
                                   max_jump_time = 3600,
                                   min_staypoint_time = 600 ) {
  flog.trace("Assessing, spos %s, epos %s, eps_cl %s", spos, epos, eps_cl)


  df %>%
    dplyr::slice( spos:epos ) %>%
    mutate( cluster = calculate_clusters_optics(., eps_cl, min_staypoint_time ),
           eps_cl = eps_cl) %>%
    group_by(cluster) %>%
    arrange( id, .by_group = TRUE) %>%
    # if we have a break in the cluster of sufficient and duration, make a new cluster
    mutate( newcluster = cumsum( id != lag( id, default=0 ) + 1 &
                                (timestamp - lag(timestamp, default=0)) > max_jump_time
                                )) %>%
    ungroup() %>%
    mutate( cluster = paste0( cluster, '.', newcluster)) %>%
    dplyr::select(-newcluster)

}

#################################################################################
# normalize
#################################################################################

normalize <- function(x) {
  denom = max(x)-min(x)
  if (denom==0) {
    return( rep( 0, length(x)))
  }
  return ((x - min(x)) / denom )
}

#################################################################################
# calculate_clusters_optics
# for this selectd chunk of df, are there any clusters at this eps_cl level of at least min_staypoint_time length?
#################################################################################

calculate_clusters_optics = function( df, eps_cl, min_staypoint_time ) {
  dbscan_minimum_points_error_threshold = 5
  flog.trace("Calculating Clusters, df length %s, ranging from IDs %s to %s, eps_cl=%s  ", nrow(df), min(df$id), max(df$id ), eps_cl)

  # to search for clusters, this chunk must be of sufficent duration, and have sufficient points
  if( ((max( df$timestamp ) - min( df$timestamp)) <  min_staypoint_time  ) |
     (nrow(df ) <= dbscan_minimum_points_error_threshold )) {
    return( rep( 0, nrow( df ) ))
  }

  df %>%
    dplyr::select( longitude, latitude) %>%
    mutate_all(normalize) %>%
    optics( ) %>%
    extractDBSCAN(eps_cl = eps_cl ) %>%
    { . } -> res
  res$cluster
}


#################################################################################
# assess_cluster_optics
# return the longest duration cluster that matches the size and duration criteria
#################################################################################

assess_cluster_optics = function( df, min_staypoint_time, max_staypoint_distance  ) {
  ncluster = df %>% distinct( cluster ) %>% nrow()
    flog.trace("Assessing Clusters,  nrow(df) = %s, ncluster= %s", nrow(df), ncluster )

  max_staypoint_diameter = max_staypoint_distance * 2
  # find /the/ best chunk
  df %>%
    filter( cluster > 0 ) %>%
    mutate(
        m_lat = ll2m( latitude, min(latitude), m_per_latitude),
        m_lon = ll2m( longitude, min(longitude), m_per_longitude)) %>%
    group_by( cluster,eps_cl) %>%  # eps_cl is a constant, but we want to bring it out
    summarise(
              w=max(m_lat) - min(m_lat),
              h = max(m_lon) - min(m_lon),
              t = (max(timestamp) - min(timestamp)) ,
              max_ts = max(timestamp) ,
              min_ts = min(timestamp),
              cluster_min_id = min(id),
              cluster_max_id = max(id)
    )  %>%
    filter( t >= min_staypoint_time &
           (w <= max_staypoint_diameter &
            h <= max_staypoint_diameter)  ) %>%
    ungroup() %>%
    top_n(1, t) %>%
    head(1) %>%
    inner_join( df %>% dplyr::select( cluster, id ), by='cluster') %>%
    nest( ids=c(id) ) %>%
    { . } -> rv

flog.trace("Finished assessing Clusters, found %s valid cluster", nrow(rv))
rv


}
