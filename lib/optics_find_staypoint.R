
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
  a %>% bind_rows( do1( b, eps_cl, min_staypoint_time = 10, max_staypoint_distance = 500 )   )  %>% 
  { . } -> a
        }
}


# testing codde starts here

if(FALSE) {


  df=d%>% arrange(timestamp) %>% mutate( id = row_number()) 
  spos = 1
  epos = nrow(df)
  min_staypoint_time = 10
  max_staypoint_distance = 1000 
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
                                 maxgap=300))  %>%
ungroup()

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

  min_staypoint_time = 100
 max_staypoint_distance = 900

  df = readd(interpolated_locations_120_filtered_accuracy_10)


 df = readd(interpolated_locations_120_filtered_accuracy_10)
df_summarise_staypoints_optics_distance_900_100_interpolated_locations_
 cached() %>% enframe() -> cache

 cache %>% 
   filter( startsWith( value,  'optics_distance_900_100_interpolated_locations_120'))

readd('optics_distance_900_100_interpolated_locations_120_filtered_accuracy_10')
readd('optics_distance_900_100_interpolated_locations_120_filtered_accuracy_100')
optics_distance_900_100_interpolated_locations_120_filtered_accuracy_100
df = readd( interpolated_locations_120_filtered_accuracy_100)

find_cluster_optics(df , 900, 100)

  df %>% 
    distinct( userid, night) %>%
    head(4) %>%
    tail(1) %>%
    inner_join( df ) %>% 
    { . } -> df


  df %>%
    group_by( userid, night ) %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>% 
    ungroup() %>%
    { . } -> df1


find_cluster_optics_single(df1 ,min_staypoint_time = min_staypoint_time , max_staypoint_distance = max_staypoint_distance )


}
#################################################################################
# find_cluster_optics_all
#################################################################################

find_cluster_optics_all = function( df, min_staypoint_time = 10, max_staypoint_distance = 1000)   {
  flog.threshold(WARN)

  df %>%
    group_by( userid, night ) %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>%
    do( find_cluster_optics_single( . ,min_staypoint_time = min_staypoint_time , max_staypoint_distance = max_staypoint_distance )) %>%
    group_by( userid, night ) %>%
    mutate( n_staypoint = row_number()) %>%
    ungroup() %>% 
    { . } -> clusters

df$n_staypoint=0
  for( i in 1:nrow(clusters)) {
    df[ pluck(clusters,'ids', i, 'id' ), ]$n_staypoint= pluck( clusters,'n_staypoint', i)
  }

df
}


#################################################################################
# find_cluster_optics_single 
#################################################################################
find_cluster_optics_single_test = function( ) {

  readd(interpolated_locations_120_filtered_accuracy_10) ->a
  a %>% inner_join( a %>% distinct( userid, night ) ) %>%
    arrange( timestamp, .by_group = TRUE) %>%
    mutate( id = row_number()) %>% 
    { . } -> df

 spos = 1
 epos = nrow(df)
 min_staypoint_time = 10
 max_staypoint_distance = 1000
 eps_cl_index = 1 

}
  #################################################################################
find_cluster_optics_single = function( df, spos = 1, epos = nrow(df), min_staypoint_time = 10, max_staypoint_distance = 1000, eps_cl_index = 1 )   {

  rv=tibble()
  # return if epos or spos messed up
  if (epos <= spos | epos > nrow(df) | spos <=0 ) {
    return( rv )
  }

  # return if end - start could not be long enough for a staypoint
  if ((pluck(df, 'timestamp', epos ) - pluck(df, 'timestamp', spos ) )/60 < min_staypoint_time) {
    return( rv )
  }

  found=FALSE
  flog.info("Starting find_cluster_optics_single userid %s night %s spos %s epos %s  esp_cl %s ", pluck(df, 'userid', 1), pluck(df, 'night', 1), spos, epos, eps_cl_levels[ eps_cl_index])
  while(!found & eps_cl_index <= length( eps_cl_levels)) {
    flog.debug("inside find chunks loop, spos %s epos %s  esp_cl %s ", spos, epos, eps_cl_levels[ eps_cl_index])

    #browser()
    # find clusters and assess goodness 
    rv = assess_eps_level_optics( df, spos, epos, eps_cl_levels[ eps_cl_index], min_staypoint_time,  max_staypoint_distance)

    # if we have good clusters
    if (nrow(rv) > 0) {
      flog.info("we found %s good clusters, dive into remainders ", nrow(rv))
      found=TRUE
      downstream_chunks = tibble()

      # assess each chunk prior to this chunk
      for(i in 1:nrow(rv)) {
        flog.info("looking at the bits around cluster # %s, running from %s to %s", 
                  i, pluck(rv, 'cluster_spos', i) , pluck(rv, 'cluster_epos', i) )
        flog.info("investigating bit before %s th chunk, starting with %s", i, spos)
        next_chunks = find_cluster_optics_single( df, spos=spos, 
                                                 epos = pluck(rv, 'cluster_spos', i) - 1, 
                                                 min_staypoint_time, 
                                                 max_staypoint_distance, eps_cl_index)
        downstream_chunks = bind_rows( downstream_chunks, next_chunks )
        spos = pluck(rv,  'cluster_epos', i) + 1
      }
      flog.info("investigating last chunk, starting with %s to %s",  spos, epos)
      next_chunks = find_cluster_optics_single( df, spos=spos, 
                                               epos = epos , 
                                               min_staypoint_time, 
                                               max_staypoint_distance, eps_cl_index)
      downstream_chunks = bind_rows( downstream_chunks, next_chunks )
      rv = bind_rows( rv, downstream_chunks)
    }
    eps_cl_index = eps_cl_index + 1
  }
  rv 
}



#################################################################################
# optics_plot
#################################################################################


optics_plot = function( df, eps_cl, min_staypoint_time = 10)   {

  df %>%
    mutate( cluster = calculate_clusters_optics(., eps_cl, min_staypoint_time ), eps_cl = eps_cl) %>%
    group_by(cluster) %>%
    arrange( id, .by_group = TRUE) %>%
    # do we have a break in the cluster of sufficient size
    mutate( newcluster = cumsum( id != lead( id, default=0 ) - 1 &
                                (lead(timestamp, default=0) - timestamp)/60  > max_discontinuity
                              )) %>%
    ungroup() %>%
    mutate( cluster = paste0( cluster, '.', newcluster)) %>%
    dplyr::select(-newcluster) %>%
    mutate( 
           m_lat = ll2m( latitude, min(latitude), m_per_latitude),
           m_lon = ll2m( longitude, min(longitude), m_per_longitude)) %>%
    ggplot( aes( m_lat, m_lon, color=as.factor(cluster))) +
    geom_point()
}


#################################################################################
# find_cluster_optics_single 
#################################################################################


assess_eps_level_optics = function( df, spos = 1, epos = nrow(df), eps_cl, min_staypoint_time = 10, max_staypoint_distance = 1000 )   {
  flog.info("Assessing, spos %s, epos %s, eps_cl %s", spos, epos, eps_cl)

  df %>%
    filter( id >= spos & id <= epos ) %>% 
    mutate( cluster = calculate_clusters_optics(., eps_cl, min_staypoint_time ), eps_cl = eps_cl) %>%
    group_by(cluster) %>%
    arrange( id, .by_group = TRUE) %>%
    # do we have a break in the cluster of sufficient size
    mutate( newcluster = cumsum( id != lead( id, default=0 ) - 1 &
                                (lead(timestamp, default=0) - timestamp)/60  > max_discontinuity
                                )) %>%
    ungroup() %>%
    mutate( cluster = paste0( cluster, '.', newcluster)) %>%
    dplyr::select(-newcluster) %>%
    assess_cluster_optics( min_staypoint_time = min_staypoint_time, max_staypoint_distance = max_staypoint_distance ) 

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
#################################################################################

calculate_clusters_optics = function( df, eps_cl, min_staypoint_time ) {
  dbscan_minimum_points_error_threshold = 5
  flog.info("Calculating Clusters, df length %s, ranging from %s to %s, eps_cl=%s  ", nrow(df), min(df$id), max(df$id ), eps_cl)

  if( ((max( df$timestamp ) - min( df$timestamp)) / 60 < min_staypoint_time  ) |
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
  flog.info("Assessing Clusters,  nrow(df) = %s, ncluster= %s", nrow(df), max( df$cluster))

  df %>%
    filter( cluster > 0 ) %>%
    mutate( 
        m_lat = ll2m( latitude, min(latitude), m_per_latitude),
        m_lon = ll2m( longitude, min(longitude), m_per_longitude)) %>%
    group_by( cluster,eps_cl) %>%  # eps_cl is a constant, but we want to bring it out
    summarise( 
              w=max(m_lat) - min(m_lat),
              h = max(m_lon) - min(m_lon), 
              t = (max(timestamp) - min(timestamp)) / 60,
              max_ts = max(timestamp) ,
              min_ts = min(timestamp), 
              cluster_spos = min(id), 
              cluster_epos = max(id)
    )  %>%
    filter( t >= min_staypoint_time &
           (w <= max_staypoint_distance & h <= max_staypoint_distance)  ) %>%
    ungroup() %>%
    top_n(1, t) %>% 
    head(1) %>% 
    inner_join( df %>% dplyr::select( cluster, id ), by='cluster') %>% 
    nest( ids=c(id) )



}
