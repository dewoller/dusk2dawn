

################################################################################
#test_interpolate_points 
################################################################################

test_interpolate_points = function() {
  interpolate_points(0, 10, 1, 1,1, 10,10 )
}


################################################################################
#interpolate_points 
################################################################################

interpolate_points = function( timestamp, interval, period, longitude, latitude, longitude_next, latitude_next ) {
  #browser()
  n = as.integer( interval / period )
  points = 0:n
  latitude_incr = (latitude_next - latitude) / n
  longitude_incr = (longitude_next - longitude) / n

  tibble( 
         timestamp = timestamp+(period * points),
         latitude = latitude + ( latitude_incr * points),
         longitude = longitude + ( longitude_incr * points)
         ) %>% list()
}



################################################################################
#test_interpolate_night 
################################################################################


test_interpolate_night = function() {
  #def interpolate(self, data_fc, dummy_fc, max_delay, freq, max_drop_time = 1, max_distance, verbose=True):
  max_delay=120
  period=1
  max_drop_time = 1
  max_distance = 100
  interpolate_locations( b,  max_delay , period, max_drop_time, max_distance )  -> d

  interpolate_locations( df_location,  max_delay , period, max_drop_time, max_distance )  -> a

}

################################################################################
#interpolate_locations 
################################################################################

interpolate_locations = function( df,  max_delay = 120, period = 1, max_drop_time = 1, max_distance = 100 ) {
  # browser()

  df %>%
    dplyr::select( userid, night, latitude, longitude, timestamp) %>%
    group_by( userid, night ) %>%
    arrange(timestamp) %>%
    mutate( interval = lead(timestamp ) - timestamp,
           latitude_next = lead( latitude), 
           longitude_next = lead(longitude)) %>%
    filter( interval > max_delay & interval < max_drop_time * 3600) %>%
    drop_na( latitude_next, longitude_next ) %>%
    filter( raster::pointDistance( cbind(longitude, latitude), 
                          cbind(longitude_next, latitude_next), 
                          lonlat=TRUE ) < max_distance ) %>%
    rowwise() %>%
    mutate( new_points = interpolate_points( timestamp, interval, period, longitude, latitude, longitude_next, latitude_next ) ) %>%
    dplyr::select( new_points, userid, night ) %>%
    unnest( new_points) %>% 
    { . } -> df_new_points

  bind_rows( dplyr::select( df, userid, night, latitude, longitude, timestamp), df_new_points )
}

################################################################################
# find_meanshift_mode
################################################################################
find_meanshift_mode = function( df_location, min_staypoint_time, max_staypoint_distance, iterations=100, hm=10,ht = 600 ) {

  df_location %>%
    mutate( 
           m_lat = ll2m( latitude, min(latitude), m_per_latitude),
           m_lon = ll2m( longitude, min(longitude), m_per_longitude)
           ) %>%
    group_by( userid, night) %>%
    group_modify( ~find_meanshift_mode_single_night (.x, min_staypoint_time, max_staypoint_distance, iterations, hm,ht)) %>%
    filter( n_staypoint > 0) 
}
################################################################################
#find_meanshift_mode_single_night
################################################################################

find_meanshift_mode_single_night = function( df_location, min_staypoint_time = 600 , max_staypoint_distance = 10 , iterations, hm,ht  ) {


df_location %>%
    mutate(ts = timestamp - min(timestamp)) %>%
    dplyr::select( m_lat, m_lon, ts ) %>%
    as.matrix() %>%
    meanShiftR::meanShift(queryData=., trainData= ., iterations=iterations, bandwidth=c( hm, hm, ht)) %>% 
    { . } -> f_ts1

df_location %>%
    bind_cols( list(label=f_ts1$assignment[,1], e_lat = f_ts1$value[,1], e_lon = f_ts1$value[,2] )) %>%
    group_by(label) %>%
    mutate( mn_lat = mean( m_lat) ) %>%
    mutate( mn_lon = mean( m_lon) ) %>%
    ungroup() %>%
    mutate( dist = pdist(.[ c('m_lon', 'm_lat')], .[ c('mn_lon', 'mn_lat')])) %>%
    filter( dist < max_staypoint_distance ) %>%
    mutate( label = as.factor(label) ) %>%
    group_by(label) %>%
    mutate( duration = max(timestamp)-min(timestamp) ) %>% 
    filter( duration > min_staypoint_time ) %>%
    ungroup() %>%
    mutate( n_staypoint = as.numeric( label)) %>%
    dplyr::select( -label )
}


################################################################################
#pdist 
################################################################################


pdist = function( x, y ) {
  unlist(((x[1] - y[1])^2 + (x[2]-y[2])^2 ) ^.5 )
}

################################################################################
#find_significant_densities 
################################################################################



