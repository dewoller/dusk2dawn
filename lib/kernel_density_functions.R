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



