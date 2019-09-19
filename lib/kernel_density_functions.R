
test_interpolate_points = function() {
  interpolate_points(0, 10, 1, 1,1, 10,10 )
}

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




test_interpolate_night = function() {
  #def interpolate(self, data_fc, dummy_fc, max_delay, freq, max_drop_time = 1, max_distance, verbose=True):
  max_delay=120
  period=1
  max_drop_time = 1
  max_distance = 100
  interpolate_locations( b,  max_delay , period, max_drop_time, max_distance )  -> d

  interpolate_locations( df_location,  max_delay , period, max_drop_time, max_distance )  -> a

}

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
#find_significant_densities 
################################################################################

find_significant_densities = function( ll, xmin = NA, xmax = NA, contigious_range=1) {

  #  H = structure(c(1.23096435484775e-07, -4.58028260474003e-08, -4.58028260474003e-08,
  #                1.94021167344124e-08), .Dim = c(2L, 2L))

  if (is.na( xmin )) {
    #kfs.ll = kfs( ll, H=H )
    kfs.ll = kdde( ll, deriv.order=0 )
  } else {
    gridsize = c(
                 (max( ll$latitude ) - min( ll$latitude ))  * m_per_latitude / desired_grid, 
                 (max( ll$longitude ) - min( ll$longitude ))  * m_per_longitude / desired_grid)
    kfs.ll = kdde( ll, xmin=xmin, xmax=xmax, deriv.order=0)
  }
  #  browser()

  range_lat = kfs.ll$eval.points[[1]][2] - kfs.ll$eval.points[[1]][1] 
  range_lon = kfs.ll$eval.points[[2]][2] - kfs.ll$eval.points[[2]][1] 

  which( kfs.ll[['estimate']]==1, arr.ind=TRUE) %>%
    as_tibble() %>%
    arrange( row, col ) %>%
    mutate( contigious = !(lead(row, default=999999)-row <=contigious_range
                           & abs( lead(col, default=999999) - col ) <=contigious_range)) %>%
    mutate( feature_group = cumsum(lag( contigious, default=1))) %>%
    group_by( feature_group )  %>%
    summarise( 
              min_lat = kfs.ll$eval.points[[1]][ min( row )],
              max_lat = kfs.ll$eval.points[[1]][ max( row )],
              min_lon = kfs.ll$eval.points[[2]][ min( col )],
              max_lon = kfs.ll$eval.points[[2]][ max( col )]) %>% 
    mutate( range_lat = range_lat ) %>%
    mutate( range_lon = range_lon ) %>%
    mutate( original=list(ll), 
           kfs = list(kfs.ll)) %>%
    { . } -> kfs.ll.grouped

  kfs.ll.grouped
}



