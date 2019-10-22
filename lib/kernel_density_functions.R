

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

m_per_latitude = 111320
m_per_longitude = 111319.488
desired_grid = 10 #meters
#

################################################################################
# m2ll 
################################################################################
m2ll = function( m, base_ll, m_per_factor )  {
  m/m_per_factor + base_ll 
}
#
#


################################################################################
# ll2m
################################################################################
ll2m = function( ll, base_ll , m_per_factor )  {
  (ll - base_ll ) * m_per_factor 
}
#


################################################################################
# find_meanshift_mode
################################################################################
find_meanshift_mode = function( df_location, min_staypoint_time, max_staypoint_distance ) {

  df_location %>%
    mutate( 
           m_lat = ll2m( latitude, min(latitude), m_per_latitude),
           m_lon = ll2m( longitude, min(longitude), m_per_longitude)
           ) %>%
    group_by( userid, night) %>%
    group_modify( ~find_meanshift_mode_single_night (.x, min_staypoint_time, max_staypoint_distance)) %>%
    filter( n_staypoint > 0) %>% 
    { . } -> df1

}

################################################################################
#find_meanshift_mode_single_night
################################################################################
debug(find_meanshift_mode_single_night)
min_staypoint_time = 600 
max_staypoint_distance = 10 

find_meanshift_mode_single_night = function( df_location, min_staypoint_time = 600 , max_staypoint_distance = 10   ) {


df_location %>%
    mutate(ts = timestamp - min(timestamp)) %>%
    dplyr::select( m_lat, m_lon, ts ) %>%
    as.matrix() %>%
    meanShiftR::meanShift(queryData=., trainData= ., iterations=100, alpha=0 ) %>% 
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

find_significant_densities = function( ll, xmin = NA, xmax = NA, contigious_range=1) {

    H = structure(c(1.23096435484775e-07, -4.58028260474003e-08, -4.58028260474003e-08,
                  1.94021167344124e-08), .Dim = c(2L, 2L))

  if (is.na( xmin )) {
    kfs.ll = kfs( ll, H=H, gridsize = 300 )
    #kfs.ll = kdde( ll, deriv.order=0 )
  } else {
    gridsize = c(
                 (max( ll$latitude ) - min( ll$latitude ))  * m_per_latitude / desired_grid, 
                 (max( ll$longitude ) - min( ll$longitude ))  * m_per_longitude / desired_grid)
    kfs.ll = kdde( ll, xmin=xmin, xmax=xmax, deriv.order=0)
  }
  #  browser()

  range_lat = kfs.ll$eval.points[[1]][2] - kfs.ll$eval.points[[1]][1] 
  range_lon = kfs.ll$eval.points[[2]][2] - kfs.ll$eval.points[[2]][1] 

  library(raster)
  library(igraph)

  raster::clump( raster( kfs.ll[['estimate']]), directions=8) %>% as.matrix()

  #turn the clumps into a list
  tot <- max(Clumps, na.rm=TRUE)
  res <- vector("list",tot)
  for (i in 1:tot){
    res[i] <- list(which(Clumps == i, arr.ind = TRUE))
}


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


getHighDensitySections_1d = function( x, target_size = 10 ) {
  b=kde( x )
  # move probabilty line up until we get something where
  # the estimated x is within our target_size range
  found = FALSE
  contours = paste0( 1:99, '%')
  contour_idx = 1
  rv = data.frame()
  while (!found & contour_idx <= length(contours)) {
    #message(contour_idx)

    rle(b$estimate >= b$cont[ contours[contour_idx]] ) %>%
      unclass() %>%
      as.data.frame() %>%
      mutate(end = cumsum(lengths),
             start = c(1, dplyr::lag(end)[-1] + 1)) %>%
      filter( values ) %>%
      mutate( sval = b$eval.points[start],
             eval = b$eval.points[end],
             distance = eval - sval
             ) %>%
      filter( distance < target_size ) %>%
      mutate( probability_level = contour_idx ) %>%
      bind_rows( rv) %>%
      { . } -> rv
    contour_idx = contour_idx + 5
  }
  ir <- IRanges(rv$start, rv$end)
  rv$group2 <- subjectHits(findOverlaps(ir, reduce(ir)))

  rv %>%
    group_by( group2 ) %>%
    filter( probability_level == min(probability_level))
}


################################################################################
getHighDensitySections_2d_all = function(  df, target_size=10 ) {

  df %>%
    group_by( userid, night) %>%
    mutate( 
           m_lat = ll2m( latitude, min(latitude), m_per_latitude),
           m_lon = ll2m( longitude, min(longitude), m_per_longitude)
           ) %>%
    do( getHighDensitySections_2d_single( ., target_size ))  %>%
    ungroup()



}


wtf = function () {
  target_size=10 

  df_first10_location_e %>%
    group_by( userid, night) %>%
    mutate( 
          m_lat = ll2m( latitude, min(latitude), m_per_latitude),
          m_lon = ll2m( longitude, min(longitude), m_per_longitude)
          ) %>%
  #  filter( userid == '02f95f41-cc1c-42d9-ab19-ef86a2fbbf4e' & night=='2014-09-27') %>%
    do( getHighDensitySections_2d (., target_size = target_size))  %>% 
    { . } -> a

  str( a$kdes )

  a$kdes[[1]]

  pluck( a, 3, 5, 1)

  a$kdes
}

################################################################################
getHighDensitySections_2d_single = function(  df, target_size=10 ) {

  df %>%
    getHighDensitySections_2d (target_size = target_size) %>% 
    as_tibble(kdes) %>%
    rowwise() %>%
    mutate( m_lat = list( pluck( kdes, 2, 1 ))) %>%
    filter( !is.null( m_lat ) ) %>%
    mutate( m_lon = list( pluck( kdes, 2, 2 ))) %>%
    ungroup() %>%
    dplyr::select( -kdes ) %>%
    mutate( group = row_number() ) %>%
    unnest( m_lat, m_lon ) %>%
    mutate( 
           latitude  = m2ll( m_lat, min(df$latitude), m_per_latitude),
           longitude  = m2ll( m_lon, min(df$longitude), m_per_longitude)
           ) %>%
    group_by( group ) %>%
    dplyr::summarise( latitude = mean(latitude), longitude = mean(longitude)) 

}

################################################################################
target_size = 10; xmin=NA; xmax=NA

getHighDensitySections_2d = function( df, target_size = 10, xmin=NA, xmax=NA ) {


  message(paste( 'xmin=', xmin, collapse=','), 2)
  message(paste( 'xmax=', xmax, collapse=','), 2)
  # dput(xmin)
  # dput(xmax)

  df %>%
    dplyr::select( m_lat, m_lon) %>% 
    { . } -> df_ll

  if( is.na(xmin) ) {
    #kde_ll = kde( list(df$m_lat, df$m_lon))
    kde_ll = kde(df_ll) 
  } else {
    #browser()
    gridsize = max( xmax-xmin ) / target_size 

    df_ll %>%
      filter( m_lat >= xmin[1] & m_lat <= xmax[1] ) %>%
      filter( m_lon >= xmin[2] & m_lon <= xmax[2] ) %>% 
      { . } -> df_ll

    kde_ll = tryCatch( 
                      { kde( df_ll, gridsize=gridsize )  }, 
                      error = function(cond) {return(NA)}
    )

    if (is.logical(kde_ll) & is.na( kde_ll)) {
      return(NA)
    }

  }
  grid_x = kde_ll$eval.points[[1]][2] - kde_ll$eval.points[[1]][1]
  grid_y = kde_ll$eval.points[[2]][2] - kde_ll$eval.points[[2]][1]



  # move probabilty line up until we get something where
  # the estimated x is within our target_size range
  # zoom into the densest regions
  found = FALSE
  contours = paste0( 1:99, '%')
  contour_idx = 1
  contour_step = 5
  rv = data.frame()
  while (!found & contour_idx <= length(contours)) {
    message(paste("inside", contours[contour_idx]), contour_idx)

    # clump the density estimates above the current probability level
    ifelse( kde_ll$estimate >= kde_ll$cont[ contours[contour_idx ] ], 1, 0 ) %>%
      raster() %>%
      raster::clump(directions=8) %>%
      { . } -> clumps

    freq(clumps) %>%
      as_tibble() %>%
      dplyr::filter( !is.na( value)) %>%
      nrow() %>% 
      { . } -> nclump

    # if we have no clumps (from the bottom up), this is a bust, return NA
    # if we have 1 clump of sufficiently small size, yeah!!
    # otherwise, traverse all the clumps, zoom into them

    if (nclump == 0) {
      message(paste('no clumps found at contour ',contour_idx, contours[contour_idx] ))
      found=FALSE
      next
    }


    message(paste("nclump", nclump))
    #browser()

    # if we have a single clump, that is small enough, return it
    # otherwise, zoom out until clump is small enough and return it
    # or zoom into each specific clump
    if( nclump == 1) {

      # find the size of the current area, abort if so small that it crashes
      contourArea =  0
      contourArea =  tryCatch(  {
        contourSizes( kde_ll, cont  = contour_idx, approx=TRUE )
      }, 
      error = function(cond) {return(NA)}
      )
      #message(paste("contourArea", contourArea))

      if( is.na( contourArea)) { 
        contour_idx = contour_idx + contour_step
        next
      }

      if ( contourArea < target_size^2 ) {
        message( paste('found single small clump, size=', contourArea, 'prob=', contour_idx ))
        return(kde_ll)
      }
    } else {

      # we have more than 1 clump, 
      if (contourArea / nclump < target_size ^ 0.5 ) {
        message("we have a unreasonable number of clumps")
      } else {
        # zoom into each clump
        # for each clump, find the extents of the clump

        clumps %>%
          as.matrix() %>% 
          { . } -> clumps_m
        which( !is.na( clumps_m ), arr.ind=TRUE) %>%
          as_tibble() %>%
          bind_cols( clumps_m[ which( !is.na(clumps_m )) ] %>% enframe('id', 'group') ) %>% 
          # find the grid location for this clump point
          mutate(
                 x = kde_ll$eval.points[[1]][.$row], 
                 y = kde_ll$eval.points[[2]][.$col]
                 ) %>%
          group_by( group ) %>%
          summarise(
                    min_x = min(x) - grid_x, 
                    max_x = max(x) + grid_x, 
                    min_y = min(y) - grid_y, 
                    max_y = max(y) + grid_y
                    ) %>%
          rowwise() %>%
          do( rv = getHighDensitySections_2d( df, xmin = c( .$min_x, .$min_y ), xmax = c( .$max_x, .$max_y )) ) %>%
          bind_rows(rv) %>% 
          { . } -> rv
      }
    }
    contour_idx = contour_idx + contour_step
  }
  message("finishing")
  rv
}

################################################################################




getHighDensity_test = function( df, target_size = 10, xmin=NA, xmax=NA ) {


  df %>%
    dplyr::select( m_lat, m_lon) %>% 
     kde()  %>% 
     { . } -> kde_ll 

  grid_x = kde_ll$eval.points[[1]][2] - kde_ll$eval.points[[1]][1]
  grid_y = kde_ll$eval.points[[2]][2] - kde_ll$eval.points[[2]][1]



  # move probabilty line up until we get something where
  # the estimated x is within our target_size range
  # zoom into the densest regions

  found = FALSE
  contours = paste0( 1:99, '%')
  contour_idx = 1
  contour_step = 5
  rv = data.frame()

  while (!found & contour_idx <= length(contours)) {

    message(paste("inside", contours[contour_idx]), contour_idx)

    # clump the density estimates above the current probability level
    ifelse( kde_ll$estimate >= kde_ll$cont[ contours[contour_idx ] ], 1, 0 ) %>%
      raster() %>%
      raster::clump(directions=8) %>%
      { . } -> clumps

    m = function(x ) {
      (!is.na( x )  ) & (as.matrix(x) == 1) 
    }

    !is.na( as.matrix(clumps )) &  (as.matrix(clumps)==1)

    clumps %>%
      as.matrix() %>%
      m(.) %>%
      which( arr.ind=TRUE) %>%
      str()

    freq(clumps) %>%
      as_tibble() %>%
      dplyr::filter( !is.na( value)) %>%
      nrow() %>% 
      { . } -> nclump

    # if we have no clumps (from the bottom up), this is a bust, return NA
    # if we have 1 clump of sufficiently small size, yeah!!
    # otherwise, traverse all the clumps, zoom into them

    if (nclump == 0) {
      message(paste('no clumps found at contour ',contour_idx, contours[contour_idx] ))
      found=FALSE
      next
    }


    message(paste("nclump", nclump))
    #browser()

    # if we have a single clump, that is small enough, return it
    # otherwise, zoom out until clump is small enough and return it
    # or zoom into each specific clump
    if( nclump == 1) {

      # find the size of the current area, abort if so small that it crashes
      contourArea =  0
      contourArea =  tryCatch(  {
        contourSizes( kde_ll, cont  = 100-contour_idx, approx=TRUE )
      }, 
      error = function(cond) {return(NA)}
      )
      #message(paste("contourArea", contourArea))

      if( is.na( contourArea)) { 
        contour_idx = contour_idx + contour_step
        next
      }

      if ( contourArea < target_size^2 ) {
        message( paste('found single small clump, size=', contourArea, 'prob=', contour_idx ))
        return(kde_ll)
      }
    } else {

        # zoom into each clump
        # for each clump, find the extents of the clump

        clumps %>%
          as.matrix() %>% 
          { . } -> clumps_m

        which( !is.na( clumps_m ), arr.ind=TRUE) %>%
          as_tibble() %>%
          bind_cols( clumps_m[ which( !is.na(clumps_m )) ] %>% enframe('id', 'group') ) %>% 
          # find the grid location for this clump point
          mutate(
                 x = kde_ll$eval.points[[1]][.$row], 
                 y = kde_ll$eval.points[[2]][.$col]
                 ) %>%
          group_by( group ) %>%
          summarise(
                    min_x = min(x) - grid_x, 
                    max_x = max(x) + grid_x, 
                    min_y = min(y) - grid_y, 
                    max_y = max(y) + grid_y
                    ) %>% 
                    { . } -> chunk_extents


      df %>%
        filter( m_lat >= chunk_extents[1,]$min_x & m_lat <= chunk_extents[1,]$max_x ) %>%
        filter( m_lon >= chunk_extents[1,]$min_y & m_lon <= chunk_extents[1,]$max_y ) %>% 
        { . } -> df

    }
    contour_idx = contour_idx + contour_step
  }
  message("finishing")
  rv
}

################################################################################




