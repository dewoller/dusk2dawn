needs(geosphere)
needs(zoo)
needs(glue)
needs(tibbletime)

################################################################################
# distance2centroid 
################################################################################
distance2centroid = function( df ) {
  # distance from last point to centroid
  distm(
        colMeans( df  %>% select(  longitude, latitude )),
        df %>% slice(n()) %>% select( longitude, latitude ),
        fun = distHaversine
  )
}

################################################################################
# distanceBetween 
################################################################################
distanceBetween = function( df ) {
  # distance from last 2 points
  distm(
        df %>% slice(n()-1)   %>% select( longitude, latitude ),
        df %>% slice(n()) %>% select( longitude, latitude ),
        fun = distHaversine
  )
}


################################################################################
# geo_dist_calc 
# expects 4 columns;  lat, lon, lat, lon
################################################################################

geo_dist_calc <- function(x) {
  x=as.matrix(x)
  x = cbind( x, rbind( tail( x, -1 ), matrix(c(NA,NA), nrow=1)) )
  apply(x, 1, function(x) sqrt((x[1] - x[3]) ^ 2 + (x[2] - x[4]) ^ 2))
}



################################################################################
# calculate_distance 
################################################################################
calculate_distance = function( .x, .y ) {
  # distance from last 2 points
  warning('must be longitude, latitude pairs')
  distm(
        c( .x[1], .y[1]),
        c( .x[2], .y[2]),
        fun = distHaversine
  )
}
calculate_distance_roll = rollify( calculate_distance, window=2)

################################################################################
# findStayPoint 
################################################################################

findStayPoint = function (df, max_jump_time, min_staypoint_time, max_staypoint_distance) {
  n_staypoint = 0  # 0 not in, 1:n which staypoint
  df$n_staypoint = 0
  df$duration = -1
  df$last_duration = -1
  df$distance = -1
  df$velocity = -1
  df$start = -1
  df$n_staypoint = 0
  n_staypoint = n_staypoint + 1
  in_staypoint=FALSE
  nrow = nrow( df )
  sp_start=1
  sp_end=1

  while ( sp_end <= nrow ) {
    df[ sp_end,]$start = sp_start

    if ( sp_end <= sp_start ) {
      sp_end = sp_end + 1
      next
    }

    last_duration = df[ sp_end, ]$time_stamp - df[ sp_end-1, ]$time_stamp
    entire_duration = df[ sp_end, ]$time_stamp - df[ sp_start, ]$time_stamp
    df[ sp_end,]$duration = entire_duration
    df[ sp_end,]$last_duration = last_duration

    #cat( paste( sp_start, sp_end, 'last_duration', last_duration , 'entire_duration',    entire_duration,in_staypoint,"\n"))

    if ( last_duration >= max_jump_time )   {

      # we have too much time between this point and the last point
      sp_start = sp_end
      if (in_staypoint ){
        n_staypoint = n_staypoint + 1
        in_staypoint = FALSE
      } else {
        # leave untagged points outside staypoint
        df[ sp_start:sp_end,]$distance = -1
      }
      next
    }

    # is this point within current staypoint centroid
    d = distance2centroid( slice( df, sp_start:sp_end ))
    df[ sp_end,]$distance = d
    df[ sp_end,]$velocity = distanceBetween( slice( df, (sp_end-1):sp_end )) / last_duration

    if ( d > max_staypoint_distance ) {

      # we have physically moved out of the previous staypoint zone, sp_start:sp_end
      if ( in_staypoint ){

        # this is a mark, keep the previous staypoint, sp_start afresh
        n_staypoint = n_staypoint + 1
        in_staypoint = FALSE
        sp_start=sp_end
      } else {

        # keep looking, move the staypoint zone forward
        sp_start = sp_start + 1
      }
      next
    }

    # we are within staypoint distance of centroid( sp_start:sp_end )
    if ((entire_duration >= min_staypoint_time ) ) {

      # we have been in staypoint sufficient time
      df[ sp_start:sp_end,]$n_staypoint = n_staypoint
      in_staypoint = TRUE
    }
    sp_end = sp_end + 1
  }

  df
}


################################################################################
# gpsbabel 
################################################################################
gpsbabel = function( df, parameters ) {

  f_in=tempfile()
  f_out=tempfile()
  f_in='/tmp/a.xml'
  f_out='/tmp/b.xml'

  df %>% 
    mutate( .id = row_number()) %>% 
    { . } -> df


  df %>% 
    select( longitude, latitude, .id ) %>% 
    rename( name=.id) %>% 
    glue_data( '<rtept lat="{latitude}" lon="{longitude}" name="{name}"></rtept>') %>% 
    paste( collapse = "\\n") %>%
    { . } -> body


  writeLines(paste0( "<gpx>\n <rte>\n", body, "</rte>\n</gpx>\n"), f_in)


  system( paste ( " gpsbabel -i gpx -f  ", 
                 f_in,
                 "-x ", 
                 parameters,
                 "-o csv -F ",
                 f_out 
  )
  )

  read_csv( f_out, col_names=c("longitude", "latitude", ".id"), col_types='ddc') %>%
    mutate( .id = str_replace(.id, 'RPT','') %>% as.numeric()) %>% 
    { . } -> df_simplified

  df %>%
    select( -longitude, -latitude ) %>%
    inner_join( df_simplified, by=".id")  %>%
    select(-.id)

}
 
################################################################################
# calc_interval_distance 
################################################################################
calc_interval_distance = function( longitude, latitude ) {
  c( longitude, latitude ) %>%
    matrix( ncol = 2 ) %>%
    spDists( segments=TRUE, longlat=TRUE) %>%
    c(NA,.)
}

################################################################################
# calc_distance_from_start 
################################################################################
calc_distance_from_start = function( longitude, latitude ) {
  c( longitude, latitude ) %>%
    matrix( ncol = 2 ) %>%
    spDistsN1(., .[1,], longlat=TRUE) 
}

