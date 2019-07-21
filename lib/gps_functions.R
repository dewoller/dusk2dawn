library(geosphere)
library(zoo)
library(glue)
library(tibbletime)

################################################################################
# distance2centroid 
################################################################################
distance2centroid = function( df ) {
  # distance from last point to centroid
  distm(
        colMeans( df[c('longitude', 'latitude' )]),
        df[ nrow(df), c('longitude', 'latitude' )],
        fun = distHaversine
  )
}

################################################################################
# distanceBetween 
################################################################################
distanceBetween = function( df ) {
  #  calculate distance between last 2 points of df
  distm(
        df[nrow(df)-1,c('longitude', 'latitude' )],
        df[nrow(df),c('longitude', 'latitude' )],
        fun = distHaversine
  )
}

################################################################################
# calc_interval_distance 
# calc distance between every successive pair of points
################################################################################
calc_interval_distance = function( longitude, latitude ) {
  c( longitude, latitude ) %>%
    matrix( ncol = 2 ) %>%
    spDists( segments=TRUE, longlat=TRUE) %>%
    c(NA,.)
}

################################################################################
# calc_distance_from_start 
# from start of array to current point
################################################################################
calc_distance_from_start = function( longitude, latitude ) {
  c( longitude, latitude ) %>%
    matrix( ncol = 2 ) %>%
    spDistsN1(., .[1,], longlat=TRUE) 
}




################################################################################
# geo_dist_pairs 
# expects 2 columns;  lon, lat, and a point, lon, lat
# returns distance between each pair of points
################################################################################

geo_dist_pairs <- function(lon, lat, clon, clat) {
  pts = c(lon, lat) %>%
    matrix( ncol=2 ) 

  spDistsN1(pts, c( clon, clat), longlat = TRUE) 
}




################################################################################
# geo_dist_calc 
# expects 2 columns;  lat, lon
# returns distance between each pair of points
################################################################################

geo_dist_calc <- function(x) {
  x=as.matrix(x)
  x = cbind( x, rbind( tail( x, -1 ), matrix(c(NA,NA), nrow=1)) )
  apply(x, 1, function(x) sqrt((x[1] - x[3]) ^ 2 + (x[2] - x[4]) ^ 2))
}



################################################################################
# calculate_distance 
# distance between the first 2 points
################################################################################
calculate_distance = function( .x, .y ) {
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

  # current staypoint proposal
  # 0 not in, 1:n which staypoint
  n_staypoint =  1          

  df$duration = -1
  df$last_duration = -1
  df$distance2centroid = -1
  df$velocity = -1
  df$start_sp_proposal_index = -1
  df$n_staypoint = 0
  df$reason=NA
  in_staypoint=FALSE
  nrow = nrow( df )
  sp_start=1
  sp_end=1

  while ( sp_end <= nrow ) {

    # track the start of the current staypoint proposal
    df[ sp_end,]$start_sp_proposal_index = sp_start

    if ( sp_end <= sp_start ) {
      sp_end = sp_end + 1
      next
    }

    # track the durations
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
        df[ sp_end,]$reason = "Excessive Jump Time"
      }
      next
    }

    # is this point within current staypoint centroid
    d = distance2centroid( slice( df, sp_start:sp_end ))
    df[ sp_end,]$distance2centroid = d
    df[ sp_end,]$velocity = distanceBetween( slice( df, (sp_end-1):sp_end )) / last_duration

    if ( d > max_staypoint_distance ) {

      # we have physically moved out of the previous staypoint zone, sp_start:sp_end
      if ( in_staypoint ){

        # this is a mark, keep the previous staypoint, sp_start afresh
        n_staypoint = n_staypoint + 1
        in_staypoint = FALSE
        sp_start=sp_end
        df[ sp_end,]$reason = "Too Far From Centroid, closing staypoint"
      } else {
        # we are STILL not in a staypoint
        # keep looking, move the staypoint zone forward
        df[ sp_start,]$reason = "Too Far From Centroid, moving start forward"
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
# for cleaning points using gpsbabel
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
# arrange for party_df  
################################################################################
carrange_.party_df <- function (.data, ..., .dots = list()) 
{
  multidplyr:::shard_call(.data, quote(dplyr::arrange), ..., .dots = .dots, 
                          groups = .data$groups[-length(.data$groups)])
}


