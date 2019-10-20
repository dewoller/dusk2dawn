library(ks)
library(IRanges)
library(drake)
library(tidyverse)
library(janitor)
library( raster )
loadd(df_location)
loadd(staypoints_distance_300_900_10_filtered_sigma_100 )
source( 'lib/kernel_density_functions.R')
df_location %>%
  filter( accuracy < 20) %>%
  group_by( userid, night) %>%
  count( sort=TRUE)
df_location %>%
  filter( userid== '39146290-ed15-4ca3-9f00-ce935128c1a6' & night == '2014-10-10') %>%
  { . } -> df_single_location
df_single_location %>%
  interpolate_locations () %>%
  {.} -> df_single_location_e
loadd(df_all_ts)
#
df_location %>% 
  distinct( userid, night) %>% 
  head(10) %>%
  inner_join( df_location ) %>%
  interpolate_locations() %>% 
  { . } -> df_first10_location_e
#
m_per_latitude = 111320
m_per_longitude = 111319.488
desired_grid = 10 #meters
#
m2ll = function( m, base_ll, m_per_factor )  {
  m/m_per_factor + base_ll 
}
#
#
ll2m = function( ll, base_ll , m_per_factor )  {
  (ll - base_ll ) * m_per_factor 
}
#
df_single_location_e %>%
  mutate( 
         m_lat = ll2m( latitude, min(latitude), m_per_latitude),
         m_lon = ll2m( longitude, min(longitude), m_per_longitude)
         ) %>%
  { . } -> df


visualise = function() {
library(gridExtra)
grid.arrange(rectGrob(), rectGrob())
x=1

1:9 %>%
  lapply( function(x) {
         message(x)
         minlat = 261
         maxlat = 300
  b=kde( df$m_lat,  xmin=minlat, xmax=maxlat)
   tibble( b$eval.points, b$estimate ) %>%
   mutate( color = b$estimate >= b$cont['80%']) %>%
    set_names(c('x','y', 'color')) %>%
    { . } -> df1
    ggplot( ) +
    geom_point(data = df1, aes( x,y, color=color ))
  #+
  #  geom_histogram( data=(df %>% filter( m_lat >= minlat & m_lat <= maxlat)), aes( x=m_lat), alpha=.2)

         }
################################################################################

a = getHighDensitySections_2d_all( df_first10_location_e)

d=getHighDensitySections_2d( df )



df %>%
  getHighDensitySections_2d (target_size = 10) %>% 
  { . } -> a

options(error=stop)
options(error=recover)
options(error=dump.frames)

 xmax=c(296.650250604004, 2158.20694173654)
 xmin = c(185.217270473926, 2077.84944215853)
target_size=10
################################################################################
################################################################################
################################################################################

################################################################################
getHighDensitySections_2d = function(df, xmin=NA, xmax=NA, target_size=10, level = 0, clumpid = ''){


  message(paste( 'xmin=', xmin, collapse=','), 2)
  message(paste( 'xmax=', xmax, collapse=','), 2)
  message(paste("userid, night, clumpid", df[1,]$userid, df[1,]$night, clumpid))
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
  leeway=0
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
      found=TRUE
      next
    }

    contourArea =  tryCatch(  {
      contourSizes( kde_ll, cont  = contour_idx, approx=TRUE )
    }, 
    error = function(cond) {return(NA)}
    )
    if( is.na( contourArea)) { 
      found=TRUE
      next
    }

    average_clump_size = contourArea / nclump 
    message(paste("level", level))
    message(paste("nclump", nclump))
    message(paste("contourArea", contourArea))
    message(paste("avg clump size=", average_clump_size))
    #browser()

    # if we have a single clump, that is small enough, return it
    # otherwise, zoom out until clump is small enough and return it
    # or zoom into each specific clump
    if( nclump == 1) {

      # find the size of the current area, abort if so small that it crashes

      if ( contourArea < target_size^2 ) {
        message( paste('found single small clump, size=', contourArea, 'prob=', contour_idx ))
        rv = kde_ll
        found=TRUE
      } # otherwise, zoom out
    } else if (level < 3) {
      # we have more than 1 clump, and we have not gone too deep, and there are not too many small clumps
      # zoom into each clump
      # for each clump, find the extents of the clump
      #browser()

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
        summarise( # zoom out 1 in every direction to give some leeway
                  min_x = min(x) - grid_x * leeway, 
                  max_x = max(x) + grid_x * leeway, 
                  min_y = min(y) - grid_y * leeway, 
                  max_y = max(y) + grid_y * leeway
                  ) %>%
        filter( (max_x - min_x) * (max_y-min_y) > target_size ^ 2) %>%
        rowwise() %>%
        mutate(my_clumpid = paste0(clumpid, ':', level, '.', group)) %>% 
        do( kdes = getHighDensitySections_2d( df, xmin = c( .$min_x, .$min_y ), xmax = c( .$max_x, .$max_y ), level = level+1, clumpid =  .$my_clumpid)) %>% 
        { . } -> rv
      found=TRUE
    }
    contour_idx = contour_idx + contour_step
  }
  message(paste("finishing", length(rv)))
  return(rv)
}



test()
test = function(  x =1) {
  print(x)
  Sys.sleep(x)
  test( x + 1)
}
