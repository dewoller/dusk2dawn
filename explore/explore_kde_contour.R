library(ks)
library(IRanges)
library(drake)
library(tidyverse)
library(janitor)
library( raster )
library(geosphere)
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


df

##  Simulate some lat/lon data:
x <- df$m_lat
y <- df$m_lon

##  Version 1 (without ggplot2):
library(MASS)
dens <- kde2d(x, y, n=200)

##  The contours to plot:
prob <- c(0.01, 0.5)
dx <- diff(dens$x[1:2])
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy 
levels <- sapply(prob, function(x) { 
                   approx(c1, sz, xout = 1 - x)$y
                  })
plot(x,y)
contour(dens, levels=levels, labels=prob, add=T)

##  Create spatial objects:
library(sp)
library(maptools)

pts <- SpatialPoints(cbind(x,y))

lines <- ContourLines2SLDF(contourLines(dens, levels=levels))

##  Convert SpatialLinesDataFrame to SpatialPolygons:
lns <- slot(lines, "lines")
polys <- SpatialPolygons( lapply(lns, function(x) {
                                   Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], 
                                                              "coords"))), ID=slot(x, "ID"))
}))

##  Construct plot from your points, 
plot(pts)

##  Plot points within contours by using the over() function:
points(pts[!is.na( over(pts, polys[1]) )], col="red", pch=20)
points(pts[!is.na( over(pts, polys[2]) )], col="blue", pch=20)

contour(dens, levels=levels, labels=prob, add=T)

df %>%
  dplyr::select( m_lat, m_lon) %>%
  kde(  ) %>% 
  { . } -> k

contourSizes(k, cont = 50, approx=TRUE)




df



xy <- df[,c(3,4)]

spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


sfdf = st_as_sf( spdf)
library(sf)


library(tmap)

tm_shape(sfdf, unit = "km") + tm_dots() 


library(spatstat)

sfdf %>%
  as("Spatial") %>%
  as("ppp") %>% 
  { . } -> break.sf.ppp 

qcounts<-quadratcount(break.sf.ppp, nx = 36, ny = 36)
plot(unmark(break.sf.ppp), pch=20, cols="grey70", main=NULL)
plot(qcounts, col = "red", add=TRUE)

break.intensity <- intensity(qcounts)
plot( intensity(qcounts, image=TRUE), main="Car Break-ins in San Francisco, 2017")
plot(unmark(break.sf.ppp),  pch=20, cex=0.6, col=rgb(0,0,0,.2),  add=TRUE)


quadratcount(break.sf.ppp, nx = 40, ny = 40) %>%
  { . } -> qcounts
class(qcounts)

  data.frame()  %>%
  summarise(var(Freq)/mean(Freq)) %>% 

break.ndist <- nndist(break.sf.ppp)
mean(break.ndist)

hist(break.ndist, breaks = 50)


qcounts %>%
  as.matrix() %>% 
  { . } -> clumps_m

which( clumps_m !=0, arr.ind=TRUE) %>%
  as_tibble() %>%
  bind_cols( clumps_m[ which( clumps_m !=0) ] %>% enframe('id', 'group') ) %>% 
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

break.intensity <- intensity(qcounts)
plot( intensity(qcounts, image=TRUE), main="Car Break-ins in San Francisco, 2017")
plot(unmark(break.sf.ppp),  pch=20, cex=0.6, col=rgb(0,0,0,.2),  add=TRUE)


break.ndist <- nndist(break.sf.ppp)
mean(break.ndist)

envelope(break.sf.ppp, fun = Kest, correction="border", nsim = 50) %>% plot()


library(MASS)
x <- rnorm(1000, 0, 1.3)
y <- rnorm(1000, 0, 1);
bw=data.frame( x, y)
dens <- kde2d(x, y, n=200); ## estimate the z counts

levels <- c(.98, .95, .90, .8, .5, .1)
dx <- diff(dens$x[1:2])
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy
levels <- sapply(prob, function(x) {
                   approx(c1, sz, xout = 1 - x)$y
            })

plot(x,y)
contour(dens, levels=levels, labels=prob, add=T)


## Interactively check points

## Points within polygons
library(sp)
dens <- kde2d(x, y, n=200)  # don't clip the contour
ls <- contourLines(dens, nlevel=10)
inner <- point.in.polygon(x, y, ls[[2]]$x, ls[[2]]$y)
out <- point.in.polygon(x, y, ls[[1]]$x, ls[[1]]$y)

## Plot
bw$region <- factor(inner + out)
plot(x ~ y, col=region, data=bw, pch=15)

contour(dens, levels=(1-(map(ls, 1) %>% unlist())), labels=prob, add=T)

pluck(ls, 'level')

install.packages('hdrcde')

library(hdrcde)

x<-rlnorm(100)
d<-density(x)

# calcualte KDE with help of the hdrcde package
hdrResult<-hdr(den=d,prob=0)

# define the linear interpolation function for the density estimation
dd<-approxfun(d$x,d$y)
# get the density value of the KDE peak
vDens<-dd(hdrResult[['mode']])


df %>%
  dplyr::select( m_lat, m_lon ) %>%
  kms( verbose=TRUE ) %>% 
  { . } -> b


tibble( df, a$label, b$label, d$label) %>%

  count( `a$label`, `b$label`, `d$label`)



df %>%
  dplyr::select( m_lat, m_lon ) %>%
  kms( tol.clust=10, min.clust.size=100 ) %>% 
  { . } -> e

kde( x=df[, c('m_lat', 'm_lon')] ) %>% plot()
plot(e)

df %>%
  bind_cols( list(label=e$label, e_lat = e$end.points[,1], e_lon = e$end.points[,2] )) %>%
  mutate( label = as.factor(label)) %>%
  ggplot(aes( x=e_lat,y=e_lon ) ) +
  geom_point( aes( color = label)) +
  geom_point( data=data.frame(e$mode), aes( x=m_lat,y=m_lon, color='black', size=5 ) ) 

df %>%
  bind_cols( list(label=f$assignment, e_lat = f$value[,1], e_lon = f$value[,2] )) %>%
  mutate( label = as.factor(label)) %>%
  ggplot(aes( x=m_lat,y=m_lon ) ) +
  geom_point( aes( color = label)) 

data.frame(e$mode)
df %>%
  bind_cols( list(label=e$label)) %>%
  count( label )

library(meanShiftR)

df %>%
  dplyr::select( m_lat, m_lon ) %>%
  as.matrix() %>%
  meanShift(queryData=., trainData=. ) %>% 
{ . } -> f



df_single_location %>%
  mutate( 
         m_lat = ll2m( latitude, min(latitude), m_per_latitude),
         m_lon = ll2m( longitude, min(longitude), m_per_longitude)
         ) %>% 
         { . } -> df1

df_single_location_e %>%
         mutate( 
                m_lat = ll2m( latitude, min(latitude), m_per_latitude),
                m_lon = ll2m( longitude, min(longitude), m_per_longitude)
                ) %>% 
         { . } -> df1_e

       df1_e %>%
         dplyr::select( m_lat, m_lon ) %>%
         as.matrix() %>%
         meanShift(queryData=., trainData= . ) %>% 
         { . } -> f

df1_e %>%
  mutate(ts = timestamp - min(timestamp)) %>%
  dplyr::select( m_lat, m_lon, ts ) %>%
  as.matrix() %>%
  meanShift(queryData=., trainData= ., iterations=100, bandwidth=c(10,10,600), alpha=0 ) %>% 
  { . } -> f_ts1

df1_e %>%
  count( timestamp, sort=TRUE)

unique(f_ts1$assignment)

df1_e %>%
  bind_cols( list(label=f_ts$assignment[,1], e_lat = f_ts$value[,1], e_lon = f_ts$value[,2] )) %>%
  arrange(label) %>%
  group_by(label) %>%
  mutate( tsw = max(timestamp)-min(timestamp) ) %>%
  filter( tsw > 600 ) %>%
  ungroup() %>%
  mutate( label = as.factor(label) ) %>%
  mutate( tsb = timestamp-min(timestamp),
         ts = cut( tsb, 5)) %>%
  ggplot(aes( x=m_lat,y=m_lon ) ) +
  geom_point( aes( color = ts), size=.01) +
  facet_wrap(~label, scales='free')

debug(try)


df_e = df1_e
df1_e %>%
  bind_cols( list(label=f_ts1$assignment[,1], e_lat = f_ts1$value[,1], e_lon = f_ts1$value[,2] )) %>%
  arrange(label) %>%
  group_by(label) %>%
  mutate( mn_lat = mean( m_lat) ) %>%
  mutate( mn_lon = mean( m_lon) ) %>%
  ungroup() %>%
  mutate( dist = try(.[ c('m_lon', 'm_lat')], .[ c('mn_lon', 'mn_lat')])) %>%
  filter( dist < 5) %>%
  mutate( label = as.factor(label) ) %>%
  group_by(label) %>%
  mutate( tsw = max(timestamp)-min(timestamp) ) %>% 
  filter( tsw > 1200 ) %>%
  ungroup() %>%
  mutate( tsb = timestamp-min(timestamp),
         ts = cut( tsb, 5)) %>%
  ggplot(aes( x=m_lat,y=m_lon ) ) +
  geom_point( aes( color = ts)) +
  geom_point(aes( x=mn_lat,y=mn_lon ), color='black', size=4, shape=1 ) +
  facet_wrap(~label, scales='free')



df1_e %>%
  
