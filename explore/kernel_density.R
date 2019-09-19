
m_per_latitude = 111320
m_per_longitude = 111319.488
desired_grid = 10 #meters

library(drake)
library(tidyverse)
library(janitor)
library( raster )

library(ctmm)
needs(meanShiftR)
needs(LPCM)
needs(MeanShift)

#install.packages('KernSmooth')
#install.packages('ks')

library(ks)

#vignette(kde)


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


if(FALSE) {
staypoints_distance_300_900_10_filtered_sigma_100 %>%
  filter( userid== '39146290-ed15-4ca3-9f00-ce935128c1a6' & night == '2014-10-10') %>%
  summarise( a=max( n_staypoint)) %>%
  arrange( desc( a))
}

#plot( a$estimate )


#needs( hdrcde)
library( hdrcde)


# Old faithful data
faithful.cde <- cde(faithful$waiting, faithful$eruptions,
					x.name="Waiting time", y.name="Duration time")
plot(faithful.cde)
plot(faithful.cde, plot.fn="hdr")

# Melbourne maximum temperatures with bias adjustment
x <- maxtemp[1:3649]
y <- maxtemp[2:3650]
maxtemp.cde <- cde(x,y, x.name="Today's max temperature",y.name="Tomorrow's max temperature")
# Assume linear mean
fit <- lm(y~x)
fit.mean <- list(x=6:45,y=fit$coef[1]+fit$coef[2]*(6:45))
maxtemp.cde2 <- cde(x,y,mean=fit.mean,
					x.name="Today's max temperature",y.name="Tomorrow's max temperature")
plot(maxtemp.cde)

bands <- cde.bandwidths(faithful$waiting,faithful$eruptions,method=2)
plot(cde(faithful$waiting,faithful$eruptions,a=bands$a,b=bands$b))


# Old faithful eruption duration times
hdr(faithful$eruptions)
hdr.boxplot(faithful$eruptions)
hdr.den(faithful$eruptions)
# Simple bimodal example
x <- c(rnorm(100,0,1), rnorm(100,5,1))
par(mfrow=c(1,2))
boxplot(x)
hdr.boxplot(x)
par(mfrow=c(1,1))
hdr.den(x)
options(error=stop)
# Old faithful eruption duration times
hdr(df_single_location_e$longitude) %>% summary()
hdr.boxplot(df_single_location_e$longitude)
hdr.den(df_single_location_e$longitude)

# Old faithful eruption duration times
hdr(df_single_location_e$latitude)
hdr.boxplot(df_single_location_e$latitude)
hdr.den(df_single_location_e$latitude)
# Old faithful eruption duration times
hdr(df_single_location_e$timestamp)
hdr.boxplot(df_single_location_e$timestamp)
hdr.den(df_single_location_e$timestamp)
# Simple bimodal example
x <- c(rnorm(100,0,1), rnorm(100,5,1))
par(mfrow=c(1,2))
boxplot(x)
hdr.boxplot(x)
par(mfrow=c(1,1))
hdr.den(x)


x <- c(rnorm(200,0,1),rnorm(200,4,1))
y <- c(rnorm(200,0,1),rnorm(200,4,1))
hdr.boxplot.2d(x,y)
hdrinfo <- hdr.2d(x,y)
plot(hdrinfo, pointcol="red", show.points=TRUE, pch=3)

hdrinfo %>% as_tibble(pluck(.,'x'), pluck(., 'y'))

hdrinfo %>%
  pluck('den') %>%
  pluck('z') %>%
  as_tibble() %>%
  gather(column, value) %>%
  ggplot( aes( x)) + geom_histogram() +
  facet_wrap(~column)


hdr.boxplot.2d(df_single_location_e$latitude,df_single_location_e$longitude)
hdrinfo <- hdr.2d(df_single_location_e$latitude,df_single_location_e$longitude)
plot(hdrinfo, pointcol="red", show.points=TRUE, pch=3)
hdrinfo$mode

hdr.boxplot.2d(df_single_location_e$latitude,df_single_location_e$longitude)
hdrinfo <- hdr.2d(df_single_location_e$latitude,df_single_location_e$longitude)
plot(hdrinfo, pointcol="red", show.points=TRUE, pch=3)

faithful.cde <- cde(faithful$waiting,faithful$eruptions)
plot(faithful.cde,xlab="Waiting time",ylab="Duration time",plot.fn="hdr")


x <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
y <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
a=hdr.2d(df_single_location_e$latitude,df_single_location_e$longitude)
hdrscatterplot(df_single_location_e$latitude,df_single_location_e$longitude)
a(df_single_location_e$latitude,df_single_location_e$longitude)



a = function (x, y, prob = c(50, 95, 99) , den = NULL , kde.package = c("ash", "ks"), h = NULL, xextend = 0.15, yextend = 0.15)
{
  browser()
  if (max(prob) > 50)
    alpha <- (100 - prob)/100
  else alpha <- prob
  alpha <- sort(alpha)
  if (is.null(den))
    den <- den.estimate.2d(x, y, kde.package, h, xextend, yextend)
  fxy <- interp.2d(den$x, den$y, den$z, x, y)
  falpha <- quantile(fxy, alpha)
  index <- which.max(fxy)
  mode <- c(x[index], y[index])
  return(structure(list(mode = mode, falpha = falpha, fxy = fxy,
                        den = den, alpha = alpha, x = x, y = y), class = "hdr2d"))
}



den.estimate.2d <- function(x, y, kde.package=c("ash","ks"), h=NULL, xextend=0.15,yextend=0.15)
{
  kde.package <- match.arg(kde.package)
  # Find ranges for estimates
  xr <- diff(range(x,na.rm=TRUE))
  yr <- diff(range(y,na.rm=TRUE))
  xr <- c(min(x)-xr*xextend,max(x)+xr*xextend)
  yr <- c(min(y)-yr*yextend,max(y)+yr*yextend)
  if(kde.package=="ash")
  {
    if(is.null(h))
      h <- c(5,5)
    den <- ash::ash2(ash::bin2(cbind(x,y),rbind(xr,yr)),h)
  }
  else
  {
    X <- cbind(x,y)
    if(is.null(h))
      h <- ks::Hpi.diag(X,binned=TRUE)
    else
      h <- diag(h)
    den <- ks::kde(x=X,H=h,xmin=c(xr[1],yr[1]),xmax=c(xr[2],yr[2]))
    den <- list(x=den$eval.points[[1]],y=den$eval.points[[2]],z=den$estimate)
  }
  return(den)
}



"interp.2d" <- function(x, y, z, x0, y0)
{
  # Bilinear interpolation of function (x,y,z) onto (x0,y0).
  # Taken from Numerical Recipies (second edition) section 3.6.
  # Called by hdr.2d
  # Vectorized version of old.interp.2d.
  # Contributed by Mrigesh Kshatriya (mkshatriya@zoology.up.ac.za)

  nx <- length(x)
  ny <- length(y)
  n0 <- length(x0)
  z0 <- numeric(length = n0)
  xr <- diff(range(x))
  yr <- diff(range(y))
  xmin <- min(x)
  ymin <- min(y)
  j <- ceiling(((nx - 1) * (x0 - xmin))/xr)
  k <- ceiling(((ny - 1) * (y0 - ymin))/yr)
  j[j == 0] <- 1
  k[k == 0] <- 1
  j[j == nx] <- nx - 1
  k[k == ny] <- ny - 1
  v <- (x0 - x[j])/(x[j + 1] - x[j])
  u <- (y0 - y[k])/(y[k + 1] - y[k])
  AA <- z[cbind(j, k)]
  BB <- z[cbind(j + 1, k)]
  CC <- z[cbind(j + 1, k + 1)]
  DD <- z[cbind(j, k + 1)]
  z0 <- (1 - v) * (1 - u) * AA + v * (1 - u) * BB + v * u * CC + (1 - v) * u * DD
  return(z0)
}

plot(lane2)
plot(lane3)

lane2.fit <- modalreg(lane2$flow, lane2$speed, xfix=(1:55)*40, a=100, b=4)

faithful.cde <- cde(faithful$waiting,faithful$eruptions,
					x.name="Waiting time", y.name="Duration time")
plot(faithful.cde)
plot(faithful.cde,plot.fn="hdr")


x <- c(rnorm(100,0,1),rnorm(100,4,1))
den <- density(x,bw=bw.SJ(x))
trueden <- den
trueden$y <- 0.5*(exp(-0.5*(den$x*den$x)) + exp(-0.5*(den$x-4)^2))/sqrt(2*pi)
sortx <- sort(x)
par(mfcol=c(2,2))
for(conf in c(50,95))
{
  m <- hdrconf(sortx,trueden,conf=conf)
  plot(m,trueden,main=paste(conf,"% HDR from true density"))
  m <- hdrconf(sortx,den,conf=conf)
  plot(m,den,main=paste(conf,"% HDR from empirical density\n(n=200)"))
}

ks::kde(df_single_location_e[3:5]) -> a3

plot(a3$estimate)
install.packages('rgl')
install.packages('misc3d')


df_single_location %>%
  filter( accuracy < 20) %>%
  interpolate_locations( )  -> d

b=df_single_location

(max( a$longitude ) - min(a$longitude))  * m_per_longitude / desired_grid
(max( a$latitude ) - min(a$latitude))  * m_per_latitude / desired_grid

df_single_location %>%
  filter( accuracy < 20) %>%

d %>%
  dplyr::select( latitude, longitude, timestamp) %>%
  ggplot(aes(latitude,longitude))+
  geom_point( mapping=aes(latitude, longitude))  +
  stat_density2d(geom="polygon",aes(alpha = .1), fill="orangered",color="red4",linetype=2, n=100, h=.00005)+
  geom_point(  mapping=aes(latitude, longitude))  +
  theme_bw()+
  scale_x_continuous("X-coordinate")+
  scale_y_continuous("Y-coordinate")

dens <- kde2d(d$latitude, d$longitude, n=300, h=.00005)

e = dens$z > 0
sum(e)



image(dens)
contour(dens, add=T)
plot(a$latitude, a$longitude, add=T )

colour_flow <- colorRampPalette(c('white', 'blue', 'yellow', 'red', 'darkred'))
filled.contour(dens, color.palette=colour_flow)


# set the bandwidth
h <- c(.00005,.00005)

# create example data
x <- rbind( a$latitude, a$longitude)

#plot initial points
plot(x, col=rep(c('red','green'),each=n/2),
     cex=1, xlab='x',ylab='y',pch=20)


########### meanShiftR ###################
run.time <- proc.time()
result <- meanShift( x)
meanShiftR_kd_runtime <- (proc.time()-run.time)[3]

# assignment
meanShiftR_kd_assignment <- result$assignment

# value
meanShiftR_kd_value <- result$value


########### meanShiftR ###################
run.time <- proc.time()
result <- meanShift(
                    x,
                    x,
                    bandwidth=h,
                    alpha=0,
                    iterations = iter
)
meanShiftR_runtime <- (proc.time()-run.time)[3]

# assignment
meanShiftR_assignment <- result$assignment

# value
meanShiftR_value <- result$value


########### LPCM ###################
runtime <- proc.time()
result <- ms(
             x,
             h=h,
             scaled=FALSE,
             iter=iter,
             plotms=-1)
LPCM_runtime <- (proc.time()-runtime)[3]

# assignment
LPCM_assignment <- result$cluster.label

# value
LPCM_value <- result$cluster.center[LPCM_assignment,]


########### MeanShift ###################
options(mc.cores = 4)
z <- t(x)
runtime <- proc.time()
result <- msClustering(
                       X=z,
                       h=h,
                       kernel="gaussianKernel",
                       tol.stop=1e-08,
                       tol.epsilon=1e-04,
                       multi.core=T)
MeanShift_runtime <- (proc.time()-runtime)[3]

MeanShift_assignment <- result$labels
MeanShiftkvalue <- t(result$components[,result$labels])

# print
plot(x, col=sapply(meanShiftR_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, xlab='x',ylab='y',pch=20)

result <- data.frame(
                     runtime=c( meanShiftR_runtime,
                               meanShiftR_kd_runtime,
                               LPCM_runtime,
                               MeanShift_runtime),
                     maxDiff=c(max(abs(meanShiftR_value - LPCM_value)),
                               max(abs(meanShiftR_kd_value - LPCM_value)),
                               0,
                               max(abs(MeanShift_value - LPCM_value))
                               ),
                     assignmentDiff=c(sum(meanShiftR_assignment != LPCM_assignment),
                                      sum(meanShiftR_kd_assignment != LPCM_assignment),
                                      0,
                                      sum(MeanShift_assignment != LPCM_assignment)
                     )
)

colnames(result) <- c('Run-Time',
                      'Maximum Absolute Difference',
                      'Label Disagreements')

rownames(result) <- c('meanShiftR',
                      'meanShiftR K-D Tree',
                      'LPCM ms',
                      'meanShift msClustering')

library(xtable)
print(xtable(result,digits=6,display=c('s','f','f','d')), type='html')




## generate a noisy curve observed on a regular grid
set.seed( 1 )
n.grid <- 1000
x <- seq( 2, 8, length=n.grid )
sigma.epsilon1 <- 2
sigma.epsilon2 <- 2.5
sigma.epsilon3 <- 3
sigma.epsilon4 <- 1
epsilon <- rnorm( 1000, sd=rep( c( sigma.epsilon1, sigma.epsilon2, sigma.epsilon3, sigma.epsilon4 ), rep( 250, 4 ) ) )
y <- x*sin( 3*x ) + 0.3*x^2 + epsilon

## project on wavelet basis with soft universal thresholding
## of the wavelet coefficients
wave <- projectCurveWavelets( x, y, type="soft", policy="universal" )

## plot wavelet reconstruction of the curve
## Not run:
x.norm <- ( x - min( x ) ) / ( max( x ) - min( x ) )
plot( x.norm, y )
lines( wave$x.grid, wave$y.wavelet, col=2, lwd=3 )
## End(Not run)

## inspect wavelet coefficients
wave.coeffs <- wave$coefficients
print( length( wave.coeffs ) ) ## 1023 coefficients
print( sum( wave.coeffs != 0 ) ) ## only 12 are non-zero



library(raster)
# NOT RUN {
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
pol <- rasterToPolygons(r, fun=function(x){x>6}, n=4)

plot(r )
plot(pol, add=TRUE, col='red')
# }


library(feature)
data(earthquake)
eq3 <- earthquake[,-3]
eq3.fs <- featureSignif(x=eq3, bw=0.1)
plot(eq3.fs, xlab="-log(-depth)")
eq3.SiZer <- SiZer(eq3, bw=c(0.05, 0.5), xlab="-log(-depth)")

-earthquake[,-3]

options(error=recover)


################################################################################
# find significant features
################################################################################

m_per_latitude = 111320
m_per_longitude = 111319.488
desired_grid = 10 #meters

# find significant features
df_single_location_e  %>%
  dplyr::select( latitude,longitude) %>%
  { . } -> d


   d %>%
     find_significant_densities() %>%
  { . } -> b

################################################################################
# explore single significant section
################################################################################

b %>%
  head(1) %>%
  mutate( min_lat = min_lat - range_lat, 
         max_lat = max_lat + range_lat, 
         min_lon = min_lon - range_lon, 
         max_lon = max_lon + range_lon) %>%
  rowwise() %>%
  mutate( results = find_significant_densities( 
                        d %>% filter( latitude >= min_lat & latitude <= max_lat  &
                                     longitude >= min_lon & longitude <= max_lon ), 
                        xmin=c(min_lat, min_lon),
                        xmax=c(max_lat, max_lon) ,
                        contigious_range=2
                        ) %>% list()) %>%
{ . } -> e


ks = e[1,]$results[[1]][1,]$kfs[[1]]
orig = e[1,]$results[[1]][1,]$original[[1]]

e$range_lat * m_per_latitude
e$range_lon * m_per_longitude

x

df_single_location %>%
  filter( latitude >= e[1,]$min_lat & latitude <= e[1,]$max_lat  & longitude >= e[1,]$min_lon & longitude <= e[1,]$max_lon ) %>% 
  { . } -> df_single_chunk

df_single_location_e %>%
  filter( latitude >= e[1,]$min_lat & latitude <= e[1,]$max_lat  & longitude >= e[1,]$min_lon & longitude <= e[1,]$max_lon ) %>% 
  { . } -> df_single_chunk_e

df_single_chunk %>%
  group_by( userid, night ) %>%
  summarise( min_t = min(timestamp), max_t = max(timestamp)) %>%
  inner_join( df_all_ts, by=c( "userid", "night")) %>%
  filter( timestamp >= min_t & timestamp <=max_t ) %>%
  rename( survey_ts = timestamp) %>%
  inner_join( df_single_location  , by=c( "userid", "night")) %>%
  mutate( diff = abs( survey_ts - timestamp)) %>%
  dplyr::select( diff, survey_ts, which, everything()) %>%
  arrange( diff ) %>% 
  { . } -> f

f %>%
  group_by( userid, night, survey_ts, diff, which  ) %>%
  summarise( 
            latitude = mean(latitude),
            longitude = mean(longitude),
  )  %>%
  group_by( userid, night, survey_ts,  which  ) %>%
  filter( diff == min(diff)) %>% 
  { . } -> df_single_survey




