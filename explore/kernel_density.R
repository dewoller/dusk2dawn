library(drake)
library(tidyverse)
needs(meanShiftR)
needs(LPCM)
needs(MeanShift)

loadd(df_location)
drake_history() -> a
drake_gc( verbose=2) 


staypoints_distance_300_900_10_filtered_sigma_100
-> a

a %>% count(current)

a %>% 
  arrange( target )  %>%
  filter( current ) %>%
  select(target, runtime) %>%

df_all_staypoints_multi  %>%
  group_by( filename ) %>%
  summarise( n= n()) %>%
  arrange( desc(n)) %>%
  head( 1) %>% 
  { . } -> max_filename

df_all_staypoints_multi %>%
  inner_join( max_filename ) %>%
  filter( n_staypoint==44) %>% 
  select( userid, night) %>% 
  { . } -> max_sp


df_location %>% 
 count( userid, night, sort=TRUE) %>%
 head(1) %>% 
 { . } -> t


  df_location %>%
    inner_join( a ) %>%
  filter( accuracy < 20) %>%
  dplyr::select( latitude, longitude, timestamp) %>% 
  ggplot(aes(latitude,longitude))+
  geom_point( mapping=aes(latitude, longitude))  +
  stat_density2d(geom="polygon",aes(alpha = .1), fill="orangered",color="red4",linetype=2, n=100, h=.00005)+ 
  geom_point(  mapping=aes(latitude, longitude))  +
  theme_bw()+
  scale_x_continuous("X-coordinate")+
  scale_y_continuous("Y-coordinate")

dens <- kde2d(a$latitude, a$longitude, n=100, h=.00005)
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


