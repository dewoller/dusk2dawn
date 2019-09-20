library('fields')
# taken from the linked stackoverflow question
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)

image.plot(x = c(0:10), y = c(0:10), z = matrix(runif(100, 0,1), nrow = 10),
           col = terrain.colors(20)) # fake data so lines() plays nice.
lines(fortify(SpP))


??image.plot


require(ggplot2)
ggplot(aes(x = long, y = lat, group = group), data = fortify(SpP)) + geom_path()

fortify(SpP)

image.plot(x = c(0:10), y = c(0:10), z = matrix(runif(100, 0,1), nrow = 10),
           col = terrain.colors(20)) # fake data so lines() plays nice.
plot(SpP)



image.plot(x = c(0:10), y = c(0:10), z = matrix(runif(100, 0,1), nrow = 10),
col = terrain.colors(20)) # fake data so lines() plays nice.
plot(SpP)



# Simulated data from a bivariate normal
n <- 200
set.seed(35233)
x <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
                      sigma = rbind(c(1.5, 0.25), c(0.25, 0.5)))




df_single_location_e %>%
  dplyr::select( latitude,longitude) %>% 
  { . } -> x

# Compute kde for a diagonal bandwidth matrix (trivially positive definite)
#H <- diag(c(1.25, 0.75))
kde <- ks::kde(x = x, H=H, gridsize=500 )

#H = kde$H

# The eval.points slot contains the grids on x and y
str(kde$eval.points)
## List of 2
##  $ : num [1:151] -8.58 -8.47 -8.37 -8.26 -8.15 ...
##  $ : num [1:151] -5.1 -5.03 -4.96 -4.89 -4.82 ...

# The grids in kde$eval.points are crossed in order to compute a grid matrix
# where to compute the estimate
dim(kde$estimate)
## [1] 151 151

# Manual plotting using the kde object structure
image(kde$eval.points[[1]], kde$eval.points[[2]], kde$estimate,
      col = viridis::viridis(20), xlab = "x", ylab = "y")
points(kde$x) # The data is returned in $x



abs(max(ks::kde(x = x, H = H, eval.points = x, binned = TRUE)$estimate -
        ks::kde(x = x, H = H, eval.points = x, binned = FALSE)$estimate))




plot(kde, display = "slice", cont = c(25, 50, 75), xlab = "x", ylab = "y")



plot(kde, display = "slice", cont = c(10,95,99, 99.9), xlab = "x", ylab = "y")

# Perspective plot
plot(kde, display = "persp", col.fun = viridis::viridis, xlab = "x", ylab = "y")



df_single_location_e %>%
  dplyr::select( latitude,longitude) %>% 
  mutate_all( function(x) (x - min(x))/(max(x)-min(x))) %>%
  { . } -> x


boxplot(x$latitude)

# Simulated data from a trivariate normal
n <- 500
set.seed(213212)
x <- mvtnorm::rmvnorm(n = n, mean = c(0, 0, 0),
                      sigma = rbind(c(1.5, 0.25, 0.5),
                                    c(0.25, 0.75, 1),
                                    c(0.5, 1, 2)))

# Show nested contours of high density regions
ks=ks::kde(x = x)
ks=ks::kde(x = x, gridsize=151)

plot(ks)

e
ks
ks$H


x = orig

eigen(ks$H)$values

# Simulated univariate data
n <- 1e3
set.seed(324178)
x <- nor1mix::rnorMix(n = n, obj = nor1mix::MW.nm10)

# Location of relative extrema
dens <- function(x) nor1mix::dnorMix(x, obj = nor1mix::MW.nm10)
minus_dens <- function(x) -dens(x)
extrema <- c(nlm(f = minus_dens, p = 0)$estimate,
             nlm(f = dens, p = 0.75)$estimate,
             nlm(f = minus_dens, p = 1.5)$estimate)

# Plot target density
par(mfrow = c(2, 2))
plot(x)
rug(as.matrix(x))
abline(v = extrema, col = c(3, 2, 3))


ks::kdde(x = x, deriv.order = 0)$estimate 
ks$estimate 

plot(kdde_0, display = "persp", col.fun = viridis::viridis, xlab = "x", ylab = "y")

# Density estimation (automatically chosen bandwidth)
kdde_0 <- ks::kdde(x = x, deriv.order = 0)
plot(kdde_0, display = "filled.contour2", xlab = "x", ylab = "y")


# Density derivative estimation
kdde_1 <- ks::kdde(x = x, deriv.order = 1, gridsize=151)
str(kdde_1$estimate)
## List of 2
##  $ : num [1:151, 1:151] -2.94e-19 1.72e-19 -2.45e-19 3.49e-20 -1.02e-19 ...
##  $ : num [1:151, 1:151] 1.32e-19 1.09e-19 4.71e-19 2.03e-19 8.80e-20 ...
# $estimate is now a list of two matrices with each of the derivatives

# Plot of the gradient field - arrows pointing towards the modes
plot(kdde_1, display = "quiver", xlab = "x", ylab = "y")



kdde_2 <- ks::kdde(x = x, deriv.order = 2)
str(kdde_2$estimate)

par(mfcol = c(2, 2))
for(i in 1:4) {
  plot(kdde_2, display = "filled.contour2", which.deriv.ind = i,
       xlab = "x", ylab = "y")
}

points(x)

points( df_single_survey[,c('latitude','longitude')],   pch=19, col = "red")




################################################################################
#find_significant_densities 
################################################################################

x= orig$latitude
y=orig$longitude

x= df_single_location$latitude
y=df_single_location$longitude
z = ks$estimate

contour( 
        ks$eval.points[[1]],
        ks$eval.points[[2]],
        ks$estimate
)

dens <- kde2d(x, y, n=200)
# the contours to plot
prob <- c(0.9, 0.5)
dx <- diff(dens$x[1:2])
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy 


approx(c1, sz, xout = .9, ties=mean)

ks

levels <- sapply(prob, function(x) { 
                   approx(c1, sz, xout = 1 - x, ties=mean)$y
       })

plot(x,y)
contour(dens, levels=levels, labels=prob, add=T)

## Points within polygons
library(sp)
ls <- contourLines(dens, level=levels)
inner <- point.in.polygon(x,y , ls[[2]]$x, ls[[2]]$y)
out <- point.in.polygon(x,y, ls[[1]]$x, ls[[1]]$y)

## Plot
bw$region <- factor(inner + out)
plot(lat ~ lon, col=region, data=bw, pch=15)
contour(dens, levels=levels, labels=prob, add=T)






##  Simulate some lat/lon data:
x <- rnorm(363, 45, 10)
y <- rnorm(363, 45, 10)

##  Version 1 (without ggplot2):
library(MASS)
dens <- kde2d(x, y, n=200)

##  The contours to plot:
prob <- c(0.9, 0.5)
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
pts[!is.na( over(pts, polys[1]) )]
points(pts[!is.na( over(pts, polys[2]) )], col="blue", pch=20)

contour(dens, levels=levels, labels=prob, add=T)


  
