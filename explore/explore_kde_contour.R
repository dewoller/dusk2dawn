library(ks)
# find significant features
df_single_location_e  %>%
  dplyr::select( latitude,longitude) %>%
  { . } -> d

ks.ll = kde(d)

plot(fhat)

contour.95 <- with(ks.ll,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["55%"])[[1]])

contour.95 %>%
  data.frame() %>%
  ggplot( aes( x,y )) + geom_point()

ks.ll = kde(d)

contour.95 <- with(ks.ll,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["95%"])[[1]])
plot(contour.95)

ks.ll$cont["15%"][[2]]

# find area of polygon
library(sp)
library(rgeos)
poly <- with(contour.95,data.frame(x,y))
poly <- rbind(poly,poly[1,])    # polygon needs to be closed...
spPoly <- SpatialPolygons(list(Polygons(list(Polygon(poly)),ID=1)))
gArea(spPoly) * m_per_latitude * m_per_longitude

contourSizes(ks.ll, cont = 95, approx=TRUE) * m_per_latitude * m_per_longitude


ggplot(data=d, aes(latitude, longitude)) +
  geom_point() +
  geom_path(aes(x, y), data=contour.95) +
  theme_bw()

m_per_latitude = 111320
m_per_longitude = 111319.488
desired_grid = 10 #meters

library(drake)
library(tidyverse)
library(janitor)
library(ks)

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


df_single_location_e  %>%
  dplyr::select( latitude,longitude) %>%
  { . } -> d

# find significant features
df_single_location

d %>%
  find_significant_densities( contigious_range=4) %>% 
  { . } -> d4


d %>%
  find_significant_densities() %>%
  { . } -> b


df_single_location %>%
  dplyr::select( userid, night, timestamp, latitude, longitude ) %>%
  merge( y=d4 %>% dplyr::select( -original, -kfs),  all=TRUE ) %>%
  as_tibble() %>%
  filter( 
         latitude >= min_lat & 
           longitude >= min_lon & 
           latitude <= max_lat & 
           longitude <= max_lon  ) %>%
  group_by( feature_group ) %>%
  summarise( latitude = median(latitude ), 
            longitude = median( longitude),
            n = n()) %>% 
  { . } -> e




