library(ks)
library(IRanges)
library(drake)
library(tidyverse)
library(janitor)
library( raster )
loadd(df_location)
loadd(staypoints_distance_300_900_10_filtered_sigma_100 )
source( 'lib/kernel_density_functions.R')

#df_location %>%
#  filter( accuracy < 20) %>%
#  group_by( userid, night) %>%
#  count( sort=TRUE)

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


library(adehabitatHR)
library(move)
