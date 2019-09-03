

#row= tribble( ~filename,'data/save_1200_1200_10_20_.rds')
#source('lib/evaluate_staypoint_estimates_helper.R')
analyse_staypoint_set_time_and_geography_detail( row )

a <- analyse_staypoint_base_information_detail (row) 

map_one_night <- function( userid, night) {
  


}


tm_shape(NLD_prov) + tm_polygons('population') + tm_layout(basemaps = c('OpenStreetMap'))

needs(sf)

boston <- st_read("../data/boston.geojson")

proj4 <- st_crs(world)$proj4string


tm_shape(boston) +
  tm_polygons('#f0f0f0f0', border.alpha = 0.2) +
  tm_shape(student_points_rastered) +
  tm_raster(alpha = 0.7, title = '# of students')

student_points <- b %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = proj4)

tm_shape(boston) +
  tm_polygons('#f0f0f0f0', border.alpha = 0.2) +
  tm_shape(student_points_rastered) +
  tm_raster(alpha = 0.7, title = '# of students')


row= tribble( ~filename,'data/save_1200_60_10_10_df.rds')


 analyse_staypoint_set_geography_summary(row) 







test == function() {
#
  df_osm_amenity  = get_df_osm_locations_amenity()
  df_osm_leisure  = get_df_osm_locations_leisure()

library(tmap)
tmap_mode("plot")
df_rds %>%
  inner_join( limit )
#    group_by( n_staypoint) %>%
#    summarise( latitude=mean(latitude), longitude=mean(longitude)) %>%
st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant") -> df
df %>%
  #  mutate( ts = as.numeric( cut(timestamp, 8))) %>%
  #    filter( n_staypoint>0 ) %>%
  mutate( n_staypoint = as.factor( n_staypoint )) %>%
  mutate( size=ifelse( n_staypoint==0, .01, 1 )) %>%
  tm_shape()  + 
  tm_symbols(col = "n_staypoint", shape = "n_staypoint", scale = .3, size='size')  +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_basemap(leaflet::providers$Stamen.TonerLite)  +
  #tm_shape( df_bars ) +
  #tm_bubbles( col='red', scale=.3) +
  #tm_shape( df_osm_amenities$osm_points %>% filter( !is.na( amenity)) ) +
  #tm_bubbles( col='black', scale=.3, id='name')+
  #tm_shape( df_osm_leisure$osm_points %>% filter( !is.na( leisure)) ) +
  #tm_bubbles( col='black', scale=.3, id='name')+
  #tm_shape( df_osm_amenities$osm_polygon %>% filter( !is.na( amenity)) ) +
  #  tm_polygons(col='black',  id='name')+
  tm_shape( df_osm_leisure$osm_polygon %>% filter( !is.na( leisure)) ) +
  tm_polygons( col='gray', id='name') +
  tm_scale_bar()

of_lausanne$osm_points %>%
  as_tibble() %>%
  filter( is.na(amenity )) %>% View

df_lausanne$osm_points$amenity %>%
  enframe() %>%
  count( value, sort=TRUE) %>% View

df_zurich$osm_points$amenity %>%
  enframe() %>%
  count( value, sort=TRUE) %>% View


df_zurich$osm_points$leisure %>%
  enframe() %>%
  count( value, sort=TRUE) %>% View

df_zurich$osm_polygons %>%
  enframe() %>%
  count( value, sort=TRUE) %>% View

df_zurich


readRDS(row$filename)  %>%
  inner_join( limit ) %>% 
  { . } -> a


a %>%
  ungroup() %>%
  arrange( desc( latitude)) %>% 
  select( latitude ) %>%
  head(1) %>% 
  { . } -> b


a %>% View
a %>% inner_join( b) %>% View


a %>%
  ggplot( aes( longitude )) +
  geom_histogram( )

a %>%
  ggplot( aes( latitude )) +
  geom_histogram( )

}


