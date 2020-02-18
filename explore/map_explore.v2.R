

#row= tribble( ~filename,'data/save_1200_1200_10_20_.rds')
#source('lib/evaluate_staypoint_estimates_helper.R')
analyse_staypoint_set_time_and_geography_detail( row )

a <- analyse_staypoint_base_information_detail (row)

map_one_night <- function( userid, night) {



}

data(World)
tm_shape(NLD_prov) + tm_polygons('population') + tm_layout(basemaps = c('OpenStreetMap'))

needs(sf)

boston <- st_read("../data/boston.geojson")

proj4 <- st_crs(World)$proj4string


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
library(sf)
tmap_mode("plot")

df_pruned_points() %>%
#    group_by( n_staypoint) %>%
#    summarise( latitude=mean(latitude), longitude=mean(longitude)) %>%
st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")  %>%
  #  mutate( ts = as.numeric( cut(timestamp, 8))) %>%
  #    filter( n_staypoint>0 ) %>%
#  mutate( n_staypoint = as.factor( n_staypoint )) %>%
#  mutate( size=ifelse( n_staypoint==0, .01, 1 )) %>%
  tm_shape()  +
  tm_bubbles(size=.01)  +
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
#  tm_shape( df_osm_leisure$osm_polygon %>% filter( !is.na( leisure)) ) +
#  tm_polygons( col='gray', id='name') +
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

df1 %>%
  ggplot( aes( latitude )) +
  geom_histogram( )

}



library(tmap)
library(tmaptools)
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3
tmap_mode("plot")
#> tmap mode set to plotting
data(NLD_muni)
NLD_bb <- bb(st_transform(NLD_muni, 4326))
test <- read_osm(NLD_bb, type = "osm")
tm_shape(test) + tm_rgb()


CBS_bb <- bb("CBS Weg 11, Heerlen", width=.003, height=.002)
CBS_osm1 <- read_osm(CBS_bb, type="osm")
tm_shape(CBS_osm1) + tm_rgb()

CBS_bb <- bb("La Trobe University, Bundoora, Victoria, Australia", width=.003, height=.002)
CBS_osm1 <- read_osm(CBS_bb, type="osm")
tm_shape(CBS_osm1) + tm_rgb()


# load Netherlands shape
data(NLD_muni)

# read OSM raster data
osm_NLD <- read_osm(bb(NLD_muni, ext=1.1, projection ="longlat"))

# plot with regular tmap functions
tm_shape(osm_NLD) +
  tm_raster() +
  tm_shape(NLD_muni) +
  tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Purples")



library(spatialEco)


points2poly = function( df ) {

  df[[1]] %>%
    st_buffer(dist =  meuse$elev*15) %>%
    sf::st_segmentize(dfMaxLength = 5) %>%
    sf::st_coordinates() %>%
    as_tibble() %>%
    dplyr::select(X, Y) %>%
    sf::st_as_sf(coords = c("X", "Y")) %>%
    spatialEco::convexHull(alpha = 10000, sp=FALSE) %>%
    pluck('geometry')  

}

a=points2poly(meuse)

data(meuse)
sp::coordinates(meuse) = ~x+y
meuse <- as(meuse, "sf")


meuse %>%
  filter( !is.na(landuse)) %>%
  nest(data=-landuse) %>%
  group_by( landuse) %>%
  mutate( poly = points2poly( data )  ) %>%
  select(-data) %>%
  ungroup() %>%

plot(sf::st_geometry(a), cex=100, col="red")
plot(sf::st_geometry(meuse_poly), add=TRUE)

par(mfcol=c(2,2))
for (a in c(500, 1500, 5000, 100000)) {
  plot(sf::st_geometry(ch), cex=1.5, col="red")
  plot(sf::st_geometry(meuse_poly), add=TRUE)
  title(paste0("alpha=", a))
}

#
#

library(tmap)
library(tmaptools)
    # load Netherlands shape
    data(NLD_muni)

    # read OSM raster data
    osm_NLD <- read_osm(NLD_muni, ext=1.1)

    # plot with regular tmap functions
    tm_shape(osm_NLD) +
        tm_raster() +

    tm_shape(NLD_muni) +
        tm_polygons("population", convert2density=TRUE, style="kmeans", alpha=.7, palette="Purples")

    #### A close look at the building of Statistics Netherlands in Heerlen

    # create a bounding box around the CBS (Statistics Netherlands) building
    CBS_bb <- bb("CBS Weg 11, Heerlen", width=.003, height=.002)

    # read Microsoft Bing satellite and OpenCycleMap OSM layers
    CBS_osm1 <- read_osm(CBS_bb, type="bing")
    CBS_osm2 <- read_osm(CBS_bb, type="opencyclemap")

    # plot OSM raster data
    qtm(CBS_osm1)
    qtm(CBS_osm2)

    # read vectorized OSM data
    CBS_osm3 <- read_osm(CBS_bb,
        roads=osm_line("highway"),
        parking=osm_poly("amenity=parking"),
        building=osm_poly("building"),
        park=osm_poly("leisure=park"),
        railway_area=osm_poly("landuse=railway"),
        railway=osm_line("railway"),
        forest=osm_poly("landuse=forest"),
        grass=osm_poly("landuse=grass"),
        bicycle=osm_line("highway=cycleway"))

    # plot vectorized OSM data
    tm_shape(CBS_osm3$grass, bbox=CBS_bb) + tm_polygons("darkolivegreen3") +
        tm_shape(CBS_osm3$forest) + tm_fill("forestgreen") +
        tm_shape(CBS_osm3$railway_area) + tm_fill(col="grey70") +
        tm_shape(CBS_osm3$parking) + tm_polygons("gold") +
        tm_shape(CBS_osm3$building) + tm_polygons("grey50") +
        tm_shape(CBS_osm3$roads, bbox=CBS_bb) + tm_lines(col="gold", lwd=3) +
        tm_shape(CBS_osm3$bicycle) + tm_lines(col="blue", lwd=3) +
        tm_shape(CBS_osm3$railway) + tm_lines(col="grey20", lwd=3, lty="dashed") +
#         tm_layout(bg.color="grey90")


data(metro)

