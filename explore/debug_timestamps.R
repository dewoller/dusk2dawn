df_all %>% 
filter( userid=='05f35693-7fec-4372-af78-7bd904c187e0' ) %>%  
filter( load_timestamp != '')    %>%


df_all

df_all_ts %>%
  ungroup() %>%
  count( which, timestamp, userid, night, sort=TRUE ) %>%
  filter( n>1) %>%
  head(1) %>%
  inner_join( df_all ) %>% 

  df_all %>% count( type)

  arrange(id)

  summarise( sum(n)) %>%
  inner_join( df_all_ts, by=c('which','timestamp', 'userid', 'night')) %>%
  inner_join( df_all, by=c('id', 'userid', 'night')) %>% 
  dplyr::select( which,timestamp, userid, night, starts_with('forg')) %>% View

df_all %>%
  filter( userid=='05f35693-7fec-4372-af78-7bd904c187e0' ) %>%
  filter( load_timestamp != '' & day==1114) %>%
  mutate_all( as.character ) %>%
  pivot_longer( -id,  names_to = "income", values_to = "count") ->a

a

 a %>%
  filter( id==37 ) %>%
  inner_join( a %>%
  filter( id==38 ), by='income') %>%
  filter( count.x != count.y) 

df_all %>%
  filter( userid=='05f35693-7fec-4372-af78-7bd904c187e0' ) %>%
  filter( load_timestamp != '' & day==1114) %>%
  mutate_all( as.character ) %>%
  pivot_longer( -id,  names_to = "income", values_to = "count")


df_all 

df_all_ts %>%
  ungroup() %>%
  count( timezone, sort=TRUE ) %>%


df_all %>%
  mutate_all( as.character ) %>%
  dplyr::select( id, userid, night, 
                starts_with('pre_'), starts_with('load_'), starts_with('forg_'), starts_with('dq_'), starts_with('tom_'), starts_with('env_'), starts_with('video_'))  %>%
  pivot_longer( cols=c(-id, -userid, -night) ) %>% 
  { . } -> a


df_all_ts %>%
  ungroup() %>%
  count( which, timestamp, userid, night, sort=TRUE ) %>%
  filter( n>1) %>%
  mutate_all( as.character ) %>%
  inner_join( a ) %>%
  count( which, timestamp, userid, night, n, name, value) %>%
  mutate_all( as.character ) %>% 
  filter( nn != n) %>%

df_all %>%
  dplyr::select(ends_with('stamp')) %>%
  dplyr::select(starts_with('episod')) %>%

'pre_', 'load_', 'forg_', 'dq_', 'tom_', 'env_', 'video_',

starts_with('pre_'), starts_with('load_'), starts_with('forg_'), starts_with('dq_'), starts_with('tom_'), starts_with('env_'), starts_with('video_',)



df_location %>% count(timezone)



df_all %>% 
  dplyr::select(ends_with('timezone_id')) %>%
  pivot_longer(cols=everything()) %>%
  count( value, sort=TRUE) %>%

df_all %>% 
  dplyr::select(ends_with('timezone_display_name')) %>%
  pivot_longer(cols=everything()) %>%
  count( value, sort=TRUE) %>%

  df_all %>% 
  dplyr::select(ends_with('timezone_raw_offset')) %>%
  pivot_longer(cols=everything()) %>%
  count( value, sort=TRUE) %>%


  dplyr::select(ends_with('timEurope/Zurich ezone_id'), ends_with('timezone_display_name'), ends_with('timezone_raw_offset')) %>%


read_csv('/store/aarnet_owncloud/R/location.csv',
            col_types= cols(
                  userid = col_character(),
                  night = col_double(),
                  type = col_character(),
                  time_stamp = col_double(),
                  timezone = col_double(),
                  local_time = col_character(),
                  source = col_double(),
                  latitude = col_double(),
                  longitude = col_double(),
                  speed = col_double(),
                  accuracy = col_double(),
                  provider = col_character(),
                  bearing = col_double()
              )
                    ) %>% 
bind_rows( 
          read_csv('/store/aarnet_owncloud/R/passivelocation.csv',
                    col_types= cols(
                        userid = col_character(),
                        night = col_double(),
                        type = col_character(),
                        time_stamp = col_double(),
                        timezone = col_double(),
                        local_time = col_character(),
                        source = col_double(),
                        latitude = col_double(),
                        longitude = col_double(),
                        speed = col_double(),
                        accuracy = col_double(),
                        provider = col_character(),
                        bearing = col_double()
                    ))) %>% 
{ . } -> df_loca


library(lubridate)
df_loca %>%
  mutate( computed_date = parse_date_time( local_time, 'ymdHMS', tz=a),
          computed_ts = as.integer(computed_date), 
 diff =  time_stamp - computed_ts) %>%
 count(diff, sort=TRUE) %>%
  filter( time_stamp != computed_ts) %>%



df_all_ts_valid  %>%
  distinct( userid, night, timezone) %>%
  count( userid, night, sort=TRUE, name='n_survey') %>%
  filter( n_survey>1 ) %>%
  inner_join( df_all_ts_valid %>% count( userid, night, timezone, which, ts)) %>%
  arrange( userid, night) %>%
  distinct( userid, night ) %>%
  head(9) %>%
kableExtra::kable() %>% clipr::write_clip()

df_all_ts_valid  %>%
  distinct( userid, night, timezone) %>%
  count( userid, night, sort=TRUE, name='n_survey') %>%
  filter( n_survey>1 ) %>%
  inner_join( df_all_ts_valid %>% count( userid, night, timezone, which, ts)) %>%
  head(9) %>%
  distinct( userid, night) %>%
  inner_join( df_location) %>%
  distinct(timezone)

df_all_ts_valid  %>%
  distinct( userid, night, timezone) %>%
  count( userid, night, sort=TRUE, name='n_survey') %>%
  filter( n_survey>1 ) %>%
  inner_join( df_all_ts_valid %>% count( userid, night, timezone, which, ts)) %>%
  head(9) %>%
  distinct( userid, night) %>%
  inner_join( df_location) %>% 
  { . } -> a


library(tmap)
library(sf)
tmap_mode("plot")

a %>% 
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
  { . } -> df

  #  mutate( ts = as.numeric( cut(timestamp, 8))) %>%
  #    filter( n_staypoint>0 ) %>%
  tm_basemap(leaflet::providers$Stamen.TonerLite)  +
  tm_shape(World %>% filter(name=="Switzerland")) +
  tm_polygons() +
  tm_shape(df)  + 
  tm_bubbles( col='red', scale=.3) +
  tm_scale_bar(position=c("left", "bottom")) +
  #tm_shape( df_bars ) +
  #tm_bubbles( col='red', scale=.3) +
  #tm_shape( df_osm_amenities$osm_points %>% filter( !is.na( amenity)) ) +
  #tm_bubbles( col='black', scale=.3, id='name')+
  #tm_shape( df_osm_leisure$osm_points %>% filter( !is.na( leisure)) ) +
  #tm_bubbles( col='black', scale=.3, id='name')+
  #tm_shape( df_osm_amenities$osm_polygon %>% filter( !is.na( amenity)) ) +
  #  tm_polygons(col='black',  id='name')+
  tm_scale_bar()

data(World, metro, rivers, land)
data(Europe)

tm_shape(Europe) +
  tm_polygons("HPI")
