needs( 'fuzzyjoin')
library(IRanges)
library(tmap)

df_location %>%
inner_join( df_best_location %>% distinct( userid, night) %>% head(2) %>% tail(1)) %>% 
{ . } -> b

df_file_names %>% 
pull(filename) %>% 
.[1] %>% 
readRDS -> a


b %>%
  dplyr::rename( timestamp= timestamp ) %>% 
  arrange( timestamp ) %>%
  prune_gps_outliers( sigma=1, width=15) %>% 
  { . } -> df_pruned
#

df_pruned  %>%
  findStayPoint() %>% 
  filter( n_staypoint > 0 ) %>%
  group_by( n_staypoint) %>%
  summarise( latitude = mean( latitude), 
            longitude = mean( longitude), 
            time = max( timestamp ) - min(timestamp)) %>%
  sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326) 
{ . } -> df_sp_pruned


df_all_ts %>% 
  inner_join( b %>% 
      ungroup() %>%
      distinct( userid, night) 
    ) %>%
  distinct( which, timestamp, userid, night) %>%
  arrange( timestamp) %>%
  difference_right_join( df_pruned , by=c('timestamp'), max_dist=300, distance_col='ts_dist')  %>% 
  filter( !is.na( ts_dist )) %>%
  group_by(which, timestamp.x) %>%
  filter( ts_dist == min(ts_dist )) %>%
  sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
  { . } -> surveys

#
  df_pruned %>% 
    sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>%
    arrange(timestamp) %>%
    summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")  %>%
    tm_shape()  + 
    tm_lines() + 
    tm_shape( surveys ) +
    tm_symbols( col='red', alpha=.1 ) +
    tm_shape( df_sp_pruned ) +
    tm_symbols( col='green', alpha=.5 ) 


  bprime = b %>%
    sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>%
    arrange(timestamp) %>%
    summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")  

tmap_mode('view')

  bsp %>% distinct( n_staypoint)
  df_sp_pruned%>% distinct( n_staypoint)

  df_sp_pruned %>% View


  my_db_read( 'select * from location limit 1' ) %>% names
