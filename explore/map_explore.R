

loadd()
df_sp_joined_geography = 


df_target_locations_combined  %>%
  mutate( id = row_number()) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
  st_transform( 27700) %>%
  { . } -> df_target_locations_combined_P



df_all_staypoints_multi  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform( 27700) %>% 
  st_join( df_target_locations_combined_P, join = st_is_within_distance, dist = 20) %>%
  filter(!is.na(name)) %>% 
  { . } -> c


DT_sf = st_as_sf(DT, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")
