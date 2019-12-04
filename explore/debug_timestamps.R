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

df_all_ts %>%


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
