now() %>% as.integer()

now(tz='Europe/Zurich')  %>% as.integer()
now()  %>% as.integer()

a=now()





test = tibble(time = c(as_datetime('2019-01-01 00:00:00'),
                       as_datetime('2019-01-01 01:00:00'),
                       as_datetime('2019-01-01 00:00:00'),
                       as_datetime('2019-01-01 01:00:00')),
              tz = c('EST','EST','Asia/Hong_Kong','Asia/Hong_Kong'))

test %>%
  mutate(tz_group = tz) %>%
  tail(2) %>%
  nest( tz_times=c(time,tz)) %>%
  mutate(tz_times = map(tz_times, ~ mutate(., localtime = with_tz(time, tz[1]) ) )) %>%
  unnest( tz_times ) %>%
  mutate( ts = as.integer(localtime)) %>%
  { . } -> ass

tibble(time = c(('2019-01-01 00:00:00'),
                       ('2019-01-01 01:00:00'),
                       ('2019-01-01 00:00:00'),
                       ('2019-01-01 01:00:00')),
              tz = c('EST','EST','Asia/Hong_Kong','Asia/Hong_Kong')) %>%
  mutate( id=1:4) %>%
  mutate(tz_group = tz) %>%
  nest( tz_times=c(time,tz)) %>%
  mutate(tz_times = map(tz_times, ~ mutate(., localtime = ymd_hms( time, tz= tz[1])))) %>%
  unnest( tz_times ) %>%
  mutate( ts = as.integer(localtime)) %>%
  { . } -> ass




tibble(time = c(('2019-01-01 00:00:00'),
                ('2019-01-01 01:00:00'),
                ('2019-01-01 00:00:00'),
                ('2019-01-01 01:00:00')),
       tz = c('EST','EST','Asia/Hong_Kong','Asia/Hong_Kong')) %>%
group_by( tz ) %>% # group together all the same timezones so  that calculation is faster
mutate( ts = ymd_hms( time, tz=tz[1])) %>%
  ungroup() %>%
  mutate( timestamp= seconds( ts ) %>% as.numeric()) %>%


mutate( ts = ymd_hms( timestamp, tz=min( timezone))) %>%




get_df_all_new <- function( ) {





  survey_type='pre'
  get_one_survey_type = function( df_all, survey_type)   {

    df_all %>%
      dplyr::select( id, userid, night, starts_with( paste0( survey_type, '_'))) %>%
      dplyr::select( id, userid, night, ends_with( '_timestamp'), ends_with('_timezone_id')) %>%
      distinct()  %>%
      dplyr::rename( timestamp=4, timezone=5) %>%
      mutate(which=survey_type ) %>%
      filter( timestamp != "")

  }

loadd(df_all)

  bind_rows(
            get_one_survey_type( df_all, 'dq'),
            get_one_survey_type( df_all, 'env'),
            get_one_survey_type( df_all, 'forg'),
            get_one_survey_type( df_all, 'video'),
            get_one_survey_type( df_all, 'load'),
            get_one_survey_type( df_all, 'pre'),
            get_one_survey_type( df_all, 'tom')
            ) %>%
  mutate(tz_group = timezone) %>%
  nest( tz_times=c(timestamp,timezone)) %>%
  mutate(tz_times = map(tz_times, ~ mutate(., localtime = ymd_hms( timestamp, tz= timezone[1])))) %>%
  unnest() %>%
  mutate( original_timestamp = timestamp) %>%
  mutate( timestamp= localtime  %>% as.numeric()) %>%
    { . } -> df_all_ts_new

readd(df_all_ts) %>%
  inner_join( df_all_ts_new, by=c('id','which')) %>%
  mutate( diff = timestamp.x-timestamp.y) %>%
  distinct(diff)

df_all_ts_new %>%
  mutate(time_of_day = (hour(original_timestamp) * 60 + minute(original_timestamp)) / 60)  %>%
ggplot( aes( time_of_day)) +
  geom_histogram( ) +
  ggtitle('Assuming timestamp is LOCALTIME, in that timezone') +
  facet_wrap( ~which ) -sadgg



df_all_ts_new %>%
  mutate( gmt = ymd_hms( original_timestamp)) %>%
  group_by( timezone ) %>% # group together all the same timezones so  that calculation is faster
  mutate( ts = ymd_hms( gmt, tz=timezone[1])) %>%
  ungroup() %>%
  mutate(time_of_day = (hour(ts) * 60 + minute(ts)) / 60)  %>%
  ggplot( aes( time_of_day)) +
  geom_histogram( ) +
  ggtitle('Assuming timestamp is GMT ') +
  facet_wrap( ~which ) -sadgg

a = ymd('2014-10-26')

df_all_ts_new %>%
  filter( localtime >= a & localtime < a+2 ) %>%
  mutate(time_of_day = (hour(original_timestamp) * 60 + minute(original_timestamp)) / 60)  %>%
  ggplot( aes( time_of_day)) +
  geom_histogram( ) +
  ggtitle('Assuming timestamp is LOCALTIME, in that timezone, DST changeover day only') +
  facet_wrap( ~which ) -sadgg

df_all_ts_new %>%
  filter( localtime >= a & localtime < a+2 ) %>%
  mutate(dst(localtime))  %>%
  pluck('localtime')

Sys.time()S

times = c("26-10-2014 01:00", "26-10-2014 02:00",
          "26-10-2014 03:00", "26-10-2014 04:00")

b= dmy_hm(times, tz = "Europe/Brussels") %>% as.integer()

b[1]-b[2]
b[1]-b[3]
b[1]-b[4]


function() {

}
read_csv('data/EveningMasterFullAnonym.csv', col_types= get_df_all_specs()) %>%
  dplyr::rename( userid = user ) %>%
  mutate( night =  ymd(sprintf('2014%04s', day ))) %>%
  dplyr::select( id, userid, night, ends_with( '_timestamp')) %>%
  tidyr::gather( category, timestamp, -id, -userid, -night) %>%
  mutate( which=str_replace(category, '_timestamp','')) %>%
  filter(!is.na(timestamp)) %>%
  select(-category) %>%
  { . } -> df1

read_csv('data/EveningMasterFullAnonym.csv', col_types= get_df_all_specs()) %>%
  dplyr::select( id, ends_with( '_timezone_id')) %>%
  tidyr::gather( category, timezone, -id) %>%
  mutate( which=str_replace(category, '_timezone_id','')) %>%
  filter(!is.na(timezone)) %>%
  select(-category) %>%
  { . } -> df2

df2 %>%
  distinct(timezone) %>%
  anti_join( dftz)

OlsonNames() %>%
  enframe() %>%
  rename(timezone = value) %>% 
  { . } -> dftz


df1  %>% 
  inner_join( df2, by=c('id', 'which')) %>%
  group_by(timezone) %>%
  mutate( )

 
