
library( knitr )
opts_chunk$set(cache=TRUE, autodep=TRUE)

source('lib/functions.R')
source('lib/get_data.R')
source('lib/location_prep.R')
library(tidyverse)
library( dplyr)
library(lubridate)

# load in the individual locations information
df_location = get_df_location()

first = df_location %>% head( 1 ) %>% select( userid, night)

# load in the best guess location information
df_best_location = get_df_best_location( df_location )


df_location %>%
  group_by( userid, night, from ) %>%
  summarise( max=median( time_stamp)) %>%
  spread( from, max) %>%
  mutate( val=round( active-passive, -3 ) ) %>%
  ungroup() %>%
  count( val, sort=TRUE )

  ggplot( aes(val )) + geom_histogram()




df_location %>%
  inner_join( df_location %>% distinct( userid ) %>% head( 16 ) %>% tail(1) ) %>%
    mutate( x=cut( accuracy, 6)) %>%
    arrange( time_stamp ) %>%
    ggplot( aes( latitude, longitude, group=from, color=from )) +
    geom_line() +
    geom_point( aes(size=x)) +
    facet_wrap( ~night)




