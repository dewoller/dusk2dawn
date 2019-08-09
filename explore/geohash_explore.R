#library(devtools)
#install_github("Ironholds/geohash")

source('lib/functions.R')
source('lib/get_data.R')
library(geohash)

df_location = get_df_location()

df_location %>%
  head(10000) %>%
  mutate( gh = gh_encode( latitude, longitude, 8)) %>%
  arrange( timestamp) %>%
  select( gh, latitude, longitude, everything() ) %>%
  count( gh, sort=TRUE )



gh_encode
