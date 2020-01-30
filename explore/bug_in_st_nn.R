library(nngeo)
library(purrr)
library(tidyverse)
data(cities)
data(towns)

cities = st_transform(cities, 32636)
towns = st_transform(towns, 32636)

st_nn(cities, towns, k=1, returnDist=TRUE) ->a


a[[1]] %>% unlist()

cities %>%
  rename(city_name = name) %>%
  mutate( dist= unlist(a[[2]]),
  town_id = unlist(a[[1]])
  )  %>%
  mutate( town_name = towns[ town_id,] $name )%>%



reprex::reprex()

 cached() %>% enframe() -> cache

cache %>%
  filter( str_detect(value, '^df_summarise_staypoints')) %>%


