

tribble( ~dataset, "df_osm_amenities","df_osm_leisure"  ) %>% 
rowwise() %>%
do( saveRDS(get(.$dataset),  file = paste0('data/shiny/', .$dataset, '.rds' )))

tribble( ~dataset, "df_bars","df_best_location", "df_all_ts"  ) %>% 
rowwise() %>%
do( saveRDS(get(.$dataset),  file = paste0('data/shiny/', .$dataset, '.rds' )))
