cached() %>% enframe() -> cache

cache %>%
  filter( endsWith( value, 'best_algorithm'))


cache %>%
  filter( str_detect(value, 'df_count' ))

