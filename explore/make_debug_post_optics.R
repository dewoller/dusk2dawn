
cached() %>% 
enframe() %>% 
{ . } -> cache


cache %>%
  filter( str_detect( value, 'optic'))
