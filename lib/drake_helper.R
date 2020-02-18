
stub="df_matching_survey"
n=1

readd_f = function( stub, n=1 ) {

  cached() %>% 
    enframe() %>% 
    { . } -> cache


  cache %>%
    filter( startsWith( value, 'staypoints_distance' )) %>%
    filter( str_detect( value, 'inter')) %>%
    head(n) %>%
    tail(1) %>%
    purrr::pluck("value") %>% 
    readd( character_only=TRUE)
}
