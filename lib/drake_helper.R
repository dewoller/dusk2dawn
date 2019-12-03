
stub="df_matching_survey"
n=1
readd_f = function( stub, n=1 ) {

  cached() %>% 
    enframe() %>%
    filter( startsWith( value, stub )) %>%
    head(n) %>%
    tail(1) %>%
    purrr::pluck("value") %>% 
    readd( character_only=TRUE)
}
