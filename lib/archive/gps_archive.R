
################################################################################
# gpsbabel 
# for cleaning points using gpsbabel
################################################################################
gpsbabel = function( df, parameters ) {

  f_in=tempfile()
    f_out=tempfile()
    f_in='/tmp/a.xml'
    f_out='/tmp/b.xml'

    df %>% 
    mutate( .id = row_number()) %>% 
    { . } -> df


  df %>% 
    select( longitude, latitude, .id ) %>% 
    rename( name=.id) %>% 
    glue_data( '<rtept lat="{latitude}" lon="{longitude}" name="{name}"></rtept>') %>% 
    paste( collapse = "\\n") %>%
    { . } -> body


  writeLines(paste0( "<gpx>\n <rte>\n", body, "</rte>\n</gpx>\n"), f_in)


    system( paste ( " gpsbabel -i gpx -f  ", 
          f_in,
          "-x ", 
          parameters,
          "-o csv -F ",
          f_out 
          )
        )

    read_csv( f_out, col_names=c("longitude", "latitude", ".id"), col_types='ddc') %>%
    mutate( .id = str_replace(.id, 'RPT','') %>% as.numeric()) %>% 
    { . } -> df_simplified

  df %>%
    select( -longitude, -latitude ) %>%
    inner_join( df_simplified, by=".id")  %>%
    select(-.id)

}

################################################################################
# arrange for party_df  
################################################################################
carrange_.party_df <- function (.data, ..., .dots = list()) 
{
  multidplyr:::shard_call(.data, quote(dplyr::arrange), ..., .dots = .dots, 
      groups = .data$groups[-length(.data$groups)])
}


