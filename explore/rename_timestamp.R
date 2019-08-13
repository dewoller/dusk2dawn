list.files( path='data/', pattern='save_v0.*rds', full.names=TRUE ) %>%
  enframe(value = 'filename' ) %>%
  rowwise() %>%
  do( rename_time_stamp(.) ) %>% 
  { . } -> df_files


rename_time_stamp <- function( row ) {

  print( paste( row, collapse=',' ))
  out_f = row$filename %>% str_replace( 'v0', 'v1')


  readRDS(row$filename)  %>%
  rename( timestamp = starts_with('time_stamp') ) %>%
  saveRDS( file=out_f)

  data.frame(1)
}
