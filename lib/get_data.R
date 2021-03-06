my_db_name = 'dusk2dawn'
# -------------------------------------------------
my_db_write <- function ( df, table_name ) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = my_db_name,
                   host = "localhost", port = 5432,
                   user = "dewoller", password = Sys.getenv('PASSWD'))
  on.exit(dbDisconnect(con))
  dbWriteTable( con, table_name, df )
}

## -------------------------------------------------
my_db_append <- function ( df, table_name ) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = my_db_name,
          host = "localhost", port = 5432,
          user = "dewoller", password = Sys.getenv('PASSWD'))
  on.exit(dbDisconnect(con))
  dbWriteTable( con, table_name, df, append=TRUE )
}
# -------------------------------------------------

# -------------------------------------------------
my_db_read <- function ( query ) {

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = my_db_name,
          host = "localhost", port = 5432,
          user = "dewoller", password = Sys.getenv('PASSWD'))
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query )

}


# -------------------------------------------------

get_location = function( ) {
  my_db_read( 'select * from location') %>% 
    mutate( from='active') %>%
    dplyr::rename( timestamp = time_stamp) %>%
    as_tibble() 
  }
# -------------------------------------------------

get_passive_location = function( ) {
  my_db_read( 'select * from passivelocation') %>% 
    mutate( from='passive') %>%
    dplyr::rename( timestamp = time_stamp) %>%
    as_tibble() 
}

# -------------------------------------------------

get_df_location = function( ) {
  readRDS('data/df_location.rds')
  #bind_rows( get_location(), get_passive_location())
}

# -------------------------------------------------

get_df_single_location = function( ) {

  readRDS('data/df_singlelocation.rds')
#  df = get_df_location()

#  df %>%
#    inner_join( head( df, 1 ) %>% select( userid, night), by=qc(userid, night))

}

