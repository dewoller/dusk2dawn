safe_load("RPostgreSQL")
safe_load("keyring")
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
