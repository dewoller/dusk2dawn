source('lib/functions.R')
library(purrr)
source('lib/get_data.R')

read.csv('data/EveningMasterFullAnonym.csv') %>% 
  as.tibble %>% 
{ . } -> df_all


# for each directory in sensors
# for each file in directory
# read file
# push file to database

dir_name  = '/mnt/sda5/home/dewoller/mydoc/research/dusk2dawn/initialAnalysis/data/sensors'

#df_all %>% head(10) %>% my_dbWriteTable( "test")

#-------------------------------------------------------------
# not enough memory!
import_all_files = function( dir_stub ) {

  my_db_read( paste0( "drop table if exists ",  dir_stub, "") ) %>% print()
  mdir = paste0(dir_name , '/', dir_stub)

  map( list_files( mdir, min_size=30000 ), get_one_file, dir_stub )  %>%
    tibble() %>% 
    unnest() %>%
    my_db_write( dir_stub )
  FALSE

}

list.files( mdir,full.names=TRUE) 

#-------------------------------------------------------------
list_files  <- function(path=".", pattern=NULL, min_size=50000000, 
                        all.files=FALSE, full.names=FALSE, recursive=FALSE,
                        ignore.case=FALSE, include.dirs=FALSE, no..=FALSE) {

  pre <- list.files(path, pattern, all.files, full.names, recursive, ignore.case, 
                    include.dirs, no..)

  purrr::discard(pre, ~file.size(.)<min_size)

}
#-------------------------------------------------------------
create_table = function( dir_stub ) {

  my_db_read( paste0( "drop table if exists ",  dir_stub, "") ) %>% print()
  mdir = paste0(dir_name , '/', dir_stub)

  list_files( mdir, min_size=3000, full.names=TRUE ) %>%
    head(1) %>%
    map( get_one_file, dir_stub)  %>%
    tibble() %>% 
    unnest() %>%
    my_db_write( dir_stub )

  my_db_read( paste0( "truncate table ",  dir_stub, "") ) %>% print()
  my_db_read( paste0( "alter table ",  dir_stub, ' drop column "row.names"') ) %>% print()

  FALSE

}


#-------------------------------------------------------------
create_indexes = function( dir_stub ) {
  cat(dir_stub)

  my_db_read( paste0( "create index on ",  dir_stub, " (userid, night)") ) 
  my_db_read( paste0( "create index on ",  dir_stub, " (night)") ) 
  FALSE

}

dir_stub='sms'
#-------------------------------------------------------------
get_one_file = function( filename, dir_stub ) {

  read.csv( filename, stringsAsFactors=FALSE)  %>% head(1) 
}
  

#-------------------------------------------------------------
list.files( dir_name ) %>% 
  as.tibble() %>%
  filter( !endsWith( value, 'gz')) %>%
  rowwise() %>%
  mutate( create_table(value) )

list.files( dir_name ) %>% 
  as.tibble() %>%
  filter( !endsWith( value, 'gz')) %>%
  rowwise() %>%
  mutate( create_indexes(value) )

``

