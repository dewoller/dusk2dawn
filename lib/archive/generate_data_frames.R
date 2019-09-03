# get the actual data from the cache, put it in the parent enironment

cache_directory_name = 'data/cache/'
cache_file_prefix = 'mycache_'
df_suffix = '_rr'
library('DataCache')

clear_cache = function( df_suffix ='_rr') { 

  rmcache = paste0( 'rm ', cache_directory_name, '/', cache_file_prefix, df_suffix, '*')
#  dput(rmcache)
  system( rmcache )
}

test_generate_data_frames = function() {


  debug( data.cache )
  undebug( data.cache )
  debug( get_data_from_cache )
  debug( generate_data_frames )
  undebug( get_data_from_cache )
  undebug( generate_data_frames )
  generate_data_frames('')

  get_data_from_cache('_rr')
  get_data_from_cache('')

  clear_cache('_rr')

  clear_cache('full')
  

}


get_data_from_cache = function( df_suffix ='_rr') {
  data_id = paste0(cache_file_prefix, 
                  ifelse( df_suffix=='', 'full', df_suffix) )

  data.cache( generate_data_frames, 
             frequency=yearly, 
             cache.name=data_id,
             cache.dir=cache_directory_name,
             envir=parent.frame(1), 
             wait=FALSE,
             df_suffix )

}



