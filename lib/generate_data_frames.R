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

# generate all the data
generate_data_frames = function( df_suffix='_rr' ) {

  table = paste0( 'continuing', df_suffix )
  df <- get_continuing_df( table, benzo=TRUE )  %>%
    mutate(drug_type=as.factor( ifelse(is_benzo(type_code), 'benzodiazepine', 'opioid')),
           quarter = quarter(supply_date, with_year = TRUE), 
           supply_year = as.factor(year(supply_date))
           )

  if (df_suffix == '_rr' ) {
     multiplier = 8459157/9480
  } else if (df_suffix == '') {
    multiplier = 1
  }

  age_groups = structure(1:4, .Label = c("0-19", 
                                        "20-44", 
                                        "45-64", 
                                        "65+"), 
                        class = "factor")


read.dta13('data/nsw_vic_populato_and_seifa_data.dta') %>% 
    tbl_df() %>%
    rename( lga_name = lga, 
           lga = lgacode,
            sex = sex2,
            population = populato 
           ) %>%
    filter( substring( lga,1,1) != '8') %>%
    mutate( supply_year=as.factor(year ),
            age = age_groups[as.integer( age4) ],
            state = ifelse( substring( lga,1,1) == '1', 'NSW', 'VIC'),
            lga = as.factor(lga)
          ) %>%
    select(-year, -age4, -lga_name ) %>%
    mutate( seifa=ordered(seifa, levels=c('Least','Moderate','High','Very High'))) %>%
      {.} -> df_population

    #
    #
    # 
    # list all schemes
#    df %>% 
#      distinct( pin, scheme ) %>%
#      group_by(pin) %>% 
#      summarise( all_scheme=paste(sort(scheme), collapse=','))  %>%
#      {.} -> df_patient_scheme
    #
    # select out patients
    #
    #

    df %>% 
      distinct (pin, sex, age, state, lga) %>% 
      mutate( sex = as.factor(sex)) %>%
#      inner_join( df_patient_scheme, by="pin") %>%
      {.} -> df_patient

    #
    #

    # doses by year, type and patient
    # for calculation of DDD

    df%>%  
      group_by(pin,  supply_year, drug_type) %>%
      summarise(
                n_dose = sum(n_dose),
                quantity = sum(quantity),
                n_script = n()
            ) %>%
      ungroup() %>%
      {.} -> df_patient_dose

    #
    df%>%
      group_by(pin, drug_type) %>%
      summarise( 
                n_quarter = n_distinct( quarter ),
                usage_category= cut( n_quarter, 
                                    c(-1, 1,7,13, 999999), 
                                    labels = qw("one-off short-term long-term regular"),
                                    ordered_result=TRUE
                                    ) 
                ) %>%
      {.} -> df_patient_usage

    list( "df_patient_usage" = df_patient_usage, 
         "df_population" = df_population,
         "df_patient" = df_patient,
#         "df_patient_scheme" = df_patient_scheme,
         "df_patient_dose" = df_patient_dose,
#         "base_map" =  get_australia_base_map(c(1,2,8)), 
         "age_groups" = age_groups,
         "multiplier" = multiplier, 
         "df"=df
         )
}


