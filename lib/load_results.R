
library(tidyverse)
library(drake)
# load in the individual locations information and summarise it


decipher_source  = function( df ) {

  df %>%
    mutate( base_file=source) %>%
    mutate( source = str_replace( source, '_', '.')) %>%
    mutate( source = str_replace( source, 'interpolated_', 'interpolated.')) %>%
    mutate( source = str_replace( source, 'filtered_', 'filtered.')) %>%
    mutate( source = str_replace( source, 'filtered_accuracy', 'filtered.accuracy')) %>%
    mutate( interpol_parm = str_replace( source, '.*interpolated.locations_(\\d*+)_.*', '\\1' ) ) %>%
    mutate( interpol_parm = ifelse(str_detect(interpol_parm,'^\\d*$'), interpol_parm, '0')) %>%
    mutate( source = str_replace( source, 'interpolated.locations_(\\d*+)_', '' ) ) %>%
    separate( col=source,
            into=c( paste('match_', 10:17, sep='') ),
            sep='_',
            convert=TRUE,
            extra='merge',
            remove=FALSE,
            fill='warn')  %>%
    mutate_at( vars(starts_with( 'match_')), as.character) %>%
    dplyr::rename( max_jump_time=match_11, min_sp_duration=match_12, max_sp_radius=match_13) %>%
    dplyr::rename( filter_parm1=match_15) %>%
    dplyr::rename( filter_parm2=match_16) %>%
    dplyr::rename( filter_type=match_14) %>%
    dplyr::rename( algorithm=match_10) %>%
    dplyr::select( -starts_with('match_')) %>%
  mutate( accuracy_filter = ifelse( filter_type=='filtered.accuracy', filter_parm1 , NA))

}

fix_source = function ( df ) {
df %>%
  mutate(source = str_replace(source, ".*(staypoints|optics)_distance_", "\\1_distance_"))

}


get_df_results = function() {
# combine the number of found surveys and the number of found staypoints

  readd(df_matching_survey_categories_summary_all) %>%
    fix_source() %>%
    group_by( source, category) %>%
    summarise( category_matches = sum(n))  %>%
    ungroup() %>%
    spread( category, category_matches) %>%
    janitor::clean_names() %>%
    { . } -> df1

  readd(df_all_sp_match_survey) %>%
    fix_source() %>%
    group_by( source ) %>%
    summarise( surveys_total = sum(n))  %>%
    ungroup() %>%
    { . } -> df2

  readd( df_all_count_staypoints_per_algorithm ) %>%
    fix_source() %>%
    { . } -> df3

  df1 %>%
    inner_join( df2, by='source') %>%
    inner_join( df3 , by='source') %>%
  decipher_source() %>%
  mutate( precision =  (sp /(sp+non_sp)), recall = (sp/(sp+sp_total))) %>%
  { . } -> df

}

df_results = get_df_results()



