
# load in the individual locations information
loadd(df_all_sp_match_survey_combined) 


df_all_sp_match_survey_combined %>%
  mutate( base_file=source) %>%
  mutate( source = str_replace( source, '_', '.')) %>%
  mutate( source = str_replace( source, 'interpolated_', 'interpolated.')) %>%
  mutate( source = str_replace( source, 'filtered_', 'filtered.')) %>%
  mutate( source = str_replace( source, 'filtered_accuracy', 'filtered.accuracy')) %>% 
  { . } -> df1

df1 %>%  
  as.Node( pathName = 'source', pathDelimiter='_') %>% 
  { . } -> tree



df1 %>% 
  separate( col=source, 
           into=c( paste('match_', 10:17, sep='') ), 
           sep='_', 
           convert=TRUE, 
           extra='merge',
           remove=FALSE)  %>% 
filter( match_14 == 'interpolated.locations' ) %>%
dplyr::rename( interpol=match_15, accuracy_filter=match_17) %>%
dplyr::rename( match1=match_11, match2=match_12, match3=match_13) %>%
dplyr::select( -starts_with('match_')) %>%
mutate(algorithm='matching', 
       interpol = as.integer( interpol), 
       accuracy_filter=as.integer( accuracy_filter))%>%
{ . } -> df1_interpolated

df1 %>% 
  separate( col=source, 
           into=c( paste('match_', 10:17, sep='') ), 
           sep='_', 
           convert=TRUE, 
           extra='merge',
           remove=FALSE)  %>% 
filter( match_14 != 'interpolated.locations' ) %>%
dplyr::rename( match1=match_11, match2=match_12, match3=match_13) %>%
dplyr::rename( filter_parm_1=match_15) %>%
dplyr::rename( filter_parm_2=match_16) %>%
dplyr::rename( filter_type=match_14) %>%
dplyr::select( -starts_with('match_')) %>%
mutate( accuracy_filter = ifelse( filter_type=='filtered.accuracy', as.integer( filter_parm_1) , NA)) %>%
mutate(algorithm='matching') %>%
{ . } -> df1_filtered

bind_rows( df1_filtered, df1_interpolated) %>% 
  mutate( survey_rate = surveys_total / sp_total ) %>%
{ . } -> df_results

df_results %>%
  mutate( type = case_when(
                           str_detect( source, 'accuracy' ) ~ 'accuracy',
                           str_detect( source, 'geohash' ) ~ 'geohash',
                           str_detect( source, 'sigma.v2' ) ~ 'sigma.v2',
                           str_detect( source, 'sigma' ) ~ 'sigma',
                           TRUE ~ 'error'
                           )) %>% 
                           { . } -> df_results



