
library(tidyverse)
library(drake)
# load in the individual locations information and summarise it
loadd(df_all_sp_match_survey_combined)


df_all_sp_match_survey_combined %>%
  mutate( base_file=source) %>%
  mutate( source = str_replace( source, '_', '.')) %>%
  mutate( source = str_replace( source, 'interpolated_', 'interpolated.')) %>%
  mutate( source = str_replace( source, 'filtered_', 'filtered.')) %>%
  mutate( source = str_replace( source, 'filtered_accuracy', 'filtered.accuracy')) %>%
  { . } -> df1

df1 %>%
  mutate( interpol_parm = str_replace( source, '.*interpolated.locations_(\\d*+)_.*', '\\1' ) ) %>%
  mutate( source = str_replace( source, 'interpolated.locations_(\\d*+)_', '' ) ) %>%
  separate( col=source,
           into=c( paste('match_', 10:17, sep='') ),
           sep='_',
           convert=TRUE,
           extra='merge',
           remove=FALSE)  %>%
  dplyr::rename( algo_parm1=match_11, algo_parm2=match_12, algo_parm3=match_13) %>%
  dplyr::rename( filter_parm_1=match_15) %>%
  dplyr::rename( filter_parm_2=match_16) %>%
  dplyr::rename( filter_type=match_14) %>%
  dplyr::rename( algorithm=match_10) %>%
  dplyr::select( -starts_with('match_')) %>%
mutate( accuracy_filter = ifelse( filter_type=='filtered.accuracy', as.integer( filter_parm_1) , NA)) %>%
  mutate( survey_rate = surveys_total / sp_total ) %>%
{ . } -> df_results




