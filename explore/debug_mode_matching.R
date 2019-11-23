
 df_staypoints = readd(meanshift_mode_300_10_10_1_6_interpolated_locations_120_filtered_accuracy_10)
 loadd( df_survey_nested )



 df_staypoints  %>%
   group_by( userid, night) %>% 
   summarise( sp_total = max( n_staypoint)) %>% 
   { . } -> df_sp_total

 df_sp_total %>% 
   ungroup() %>%
   summarise( sum( sp_total ))


 df %>%
   right_join( df_sp_total, by=qc(userid, night)) %>% 
   { . } -> df_matching_survey


 df_matching_survey %>% summarise( sum( n_staypoint, na.rm=TRUE) ,sum( sp_total) )



 df_matching_survey %>%
   summarise( sp_total = sum(sp_total), surveys_total= count() %>%
