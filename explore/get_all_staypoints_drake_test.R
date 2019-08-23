readd(df_all_staypoints_multi ) -> a

a %>%
  head(10) %>% 
  unnest(row) %>%
  distinct( userid, night, filename )


a[9,]$row


loadd()
#
df_filenames #
get_all_staypoints( df_filenames %>% head(1))



df_filenames  %>% head(1) -> a
df_filenames  %>% tail(1) -> b

crossing(a,b)

consolidate_staypoints(a)


a %>%
  rowwise() %>%
  do( consolidate_staypoints(.) )


undebug(consolidate_staypoints)



df_sp_joined_geography = get_df_sp_joined_geography( df_all_staypoints_multi , df_target_locations_combined)

df_sp_joined_geography  %>%
  distinct(userid)
