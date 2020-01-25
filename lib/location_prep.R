#********************************************************************************
# get_df_best_location 
#********************************************************************************

get_df_best_location <- function( df_location ) {

  df_location %>%
  arrange( userid, night, timestamp ) %>%
  #
  # clean gps noise, take the most accurate point for a timestamp
  filter( longitude > 0 &   longitude <10 & latitude > 40) %>%
  group_by( userid, night, local_time ) %>%
  filter( accuracy == min(accuracy)) %>%
  #
  # take the mean location for accuracy ties
  group_by( userid, night, local_time, timestamp, accuracy ) %>%
  summarise( longitude=mean(longitude), latitude=mean(latitude) ) %>%
  group_by( userid, night) %>%
  #
  # we want people who had at least 1 reading/night
  filter( n() > 1 ) %>%  
  #
  # find distance, speed between successive gps locationa on a night
  mutate( interval = difference( timestamp, 1 ), 
      dist = calc_interval_distance(longitude, latitude),
      speed = dist/interval * 1000) %>%  # in m/sec
  select( interval, dist, speed, accuracy, everything())  %>% 
  ungroup() 

}

#********************************************************************************
# get_df_all 
# get survey data
#********************************************************************************

get_df_all <- function( ) {


  read_csv('data/EveningMasterFullAnonym.csv', col_types= get_df_all_specs()) %>%
    dplyr::rename( userid = user ) %>%
    mutate( night =  ymd(sprintf('2014%04s', day )))

}


#********************************************************************************
# get_df_all_ts
# get timestamp information from survey data
#********************************************************************************
get_df_all_ts <- function( df_all ) {


  survey_type='pre'
  get_one_survey_type = function( df_all, survey_type)   {

    df_all %>%
      dplyr::select( id, userid, night, starts_with( paste0( survey_type, '_'))) %>%
      dplyr::select( id, userid, night, ends_with( '_timestamp'), ends_with('_timezone_id')) %>%
      distinct()  %>%
      dplyr::rename( timestamp=4, timezone=5) %>%
      mutate(which=survey_type ) %>%
      filter( timestamp != "")

  }

  bind_rows(
            get_one_survey_type( df_all, 'dq')
#            get_one_survey_type( df_all, 'env'),
#            get_one_survey_type( df_all, 'forg'),
#            get_one_survey_type( df_all, 'video'),
#            get_one_survey_type( df_all, 'load'),
#            get_one_survey_type( df_all, 'pre'),
#            get_one_survey_type( df_all, 'tom')
            ) %>%
    group_by( timezone ) %>% # group together all the same timezones so  that calculation is faster
    mutate( ts = ymd_hms( timestamp, tz=min( timezone))) %>%
    ungroup() %>%
    mutate( timestamp= seconds( ts ) %>% as.numeric()) %>%
    { . } -> df_all_ts

   df_all_ts
}

#********************************************************************************
# get_df_all_ts_valid
# get valid surveys;  eliminate pre, tom and load categories, and surveys without gps points
#********************************************************************************
get_df_all_ts_valid <- function( df_all_ts, df_location ) {

  df_all_ts %>%
    filter( which %in% c('dq','env','forg','video')) %>%
    inner_join( df_location %>% distinct( userid, night), by=c('userid','night'))
}


 get_df_all_specs =  function() {

   cols(
        id = col_double(),
        user = col_character(),
        evening = col_double(),
        day = col_character(),
        intention_id = col_double(),
        loading_id = col_double(),
        episode_id = col_double(),
        type = col_character(),
        episod_id = col_double(),
        episod_drink_survey = col_double(),
        episod_ambiance_survey = col_double(),
        episod_video_survey = col_double(),
        forgotten_id = col_double(),
        tomorrow_id = col_double(),
        warning = col_character(),
        episod_episode = col_character(),
        episod_picture_file_exists = col_logical(),
        episod_video_done = col_logical(),
        episod_video_url = col_character(),
        episod_video_file_exits = col_logical(),
        episod_warning = col_character(),
        pre_timestamp = col_character(),
        pre_timezone_id = col_character(),
        pre_timezone_display_name = col_character(),
        pre_timezone_raw_offset = col_double(),
        pre_beer_cider = col_double(),
        pre_coffee_tee = col_double(),
        pre_cocktail_alcopops = col_double(),
        pre_energy = col_double(),
        pre_fruit_juice = col_double(),
        pre_liqueur_aperitive = col_double(),
        pre_milky = col_double(),
        pre_shots = col_double(),
        pre_soft_drinks = col_double(),
        pre_spirits = col_double(),
        pre_water = col_double(),
        pre_wine_champagne = col_double(),
        load_timestamp = col_character(),
        load_timezone_id = col_character(),
        load_timezone_display_name = col_character(),
        load_timezone_raw_offset = col_double(),
        load_beer_cider = col_double(),
        load_coffee_tee = col_double(),
        load_cocktail_alcopops = col_double(),
        load_energy = col_double(),
        load_fruit_juice = col_double(),
        load_liqueur_aperitive = col_double(),
        load_milky = col_double(),
        load_shots = col_double(),
        load_soft_drinks = col_double(),
        load_spirits = col_double(),
        load_water = col_double(),
        load_wine_champagne = col_double(),
        forg_timestamp = col_character(),
        forg_timezone_id = col_character(),
        forg_timezone_display_name = col_character(),
        forg_timezone_raw_offset = col_double(),
        forg_beer_cider = col_double(),
        forg_coffee_tee = col_double(),
        forg_cocktail_alcopops = col_double(),
        forg_energy = col_double(),
        forg_fruit_juice = col_double(),
        forg_liqueur_aperitive = col_double(),
        forg_milky = col_double(),
        forg_shots = col_double(),
        forg_soft_drinks = col_double(),
        forg_spirits = col_double(),
        forg_water = col_double(),
        forg_wine_champagne = col_double(),
        dq_timestamp = col_character(),
        dq_timezone_id = col_character(),
        dq_timezone_display_name = col_character(),
        dq_timezone_raw_offset = col_double(),
        dq_drink_alcool = col_character(),
        dq_drink_glass = col_character(),
        dq_drink_name = col_character(),
        dq_env_light_percent = col_double(),
        dq_env_people_percent = col_double(),
        dq_env_sound_percent = col_double(),
        dq_social_family_members_counter = col_character(),
        dq_social_female_friends_colleagues_counter = col_character(),
        dq_social_male_friends_colleagues_counter = col_character(),
        dq_social_other_people_counter = col_character(),
        dq_social_partner_spouse_counter = col_double(),
        dq_imageFileName = col_character(),
        tom_timestamp = col_character(),
        tom_timezone_id = col_character(),
        tom_timezone_display_name = col_character(),
        tom_timezone_raw_offset = col_double(),
        tom_consequence_drankmore = col_logical(),
        tom_consequence_driving = col_logical(),
        tom_consequence_fight = col_logical(),
        tom_consequence_hangover = col_logical(),
        tom_consequence_injured = col_logical(),
        tom_consequence_memoryloss = col_logical(),
        tom_consequence_regret = col_logical(),
        tom_consequence_sexatrisk = col_logical(),
        tom_consequence_spentmore = col_logical(),
        tom_nb_drinks_last_night = col_character(),
        env_timestamp = col_character(),
        env_timezone_id = col_character(),
        env_timezone_display_name = col_character(),
        env_timezone_raw_offset = col_double(),
        env_artsy_percent = col_double(),
        env_dingy_percent = col_double(),
        env_formal_percent = col_double(),
        env_loud_percent = col_double(),
        env_oldfashioned_percent = col_double(),
        env_romantic_percent = col_double(),
        env_sophisticated_percent = col_double(),
        env_trendy_percent = col_double(),
        env_upscale_percent = col_double(),
        env_place_city = col_character(),
        env_place_id_name = col_character(),
        env_place_type = col_character(),
        video_timestamp = col_character(),
        video_timezone_id = col_character(),
        video_timezone_display_name = col_character(),
        video_timezone_raw_offset = col_double(),
        video_no_video_reason_ethical = col_logical(),
        video_no_video_reason_legal = col_logical(),
        video_no_video_reason_other = col_logical(),
        video_no_video_reason_safety = col_logical(),
        video_no_video_reason_social = col_logical(),
        video_videoAvailable = col_logical(),
        video_videoFileName = col_character(),
        video_imageFileName = col_character(),
        city = col_double(),
        city_str = col_character(),
        time_baseline = col_character(),
        dem1 = col_double(),
        dem2 = col_double(),
        dem3 = col_double(),
        dem4 = col_double(),
        dem5 = col_double(),
        dem6 = col_double(),
        dem7 = col_double(),
        out1 = col_double(),
        out2 = col_double(),
        out3 = col_double(),
        out4a = col_double(),
        out4b = col_double(),
        out4c = col_double(),
        out4d = col_double(),
        out4e = col_double(),
        out4f = col_double(),
        out5 = col_double(),
        out6_1 = col_character(),
        out7_1 = col_double(),
        out8_1 = col_double(),
        out9_1a = col_double(),
        out9_1b = col_double(),
        out9_1c = col_double(),
        out9_1d = col_double(),
        out9_1e = col_double(),
        out10_1a = col_double(),
        out10_1b = col_double(),
        out10_1c = col_double(),
        out10_1d = col_double(),
        out10_1e = col_double(),
        out10_1f = col_double(),
        out10_1g = col_double(),
        out10_1h = col_double(),
        out10_1i = col_double(),
        out10_1j = col_double(),
        out10_1k = col_double(),
        out10_1l = col_double(),
        out10_1m = col_double(),
        out6_2 = col_character(),
        out7_2 = col_double(),
        out8_2 = col_double(),
        out9_2a = col_double(),
        out9_2b = col_double(),
        out9_2c = col_double(),
        out9_2d = col_double(),
        out9_2e = col_double(),
        out10_2a = col_double(),
        out10_2b = col_double(),
        out10_2c = col_double(),
        out10_2d = col_double(),
        out10_2e = col_double(),
        out10_2f = col_double(),
        out10_2g = col_double(),
        out10_2h = col_double(),
        out10_2i = col_double(),
        out10_2j = col_double(),
        out10_2k = col_double(),
        out10_2l = col_double(),
        out10_2m = col_double(),
        out6_3 = col_character(),
        out7_3 = col_double(),
        out8_3 = col_double(),
        out9_3a = col_double(),
        out9_3b = col_double(),
        out9_3c = col_double(),
        out9_3d = col_double(),
        out9_3e = col_double(),
        out10_3a = col_double(),
        out10_3b = col_double(),
        out10_3c = col_double(),
        out10_3d = col_double(),
        out10_3e = col_double(),
        out10_3f = col_double(),
        out10_3g = col_double(),
        out10_3h = col_double(),
        out10_3i = col_double(),
        out10_3j = col_double(),
        out10_3k = col_double(),
        out10_3l = col_double(),
        out10_3m = col_double(),
        per1a = col_double(),
        per1b = col_double(),
        per1c = col_double(),
        per1d = col_double(),
        per1e = col_double(),
        per1f = col_double(),
        per1g = col_double(),
        per1h = col_double(),
        per1i = col_double(),
        per1j = col_double(),
        alc1 = col_double(),
        alc2 = col_double(),
        alc3a = col_double(),
        alc3b = col_double(),
        alc3c = col_double(),
        alc3d = col_double(),
        alc3e = col_double(),
        alc3f = col_double(),
        alc3g = col_double(),
        alc4a = col_double(),
        alc4b = col_double(),
        alc4c = col_double(),
        alc4d = col_double(),
        alc4e = col_double(),
        alc5 = col_double(),
        mot1 = col_double(),
        mot2 = col_double(),
        mot3 = col_double(),
        mot4 = col_double(),
        mot5 = col_double(),
        mot6 = col_double(),
        mot7 = col_double(),
        mot8 = col_double(),
        mot9 = col_double(),
        mot10 = col_double(),
        mot11 = col_double(),
        mot12 = col_double(),
        mot13 = col_double(),
        mot14 = col_double(),
        mot15 = col_double(),
        mot16 = col_double(),
        pre1 = col_double(),
        pre2a = col_double(),
        pre2b = col_double(),
        pre2c = col_double(),
        pre2d = col_double(),
        pre2e = col_double(),
        pre2f = col_double(),
        pre2g = col_double(),
        pre2h = col_double(),
        pre2i = col_double(),
        pre2j = col_double(),
        pre2k = col_double(),
        pre2l = col_double(),
        pre2m = col_double(),
        pre2n = col_double(),
        pre2o = col_double(),
        pre2p = col_double(),
        pre2q = col_double(),
        pre2r = col_double(),
        pre2s = col_double(),
        pre2t = col_double(),
        pre2u = col_double(),
        pre2v = col_double(),
        pre2w = col_double(),
        pre2x = col_double(),
        mob1 = col_double(),
        mob2 = col_double(),
        mob3 = col_double(),
        mob4 = col_double(),
        mob5 = col_double(),
        mob6 = col_double(),
        mob7a = col_double(),
        mob7b = col_double(),
        mob7c = col_double(),
        mob7d = col_double(),
        mob7e = col_double(),
        net1a = col_double(),
        net1b = col_double(),
        net1c = col_double(),
        net2a = col_double(),
        net2b = col_double(),
        net2c = col_double(),
        net3a = col_double(),
        net3b = col_double(),
        net3c = col_double(),
        net4a = col_double(),
        net4b = col_double(),
        net4c = col_double(),
        net5a = col_double(),
        net5b = col_double(),
        net5c = col_double(),
        net6a = col_double(),
        net6b = col_double(),
        net6c = col_double(),
        net7a = col_double(),
        net7b = col_character(),
        time_discharge = col_character(),
        rec1 = col_double(),
        rec2 = col_double(),
        rec3 = col_double(),
        rec4 = col_double(),
        rec5 = col_double(),
        exp1 = col_double(),
        exp2 = col_double(),
        exp3 = col_double(),
        exp4 = col_double(),
        exp5 = col_double(),
        exp6 = col_double(),
        exp7 = col_double(),
        exp8 = col_double(),
        eval1 = col_double(),
        eval2 = col_double(),
        eval3 = col_double(),
        eval4 = col_double(),
        eval5 = col_double(),
        eval6 = col_double(),
        eval7 = col_double(),
        eval8 = col_double(),
        eval9 = col_double(),
        eval10 = col_double(),
        eval11 = col_double(),
        eval12 = col_double(),
        eval13 = col_double(),
        eval14 = col_double(),
        eval15 = col_double(),
        alc1_dis = col_double(),
        alc3a_dis = col_double(),
        alc3b_dis = col_double(),
        alc3c_dis = col_double(),
        alc3d_dis = col_double(),
        alc3e_dis = col_double(),
        alc3f_dis = col_double(),
        alc3g_dis = col_double(),
        alc5_dis = col_double(),
        pre1_dis = col_double(),
        out5_dis = col_double(),
        fin1 = col_double(),
        fin2 = col_double(),
        fin3 = col_double(),
        cons1 = col_double(),
        cons2 = col_double(),
        cons3 = col_double(),
        cons4 = col_double(),
        cons5 = col_double(),
        cons6 = col_double(),
        cons7 = col_double(),
        cons8 = col_double(),
        cons9 = col_double(),
        cons10 = col_double(),
        cons11 = col_double(),
        cons12 = col_double(),
        cons13 = col_double(),
        cons14 = col_double(),
        out1_dis = col_double(),
        out7 = col_double(),
        out10_1a_dis = col_double(),
        out10_1e_dis = col_double(),
        out10_1f_dis = col_double(),
        out10_1g_dis = col_double(),
        out10_1h_dis = col_double(),
        out10_1i_dis = col_double(),
        out10_1k_dis = col_double(),
        out10_1l_dis = col_double(),
        out10_1m_dis = col_double(),
        app1 = col_double(),
        app2 = col_double(),
        app3 = col_double()
   )
 }
