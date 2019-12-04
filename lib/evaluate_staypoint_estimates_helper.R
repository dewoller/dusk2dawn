library(tidyverse)

row= tribble( ~filename,'data/save_v1_maxspeed_1200_1200_10_20_df.rds')

#********************************************************************************
#analyse_staypoint_base_information_detail
# just get summary information for each actual staypoint, return one line per staypoint, summarised
#********************************************************************************
analyse_staypoint_base_information_detail <- function( row ) {


  readRDS(row$filename)  %>%
    filter( n_staypoint > 0 )  %>%
    group_by( userid, night, n_staypoint) %>%
    dplyr::summarise( latitude = mean( latitude), 
              longitude  = mean( longitude),
              min_latitude = min( latitude), min_longitude  = min( longitude),
              max_latitude = max( latitude), max_longitude  = max( longitude), 
              start=min(timestamp), end=max(timestamp), duration=end-start ) %>% 
    ungroup() 

}
#********************************************************************************
#analyse_staypoint_base_information_summary
# summarise ALL staypoints for this user and night
#********************************************************************************
analyse_staypoint_base_information_summary <- function( row ) {
  print(paste(row, collapse=','))

  analyse_staypoint_base_information_detail ( row ) %>%
    { . } -> a

  a %>% 
    group_by( userid, night) %>%
    summarise( n=n()) %>%
    ungroup() %>%
    summarise( n_staypoint = nrow(a), mean_sp_night=mean(n), max_sp_night=max(n), sd_sp_night=sd(n)) %>% 
    bind_cols( row ) 

}

#********************************************************************************
#analyse_staypoint_set_geography_detail
# see if this staypoint intersects any of the geography staypoints
#********************************************************************************
analyse_staypoint_set_geography_detail <- function( row ) {
  min_overlap_distance = 20 / 1000 # 20 m


  analyse_staypoint_base_information_detail( row ) %>%
    mutate(dist=0) %>% 
    geo_inner_join(df_4sq_locations_filtered, max_dist = min_overlap_distance, distance_col='dist') %>% 
    mutate( dist = round( dist * 1000, 0)) %>%
    group_by( userid, night, n_staypoint ) %>% 
    arrange( dist , type) %>%
    do( head(., 1)) %>%
    ungroup() 

}



#********************************************************************************
#analyse_staypoint_full_4sq
# see if this staypoint intersects any of the geography staypoints
#********************************************************************************
analyse_staypoint_full_4sq<- function( df ) {
  min_overlap_distance = 20 / 1000 # 20 m

  df %>%
    geo_inner_join(df_4sq_locations_filtered, max_dist = min_overlap_distance, distance_col='dist') %>% 
    mutate( dist = round( dist * 1000, 0)) %>%
    group_by( userid, night, n_staypoint ) 

}


#********************************************************************************
#analyse_staypoint_set_geography
# see if this stay
#********************************************************************************
analyse_staypoint_set_geography_summary <- function( row ) {

  cat( row$filename )
  cat("\n")

#= glue( "data/save_{.df$i_min_staypoint_time}_{.df$i_max_jump_time}_{.df$i_max_staypoint_distance}_{.df$i_max_speed_filter}_df.rds")
#  row= tribble( ~filename,'data//save_1200_60_5_10_df.rds')

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned

  df_intersect_geo_cleaned %>%
    count( type, sort=TRUE) %>%
    bind_rows( tribble( ~type, ~n, 'junk', 0)) %>%
    spread(type, n) %>%
    bind_cols( row ) %>%
    bind_cols (enframe( nrow(df_intersect_geo_cleaned), value='nhits')) %>%
    select(  nhits, everything() ) %>%
    select( -name )

}


#********************************************************************************
#analyse_staypoint_set_time_detail 
# match the staypoints to the survey times
#********************************************************************************
analyse_staypoint_set_time_detail <- function( row ) {

  analyse_staypoint_base_information_detail( row ) %>%
    group_by( userid, night ) %>%
    nest( .key='staypoints') %>%
    ungroup() %>%
    inner_join( df_all_ts_nested, by=c('userid', 'night')) %>% 
    group_by( userid, night) %>%
    do( joined = interval_inner_join( data.frame(.$surveys), 
                                     data.frame(.$staypoints), 
                                     by=c('start','end'),
                                     maxgap=300))  %>%
    ungroup() %>%
    unnest() %>% 
    group_by( userid, night, n_staypoint ) %>% 
    arrange(  duration, which) %>%  # take the smallest duration staypoint
    do( head(., 1)) %>%
    ungroup() 

}

#********************************************************************************
#analyse_staypoint_set_time_summary 
#********************************************************************************
analyse_staypoint_set_time_summary <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
  { . } -> df_intersect_time_cleaned

  df_intersect_time_cleaned %>% 
    count( which, sort=TRUE) %>%
    spread(which, n) %>%
    bind_cols(row) %>%
    bind_cols (enframe( nrow(df_intersect_time_cleaned), value='nsurvey_hits')) %>%
    select( -name )

}

row= tribble( ~filename,'data//save_v3_geohash_300_120_5_precision_7_minpoints_3_df.rds')
#********************************************************************************
# analyse_staypoint_set_time_and_geography_summary 
#********************************************************************************
analyse_staypoint_set_time_and_geography_summary <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
    { . } -> df_intersect_time_cleaned

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned


  df_intersect_time_cleaned %>%
    full_join( df_intersect_geo_cleaned, by = qc( userid, night, n_staypoint )) %>% 
    { . } -> df_intersect_both

  df_intersect_both %>%
    count( is.na( start.x), is.na( latitude.x)) %>% 
    { . } -> a

  if (nrow(a)==3) {
    a %>%
      mutate( hit_count = qc( both_hits, survey_only_hits, geo_only_hits)) %>%
      select( n, hit_count ) %>%
      spread( hit_count, n ) %>%
      bind_cols( row )
  } else {
    row %>%
      bind_cols( tribble( ~both_hits, ~survey_only_hits, ~geo_only_hits, NA ,NA ,NA )) 
  }

}


#********************************************************************************
# analyse_staypoint_set_time_and_geography_detail 
#********************************************************************************
analyse_staypoint_set_time_and_geography_detail <- function( row ) {


  analyse_staypoint_set_time_detail( row ) %>% 
    { . } -> df_intersect_time_cleaned

  analyse_staypoint_set_geography_detail( row ) %>% 
    { . } -> df_intersect_geo_cleaned

  df_intersect_time_cleaned %>%
    full_join( df_intersect_geo_cleaned, by = qc( userid, night, n_staypoint )) %>% 
    { . } -> df_intersect_both

}

#********************************************************************************
# get_staypoint_filenames
#********************************************************************************
get_staypoint_filenames = function() {

list.files( path='data/', pattern='save_v[123].*rds', full.names=TRUE ) %>%
  enframe(value = 'filename' ) %>%
  select(-name) %>%
  #  head(1) %>%
  separate( col=filename, 
           into=c(NA, NA, qc(type, min_staypoint_time, max_jump_time, max_staypoint_distance, rest)), 
           sep='_', 
           convert=TRUE, 
           extra='merge',
           remove=FALSE)  %>%
  mutate( rest = str_replace( rest, '_df.rds', '') ) 

}
 

#********************************************************************************
#consolidate_staypoints
#********************************************************************************
consolidate_staypoints <- function( row ) {

  row = as_tibble(row)

  readRDS(row$filename)  %>%
    filter( n_staypoint > 0 ) %>%
    group_by( userid, night, n_staypoint) %>%
    summarise( longitude = mean(longitude), latitude = mean(latitude), 
              timestamp_start = min(timestamp), 
              timestamp_end = max(timestamp)) %>%
    ungroup %>%
    crossing(row)

}
#


#********************************************************************************
#get_all_staypoints
#********************************************************************************
get_all_staypoints = function( filenames)  {
  filenames %>%
    rowwise() %>%
    do( consolidate_staypoints(.) )

}


#********************************************************************************
#get_all_staypoints
#********************************************************************************
get_all_staypoints_multiprocessor = function( filenames)  {
  library(multidplyr)

detectCores() %>%
  new_cluster() %>%
	cluster_copy('consolidate_staypoints') %>%
	cluster_library( "tidyverse" ) %>%
	{.} -> cluster

  filenames %>%
    rowwise() %>%
    partition(cluster=cluster) %>%
    do( consolidate_staypoints(.) )  %>%
    collect() %>%
    unnest()


}





#********************************************************************************
#  get_df_4sq_locations_filtered 
#********************************************************************************
get_df_4sq_locations_filtered = function() {

  read_csv( 'data/foursquare_locations_data_ls.csv') %>%
    bind_rows( read_csv( 'data/foursquare_locations_data_zh.csv')) %>% 
    { . } -> df_4sq_locations


bad_categories = c( '2', 'Academic Building', 'Accessories', 'Administrative Building', 'Advertising Agency', 'Airport', 'Animal Shelter', 'Antiques', 'Apartment Building', 'Apparel', 'Aquarium', 'Arepas', 'Arcade', 'Art Gallery', 'Art Museum', 'Arts', 'Arts & Crafts', 'Assisted Living', 'Athletics & Sports', 'Auditorium', 'Auto Garage', 'Automotive', 'B & B', 'Baseball', 'Baseball Field', 'Bakery', 'Bank', 'Bank / Financial', 'Basketball', 'Basketball Court', 'Bathing Area', 'Beach', 'Beauty / Cosmetic', 'Beer Garden', 'Bike shop', 'Billiards', 'Board Shop', 'Boarding', 'Boat / Ferry', 'Bookstore', 'Boutique', 'Bowling Alley', 'Breakfast', 'Bridge', 'Building', 'Bus', 'Bus Station', 'Bus Stop', 'Butcher', 'Cafeteria', 'Car Dealer', 'Casino', 'Camera Store', 'Campaign', 'Campground', 'Candy Store', 'Capital Building', 'Car Wash', 'Cheese Shop', 'Cemetery', 'Church', 'Circus', 'City', 'City Hall', 'Classroom', 'Climbing Gym', 'Communications', 'Cocktail', 'College & Education', 'Community College', 'Conference', 'Conference room', 'Convenience Stores', 'Convention', 'Convention Center', 'Corporate / Office', 'Cosmetics', 'Courthouse', 'Coworking Space', 'Credit Union', 'Cupcakes', 'Dance Studio', 'Daycare', "Dentist's Office", 'Department Store', 'Design', 'Desserts', 'Distillery', "Doctor's Office", 'Dog Run', 'Education', 'Electronics', 'Elementary School', 'Embassy', 'Emergency Room', 'Engineering',  'Factory', 'Fair', 'Farm', "Farmer's Market", 'Field', 'Financial / Legal', 'Fire Station', 'Flea Market', 'Flower Shop', 'Food & Drink', 'Food Truck', 'Football',  'Funeral Home', 'Furniture / Home', 'Gaming Cafe', 'Garden', 'Garden Center', 'Gas Station / Garage', 'Gift Shop', 'Golf Course', 'Government', 'Grocery Store', 'Gym', 'Gym / Fitness', 'Harbor / Marina', 'Hardware', 'Health Food Store', 'High School', 'Hiking Trail', 'Historic Site', 'History Museum', 'Hobbies', 'Hockey', 'Hockey Field', 'Home', 'Hospital', 'Hostel', 'Hot Spring', 'Hotel', 'Hotel Bar', 'Housing Development', 'Ice Cream', 'Internet Cafe', 'Island', 'Indie Theater', 'IT Services', 'Jewelry', 'Kids Store', 'Lab', 'Lake', 'Lake / Pond', 'Landmark', 'Laundry', 'Library', 'Lighthouse', 'Lingerie', 'Lodge', 'Mall', 'Market', 'Martial Arts', 'Massage Studio', 'Math', 'Medical', 'Medical School', 'Meeting Room', "Men's Store", 'Military Base', 'Mobile Phone', 'Mosque', 'Motel', 'Motorcycle Shop', 'Mountain', 'Moving Target', 'Music Festival', 'Music Store', 'Museum', 'Nail Salon', 'Neighborhood', 'Non-Profit', 'Nursery School', 'Office', 'Office Supplies', 'Opera House', 'Optical', 'Optical Shops', 'Other - Buildings', 'Other - Education', 'Other - Entertainment', 'Other - Nightlife', 'Other - Shop', 'Other - Travel', 'Other Event', 'Other Outdoors', 'Outdoors & Recreation', 'Performing Arts', 'Pet Store', 'Pharmacy', 'Photography Lab', 'Pier', 'Plane', 'Playground', 'Police Station', 'Pool', 'Post Office', 'Professional', 'Public Art', 'Racetrack', 'Radio Station', 'Rec Center', 'Record Shop', 'Recruiting Agency', 'Recycling', 'Rental Car', 'Residence Hall', 'Residential', 'Rest Areas', 'River', 'Road', 'Rock Climbing', 'Salad', 'Salon / Barbershop', 'Sandwiches', 'Scenic Lookout', 'Science', 'Science Museum', 'School', 'Sculpture', 'Shoes', 'Shops', 'Shop', 'Shrine', 'Skate Park', 'Skating Rink', 'Smoke Shop', 'Snacks', 'Soccer', 'Soccer Field', 'Sorority House', 'Soup', 'Spiritual', 'Spa', 'Spa / Massage', 'Speakeasy', 'Speakeasy / Secret Spot', 'Sporting Goods', 'Stables', 'Stadium', 'Street', 'Storage',  'Student Center', 'Supermarket', 'Surf Spot', 'Synagogue', 'Tanning Salon', 'Tattoo', 'Taxi', 'Tea Room', 'Tech Startup', 'Technology', 'Temple', 'Tennis Court', 'Terminal', 'Theme Park', 'Thrift / Vintage', 'Tobacco & Cigars', 'Tourist Information', 'Toys & Games', 'Track', 'Trade School', 'Travel', 'Travel Agency', 'Trail', 'TV Station', 'University', 'Veterinarians', 'Video Games', 'Video Store', 'Voting Booth', 'Well', "Women's Store", 'Yoga Studio', 'Zoo')

restaurant_categories = c( 'African', 'American', 'Argentinian', 'Asian', 'Australian', 'Bagels', 'BBQ', 'Brazilian', 'Breakfast / Brunch', 'Burgers', 'Burritos', 'Café', 'Cafe', 'Caribbean', 'Chinese', 'Coffee Shop', 'Cuban', 'Deli / Bodega', 'Diner', 'Dive Bar', 'Eastern European', 'Ethiopian', 'Falafel', 'Fast Food', 'Food', 'Food Court', 'French', 'Fried Chicken', 'Gastropub', 'German', 'Gourmet', 'Greek', 'Hookah Bar', 'Indian', 'Indie', 'Indonesian', 'Italian', 'Japanese', 'Juice Bar', 'Korean', 'Latin American', 'Malaysian', 'Mediterranean', 'Mexican', 'Middle Eastern', 'Modern European', 'Molecular Gastronomy', 'Moroccan', 'New American', 'Other - Food', 'Paella', 'Pizza', 'Restaurant', 'Sake Bar', 'Seafood', 'South American', 'Spanish', 'Steakhouse', 'Street Food', 'Sushi', 'Swiss', 'Tapas', 'Tacos', 'Thai', 'Turkish', 'Vegetarian / Vegan', 'Vietnamese', 'Wine Shop', 'Winery')


  df_type = tribble(
                    ~primaryCategory, ~type,
                    'Subway', 'transport',
                    'Train', 'transport',
                    'Train Station', 'transport',
                    'Light Rail', 'transport',
                    'Platform', 'transport',
                    'Parking', 'transport',
                    'Field', 'park',
                    'Park', 'park',
                    'Plaza', 'park',
                    'Plaza / Square', 'park',
                    'Comedy Club', 'theater',
                    'Concert Hall', 'theater',
                    'Music Venue', 'theater',
                    'Movie Theater', 'theater',
                    'Cineplex', 'theater',
                    'Theater', 'theater')

  alcohol_venue_types = c("Bar", "Nightclub", "Lounge", "Pub", "Wine Bar", "Gay Bar",
                          "Nightlife", "Sports Bar", "Jazz Club", "Brewery", "Karaoke",
                          "Liquor Store", "Rock Club", "Festival", 'Strip Club','Entertainment', 'Event Space','Frat House',"Whisky Bar")

  df_4sq_locations %>%
    filter( !primaryCategory %in% bad_categories ) %>%
    mutate( primaryCategory = ifelse( primaryCategory %in% restaurant_categories, 'food', primaryCategory)) %>%
    left_join( df_type, by='primaryCategory') %>%
    mutate( type=ifelse( is.na(type), primaryCategory, type)) %>%
    mutate( type=ifelse( type %in% alcohol_venue_types, 'alcohol', type)) %>%
    group_by( shortUrl,  primaryCategory, type ) %>%
    summarise( name=min(name), 
              checkinsCount = max( checkinsCount),
              latitude=mean(latitude), 
              longitude=mean(longitude) 
              ) %>%
    ungroup() %>%
    {.} -> df_4sq_locations_filtered
  df_4sq_locations_filtered

}


#********************************************************************************
#  get_df_osm_locations_filtered 
#********************************************************************************
get_df_osm_locations_amenity = function( ) {
  return( readRDS('data/shiny/df_osm_amenities.rds'))

  df_lausanne <- 

    opq ("lausanne") %>%
    add_osm_feature(key = 'amenity', 
                    value= wrapr::qc("cafe bar pub restaurant bbq theatre mightclub fast_food social_facility bbq")
                    ) %>%
  osmdata_sf()

df_zurich <- opq ("zurich") %>%
  add_osm_feature(key = 'amenity', 
                  value= wrapr::qc("cafe bar pub restaurant bbq theatre mightclub fast_food social_facility bbq")
                  ) %>%
osmdata_sf()

  c( df_lausanne, df_zurich)

}


#********************************************************************************
#  get_df_osm_locations_filtered
#********************************************************************************
get_df_osm_locations_leisure = function( ) {
  return( readRDS('data/shiny/df_osm_leisure.rds'))

  df_lausanne <- 

    opq ("lausanne") %>%
    add_osm_feature(key = 'leisure',
                    value= wrapr::qc("park picnic_table playground")
                    ) %>%
  osmdata_sf()

  df_zurich <- opq ("zurich") %>%
  add_osm_feature(key = 'leisure',
                    value= wrapr::qc("park picnic_table playground")
                  ) %>%
  osmdata_sf()

  c( df_lausanne, df_zurich)

}




#********************************************************************************
#  sfc_as_cols 
#********************************************************************************
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}




#********************************************************************************
#  get_df_target_locations_combined 
#********************************************************************************
get_df_target_locations_combined = function( df_osm_amenity, df_4sq_locations_filtered) {

  (df_osm_amenity) %$%
    osm_points %>%
    filter(!is.na( name)) %>%
    sfc_as_cols (geometry, names=c('longitude', 'latitude') ) %>%
    as_tibble() %>%
    select( name, latitude, longitude) %>%
    mutate( location_type='osm') %>%
    { . } -> df_osm_amenity_simple


  (df_4sq_locations_filtered ) %>%
    select( name, latitude, longitude) %>%
    mutate( location_type='4sq') %>%
    { . } -> df_4sq_locations_simple 


  df_4sq_locations_simple  %>%
    bind_rows( df_osm_amenity_simple) %>% 
    mutate( tl_id = row_number()) %>% 
    select( latitude, longitude, tl_id) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
    st_transform( 27700)

}

#********************************************************************************
#  get_df_sp_round_location
#********************************************************************************
get_df_sp_round_location = function(df_staypoints, rounding_factor = 1000000.0   ) {



  df_staypoints %>%
    ungroup() %>%
    mutate( latitude = round( latitude*rounding_factor, 0) / rounding_factor) %>%
    mutate( longitude = round( longitude*rounding_factor, 0) / rounding_factor) 

}

#********************************************************************************
#  get_df_sp_joined_geography
#********************************************************************************
#df_staypoints = readd(df_staypoints )
#df_target_locations_combined = readd(df_target_locations_combined)
#overlap_distance_needed = 20 

#debug(get_df_sp_joined_geography)

#get_df_sp_joined_geography( df_staypoints,  df_target_locations_combined)

calculate_sp_match_geography = function( df_staypoints,  
                                             df_target_locations_combined_P, 
                                             overlap_distance_needed = 20 
                                             ) {



  df_staypoints  %>% 
    select( userid, night, n_staypoint, filename, latitude, longitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
    st_transform( 27700) %>% 
    st_join( df_target_locations_combined_P, join = st_is_within_distance, dist = overlap_distance_needed ) %>%
    filter(!is.na(tl_id)) %>% 
    inner_join( df_target_locations_combined_P %>% as_tibble(), by='tl_id') %>%
    mutate( distance_to_bar = st_distance(geometry.x, geometry.y, by_element=TRUE)   ) %>% 
    as_tibble() %>%
    dplyr::select(distance_to_bar, everything()) %>%
    dplyr::select( -geometry.y, -geometry.x ) %>%
    inner_join(df_staypoints , by=qc( userid, night, n_staypoint, filename)) %>%
    group_by( filename, userid, night, n_staypoint) %>% 
    arrange( distance_to_bar ) %>%  # find the closest bar for each proposed staypoint lat/lon pair
    do( head(., 1)) %>%
    ungroup() 

}


#********************************************************************************
#  get_df_sp_joined_geography
#********************************************************************************
#df_staypoints = readd(df_staypoints )
#df_target_locations_combined = readd(df_target_locations_combined)
#overlap_distance_needed = 20 

#debug(get_df_sp_joined_geography)

#get_df_sp_joined_geography( df_staypoints,  df_target_locations_combined)
 
get_df_sp_joined_geography_geohash= function( df_staypoints,  
                                     df_target_locations_combined, 
                                     overlap_distance_needed = 20 
                                     ) {

  geohash_precision = 7

  df_target_locations_combined  %>%
        mutate( geohash_key = gh_encode(latitude, longitude, precision=geohash_precision )) %>% 
        { . } -> df_target_locations_combined

  df_staypoints %>% 
    mutate( geohash_key = gh_encode(latitude, longitude, precision=geohash_precision )) %>% 
    inner_join(df_target_locations_combined , by='geohash_key') %>% 
    mutate( distance_to_bar = distHaversine(cbind(longitude.x, latitude.x), cbind(longitude.y, latitude.y)) ) %>% 
    dplyr::select(distance_to_bar, everything()) %>%
    dplyr::select( -latitude.y, -longitude.y ) %>%
    dplyr::rename( longitude = longitude.x, latitude = latitude.x ) %>%
    filter( distance_to_bar < overlap_distance_needed ) %>%
    group_by( filename, userid, night, n_staypoint) %>% 
    arrange( distance_to_bar ) %>%  # find the best example for each lat/lon pair
    do( head(., 1)) %>%
    ungroup() %>% 
    { . } -> df_addresses_found

  df_addresses_found
}


#********************************************************************************
#  get_df_sp_joined_geography 
#df_staypoints = readd(df_sp_rounded_location)
#df_target_locations_combined = readd(df_target_locations_combined)
#overlap_distance_needed = 20 / 1000 
#********************************************************************************
get_df_sp_joined_geography_old = function( df_staypoints,  df_target_locations_combined, overlap_distance_needed = 20 / 1000 ) {

  # find best target location address for each distinct staypoint 
  df_staypoints %>% 
    distinct( latitude, longitude )  %>%
    geo_inner_join(df_target_locations_combined , 
                   max_dist = overlap_distance_needed, 
                   distance_col='dist', 
                   by=c('longitude', 'latitude')) %>% 
    mutate( distance_to_bar = round( dist * 1000, 0)) %>% 
    select( -latitude.y, -longitude.y ) %>%
    rename( latitude = latitude.x, longitude = longitude.x) %>%
    group_by( latitude, longitude ) %>% 
    arrange( dist ) %>%  # find the best example for each lat/lon pair
    do( head(., 1)) %>%
    ungroup() %>% 
    { . } -> df_addresses_found

    df_addresses_found %>%
      right_join( df_staypoints, by=c('latitude','longitude'))
}



#********************************************************************************
#  get_df_revgeo_addresses 
#********************************************************************************
get_df_revgeo_addresses = function(  df_sp_no_bar) {

  df_sp_no_bar %>% 
    mutate( 
           latitude = round( latitude *10000 ) / 10000,
           longitude = round( longitude *10000) / 10000)  %>%
    distinct( latitude, longitude) %>%
    mutate( address = map2( longitude, latitude, revgeo, provider =  'bing', API= bing_maps_api_key, output = 'frame')) %>% 
    #mutate( address = map2( longitude, latitude, revgeo, provider =  'photon', output = 'frame')) %>% 
    unnest( address) %>%
    { . } -> df_rev_address_lookup
  df_rev_address_lookup
}




#********************************************************************************
#  get_df_sp_no_bar 
#********************************************************************************
get_df_sp_no_bar = function(   df_staypoints , df_sp_joined_geography  ) {

  df_staypoints    %>%
    anti_join( df_sp_joined_geography, by=qc( userid, night, n_staypoint, filename)) %>%
    arrange( n_staypoint)

}




#********************************************************************************
#  get_df_survey_nested
#********************************************************************************

get_df_survey_nested = function( df_all_ts ) {

  df_all_ts  %>%
    mutate( userid = as.character( userid)) %>%
    group_by( userid, night ) %>%
    mutate( timestamp_start=timestamp, timestamp_end=timestamp) %>%
    dplyr::select( timestamp_start, timestamp_end, which, userid, night) %>%
    nest( surveys = c(starts_with('timestamp'), which )) %>% 
    { . } -> df_all_ts_nested
  df_all_ts_nested

}


#********************************************************************************
#  summarise_staypoints 
#********************************************************************************
summarise_staypoints = function( df_staypoints ) {

  df_staypoints  %>%
    group_by( userid, night, n_staypoint ) %>% 
    summarise( 
              latitude = mean(latitude),
              longitude = mean(longitude),
              min_latitude=min(latitude),
              max_latitude=max(latitude),
              min_longitude=min(longitude),
              max_longitude=max(longitude),
              ts_min = min(timestamp), 
              ts_max = max(timestamp)
              ) %>%
    ungroup()
}


#********************************************************************************
#  count_staypoints 
#********************************************************************************
count_staypoints = function( df_staypoints ) {

  df_staypoints  %>%
    group_by( userid, night) %>% 
    summarise( sp_total = max( n_staypoint)) %>%
    ungroup()
}


#********************************************************************************
#  count_staypoints_per_algorithm 
#********************************************************************************
count_staypoints_per_algorithm = function( df_count_staypoints ) {

  df_count_staypoints  %>%
    summarise( sp_total = sum( sp_total )) 
}

#********************************************************************************
#  get_matching_survey 
#********************************************************************************
get_matching_survey = function( df_staypoints,  df_survey_nested ) {
  # match all staypoints up to surveys, 
  # return df with single line per userid, night and staypoint 
  # with MATCH FOUND, matches in /which_survey/

  if (nrow( df_staypoints) == 0) {
    return( tibble( ))
  }

  # keep track of ALL staypoints found so we don't lose any staypoints
  # when we join them to the surveys in the next step
  maximum_seconds_distant = 5*60

  # which staypoints match survey timestamps
  df_staypoints  %>%
    group_by( userid, night, n_staypoint ) %>%
    summarise( longitude = mean(longitude), latitude = mean(latitude), 
              timestamp_start = min(timestamp), 
              timestamp_end = max(timestamp)) %>%
    group_by( userid, night ) %>%
    nest(  staypoints = c( n_staypoint, starts_with('timestamp'), ends_with('itude'))) %>%
    inner_join( df_survey_nested , by=c('userid', 'night')) %>% 
    group_by( userid, night ) %>%
    do( joined = interval_inner_join( .$surveys[[1]], .$staypoints[[1]], by=c('timestamp_start','timestamp_end'),
                                     maxgap=maximum_seconds_distant ))  %>%
    unnest( joined ) %>%
    ungroup() %>% 
    { . } -> df 

  if(nrow(df) != 0 ) {
    df %>% 
      dplyr::rename( 
        timestamp_survey = timestamp_start.x,
        timestamp_start_location =timestamp_start.y ) %>% 
        { . } -> df
  }

  df 

}



#********************************************************************************
#  get_matching_survey_per_staypoint
#********************************************************************************
get_matching_survey_per_staypoint= function( df_matching_survey ) {
  # gather all the surveys for each staypoint

  df_matching_survey %>%
    group_by( userid, night, n_staypoint ) %>%
    mutate( minutes_since_arrival = round(( timestamp_survey - min( timestamp_start_location ))/60,2)) %>%
    arrange( timestamp_survey) %>%
    summarise( which_survey = paste('TS:', timestamp_survey, ':SURVEY:', which, ':MINUTES:', minutes_since_arrival, collapse=',')) %>%
    ungroup() 

}


#********************************************************************************
#  summarise_matching_surveys
#********************************************************************************
summarise_matching_surveys= function( df_matching_survey ) {
# for each dataset, we want the total:
# number of staypoints (sp_total), and the number of staypoints that matched surveys (survey_total)

  df_matching_survey%>%
    summarise(  surveys_total = n()) 

}



#********************************************************************************
#  df_summarise_staypoint_algorithms
#********************************************************************************
df_summarise_staypoint_algorithms = function( df_staypoints, df_matching_survey,df_sp_joined_geography ) {

  # want to get a summary of how many hits, of each type, for each algorithm
  # for each filename, calculate 
  # number of hits, 
  df_staypoints %>% 
    left_join( df_matching_survey, by=qc( userid, night, filename, n_staypoint)) %>%
    left_join( dplyr::select( df_sp_joined_geography, userid, night, filename, n_staypoint, distance_to_bar) , by=qc( userid, night, filename, n_staypoint)) 

}
