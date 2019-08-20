

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

  create_cluster(11) %>%
	cluster_assign_value('consolidate_staypoints', consolidate_staypoints) %>%
	cluster_library( "tidyverse" ) %>%
	{.} -> cluster

  filenames %>%
    rowwise() %>%
    partition(filename, cluster=cluster) %>%

    do( consolidate_staypoints(.) )  %>%
    collect()


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
    mutate( geohash_8 = gh_encode(latitude, longitude, precision=8L )) %>% 
    mutate( location_type='osm') %>%
    { . } -> df_osm_amenity_simple


  (df_4sq_locations_filtered ) %>%
    select( name, latitude, longitude) %>%
    mutate( geohash_8 = gh_encode(latitude, longitude, precision=8L )) %>% 
    mutate( location_type='4sq') %>%
    { . } -> df_4sq_locations_simple 


  df_4sq_locations_simple  %>%
    bind_rows( df_osm_amenity_simple) %>% 
    arrange( geohash_8) %>%
    { . } -> df_target_locations_combined

  df_target_locations_combined

}

