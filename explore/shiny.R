# post drake shiny
# just display an existing usernight, 
# discovered staypoints, along with survey locations and bars
needs( 'fuzzyjoin')
needs('IRanges')
library(shiny)
library(leaflet)
library(tmap)
library(tidyverse)
library(sf)
library(glue)
library(wrapr)
source('lib/get_data.R')
source('lib/gps_functions.R')


a %>%
  enframe() %>%
  select(value) %>%
  dplyr::rename( dataset = value) %>%
  filter( startsWith( dataset, 'staypoints_distance')) %>%


dfm_osm_amenities= readRDS('data/shiny/df_osm_amenities.rds')
dfm_osm_leisure= readRDS('data/shiny/df_osm_leisure.rds')
dfm_bars = readRDS('data/shiny/df_bars.rds')

df_location = readRDS('data/shiny/df_location.rds')
df_all_ts = readRDS('data/shiny/df_all_ts.rds')

df_location  %>%
  count( userid, night) %>%
  mutate( n = sprintf( '%05d', n)) %>%
  mutate( id = paste(userid, night, '(n=', n,')')) %>%
  arrange( userid, night) %>%
  { . } -> df_people_nights

icon_park = tmap_icons(system.file("img/park.png", package = "tmap"))
icon_playground = tmap_icons(system.file("img/playground.png", package = "tmap"))
icon_bar = tmap_icons(system.file("img/bar.png", package = "tmap"))
icon_survey = tmap_icons(system.file("img/survey.png", package = "tmap"))

runApp(list(
      ui = fluidPage(
               titlePanel("Dusk2dawn paths"),
               sidebarLayout(
                     sidebarPanel(
                            actionButton("goButton", "Go!"),
                            selectInput("person_night", label = "person", choices = df_people_nights$id),
                            checkboxGroupInput("show_what", label = h3("Display?"), 
                                               choices = list(
                                                              "OSM leisure (grey)" =  "OSM leisure" ,
                                                              "OSM amenities (blue)" ="OSM amenities" ,
                                                              "4square bars (orange)" = "4square bars" ,
                                                              "survey points (green)" ="survey points" ,
                                                              "staypoints (red)" =   "staypoints"    ),
                                               selected=c('staypoints', 'survey points')
                                               ),
                            actionButton("browser", "browser")
                            ),
                     mainPanel( leafletOutput("map"), verbatimTextOutput("summary"))
               )),

      server = function(input, output) {
        # generate the route
        scale_big = 0.5
        scale_medium = 0.3
        scale_small = 0.05

        df_location_single = reactive({
          df_location %>%
            inner_join( df_people_nights %>% filter( id == input$person_night ), by=c('userid', 'night')) %>% 
            arrange( timestamp ) 
        })

        df_location_filtered = reactive({
          df_location_filtered <- df_location_single()
          print( 'Filtering locations')
          if ("outlier" %in% input$pruning_algo ) {
            print( 'Pruning outliers')
            df_location_filtered %>%
              prune_gps_outliers( sigma=input$sigma, width=input$outlier_width) %>% 
              { . } -> df_location_filtered
          }
          if ("geohash" %in% input$pruning_algo ) {
            print( 'Pruning via geohash')
            df_location_filtered %>%
              prune_gps_geohash( gh_precision=input$geohash_precision, minpoints=input$geohash_n) %>% 
              { . } -> df_location_filtered
          }
          shiny::validate(
            shiny::need(nrow(df_location_filtered) != 0, "No data available for chosen filters!")
            )  
          df_location_filtered
        })

        dfm_pruned_points = reactive( {
          df_location_filtered() %>%
            sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
        })

        dfm_single  = reactive({ dfm_pruned_points() %>% head(1) })

        dfm_pruned_lines = reactive( {
          dfm_pruned_points () %>%
            arrange(timestamp) %>%
            summarise(do_union = FALSE) %>%
            sf::st_cast("LINESTRING")  
        })

        dfm_staypoints = reactive({
          df_location_filtered()  %>% 
          { . } -> df_sp_temp
        print( paste( 'finding Staypoints, n=',  nrow( df_sp_temp)))

        df_sp_temp %>%
            findStayPoint(
                          max_jump_time = input$max_jump_time,
                          min_staypoint_time= input$min_staypoint_time,
                          max_staypoint_distance= input$max_staypoint_distance
                          ) %>%
          filter( n_staypoint > 0 ) %>%
          group_by( n_staypoint) %>%
          summarise( latitude = mean( latitude),
                    longitude = mean( longitude),
                    time = round((max( timestamp ) - min(timestamp)) / 60, 1)) %>%
          sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>% 
          { . } -> df_rv

          shiny::validate( shiny::need(nrow(df_rv) != 0, "No staypoints available for chosen filters and points!"))  
          df_rv
        })

        dfm_surveys = reactive({
          print( 'Where are the surveys')

          df_all_ts %>%
            inner_join( df_people_nights %>% filter( id == input$person_night ), by=c('userid', 'night')) %>%
            distinct( which, timestamp, userid, night) %>%
            fuzzyjoin::difference_join( y=df_location_filtered() , by=c('timestamp'), max_dist=30000, distance_col='ts_dist')  %>%
            filter( !is.na( ts_dist )) %>%
            group_by(which, timestamp.x) %>%  # find the point that is the at the minimum distance for each timestamp
            filter( ts_dist == min(ts_dist )) %>%
            sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326)

        })


        output$map = renderLeaflet( {

          crop_via_points <- function( dfm, type="point" ) {
            rv = dfm %>% st_crop( dfm_pruned_points() %>% st_bbox() ) 
            if (nrow(rv) == 0) {
              showNotification(paste( "I have no points of type ", type), type='warning')
              rv = dfm_single()
            }
            rv
          }

          input$goButton
          isolate( {
          print( 'generating Map')

          tm <-  tm_scale_bar(position=c("left", "bottom")) +
            tm_basemap(leaflet::providers$Stamen.TonerLite)

          if ("OSM leisure" %in% input$show_what ) {

            tm <- tm +
              tm_shape( dfm_osm_leisure$osm_points %>% filter( !is.na( leisure)) %>% crop_via_points() ) +
              tm_symbols( col='grey', scale=scale_medium, id='name', shape=icon_park)+
              tm_shape( dfm_osm_leisure$osm_polygon %>% filter( !is.na( leisure)) %>% crop_via_points() ) +
              tm_polygons( col='grey', id='name')

          }
          if ("OSM amenities" %in% input$show_what ) {
            tm <- tm +
              tm_shape( dfm_osm_amenities$osm_points %>% filter( !is.na( amenity)) %>% crop_via_points() ) +
              tm_symbols( col='blue', scale=scale_medium, id='name', shape=icon_bar)+
              tm_shape( dfm_osm_amenities$osm_polygon %>% filter( !is.na( amenity)) %>% crop_via_points() ) +
              tm_polygons(col='blue',  id='name')
          }
          if ("4square bars" %in% input$show_what ) {

            tm <- tm +
              tm_shape( dfm_bars %>% crop_via_points() ) +
              tm_symbols( col='orange', scale=scale_medium, id='name', alpha=.3, shape=icon_bar)

          }
          if ("survey points" %in% input$show_what ) {

            tm <- tm +
              tm_shape( dfm_surveys() ) +
              tm_symbols( col='red', scale=scale_big,alpha=.5, shape=icon_survey )

          }
          if ("staypoints" %in% input$show_what ) {

            tm <- tm +
              tm_shape( dfm_staypoints() ) +
              tm_symbols( col='green', scale=scale_big,alpha=.5 , shape=23)


            }
          })

          tm <- tm +
            tm_shape( dfm_pruned_lines() )  +
            tm_lines( alpha=.5) +
            tm_shape( dfm_pruned_points() ) +
            tm_symbols( col='black', scale=scale_small, alpha=.5, shape=24)

            browser()
          print( 'finished')
          tmap_leaflet(tm)

        })

          output$summary = renderPrint({
            input$goButton
            isolate( {
              rv = paste( '# points', nrow( df_location_filtered() ), "\n")
              if ("staypoints" %in% input$show_what ) {
                rv = paste( rv, '# staypoints', nrow( dfm_staypoints() ), "\n")
              }
              rv 
            })
          })

      }))

