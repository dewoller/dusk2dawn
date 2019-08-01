library(shiny)
library(leaflet)
library(tmap)
library(tidyverse)
library(sf)
library(glue)
library(wrapr)
source('lib/get_data.R')



df_osm_amenities= readRDS('data/shiny/df_osm_amenities.rds')
df_osm_leisure= readRDS('data/shiny/df_osm_leisure.rds')
df_bars = readRDS('data/shiny/df_bars.rds')
df_location = readRDS('data/shiny/df_location.rds')
df_all_ts = readRDS('data/shiny/df_all_ts.rds')

df_best_location  %>%
  count( userid, night) %>% 
  mutate( n = sprintf( '%05d', n)) %>%
  mutate( id = paste(n, userid, night)) %>%
  arrange( desc( id )) %>%
  { . } -> df_people_nights 

runApp(list(
      ui = fluidPage(
               titlePanel("Dusk2dawn paths"),
               sidebarLayout(
                     sidebarPanel(
                            actionButton("goButton", "Go!"),
                            selectInput("person_night", label = "person", choices = df_people_nights$id),
                            sliderInput('max_jump_time', 'max_jump_time (seconds)', 300 , 900, 300, step = 100) ,
                            sliderInput('min_staypoint_time', 'min_staypoint_time (seconds)', 60 , 1800, 300, step = 300) ,
                            sliderInput('max_staypoint_distance', 'max_staypoint_distance  (m)', 2 , 50, 50, step = 2) ,
                            sliderInput('sigma', 'sigma (sd)', .5 , 3, 1, step = .5) ,
                            checkboxGroupInput("show_what", label = h3("Display?"), choices = list("OSM leisure" = 1, "OSM amenities" = 2, "4square bars" = 3, "survey points" = 4, "staypoints" = 5))
                            ),
                     mainPanel(
                           leafletOutput("map")
                     )
               )
               ),

      server = function(input, output) {
        # generate the route 
        mscale = 0.5

        reactive({
          print( 'calculating')
          df_location %>%
            inner_join( df_people_nights %>% filter( id == input$person_night ), by=c('userid', 'night')) %>% 
            arrange( timestamp ) %>%
            prune_gps_outliers( sigma=input$sigma, width=15) %>% 
            { . } -> df_pruned
        })

        reactive({
          print( 'finding Staypoints')
          df_pruned  %>%
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
          sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326)  %>%
          { . } -> df_sp_pruned
        })

        reactive({
          print( 'finding surveys')
          df_all_ts %>% 
            inner_join( df_people_nights %>% filter( id == input$person_night ), by=c('userid', 'night')) %>% 
            distinct( which, timestamp, userid, night) %>%
            arrange( timestamp) %>%
            difference_right_join( df_pruned , by=c('timestamp'), max_dist=300, distance_col='ts_dist')  %>% 
            filter( !is.na( ts_dist )) %>%
            group_by(which, timestamp.x) %>%
            filter( ts_dist == min(ts_dist )) %>%
            sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326) %>%
            { . } -> df_surveys
        })

        output$map = renderLeaflet( {
          input$goButton
          isolate( {
          print( 'mapping')
            tm <-  df_pruned %>% 
              st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")  %>%
              arrange(timestamp) %>%
              summarise(do_union = FALSE) %>%
              sf::st_cast("LINESTRING")  %>%
              tm_shape()  + 
              tm_lines() + 
              tm_scale_bar(position=c("left", "bottom")) +
              tm_basemap(leaflet::providers$Stamen.TonerLite) 

            if (1 %in% input$show_what ) {
              tm <- tm + 
                tm_shape( df_osm_leisure$osm_points %>% filter( !is.na( leisure)) ) +
                tm_bubbles( col='grey', scale=mscale, id='name')+
                tm_shape( df_osm_leisure$osm_polygon %>% filter( !is.na( leisure)) ) +
                tm_polygons( col='grey', id='name') 
            }
            if (2 %in% input$show_what ) {
              tm <- tm + 
                tm_shape( df_osm_amenities$osm_points %>% filter( !is.na( amenity)) ) +
                tm_bubbles( col='black', scale=mscale, id='name')+
                tm_shape( df_osm_amenities$osm_polygon %>% filter( !is.na( amenity)) ) +
                tm_polygons(col='black',  id='name')
            }
            if (3 %in% input$show_what ) {
              tm <- tm + 
                tm_shape( df_bars ) +
                tm_bubbles( col='orange', scale=mscale, id='name') 
            }
            if (4 %in% input$show_what ) {
              tm <- tm + 
                tm_shape( df_surveys ) +
                tm_symbols( col='red', scale=mscale,alpha=.1 ) 
            }
            if (5 %in% input$show_what ) {
              tm <- tm + 
                tm_shape( df_sp_pruned ) +
                tm_symbols( col='green', scale=mscale,alpha=.5 )
            }
          })

          print( 'finished')
          tmap_leaflet(tm)

         })
      }
      ))
