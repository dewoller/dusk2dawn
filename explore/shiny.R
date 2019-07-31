library(shiny)
library(leaflet)
library(tmap)
library(tidyverse)



df_osm_amenities= readRDS('data/shiny/df_osm_amenities.rds')
df_osm_leisure= readRDS('data/shiny/df_osm_leisure.rds')
df_bars = readRDS('data/shiny/df_bars.rds')
df_best_location = readRDS('data/shiny/df_best_location.rds')
df_all_ts = readRDS('data/shiny/df_all_ts.rds')

df_best_location  %>%
  count( userid, night) %>% 
  mutate( id = paste(userid, night, n)) %>%
  { . } -> df_people_nights 




data(World)

file_list = list.files( path='data/', pattern='save.*rds', full.names=TRUE ) 

runApp(list(

      ui = fluidPage(
               titlePanel("Shiny tmap!"),
               sidebarLayout(
                     sidebarPanel(
                            selectInput("staypoint_algorithm", label = "Staypoint Algorithm", choices = file_list), 
                            selectInput("person_night", label = "person", choices = df_people_nights$id)  
                            ),
                     mainPanel(
                           leafletOutput("map")
                     )
               )
               ),

      server = function(input, output) {
        # generate the route 

        output$map = renderLeaflet( {
          readRDS( input$staypoint_algorithm) %>%
            inner_join( df_people_nights %>% filter( id == input$person_night )) %>% 
            st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")  %>%
            #  mutate( ts = as.numeric( cut(time_stamp, 8))) %>%
            #    filter( n_staypoint>0 ) %>%
            mutate( n_staypoint = as.factor( n_staypoint )) %>%
            mutate( size=ifelse( n_staypoint==0, .01, 1 )) %>%
            tm_shape()  + 
            tm_symbols(col = "n_staypoint", shape = "n_staypoint", scale = .3, size='size')  +
            tm_scale_bar(position=c("left", "bottom")) +
            tm_basemap(leaflet::providers$Stamen.TonerLite)  +
            #tm_shape( df_bars ) +
            #tm_bubbles( col='red', scale=.3) +
            #tm_shape( df_osm_amenities$osm_points %>% filter( !is.na( amenity)) ) +
            #tm_bubbles( col='black', scale=.3, id='name')+
            #tm_shape( df_osm_leisure$osm_points %>% filter( !is.na( leisure)) ) +
            #tm_bubbles( col='black', scale=.3, id='name')+
            #tm_shape( df_osm_amenities$osm_polygon %>% filter( !is.na( amenity)) ) +
            #  tm_polygons(col='black',  id='name')+
            tm_shape( df_osm_leisure$osm_polygon %>% filter( !is.na( leisure)) ) +
            tm_polygons( col='gray', id='name') +
            tm_scale_bar() %>% 
            { . } -> tm

                                   tmap_leaflet(tm)

         })
      }
      ))
