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



dfm_bars = readd(df_4sq_locations_filtered ) %>%
  sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

df_location = readd(df_location)
loadd(df_predictable_surveys)

df_location  %>%
  count( userid, night) %>%
  mutate( c_n = sprintf( '%4d', n)) %>%
  mutate( id = paste(str_sub(userid,1,5), str_sub(night,6,10), '(n=', c_n,')')) %>%
  arrange( desc(n)) %>%
  { . } -> df_people_nights

icon_bar = tmap_icons('https://image.flaticon.com/icons/png/512/2474/2474421.png', as.local=TRUE)
icon_survey = tmap_icons('https://image.flaticon.com/icons/png/512/2443/2443779.png', as.local=TRUE)

ui = fluidPage(
      titlePanel("Dusk2dawn paths"),
      sidebarLayout(
        sidebarPanel(
          actionButton("goButton", "Go!"),
          selectInput("person_night", label = "person", choices = df_people_nights$id),
          checkboxGroupInput("staypoint_algorithm", label = "Staypoint algorithm?",
                              choices = list(
                                            "mechanical" =  "staypoints" ,
                                            "heatmap" ="optics"
                                            ),
                              selected='optics'),
            checkboxGroupInput("min_staypoint_time", label = "min_staypoint_time (minutes)",
                choices = list(
                                "5" =  "300" ,
                                "10" ="600",
                                "15" ="900"
                                ),
                selected='300'),
          checkboxGroupInput("max_jump_time", label = "max_jump_time (hours)",
                choices = list(
                                ".5" =  "1800" ,
                                "6" ="14400"
                                ),
                selected='1800'),

          checkboxGroupInput("max_staypoint_distance", label = "max_staypoint_distance (m)",
          choices = list(
                          "10" =  "10" ,
                          "20" =  "20" ,
                          "30" =  "30" ,
                          "50" =  "50" ,
                          "100" ="100"
                          ),
          selected='100'),

          checkboxGroupInput("interpolation_delay", label = "Interpolate points between points with at least this gap (min)",
          choices = list(
                        "2" = "120" ,
                        "5" = "300" ,
                        "10" = "600"
                        ),
          selected='120'),
          checkboxGroupInput("accuracy", label = "Discard points with less than this GPS accuracy (m)",
          choices = list(
                          "10" =  "10" ,
                          "100" ="100"
                          ),
          selected='100'),


          checkboxGroupInput("show_what", label = h3("Display?"),
                              choices = list(
                                            "4square bars (orange)" = "4square bars" ,
                                            "survey points (green)" ="survey points" ,
                                            "staypoints (red)" =   "staypoints"    ),
                              selected=c('staypoints', 'survey points')
                              )
          ),
    mainPanel(
              tmapOutput("map", width = "100%", height = 600) ,
              verbatimTextOutput("summary")
            )
))
################################################################################
server = function(input, output, session) {
  # generate the route
  scale_big = 0.5
  scale_medium = 0.3
  scale_small = 0.05
#browser()

  df_person_night = reactive({
      df_people_nights %>%
        filter( id == input$person_night )
  })

  df_pruned_points = reactive({
    df_location %>%
      inner_join( df_person_night(), by=c('userid', 'night'))
  })

  dfm_pruned_points = reactive({
    df_pruned_points() %>%
      sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  })

  dfm_location_person_night = reactive({
    dfm_pruned_points() %>%
      arrange( timestamp ) %>%
      summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING")
    })

  dfm_staypoint_person_night = reactive({
    #staypoints_distance_120_300_10_interpolated_locations_120_filtered_accuracy_10
    str_c( input$staypoint_algorithm,
          'distance',
            input$max_jump_time,
            input$min_staypoint_time,
            input$max_staypoint_distance,
            'interpolated_locations',
            input$interpolation_delay,
            'filtered_accuracy',
            input$accuracy, sep='_') %>%
    readd( character_only=TRUE) %>%
    inner_join( df_person_night(), by=c('userid', 'night'))  %>%
  sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  })

  dfm_surveys = reactive({
    print( 'Where are the predictable surveys')
    #browser()

    df_predictable_surveys %>%
      inner_join( df_person_night(), by=c('userid', 'night')) %>%
      distinct( timestamp ) %>%
      fuzzyjoin::difference_join( y=df_pruned_points() , by=c('timestamp'), max_dist=30000, distance_col='ts_dist')  %>%
      filter( !is.na( ts_dist )) %>%
      group_by(timestamp.x) %>%  # find the point that is the at the minimum distance for each timestamp
      filter( ts_dist == min(ts_dist )) %>%
      { . } -> df_temp

    if (nrow(df_temp ) == 0 )  {
      dfm_pruned_points() %>% head(1)
    } else {
      df_temp %>% sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
    }
  })

  output$map <- reactive({ renderTmap({
    tm_scale_bar(position=c("left", "bottom")) +
      tm_basemap(leaflet::providers$Stamen.TonerLite) +
      tm_shape( dfm_location_person_night() )  +
      tm_lines( alpha=.5) +
      tm_shape( dfm_pruned_points() ) +
      tm_symbols( col='black', scale=scale_small, alpha=.5, shape=24) +
      tm_shape( dfm_staypoint_person_night() ) +
      tm_symbols( col='green', scale=scale_big,alpha=.5 , shape=23, zindex=401)
  }) })

  observe({
    show_what <- input$show_what
    tmapProxy("map", session, {

      crop_via_points <- function( dfm, type="point" ) {
        rv = dfm %>% st_crop( dfm_pruned_points() %>% st_bbox())
        if (nrow(rv) == 0) {
          showNotification(paste( "I have no points of type ", type, 'in the area specified'), type='warning')
          rv = dfm_pruned_points() %>% head(1)
        }
        rv
      }

      tm = tm_remove_layer(401)

      if ("4square bars" %in% show_what ) {

        tm <- tm +
          tm_shape( dfm_bars %>% crop_via_points() ) +
          tm_symbols( col='orange', scale=scale_medium, id='name', alpha=.3, shape=icon_bar, zindex = 401)

      }
      if ("survey points" %in% show_what ) {

        tm <- tm +
          tm_shape( dfm_surveys() ) +
          tm_symbols( col='red', scale=scale_big,alpha=.5, shape=icon_survey , zindex = 401)

      }
      if ("staypoints" %in% show_what ) {

        tm <- tm +
          tm_shape( dfm_staypoint_person_night() ) +
          tm_symbols( col='green', scale=scale_big,alpha=.5 , shape=23, zindex = 401)

      }
    })
  })

    #  browser()
    print( 'finished')


    output$summary = renderPrint({
        rv = paste( '# points', nrow( df_pruned_points() ), "<p>")
        if ("staypoints" %in% input$show_what ) {
          rv = paste( rv, '# staypoints', nrow( dfm_staypoint_person_night() ), "<p>")
        }
        rv
      })

}


app <- shinyApp(ui, server)
app
