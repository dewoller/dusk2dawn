
library(drake)
library(tidyverse)

process1a = function(x) {
  x
}


process1b = function(x) {
  x
}


process1c = function(x) {
  x
}


process2 = function(data, x) {
  c(data, x )
}

mycomb = function( ... ) {
  print(...)
}




drakeplan <- drake::drake_plan(
  trace=TRUE,
#
  #

  filtered_accuracy = target(
                     process1b ( df_location ),
                     transform = map(process1b_var = c(3), .tag_out=filtered_data )
  )
  ,
  interpolated_locations = target(
                                  interpolate_locations (filtered_accuracy, max_delay=max_delay, period=30),
                                  transform = map( filtered_accuracy, max_delay = !!interpolation_delay_range, .tag_out = filtered_data)

                                  ),
  ,
  process2 = target(
                            process2( filtered_data, process2_var ),
                            transform=cross( filtered_data,
                                             process2_var = c(5))
  )
,
  df_matching_survey = target(
                            mycomb( process2),
                              transform = combine(process2 ))
  ,
 
)


drakeplan %>%
  drake_config( ) %>%
  vis_drake_graph( )




