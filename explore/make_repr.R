
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
   process1c = target(
                          process1c ( filtered_accuracy ),
                          transform = map(filtered_accuracy, .tag_out=filtered_data )
  )
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




