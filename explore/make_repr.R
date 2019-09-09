
library(drake)
library(tidyverse)

process1a = function(x) {
  x
}


process1b = function(x) {
  x
}


process2 = function(data, x) {
  c(data, x )
}




drakeplan <- drake::drake_plan(
  trace=TRUE,
#
  # load in the GPS individual locations information
  #df_location = get_df_single_location() ,
  #
  process1a = target(
                            process1a (process1a_var),
                            transform = map( process1a_var = c(1,2) )
  )
  ,
#
  process1b = target(
                          process1b ( process1b_var),
                          transform = map(process1b_var = c(2,3) )
  )
  ,
  ,
  process2 = target(
                            process2( data, process2_var ),
                            transform=cross( data=c(process1a, process1b), 
                                             process2_var = c(4,5))
  )
)


drakeplan %>%
  drake_config( ) %>%
  vis_drake_graph( )

make(drakeplan )


loadd()
#options(error = recover) # setting the error option
#options(error = dump.frames) # setting the error option
