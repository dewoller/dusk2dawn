
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


mycomb <- function(...) {
  arg_symbols <- match.call(expand.dots = FALSE)$...
  arg_names <- as.character(arg_symbols)
  #browser()
  out <- NULL
  for (arg_name in arg_names) {
    print( arg_name )
    dataset <- readd(arg_name, character_only = TRUE) %>% mutate( source=arg_name )
    out <- bind_rows(out, dataset)
    #    gc() # Run garbage collection.
  }
  out
}

id = function (...) {
  paste(...)
}

r_0_range = c('0.1','0.2')
r_1b_range = c('1b1','1b2')
r_1c_range = c('1c2','1c2')

drakeplan <- drake::drake_plan(
  trace=TRUE,
#
  #
  process0 = target(
                     id ( process0_var ),
                     transform = map(process0_var = !!r_0_range )
  )
  ,
  process1b = target(
                     id ( process0,process1b_var ),
                     transform = cross( process0, process1b_var = !!r_1b_range, .tag_out=filtered_data )
  )
  ,
   process1c = target(
                          id (process0, process1c_var ),
                          transform = cross( process0, process1c_var = !!r_1c_range, .tag_out=filtered_data )
  )
  ,
  process2 = target(
                            id( filtered_data, process2_var ),
                            transform=cross( filtered_data,
                                             process2_var = c('2'))
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

make(drakeplan)


