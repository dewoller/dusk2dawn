library(tidyverse)
f_process1a = function( a ) { tribble( ~var, a)}
f_process1b = function( a) { tribble( ~var, a)}
f_process2 = function( a,b) { bind_rows( a, tribble( ~var, b))}



library(drake)

plan <- drake_plan(
  process1a = target(
    f_process1a(process1a_var),
    transform = map(process1a_var = c(1, 2), .tag_out = process1)
  ),
  process1b = target(
    f_process1b(process1b_var),
    transform = map(process1b_var = c(2, 3), .tag_out = process1)
  ),
  process2 = target(
    f_process2(process1, process2_var),
    transform = cross(process1, 
                      process2_var = c(4, 5) )
  ),
  final = target(
    bind_rows( process2, .id="target"), 
    transform = combine (process2)
  ),
  trace = TRUE
)
plan
#config <- drake_config(plan)
#vis_drake_graph(config)
make(plan)
readd(final)


library(tidyverse)
library(drake)
f_process1a = function( a ) { tribble( ~var, a)}

plan <- drake_plan(
                   process1a = target(
                                      f_process1a(process1a_var),
                                      transform = map(process1a_var = c(1, 2))
                                      ),
                  final = target(
                                  gdata::combine(process1a) ,
                                  transform = combine (process1a)
                                  ),
                   trace = TRUE
)
plan
#config <- drake_config(plan)
#vis_drake_graph(config)
make(plan)
readd(final)

reprex::reprex()

models <- c("glm", "hierarchical")
plan <- drake_plan(
                   data = target(
                                 get_data(x),
                                 transform = map(x = c("simulated", "survey"))
                                 ),
                   analysis = target(
                                     analyze_data(data, model),
                                     transform = cross(data, model = !!models, .id = c(x, model))
                                     ),
                   summary = target(
                                    summarize_analysis(analysis),
                                    transform = map(analysis, .id = c(x, model))
                                    ),
                   results = target(
                                    bind_rows(summary),
                                    transform = combine(summary, .by = data)
                   )
                   , trace=TRUE
)
plan
  print(drake_plan_source(plan))
config <- drake_config(plan)
vis_drake_graph(config)



plan <- drake_plan(
                   data = target(
                                 sim_data(mean = x, sd = y),
                                 transform = map(x = c(1, 2), y = c(3, 4), .tag_out=a)
                                 ),
                   larger = target(
                                   bind_rows(a, .id = "id") %>%
                                     arrange(sd) %>%
                                     head(n = 400),
                                   transform = combine(a)
                   )
                   , trace=TRUE
)
plan
print(drake_plan_source(plan))
config <- drake_config(plan)
vis_drake_graph(config)


drake_plan(
           x = target(
                      simulate_data(center, scale),
                      transform = map(center = c(2, 1, 0), scale = c(3, 2, 1))
           )
)

my_grid <- tibble(
                  sim_function = c("rnrom", "rt", "rcauchy"),
                  title = c("Normal", "Student t", "Cauchy")
)
my_grid$sim_function <- rlang::syms(my_grid$sim_function)

drake_plan(
           x = target(
                      simulate_data(sim_function, title, center, scale),
                      transform = map(
                                      center = c(2, 1, 0),
                                      scale = c(3, 2, 1),
                                      .data = !!my_grid,
                                      # In `.id`, you can select one or more grouping variables
                                      # for pretty target names.
                                      # Set to FALSE to use short numeric suffixes.
                                      .id = sim_function # Try `.id = c(sim_function, center)` yourself.
                      )
           )
)



drake_plan(
           x = target(
                      simulate_data(sim_function, title, center, scale),
                      transform = map(
                                      center = c(2, 1, 0),
                                      scale = c(3, 2, 1),
                                      .data = !!my_grid,
                                      # In `.id`, you can select one or more grouping variables
                                      # for pretty target names.
                                      # Set to FALSE to use short numeric suffixes.
                                      .id = c(sim_function, center) # Try `.id = c(sim_function, center)` yourself.
                      )
           )
)



plan <- drake_plan(
                   data = target(
                                 c(mean = x, sd = y),
                                 transform = map(x = c(1, 2), y = c(3, 4))
                                 ),
                   larger = target(
                                   bind_rows(data, .id = "id") %>%
                                     arrange(sd) %>%
                                     head(n = 400),
                                   transform = combine(data)
                   )
)

make(plan)
plan

