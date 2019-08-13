# load the library and create a simple function
library(clustermq)
  fx = function(x) x * 2

# queue the function call on your scheduler
Q(fx, x=1:12, n_jobs=12)
# list(2,4,6)

library(drake)
load_mtcars_example()
 clean(destroy = TRUE)
# options(clustermq.scheduler = "multicore")
make(my_plan, parallelism = "clustermq", jobs = 1, verbose = 4)



drake::drake_example("mlr-slurm")
