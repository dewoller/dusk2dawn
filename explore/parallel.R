library(parallel)
 
# Calculate the number of cores
no_cores <- detectCores() - 1
 
# Initiate cluster
cl <- makeForkCluster(no_cores)

parLapply(cl, 2:4,
          function(exponent)
            2^exponent)
