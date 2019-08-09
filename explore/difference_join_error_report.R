

library(fuzzyjoin)
library(tidyverse)
a <- data.frame(replicate(2,sample(20:30,10,rep=TRUE))) 

b <- data.frame(replicate(2,sample(1:10,10,rep=TRUE))) 

difference_right_join( a, b, by='X1', max_dist = 1, distance_col='distance')


needs(reprex)

reprex()
