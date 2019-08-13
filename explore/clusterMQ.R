options(
        clustermq.scheduler = "ssh",
        clustermq.ssh.host = "dewoller@thealfred.duckdns.org", # use your user and host, obviously
        clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging
)
library(tidyverse)
library(clustermq)

fx = function(x) x * 2
Q(fx, x=1:3, n_jobs=3)


library(drake)
drake_example("gsp")

clean()
make(plan)
make(plan, parallelism = "clustermq", jobs = 1)

