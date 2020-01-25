
load_function <- function() {
    # source('lib/functions.R')
    source("lib/get_data.R")
    source("lib/location_prep.R")
    source("lib/gps_functions.R")
    source("lib/evaluate_staypoint_estimates_helper.R")
    source("lib/keys.R")
    source("lib/kernel_density_functions.R")
    source("lib/optics_find_staypoint.R")
    source("lib/merge_adjacent_staypoints.R")
}

load_library <- function() {
    #

    library(raster)
    library(RPostgreSQL)
    library(tidyverse)
    library(lubridate)
    library(drake)
    library(osmdata)
    library(revgeo)
    library(tsibble)
    library(magrittr)
    library(stringr)
    library(knitr)
    library(wrapr) # for the qc function
    library(fuzzyjoin)
    library(IRanges)
    library(multidplyr)
    library(geohash)
    library(geosphere)
    library(zoo)
    library(glue)
    library(tibbletime)
    library(futile.logger)
    library(dbscan)
}

load_library()
load_function()

