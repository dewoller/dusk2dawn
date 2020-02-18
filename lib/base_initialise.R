
load_function <- function() {
    source('lib/functions.R')
    source("lib/get_data.R")
    source("lib/location_prep.R")
    source("lib/gps_functions.R")
    source("lib/evaluate_staypoint_estimates_helper.R")
    source("lib/keys.R")
    source("lib/kernel_density_functions.R")
    source("lib/optics_find_staypoint.R")
    source("lib/merge_adjacent_staypoints.R")
    source("explore/failure_analysis_florian_surveys.R")
}

load_library <- function() {
    #

    suppressWarnings(library(raster))
    suppressWarnings(library(RPostgreSQL))
    suppressWarnings(library(tidyverse))
    suppressWarnings(library(lubridate))
    suppressWarnings(library(drake))
    suppressWarnings(library(osmdata))
    suppressWarnings(library(revgeo))
    suppressWarnings(library(tsibble))
    suppressWarnings(library(magrittr))
    suppressWarnings(library(stringr))
    suppressWarnings(library(knitr))
    suppressWarnings(library(wrapr)) # for the qc function
    suppressWarnings(library(fuzzyjoin))
    suppressWarnings(library(IRanges))
    suppressWarnings(library(multidplyr))
    suppressWarnings(library(geohash))
    suppressWarnings(library(geosphere))
    suppressWarnings(library(zoo))
    suppressWarnings(library(glue))
    suppressWarnings(library(tibbletime))
    suppressWarnings(library(futile.logger))
    suppressWarnings(library(dbscan))
}

load_library()
load_function()

