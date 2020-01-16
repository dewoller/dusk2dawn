
max_jobs <- 32
if (startsWith(Sys.info()["nodename"], "lims")) {
    currentMachine <- "lims"
} else {
    currentMachine <- Sys.info()["nodename"]
}
#currentMachine <- "hermoine"
source('lib/base_initialise.R')


logFileName <- "staypoint_estimation.log"
sp_min_staypoint_time_range <- c(5, 10, 15) * 60
sp_max_jump_time_range <- c(30, 240) * 60
sp_max_staypoint_distance_range <- c(20, 100)
sigma_range <- c(.5, 1, 2, 3, 100)
# gh_precision_range=7:9
# gh_minpoints_range=0:2*6+3
accuracy_range <- c(100, 10)
interpolation_delay_range <- c(120, 300, 600)
# iterations_range=c(10,100,1000)
# hm_range =  c( 1, 10, 100)
# ht_range = c(6, 60, 600 )

if (currentMachine == "dewlap") {
    max_expand_setting <- 3
    df_location_initial <- get_df_single_location()
} else {
    max_expand_setting <- 99999999
    df_location_initial <- get_df_location()
}

drakeplan <- drake::drake_plan(
    max_expand = max_expand_setting,
    #
    # load in the GPS individual locations information
    df_location = df_location_initial ,
    ##
    #
    filtered_accuracy =
        target(
            prune_gps_accuracy(df_location, accuracy),
            transform = map(accuracy = !!accuracy_range, .tag_out = filtered_data)
        ),
    #  filtered_geohash = target(
    #      prune_gps_geohash (df_location, precision, minpoints),
    #      transform = cross( precision  = !!gh_precision_range,
    #      minpoints = !!gh_minpoints_range,
    #      .tag_out=filtered_data)
    #  ) ,
    filtered_sigma =
        target(
            prune_gps_outliers(df_location, .sigma = sigma),
            transform = map(sigma = !!sigma_range, .tag_out = filtered_data)
        ),
    filtered_sigma.v2 =
        target(
            prune_gps_outliers.v2(df_location, .sigma = sigma),
            transform = map(sigma = !!sigma_range, .tag_out = filtered_data)
        ),
    interpolated_locations =
        target(
            interpolate_locations(filtered_accuracy, max_delay = max_delay, period = 30),
            transform = cross(filtered_accuracy, max_delay = !!interpolation_delay_range, .tag_out = filtered_data)
        ),
    staypoints_distance =
        target(
            find_staypoint_distance(filtered_data,
                                    max_jump_time ,
                                    min_staypoint_time ,
                                    max_staypoint_distance ),
            transform = cross(filtered_data,
                max_jump_time = !!sp_max_jump_time_range,
                min_staypoint_time = !!sp_min_staypoint_time_range,
                max_staypoint_distance = !!sp_max_staypoint_distance_range,
                .tag_out = staypoint_discovery
            )
        ),
    optics_distance =
        target(
            find_cluster_optics_all(filtered_data,
                                    max_jump_time = max_jump_time,
                                    min_staypoint_time = min_staypoint_time,
                                    max_staypoint_distance = max_staypoint_distance),
            transform = cross(interpolated_locations,
                max_jump_time = !!sp_max_jump_time_range,
                min_staypoint_time = !!sp_min_staypoint_time_range,
                max_staypoint_distance = !!sp_max_staypoint_distance_range,
                .tag_out = staypoint_discovery
            )
        ),
    #
    #####################################
    # Evaluation data prep
    #####################################
    # get target locations, osm and 4sq
    df_4sq_locations_filtered = get_df_4sq_locations_filtered(),
    df_osm_amenity = get_df_osm_locations_amenity(),
    df_osm_leisure = get_df_osm_locations_leisure(),
    #
    # get surveys
    df_all = get_df_all() ,

    # get survey timestamps
    df_all_ts = get_df_all_ts(df_all) ,

    # get valid surveys;  eliminate pre, tom and load categories, and surveys without gps points
    df_all_ts_valid = get_df_all_ts_valid(df_all_ts, df_location) ,

    #  # nest surveys
    df_survey_nested = get_df_survey_nested(df_all_ts_valid) ,

    #####################################
    # Evaluate
    ####################################

    #####################################
    # evaluate each staypoints
    ####################################


    # summarise staypoints for every userid, night.  One line per staypoint / algorithm
    # for each staypoint, what is the gist of it
    df_summarise_staypoints =
        target(summarise_staypoints(staypoint_discovery),
            transform = map(staypoint_discovery)
        ),

    # combine summarised staypoints for every userid, night.  One line per staypoint / algorithm 
    df_all_summarise_staypoints =
        target(my_combine(df_summarise_staypoints),
            transform = combine(df_summarise_staypoints)
        ),


    #####################################
    #  count staypoints per userid/night/algorithm
    ####################################

    # count staypoints for every userid, night.  One dataset per algorithm
    df_count_staypoints =
        target(
            count_staypoints(staypoint_discovery),
            transform = map(staypoint_discovery)
        ),

    # total staypoints for each algorithm, one line per algorithm
    df_count_staypoints_per_algorithm =
        target(
            count_staypoints_per_algorithm(df_count_staypoints),
            transform = map(df_count_staypoints)
        ),

    df_all_count_staypoints_per_algorithm =
        target(
            my_combine(df_count_staypoints_per_algorithm),
            transform = combine(df_count_staypoints_per_algorithm)
        ),

    ####################
    # Base
    #match survey data with staypoints
    ####################

    df_matching_survey =
      target(
            get_matching_survey(staypoint_discovery, df_survey_nested),
            transform = map(staypoint_discovery)
            ),
    #
    # what category do matches fall into
    df_matching_survey_categories =
      target(
            matching_survey_categories (df_matching_survey),
            transform = map(df_matching_survey)
            ),
    # summarise matches
    df_matching_survey_categories_summary =
        target(
            summarise_matching_survey_categories(df_matching_survey_categories),
            transform = map(df_matching_survey_categories)
        ),

    # combine summarised matches
    df_matching_survey_categories_summary_all =
         target(
          my_combine(df_matching_survey_catefories_summary ),
          transform = combine(df_matching_survey_categories_summary )
      ),
    #
    # for each staypoint what survey matches it
    # consolidate surveys per staypoint
    df_matching_survey_per_staypoint =
        target(
            get_matching_survey_per_staypoint(df_matching_survey),
            transform = map(df_matching_survey)
        ),

    #
    df_matching_survey_summarised =
        target(
            summarise_matching_survey_categories ( df_matching_survey_per_staypoint ),
            transform = map(df_matching_survey_per_staypoint)
        ),

    # Each survey, along with all the staypoint points that it matches
    df_matching_survey_summarised_all=
      target(
            my_combine(df_matching_survey_summarised),
            transform = combine(df_matching_survey_summarised)
            ),

    # Each survey, along with all the staypoint points that it matches
    df_all_matching_survey =
        target(
            my_combine(df_matching_survey),
            transform = combine(df_matching_survey)
        ),

    # Each survey, along with the single staypoint points that it matches
    # unused
    df_all_matching_survey_per_staypoint =
        target(
            my_combine(df_matching_survey_per_staypoint),
            transform = combine(df_matching_survey_per_staypoint)
        ),

    # number of surveys for each staypoint, summarised
    df_all_sp_match_survey =
        target(
            my_combine(df_matching_survey_summarised),
            transform = combine(df_matching_survey_summarised)
        ),

    # combine the number of found surveys and the number of found staypoints
    df_all_sp_match_survey_combined =
        target(
            df_all_sp_match_survey %>%
                mutate(source = str_replace(source, ".*(staypoints|optics)_distance_", "\\1_distance_")) %>%
                inner_join(
                    df_all_count_staypoints_per_algorithm %>%
                        mutate(source = str_replace(source, ".*(staypoints|optics)_distance_", "\\1_distance_"))
                )
        ),

    # df_all_sp_match_survey_mode =
    # target(
    #      my_combine( df_matching_survey_summarised_mode) ,
    #      #gdata::combine( df_matching_survey_summarised) %>% rename( original_target=source),
    #      transform = combine(df_matching_survey_summarised_mode)),


    #
    # wflow_publish(knitr_in("analysis/evaluate_staypoint_estimates.Rmd"), view = FALSE),
    trace = TRUE
)

drakeplan_geography <- drake::drake_plan(
    df_target_locations_combined = get_df_target_locations_combined(df_osm_amenity, df_4sq_locations_filtered),
    df_matching_geography =
        target(
            calculate_sp_match_geography(staypoint_discovery, df_target_locations_combined),
            transform = map(staypoint_discovery)
        ),
    df_matching_geography_summarised =
        target(
            summarise_matching_geography(df_matching_geography),
            transform = map(df_matching_geography)
        ),
    #
    # dq_geocoded_addresses = get_df_revgeo_addresses( df_sp_no_bar %>% head(250) ),
    #



    trace = TRUE
)



my_combine <- function(...) {
    arg_symbols <- match.call(expand.dots = FALSE)$...
    arg_names <- as.character(arg_symbols)
    # browser()
    out <- NULL
    for (arg_name in arg_names) {
        print(arg_name)
        dataset <- readd(arg_name, character_only = TRUE) %>% mutate(source = arg_name)
        out <- bind_rows(out, dataset)
        #    gc() # Run garbage collection.
    }
    out
}


if (currentMachine == "lims" | currentMachine == "baseVM2"  ) {

    library(future.batchtools)
    future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl")
    #make(drakeplan, parallelism = "future", jobs = max_jobs, caching = "worker", elapsed = Inf, retries = 1)
    make(drakeplan, parallelism = "future", jobs = max_jobs, elapsed = Inf, retries = 1)

} else if (currentMachine == "dewlap") {
    drakeplan %>%
        drake_config() %>%
        vis_drake_graph()
    #  drake_plan_source(drakeplan)
    drakeplan %>% drake_config() %>% outdated()

} else if (currentMachine == "hermoine") {
    library(sf)
    library(nngeo)

    options(clustermq.scheduler = "multicore")
   make(drakeplan, parallelism = "clustermq", jobs = parallel::detectCores(), memory_strategy = "autoclean")
   # make(drakeplan, parallelism = "clustermq", jobs =1 , memory_strategy = "autoclean")
}

# make(drakeplanns)
