
step_last_dist <- function(recipe,
                         lat = NULL,
                         lon = NULL,
                         role = "predictor",
                         trained = FALSE,
                         log = FALSE,
                         name = "geo_dist",
                         columns = NULL,
                         skip = FALSE,
                         id = rand_id("last_dist")) {
  if (length(log) != 1 || !is.logical(log))
    stop("`log` should be a single logical value.", call. = FALSE)
  if (length(name) != 1 || !is.character(name))
    stop("`name` should be a single character value.", call. = FALSE)

  add_step(
    recipe,
    step_last_dist_new(
      lon = enquos(lon),
      lat = enquos(lat),
      role = role,
      trained = trained,
      log = log,
      name = name,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_last_dist_new <-
  function(lon, lat, role, trained,
           log, name, columns, skip, id) {
    step(
      subclass = "last_dist",
      lon = lon,
      lat = lat,
      role = role,
      trained = trained,
      log = log,
      name = name,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @importFrom stats as.formula model.frame
#' @export
prep.step_last_dist <- function(x, training, info = NULL, ...) {
  lon_name <- terms_select(x$lon, info = info)
  if (length(lon_name) > 1)
    stop("`lon` should resolve to a single column name.", call. = FALSE)
  check_type(training[, lon_name])
  lat_name <- terms_select(x$lat, info = info)
  if (length(lat_name) > 1)
    stop("`lat` should resolve to a single column name.", call. = FALSE)
  check_type(training[, lat_name])

  if (any(names(training) == x$name))
    stop("'", x$name, "' is already used in the data.", call. = FALSE)

  step_last_dist_new(
    lon = x$lon,
    lat = x$lat,
    role = x$role,
    trained = TRUE,
    log = x$log,
    name = x$name,
    columns = c(lat_name, lon_name),
    skip = x$skip,
    id = x$id
  )
}

geo_dist_calc <- function(x) {
  x=as.matrix(x)
  x = cbind( x, rbind( tail( x, -1 ), matrix(c(NA,NA), nrow=1)) )
  apply(x, 1, function(x) sqrt((x[1] - x[3]) ^ 2 + (x[2] - x[4]) ^ 2))
}


#' @importFrom tibble as_tibble
#' @export
#undebug(step_last_dist )
#debug(bake.step_last_dist )
#undebug(bake.step_last_dist )
#undebug( geo_dist_calc )

bake.step_last_dist <- function(object, new_data, ...) {

  dist_vals <-
    geo_dist_calc(new_data[, object$columns])
  if (object$log) {
    new_data[, object$name] <- log(dist_vals)
  } else {
    new_data[, object$name] <- dist_vals
  }
  new_data
}

print.step_last_dist <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Geographical distances between last 2 points","\n")
    invisible(x)
  }



#' @rdname step_last_dist
#' @param x A `step_last_dist` object.
#' @importFrom dplyr bind_rows
#' @export
tidy.step_last_dist <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      latitude = x$columns[1],
      longitude = x$columns[2],
      name = x$name
    )
  } else {
    res <- tibble(
      latitude = sel2char(x$lat),
      longitude = sel2char(x$lon),
      name = x$name
    )
  }
  res$id <- x$id
  res
}

test.step_last_dist  = function () {

recipe(Ozone ~ ., data = airquality) %>%
  step_naomit( all_numeric() ) %>%
  step_last_dist (lat='Solar.R', lon='Ozone') %>%
  prep(airquality, verbose = FALSE, retain = TRUE) %>%
  juice()

}
