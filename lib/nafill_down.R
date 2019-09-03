
step_nafill_down <- function(recipe, ..., role = NA, trained = FALSE, 
                        columns = NULL, skip = FALSE,
                        id = rand_id("nafill_down")) {
  add_step(
    recipe,
    step_nafill_down_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_nafill_down_new <- function(terms, role, trained, columns, skip, id) {
  step(
    subclass = "nafill_down",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_nafill_down <- function(x, training, info = NULL, ...) {
  step_nafill_down_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = terms_select(x$terms, info = info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_nafill_down <- function(object, new_data, ...) {
  tibble::as_tibble(tidyr::fill(new_data, object$columns, .direction='down'))
}

print.step_nafill_down <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Flling down rows with NA values in ", sep = "")
    cat(format_selectors(x$terms, wdth = width))
    cat("\n")
    invisible(x)
  }

#' @rdname step_nafill_down
#' @param x A `step_nafill_down` object.
#' @export
tidy.step_nafill_down <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}


test.step_nafill_down <- function() {

  recipe(Ozone ~ ., data = airquality) %>%
    step_nafill_down(Solar.R) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

}
