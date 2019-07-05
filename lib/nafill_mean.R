
step_nafill_mean <- function(recipe, ..., role = NA, trained = FALSE, 
                        columns = NULL, skip = FALSE,
                        id = rand_id("nafill_mean")) {
  add_step(
    recipe,
    step_nafill_mean_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_nafill_mean_new <- function(terms, role, trained, columns, skip, id) {
  step(
    subclass = "nafill_mean",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_nafill_mean <- function(x, training, info = NULL, ...) {
  step_nafill_mean_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = terms_select(x$terms, info = info),
    skip = x$skip,
    id = x$id
  )
}


mean_one = function ( x ) {
  x %>% 
    as_tibble() %>%
    mutate( m1=value, m2=value ) %>%
    fill( m1, .direction='down') %>%
    fill( m2, .direction='up') %>%
    mutate( value = (m1+m2)/2) %>%
    select(-m1, -m2) %>%
    pluck( "value" )
}

#undebug( bake.step_nafill_mean )
#' @export
bake.step_nafill_mean <- function(object, new_data, ...) {

  require(tibble)
  mutate_at( new_data, object$columns, mean_one )

}

print.step_nafill_mean <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Flling mean rows with NA values in ", sep = "")
    cat(format_selectors(x$terms, wdth = width))
    cat("\n")
    invisible(x)
  }

#' @rdname step_nafill_mean
#' @param x A `step_nafill_mean` object.
#' @export
tidy.step_nafill_mean <- function(x, ...) {
  res <-simple_terms(x, ...)
  res$id <- x$id
  res
}

format_selectors <- function(x, wdth = options()$width - 9, ...) {
  ## convert to character without the leading ~
  x_items <- lapply(x, function(x)
                    as.character(x[-1]))
  x_items <- unlist(x_items)
  format_ch_vec(x_items, width = wdth, sep = ", ")
}



## then 9 is to keep space for "[trained]"
format_ch_vec <-
  function(x,
           sep = ", ",
           width = options()$width - 9) {
    widths <- nchar(x)
    sep_wd <- nchar(sep)
    adj_wd <- widths + sep_wd
    if (sum(adj_wd) >= width) {
      keepers <- max(which(cumsum(adj_wd) < width)) - 1
      if (length(keepers) == 0 || keepers < 1) {
        x <- paste(length(x), "items")
      } else {
        x <- c(x[1:keepers], "...")
      }
    }
    paste0(x, collapse = sep)
  }

test.step_nafill_mean <- function() {
  library(recipes)

  recipe(Ozone ~ ., data = airquality) %>%
    step_nafill_mean(Solar.R, Ozone) %>%
    step_mutate
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

}


 
