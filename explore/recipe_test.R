needs(recipes)
source('lib/geodist.R')
source('lib/nafill_down.R')
source('lib/nafill_mean.R')

recipe(Ozone ~ ., data = airquality) %>%
  step_naomit( all_numeric() ) %>%
  step_last_dist (lat='Solar.R', lon='Ozone') %>%
  prep(airquality, verbose = FALSE, retain = TRUE) %>%
  juice()
