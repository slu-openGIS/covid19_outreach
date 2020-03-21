# Get Boundaries for St Louis City and County

library(tigris)
library(dplyr)
library(magrittr)
library(sf)

stl <- 
  counties(29, class = 'sf') %>%
  filter(COUNTYFP %in% c('189','510')) %>%
  st_union()

save(stl, file = 'data/stl_sf.rda')
