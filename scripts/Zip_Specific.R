# Zip Specific Data Pull

library(magrittr)
library(sf)
library(tigris)
library(dplyr)
library(httr)

# Get Zip Codes
zips <- c(62201,
          62203,
          62204,
          62205,
          62206,
          62207,
          62059)
zcta <- tigris::zctas(class = 'sf') %>%
  dplyr::filter(ZCTA5CE10 %in% zips)

source('scripts/creds.R')
source('scripts/HERE.R')
grid <- batch_bbox(st_union(zcta), 3821, 102696) # 1KM Grid Squares, Using NAD83 Feet

# Specify the HERE Categories you want
here_categories <- list(
  fueling_station = '700-7600-0116',
  deli = '100-1000-0006',
  convenience = '600-6000-0061',
  grocery = '600-6300-0066',
  drug_store = '600-6400-0000',
  liquor_store = '600-6300-0068',
  check_cashing = '700-7050-0110',
  #hair_beauty = '600-6900-0104', # Does Not Exist in Zips
  laundromat = '700-7400-0137'
)

###########
## Notes ##
###########
# We must iterate through all of the grid squares for each category, this is 1235 * categories
# The API limits the max number of responses, so we brute force by collecting small areas and removing dupes

# Iterate Through Bounding Boxes, Capturing Responses
# Write an Iterator Function

iterator <- function(category, grid, key = here_key){
  # Create Vector to Store Responses
  responses <- vector('list', length(grid))
  
  for (i in seq_along(responses)){
    responses[[i]] <- try(here_browse(category, grid[i], key)) # Try just to catch any extraneous errors
  }
  
  return(responses)
}

# And a Full Workflow
scrape_here <- function(category, grid, key = here_key){
  # Row Bind the Responses and Remove Duplicates
  iterator(category, grid) %>%
    dplyr::bind_rows(.) %>%
    dplyr::filter(!duplicated(uid)) -> df
  sf <-
    st_as_sf(df, coords = c('position.lng', 'position.lat'), crs = 4326)
  return(sf)
}

# For all of the Categories
for (i in names(here_categories)){
  assign(
    paste0(i, '_sf_zip'),
    scrape_here(here_categories[[i]], grid)
  )
}
save(fueling_station_sf_zip, deli_sf_zip, convenience_sf_zip, grocery_sf_zip, drug_store_sf_zip, liquor_store_sf_zip, check_cashing_sf_zip, laundromat_sf_zip, file = 'data/ZIP_Specific.rda')

#############################
## Raw Public Housing Data ##
#############################

# Get Public Housing Data
pub_housing <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/OZ_PUBLIC_HOUSING_DEVELOPMENTS/FeatureServer/0/query?where=STD_ST%3D'MO'&outFields=*&outSR=4326&f=geojson")

save(pub_housing, file = 'data/MO_public_housing.rda')
