# Full Workflow for Scraping

# Load Credentials (Or Provide Your Own as `here_key` and `google_key`)
source('scripts/creds.R')

# Load the HERE Functions
source('scripts/HERE.R')

# Load an SF Object and Make a Grid to Be Iterated With
load('data/stl_sf.rda')
grid <- batch_bbox(stl, 3821, 102696) # 1KM Grid Squares, Using NAD83 Feet

# Specify the HERE Categories you want
here_categories <- list(
  fueling_station = '700-7600-0116',
  deli = '100-1000-0006',
  convenience = '600-6000-0061',
  grocery = '600-6300-0066',
  drug_store = '600-6400-0000',
  liquor_store = '600-6300-0068',
  check_cashing = '700-7050-0110',
  hair_beauty = '600-6900-0104',
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
    filter(!duplicated(uid)) -> df
  sf <-
    st_as_sf(df, coords = c('position.lng', 'position.lat'), crs = 4326)
  return(sf)
}

# For all of the Categories
for (i in names(here_categories)){
  assign(
    paste0(i, '_sf'),
    scrape_here(here_categories[[i]], grid)
  )
}
save(fueling_station_sf, deli_sf, convenience_sf, grocery_sf, drug_store_sf, liquor_store_sf, check_cashing_sf, hair_beauty_sf, laundromat_sf, file = 'data/POI.rda')
