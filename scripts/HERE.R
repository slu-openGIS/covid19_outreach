# Scrape HERE Places API for a Given Bounding Box

library(httr)
library(magrittr)
library(sf)
library(dplyr)

############################
##### Helper Functions #####
############################

bbox_to_string <- function(bbox){
  if(class(bbox) != 'bbox'){stop("'bbox' must be of class bbox")}
  paste(bbox, collapse = ',')
}

# Buffer Lat/Lon (Buffer must match proj unit)
coord_buffer <- function(point, buffer, proj){
  og_crs <- st_crs(point)
  st_transform(point, proj) %>%
    st_buffer(buffer) %>%
    st_transform(og_crs)
}

# Batch BBox 3280.84ft = 1km
batch_bbox <- function(sf, cellsize, proj){
  og_crs <- st_crs(sf)
  # Make Grid
  st_transform(sf, proj) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_transform(og_crs) -> grid
  # Keep Only Intersections
  grid[st_intersects(grid, sf, sparse = FALSE)]
  
}

##########################
##### Main Functions #####
##########################

# Get Available Categories Given a bbox
# @param bbox
# @param key
#
here_categories <- function(bbox, key = here_key){
  url <- 'https://places.sit.ls.hereapi.com/places/v1/categories/places'
  httr::GET(url,
      query = list(
        apiKey = key,
        `in` = bbox_to_string(bbox)
      )
  ) %>%
  httr::content()
}

# Get Places by Category for a Given bbox (/browse endpoint)
# https://developer.here.com/documentation/geocoding-search-api/dev_guide/topics/endpoint-browse-brief.html
# 
# @param categories Character vector containing categories of interest
# @param sf SF class object for your area of interest, must be lat/lon (unprojected)
# @param key Api Key for HERE
# @param raw Logical, Should the raw API response be returned?
#
here_browse <- function(category, sf, key = here_key, raw = FALSE){
  
  # Get sf params
  centroid <- st_centroid(sf)
  
  # Make API Call to HERE
  url <- 'https://browse.search.hereapi.com/v1/browse'
  httr::GET(url,
            query = list(
              at = paste(unlist(centroid)[2],unlist(centroid)[1], sep = ','),
              categories = paste(category, collapse = ','), # to accept mulitple categories
              apiKey = key,
              limit = 100
            )
  ) %>%
  httr::content() ->
    response
  
  if(raw){
    return(response)
  }
  
  # Parse the Data
  parsed <- lapply(response$items, function(x){
    data.frame(stringsAsFactors = FALSE,
               title = x$title,
               uid = x$id,
               position = x$position,
               category = category
               )
  })
  
  # Bind into one Data.frame
  parsed <- dplyr::bind_rows(parsed)
  
  return(parsed)
}

# Get Places by Text Search for a Given bbox (/discover endpoint)
# https://developer.here.com/documentation/geocoding-search-api/dev_guide/topics/endpoint-discover-brief.html
#
# @param search String with search request
# @param bbox bbox class object containing search area
# @param key
# @param raw Logical, Should the raw API response be returned?
#
here_discover <- function(search, bbox, key = here_key, raw = FALSE){
  
  # Make API Call to HERE
  url <- 'https://discover.search.hereapi.com/v1/discover'
  httr::GET(url,
            query = list(
              `in`=paste0('bbox:',bbox_to_string(bbox)),
              q=search,
              apiKey=key,
              limit=100
            )
  ) %>%
  content() ->
    response

  if(raw){
    return(response)
  }

  # Parse the Data
  parsed <- lapply(response$items, function(x){
    data.frame(stringsAsFactors = FALSE,
               title = x$title,
               uid = x$id,
               position = x$position,
               category = search
    )
  })
  
  # Bind into one Data.frame
  parsed <- dplyr::bind_rows(parsed)
  
  return(parsed)
}


# Testing
#b = here_browse('100-1100', stl)
#d = here_discover('Coffee', st_bbox(stl))
