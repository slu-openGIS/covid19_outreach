# Extract Coordinates from sf Object
get_coords <- function(.data, names = c("x","y"), crs = 4269){
  
  # global bindings
  geometry = NULL
  
  # ensure .data is an sf object
  if ("sf" %in% class(.data) == FALSE){
    stop("An sf object must be used with 'gw_get_coords()'.")
  }
  
  # store coordinates
  coords <- sf::st_crs(.data)$epsg
  
  if (is.na(coords) == TRUE){
    coords <- 0
  }
  
  # reproject
  if (coords != crs){
    .data <- sf::st_transform(.data, crs = crs)
  }
  
  # create coordinate columns
  ret <- do.call(rbind,sf::st_geometry(.data))
  ret <- dplyr::as_tibble(ret, .name_repair = "unique")
  
  # ensure two columns are returned
  stopifnot(length(names) == ncol(ret))
  
  # name columns with coordinate data
  ret <- stats::setNames(ret, names)
  
  # combine coordinate data with source data
  dplyr::bind_cols(.data, ret) %>%
    dplyr::select(-geometry, dplyr::everything()) -> out
  
  # return output
  return(out)
  
}