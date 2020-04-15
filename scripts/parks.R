# clean parks

library(sf)

# load data
zips <- st_read(here("data", "zips.geojson"), crs = 4326,
                   stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

parks <- st_read(here("data", "parks", "parks.shp"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915)

# subset to priority zips
zips <- filter(zips, as.numeric(GEOID10) %in% c(63106, 63107, 63113, 63115, 63136, 
                                                63112, 63120, 63103, 63108, 
                                                63147, 63110, 63102, 63101, 
                                                63133, 63130))

intersect <- st_intersection(parks, zips)
intersect %>%
  group_by(TEXT_) %>%
  summarise() %>%
  st_collection_extract(., "POLYGON") -> intersect

st_geometry(intersect) <- NULL

parks <- filter(parks, TEXT_ %in% intersect$TEXT_) %>%
  select(TEXT_) %>%
  rename(name = TEXT_)

parks <- st_transform(parks, crs = 4326)

st_write(parks, "data/clean/parks_city.geojson", delete_dsn = TRUE)

rm(intersect, parks, zips)
