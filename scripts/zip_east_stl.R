library(dplyr)
library(tigris)

# download zips
zip <- zctas(year = 2018, class = "sf") %>%
  st_transform(crs = 4326)

# subset
zip <- filter(zip, GEOID10 %in% c("62201", "62203", "62204", "62205", "62206", "62207", "62059"))

# write
st_write(zip, "data/zips_east_stl.geojson")
