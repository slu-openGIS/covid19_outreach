
library(dplyr)
library(censusxy)
library(readxl)
library(sf)

extra <- read_excel("data/extra_east_stl.xlsx") 

extra %>%
  rename(
    id = ID,
    name = NAME,
    street = `STREET ADDRESS`,
    city = CITY,
    zip = ZIP
  ) %>%
  mutate(state = "IL") -> extra

extra <- cxy_geocode(extra, id = "id", street = "street", city = "city", zip = "zip", state = "state") 

extra <- st_as_sf(extra, coords = c("cxy_lon", "cxy_lat"), crs = 4326)
  
extra %>%
  select(-id) %>%
  rename(
    title = name,
    address = street
  ) %>%
  mutate(
    category = "Miscellaneous",
    county = "St. Clair",
    address2 = paste0(city, " IL, ", zip)
  ) %>%
  mutate(address_full = paste0(address, ", ", address2)) %>%
  select(-city, -state) %>%
  select(title, category, county, address, address2, zip, address_full) -> extra_out

st_write(extra_out, "data/clean2/east_stl_extra.geojson", delete_dsn = TRUE)
