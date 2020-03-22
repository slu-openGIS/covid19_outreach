# download demographic data

# dependencies
library(dplyr)
library(sf)
library(tidycensus)

# download poverty 
poverty <- get_acs(table = "B17001", geography = "tract", state = 29, county = c(189,510), output = "wide",
                   geometry = TRUE) %>%
  select(GEOID, B17001_001E, B17001_001M, B17001_002E, B17001_002M) %>%
  rename(
    total = B17001_001E, 
    total_moe = B17001_001M, 
    poverty = B17001_002E, 
    poverty_moe = B17001_002M
  ) %>%
  mutate(poverty_rate = poverty/total*1000) %>%
  select(GEOID, poverty_rate)

st_write(poverty, "data/clean/poverty.geojson", delete_dsn = TRUE)
