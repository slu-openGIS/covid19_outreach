# wrangle data to prepare for mapping

# dependencies
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tidyr)

source("scripts/get_coords.R")
source("scripts/jitter_points.R")

# load data
load("data/POI.rda")

# clean-up
rm(convenience_sf, drug_store_sf, fueling_station_sf, grocery_sf, deli_sf)

# bind and subset to only include city and county
poi <- rbind(check_cashing_sf, hair_beauty_sf, liquor_store_sf, laundromat_sf) %>%
  filter(address.county %in% c("St Louis (City)", "St Louis"))

# clean-up
rm(check_cashing_sf, hair_beauty_sf, liquor_store_sf, laundromat_sf)

# clean category and county
poi %>% 
  mutate(category = case_when(
    category == "600-6300-0068" ~ "Liquor Store",
    category == "600-6950-0000" ~ "Hair and Beauty",
    category == "700-7050-0110" ~ "Payday Loan"
  )) %>%
  mutate(address.county = ifelse(address.county == "St Louis (City)", "City of St Louis", address.county)) %>%
  rename(county = address.county) %>%
  select(title, category, county, address.label) -> poi

# remove dental and medical
poi %>%
  filter(str_detect(string = title, pattern = "MD") == FALSE) %>%
  filter(str_detect(string = title, pattern = "DMDPC") == FALSE) %>%
  filter(str_detect(string = title, pattern = "DDS") == FALSE)  %>%
  filter(str_detect(string = title, pattern = "DMD") == FALSE) %>%
  filter(str_detect(string = title, pattern = "Dental") == FALSE) %>%
  filter(str_detect(string = title, pattern = "M.S.") == FALSE) -> poi

# tidy address
poi %>%
  mutate(address.label = case_when(
    address.label == "Nouveau, A Boutique Medspa, 320 S Kirkwood Rd, St Louis, MO 63122, United States" ~ "Nouveau, 320 S Kirkwood Rd, St Louis, MO 63122, United States",
    address.label == "Thirteenth and Washington, Salon & Barber Shop, 1300 Washington Ave, St Louis, MO 63103, United States" ~ "Thirteenth and Washington Salon and Barber Shop, 1300 Washington Ave, St Louis, MO 63103, United States",
    address.label == "Queens Royal Touch Massage, Barber & Beauty, 1451 Chambers Rd, St Louis, MO 63135, United States" ~ "Queens Royal Touch Massage Barber and Beauty, 1451 Chambers Rd, St Louis, MO 63135, United States",
    address.label == "CW's Heal Thyself Herbs, Etc, Old Halls Ferry Rd, St Louis, MO 63136, United States" ~ "CW's Heal Thyself Herbs Etc, Old Halls Ferry Rd, St Louis, MO 63136, United States",
    address.label == "1860 Saloon, Game Room, & Hardshell Café, 1860 S 9th St, St Louis, MO 63104, United States" ~ "1860 Saloon Game Room and Hardshell Café, 1860 S 9th St, St Louis, MO 63104, United States",
    TRUE ~ as.character(address.label)  
  )) %>%
  separate(col = address.label, 
           into = c("name", "address", "city", "state_zip", "country"), 
           sep = ",") %>%
  select(-name, -country) %>%
  mutate(address2 = paste0(city, ", ", state_zip)) %>%
  select(-city, -state_zip) %>%
  mutate(address_full = paste0(address, ", ", address2)) %>%
  mutate(
    address = str_trim(address, side = "both"),
    address2 = str_trim(address2, side = "both"),
    address_full = str_trim(address_full, side = "both")
  ) %>%
  mutate(zip = as.numeric(word(address2, -1))) %>%
  select(title, category, county, address, address2, zip, address_full) %>%
  arrange(address_full) -> poi

# subset to priority zips
poi <- filter(poi, zip %in% c(63121, 63137, 63138, 63034))

# remove duplicates
poi <- distinct(poi, title, address, .keep_all = TRUE)

# convert to tibble
poi <- get_coords(poi, names = c("x","y"), crs = 4326)
st_geometry(poi) <- NULL

# jitter and re-project
poi %>% 
  jitter_points(factor = 60) %>%
  st_as_sf(coords = c("x","y"), crs = 4326) -> poi

# subset
beauty <- filter(poi, category == "Hair and Beauty") %>%
  arrange(title) %>%
  select(-address_full)

liquor <- filter(poi, category == "Liquor Store") %>%
  arrange(title) %>%
  select(-address_full)

payday <- filter(poi, category == "Payday Loan") %>%
  arrange(title) %>%
  select(-address_full)

# clean-up 
rm(poi, get_coords, jitter_points)

# write data
st_write(beauty, "data/clean/beauty_north_county.geojson", delete_dsn = TRUE)
st_write(liquor, "data/clean/liquor_north_county.geojson", delete_dsn = TRUE)
st_write(payday, "data/clean/payday_north_county.geojson", delete_dsn = TRUE)

all <- rbind(beauty, liquor, payday)
st_geometry(all) <- NULL
all <- arrange(all, title)

write_csv(all, "data/clean/all_locations_north_county.csv")
