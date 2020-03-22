# wrangle data to prepare for mapping

# dependencies
library(dplyr)
library(sf)
library(stringr)
library(tidyr)

# load data
load("data/POI.rda")

# bind and subset to only include city and county
poi <- rbind(convenience_sf, drug_store_sf, fueling_station_sf, grocery_sf) %>%
  filter(address.county %in% c("St Louis (City)", "St Louis"))

# clean-up
rm(convenience_sf, drug_store_sf, fueling_station_sf, grocery_sf, deli_sf)

# clean category and county
poi %>% 
  mutate(category = case_when(
    category == "600-6000-0061" ~ "Convenience",
    category == "600-6400-0000" ~ "Pharmacy",
    category == "700-7600-0116" ~ "Gas Station",
    category == "600-6300-0066" ~ "Grocery"
  )) %>%
  mutate(address.county = ifelse(address.county == "St Louis (City)", "City of St Louis", address.county)) %>%
  rename(county = address.county) %>%
  select(title, category, county, address.label) -> poi

# tidy address
poi %>%
  mutate(address.label = case_when(
    address.label == "SHC, Chesterfield, MO 63005, United States" ~ "SHC, , Chesterfield, MO 63005, United States",
    address.label == "Hudson News, St Louis, MO 63145, United States" ~ "Hudson News, , St Louis, MO 63145, United States",
    address.label == "Closed Door Pharmacy, St Louis, MO 63128, United States" ~ "Closed Door Pharmacy, , St Louis, MO 63128, United States",
    address.label == "Esso Express, Chesterfield, MO 63005, United States" ~ "Esso Express, , Chesterfield, MO 63005, United States",
    address.label == "Walmart, St Louis, MO 63122, United States" ~ "Walmart, , St Louis, MO 63122, United States",
    address.label == "Chocolate, Chocolate, Chocolate, 112 N Kirkwood Rd, St Louis, MO 63122, United States" ~ "Chocolate, 112 N Kirkwood Rd, St Louis, MO 63122, United States",
    address.label == "Saint Louis Halal Meat, Grocery & Foods - Baba's Restaurant, 10276 Page Ave, St Louis, MO 63132, United States" ~ "Saint Louis Halal Meat Grocery & Foods, 10276 Page Ave, St Louis, MO 63132, United States",
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
  select(title, category, county, address, address2, address_full) %>%
  arrange(address_full) -> poi

# subset grocery stores
poi %>%
  filter(category == "Grocery") %>%
  mutate(title = case_when(
    title == "Dierbergs Markets" ~ "Dierbergs Market",
    title == "Dierbergs" ~ "Dierbergs Market",
    title == "Family Dollar Stores" ~ "Family Dollar",
    title == "Family Fresh Produce" ~ "Family Dollar",
    title == "SAVE-A-LOT" ~ "Save-A-Lot",
    title == "Save A Lot" ~ "Save-A-Lot",
    title == "Schnucks - Butler Hill" ~ "Schnucks",
    title == "Schnucks Lindbergh" ~ "Schnucks",
    title == "Schnucks Markets" ~ "Schnucks",
    title == "Schnucks Supermarket" ~ "Schnucks",
    title == "Schunucks Supermarkets Central Stores" ~ "Schnucks",
    title == "Schunucks Supermarkets West Stores" ~ "Schnucks",
    title == "Schnuck's" ~ "Schnucks",
    title == "Shop'n Save" ~ "Shop 'n Save",
    title == "Straub's Market" ~ "Straub's",
    title == "Kmart-Florissant" ~ "Kmart",
    title == "Schnuck Markets" ~ "Schnucks",
    title == "Schnuck's #12332" ~ "Schnucks",
    title == "ALDI 41012" ~ "ALDI",
    TRUE ~ as.character(title)
  )) %>%
  filter(title %in% c("Kroger", "Schnucks Pharmacy", "Walmart Bakery", "CVS/pharmacy", "770 North Hwy 67",
                      "Brentwood Plaza", "Dierbergs Pharmacy", "Schnuck's Pharmacy",
                      "Scnuck's Market Flowers & Gifts", "Schlafly Bottleworks", "Merrill Lynch") == FALSE) %>%
  filter(address_full %in% c(",  St Louis,  MO 63122") == FALSE) %>%
  distinct(address_full, .keep_all = TRUE) %>%
  mutate(category = ifelse(title %in% c("Schnucks", "Family Dollar", "ALDI", "Dierbergs Market", 
          "Save-A-Lot", "Walmart", "Target", "Shop 'n Save", "Straub's", "Trader Joe's",
          "IGA", "Fresh Thyme Farmers Market", "Big Lots", "Whole Foods Market", "Ruler",
          "Kmart", "Fields Foods"),
          "Grocery, Chain", "Grocery, Local")) -> grocery

# add Sam's Club and Costco
poi %>%
  filter(title %in% c("Costco", "Sam's Club")) %>%
  distinct(address_full, .keep_all = TRUE) %>%
  mutate(category = "Grocery, Chain") %>%
  rbind(grocery, .) -> grocery

# subset pharmacy
poi %>%
  filter(category == "Pharmacy") %>%
  filter(title %in% c("St. Louis College of Pharmacy", "Schnuck's Pharmacy",
                      "Schnucks", "Schnucks Pharmacy", "Dierbergs Pharmacy", "Dierbergs") == FALSE) %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) %>%
  mutate(category = ifelse(title %in% c("Walgreens", "Medicine Shoppe"), 
                           "Pharmacy, Chain", "Pharmacy, Local")) -> pharmacy

# add CVS to pharmacy
poi %>%
  filter(title == "CVS/pharmacy") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  mutate(
    title = "CVS",
    category = "Pharmacy, Chain"
  ) %>%
  rbind(pharmacy, .) -> pharmacy

# subset gas stations
poi %>%
  filter(category == "Gas Station") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) -> gas

# subset Convenience stores
poi %>%
  filter(category == "Convenience") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) %>%
  filter(address_full %in% gas$address_full == FALSE) %>%
  filter(address_full %in% pharmacy$address_full == FALSE)-> convenience

# clean-up 
rm(poi)

# tidy tables
convenience <- select(convenience, -address_full)
gas <- select(gas, -address_full)
grocery <- select(grocery, -address_full)
pharmacy <- select(pharmacy, -address_full)

# write data
st_write(convenience, "data/clean/convenience.geojson", delete_dsn = TRUE)
st_write(gas, "data/clean/gas.geojson", delete_dsn = TRUE)
st_write(grocery, "data/clean/grocery.geojson", delete_dsn = TRUE)
st_write(pharmacy, "data/clean/pharmacy.geojson", delete_dsn = TRUE)

