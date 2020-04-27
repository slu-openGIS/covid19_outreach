# data set construction

# =============================================================================

# dependencies
library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(tigris)

source("scripts/get_coords.R")
source("scripts/jitter_points.R")

# =============================================================================

# load data
load("data/POI.rda")
load("data/ZIP_Specific.rda")
load("data/MOIL_public_housing.rda")

# =============================================================================

# bind 
poi <- rbind(auto_parts_sf, auto_parts_sf_zip, check_cashing_sf, check_cashing_sf_zip, 
             convenience_sf, convenience_sf_zip, # deli_sf, deli_sf_zip not being used currently
             drug_store_sf, drug_store_sf_zip, fueling_station_sf, fueling_station_sf_zip,
             grocery_sf, grocery_sf_zip, hair_beauty_sf, # no east st louis hair and beauty
             kindergarten_childcare_sf, kindergarten_childcare_sf_zip, laundromat_sf, laundromat_sf_zip,
             liquor_store_sf, liquor_store_sf_zip)

# clean-up
rm(auto_parts_sf, auto_parts_sf_zip, check_cashing_sf, check_cashing_sf_zip,
   convenience_sf, convenience_sf_zip, deli_sf, deli_sf_zip,
   drug_store_sf, drug_store_sf_zip, fueling_station_sf, fueling_station_sf_zip,
   grocery_sf, grocery_sf_zip, hair_beauty_sf, # no east st louis hair and beauty
   kindergarten_childcare_sf, kindergarten_childcare_sf_zip, laundromat_sf, laundromat_sf_zip, 
   liquor_store_sf, liquor_store_sf_zip)

# =============================================================================

# clean category and county
poi %>% 
  mutate(category = case_when(
    category == "700-7850-0123" ~ "Auto Parts",
    category == "700-7400-0286" ~ "Childcare",
    category == "600-6000-0061" ~ "Convenience",
    category == "700-7600-0116" ~ "Gas Station",
    category == "600-6300-0066" ~ "Grocery",
    category == "600-6950-0000" ~ "Hair and Beauty",
    category == "700-7400-0137" ~ "Laundry",
    category == "600-6300-0068" ~ "Liquor Store",
    category == "700-7050-0110" ~ "Payday Loan",
    category == "600-6400-0000" ~ "Pharmacy"
  )) %>%
  mutate(address.county = ifelse(address.county == "St Louis (City)", "City of St Louis", address.county)) %>%
  rename(county = address.county) %>%
  select(title, category, county, address.label) -> poi

# =============================================================================

# remove dental and medical
poi %>%
  filter(str_detect(string = title, pattern = "Russell Imboden") == FALSE) %>%
  filter(str_detect(string = title, pattern = "MD") == FALSE) %>%
  filter(str_detect(string = title, pattern = "DMDPC") == FALSE) %>%
  filter(str_detect(string = title, pattern = "DDS") == FALSE)  %>%
  filter(str_detect(string = title, pattern = "DMD") == FALSE) %>%
  filter(str_detect(string = title, pattern = "Dental") == FALSE) %>%
  filter(str_detect(string = title, pattern = "M.S.") == FALSE) -> poi

# =============================================================================

# tidy specific addresses
## tidy address
poi %>%
  mutate(address.label = case_when(
    address.label == "Nouveau, A Boutique Medspa, 320 S Kirkwood Rd, St Louis, MO 63122, United States" ~ "Nouveau, 320 S Kirkwood Rd, St Louis, MO 63122, United States",
    address.label == "Thirteenth and Washington, Salon & Barber Shop, 1300 Washington Ave, St Louis, MO 63103, United States" ~ "Thirteenth and Washington Salon and Barber Shop, 1300 Washington Ave, St Louis, MO 63103, United States",
    address.label == "Queens Royal Touch Massage, Barber & Beauty, 1451 Chambers Rd, St Louis, MO 63135, United States" ~ "Queens Royal Touch Massage Barber and Beauty, 1451 Chambers Rd, St Louis, MO 63135, United States",
    address.label == "CW's Heal Thyself Herbs, Etc, Old Halls Ferry Rd, St Louis, MO 63136, United States" ~ "CW's Heal Thyself Herbs Etc, Old Halls Ferry Rd, St Louis, MO 63136, United States",
    address.label == "1860 Saloon, Game Room, & Hardshell Café, 1860 S 9th St, St Louis, MO 63104, United States" ~ "1860 Saloon Game Room and Hardshell Café, 1860 S 9th St, St Louis, MO 63104, United States",
    address.label == "SHC, Chesterfield, MO 63005, United States" ~ "SHC, , Chesterfield, MO 63005, United States",
    address.label == "Hudson News, St Louis, MO 63145, United States" ~ "Hudson News, , St Louis, MO 63145, United States",
    address.label == "Closed Door Pharmacy, St Louis, MO 63128, United States" ~ "Closed Door Pharmacy, , St Louis, MO 63128, United States",
    address.label == "Esso Express, Chesterfield, MO 63005, United States" ~ "Esso Express, , Chesterfield, MO 63005, United States",
    address.label == "Walmart, St Louis, MO 63122, United States" ~ "Walmart, , St Louis, MO 63122, United States",
    address.label == "Chocolate, Chocolate, Chocolate, 112 N Kirkwood Rd, St Louis, MO 63122, United States" ~ "Chocolate, 112 N Kirkwood Rd, St Louis, MO 63122, United States",
    address.label == "Saint Louis Halal Meat, Grocery & Foods - Baba's Restaurant, 10276 Page Ave, St Louis, MO 63132, United States" ~ "Saint Louis Halal Meat Grocery & Foods, 10276 Page Ave, St Louis, MO 63132, United States",
    address.label == "Lunds Engines, Machines & Auto Repair, 206 Clearview Dr, St Charles, MO 63303, United States" ~ "Lunds Engines Machines & Auto Repair, 206 Clearview Dr, St Charles, MO 63303, United States",
    address.label == "Hairstylist, Sarah Crowe, 833 S Main St, St Charles, MO 63301, United States" ~ "Sarah Crowe Hairstylist, 833 S Main St, St Charles, MO 63301, United States",
    address.label == "Lavender Spa, St. Charles, 2401 Sibley St, St Charles, MO 63301, United States" ~ "Lavender Spa, 2401 Sibley St, St Charles, MO 63301, United States",
    address.label == "Earl, Lynette M Licensed Home Child Care, 3672 Lemay Woods Dr, St Louis, MO 63129, United States" ~ "Lynette M Earl Licensed Home Child Care, 3672 Lemay Woods Dr, St Louis, MO 63129, United States",
    address.label == "Listening for Learning, LC, 1658 Old Baxter Rd, Chesterfield, MO 63017, United States" ~ "Listening for Learning, 1658 Old Baxter Rd, Chesterfield, MO 63017, United States",
    address.label == "West County Day School, O'Fallon, 9983 Winghaven Blvd, O'Fallon, MO 63368, United States" ~ "West County Day School, 9983 Winghaven Blvd, O'Fallon, MO 63368, United States",
    address.label == "College Nannies, Sitters and Tutors, 5988 Mid Rivers Mall Dr, St Charles, MO 63304, United States" ~ "College Nannies Sitters and Tutors, 5988 Mid Rivers Mall Dr, St Charles, MO 63304, United States",
    address.label == "F.U.M.C., Mother's Day Out & Preschool, 801 1st Capitol Dr, St Charles, MO 63301, United States" ~ "F.U.M.C. Mother's Day Out & Preschool, 801 1st Capitol Dr, St Charles, MO 63301, United States",
    address.label == "Classic Coin Laundry - St. Louis, Mo, 7200 Balson Ave, St Louis, MO 63130, United States" ~ "Classic Coin Laundry, 7200 Balson Ave, St Louis, MO 63130, United States",
    address.label == "Norwood Title Lenders, St Louis, MO 63113, United States" ~ "Norwood Title Lenders, , St Louis, MO 63113, United States",
    address.label == "Walgreens, O'Fallon, MO 63368, United States" ~ "Walgreens, , O'Fallon, MO 63368, United States",
    address.label == "Chevron Nacogdoches, St Charles, MO 63304, United States" ~ "Chevron Nacogdoches, , St Charles, MO 63304, United States",
    address.label == "Save A Lot, Pacific, MO 63069, United States" ~ "Save A Lot, , Pacific, MO 63069, United States",
    address.label == "Ruler, Granite City, IL 62040, United States" ~ "Ruler, , Granite City, IL 62040, United States",
    address.label == "Mistress of Makeup Productions, Ballwin, MO 63011, United States" ~ "Mistress of Makeup Productions, , Ballwin, MO 63011, United States",
    address.label == "Eye Candy Studio, Ballwin, MO 63011, United States" ~ "Eye Candy Studio, , Ballwin, MO 63011, United States",
    address.label == "Rachel Mounce, St Louis, MO 63144, United States" ~ "Rachel Mounce, , St Louis, MO 63144, United States",
    address.label == "Ravishing Bridal, St Louis, MO 63110, United States" ~ "Ravishing Bridal, , St Louis, MO 63110, United States",
    address.label == "Regis Salon, Chesterfield, MO 63017, United States" ~ "Regis Salon, , Chesterfield, MO 63017, United States",
    address.label == "A True Spa, Chesterfield, MO 63005, United States" ~ "A True Spa, , Chesterfield, MO 63005, United States",
    address.label == "Sola Nails Spa, Chesterfield, MO 63005, United States" ~ "Sola Nails Spa, , Chesterfield, MO 63005, United States",
    address.label == "Whip'd Lash, Chesterfield, MO 63005, United States" ~ "Whip'd Lash, , Chesterfield, MO 63005, United States",
    address.label == "Doodlebug Home Daycare, St Louis, MO 63119, United States" ~ "Doodlebug Home Daycare, , St Louis, MO 63119, United States",
    address.label == "Dry Cleaner, Arnold, MO 63010, United States" ~ "Dry Cleaner, , Arnold, MO 63010, United States",
    address.label == "Central West End Station, St Louis, MO 63110, United States" ~ "Central West End Station, , St Louis, MO 63110, United States",
    address.label == "Harvester Cleaners, St Charles, MO 63303, United States" ~ "Harvester Cleaners, , St Charles, MO 63303, United States",
    address.label == "A Plus Cleaners, O'Fallon, IL 62269, United States" ~ "A Plus Cleaners, , O'Fallon, IL 62269, United States",
    address.label == "A+ Cleaners, O'Fallon, IL 62269, United States" ~ "A+ Cleaners, , O'Fallon, IL 62269, United States",
    address.label == "Plaza Wine & Liquor Mart, O'Fallon, IL 62269, United States" ~ "Plaza Wine & Liquor Mart, , O'Fallon, IL 62269, United States",
    TRUE ~ as.character(address.label)  
  )) -> poi

## split addresses
poi %>%
  separate(col = address.label, 
           into = c("name", "address", "city", "state_zip", "country"), 
           sep = ",") %>%
  select(-name, -country) %>%
  mutate(address2 = paste0(city, ", ", state_zip)) %>%
  select(-city, -state_zip) %>%
  mutate(address_full = paste0(address, ", ", address2)) %>%
  mutate(
    address = str_squish(address),
    address2 = str_squish(address2),
    address_full = str_squish(address_full)
  ) %>%
  select(title, category, county, address, address2, address_full) %>%
  arrange(address_full) -> poi

# =============================================================================

# remove duplicates
poi <- distinct(poi, title, category, county, address, address2, .keep_all = TRUE)

# =============================================================================

# wrangle specific categories
## grocery
### initial tidy
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
    title == "Fresh Thyme #406 S" ~ "Fresh Thyme Farmers Market",
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

### add Sam's Club and Costco
poi %>%
  filter(title %in% c("Costco", "Sam's Club")) %>%
  distinct(address_full, .keep_all = TRUE) %>%
  mutate(category = "Grocery, Chain") %>%
  rbind(grocery, .) -> grocery

## pharmacy
### initial tidy
poi %>%
  filter(category == "Pharmacy") %>%
  filter(title %in% c("St. Louis College of Pharmacy", "Schnuck's Pharmacy",
                      "Schnucks", "Schnucks Pharmacy", "Dierbergs Pharmacy", "Dierbergs") == FALSE) %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) %>%
  mutate(category = ifelse(title %in% c("Walgreens", "Medicine Shoppe"), 
                           "Pharmacy, Chain", "Pharmacy, Local")) -> pharmacy

### add CVS to pharmacy
poi %>%
  filter(title == "CVS/pharmacy") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  mutate(
    title = "CVS",
    category = "Pharmacy, Chain"
  ) %>%
  rbind(pharmacy, .) -> pharmacy

## tidy liquor stores
poi %>%
  filter(category == "Liquor Store") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) -> liquor

## tidy gas stations
poi %>%
  filter(category == "Gas Station") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) -> gas

## tidy convenience stores
poi %>%
  filter(category == "Convenience") %>%
  distinct(address_full, .keep_all = TRUE) %>%
  filter(address_full %in% grocery$address_full == FALSE) %>%
  filter(address_full %in% gas$address_full == FALSE) %>%
  filter(address_full %in% pharmacy$address_full == FALSE)-> convenience

## subset all other categories
remaining_poi <- filter(poi, category %in% c("Auto Parts", "Childcare", "Hair and Beauty",
                                             "Laundry", "Payday Loan"))

## combine
poi <- rbind(grocery, pharmacy, liquor, gas, convenience, remaining_poi)

## clean-up
rm(grocery, pharmacy, liquor, gas, convenience, remaining_poi)

## final duplicate removal
poi <- distinct(poi, title, address, address2, county, .keep_all = TRUE)

# =============================================================================

# add public housing
## create vector of metro counties
st_louis <- c("17005", "17013", "17027", "17083", "17117", "17119", "17133", "17163", 
              "29071", "29099", "29113", "29183", "29189", "29219", "29510")

## download county geometry
counties <- counties(state = c(17, 29), class = "sf") %>%
  filter(GEOID %in% st_louis) %>%
  mutate(NAME = ifelse(GEOID == "29510", "City of St. Louis", NAME)) %>%
  select(NAME) %>%
  st_transform(crs = 26915)

## subset and spatial join
pub_housing %>%
  select(FORMAL_PARTICIPANT_NAME, PROJECT_NAME, TOTAL_DWELLING_UNITS, STD_ADDR, STD_CITY, STD_ST, STD_ZIP5) %>%
  rename(
    authority = FORMAL_PARTICIPANT_NAME,
    title = PROJECT_NAME,
    units = TOTAL_DWELLING_UNITS,
    address = STD_ADDR
  ) %>%
  mutate(title = str_to_title(title)) %>%
  st_transform(crs = 26915) %>%
  st_intersection(., counties) %>%
  rename(county = NAME) %>%
  st_transform(crs = 4326) -> housing

## tidy
housing %>%
  mutate(authority = case_when(
    authority == "Housing Authority of the County of Bond" ~ "Bond County Housing Authority",
    authority == "The Housing Authority of City of East St. Louis" ~ "East St. Louis Housing Authority",
    authority == "Housing Authority of the City of Olivette" ~ "Olivette Housing Authority",
    authority == "Housing Authority of the City of Pagedale" ~ "Pagedale Housing Authority",
    TRUE ~ as.character(authority)
  )) %>%
  mutate(
    authority = as.character(authority),
    title = as.character(title),
    address = as.character(address),
    STD_CITY = as.character(STD_CITY),
    STD_ST = as.character(STD_ST),
    STD_ZIP5 = as.character(STD_ZIP5)
  ) %>%
  mutate(title = str_replace(title, pattern = "Iii", replacement = "III")) %>%
  mutate(title = str_replace(title, pattern = "Ii", replacement = "II")) %>%
  mutate(category = "Public Housing") %>%
  mutate(
    address2 = paste0(STD_CITY, ", ", STD_ST, " ", STD_ZIP5),
    address_full = paste(address, address2)
  ) %>%
  select(title, category, county, address, address2, address_full, authority, units) -> housing

## clean-up
rm(counties, pub_housing, st_louis)

# =============================================================================

# add housing to poi

## add additional variables
poi <- mutate(poi,
              authority = NA_character_,
              units = NA)

## bind
poi <- rbind(poi, housing)

## arrange
poi <- arrange(poi, category, title)

## clean-up
rm(housing)

# =============================================================================

# add zip
poi %>%
  mutate(zip = as.numeric(word(address2, -1))) %>%
  select(title, category, county, address, address2, zip, address_full, authority, units) -> poi

# =============================================================================

# jitter overlapping points
## convert to tibble
poi <- get_coords(poi, names = c("x","y"), crs = 4326)
st_geometry(poi) <- NULL

## jitter and re-project
poi %>% 
  jitter_points(factor = 60) %>%
  st_as_sf(coords = c("x","y"), crs = 4326) -> poi

# =============================================================================

# subset and write data
## auto parts
poi %>%
  filter(category == "Auto Parts") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/auto_parts.geojson", delete_dsn = TRUE)

## childcare
poi %>%
  filter(category == "Childcare") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/childcare.geojson", delete_dsn = TRUE)

## convenience
poi %>%
  filter(category == "Convenience") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/convenience.geojson", delete_dsn = TRUE)

## gas stations
poi %>%
  filter(category == "Gas Station") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/gas_station.geojson", delete_dsn = TRUE)

## grocery
poi %>%
  filter(category %in% c("Grocery, Chain", "Grocery, Local")) %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/grocery.geojson", delete_dsn = TRUE)

## hair and beauty
poi %>%
  filter(category == "Hair and Beauty") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/hair_and_beauty.geojson", delete_dsn = TRUE)

## laundry
poi %>%
  filter(category == "Laundry") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/laundromats.geojson", delete_dsn = TRUE)

## liquor store
poi %>%
  filter(category == "Liquor Store") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/liquor_store.geojson", delete_dsn = TRUE)

## payday loan
poi %>%
  filter(category == "Payday Loan") %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/payday_loan.geojson", delete_dsn = TRUE)

## pharmacy
poi %>%
  filter(category %in% c("Pharmacy, Chain", "Pharmacy, Local")) %>%
  select(-authority, -units) %>%
  st_write(., "data/clean2/pharmacy.geojson", delete_dsn = TRUE)

## auto parts
poi %>%
  filter(category == "Public Housing") %>%
  st_write(., "data/clean2/public_housing.geojson", delete_dsn = TRUE)
