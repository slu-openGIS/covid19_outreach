# add zip code column

library(dplyr)
library(readr)
library(stringr)

all <- read_csv("data/clean/outreach_locations.csv")

all <- mutate(all, zip = word(address2, start = -1))

write_csv(all, "data/clean/outreach_locations_zip.csv")
