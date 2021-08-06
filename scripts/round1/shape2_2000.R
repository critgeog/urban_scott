##--------------------------------------------------------------------------------------
# shape3_1990tracts.R
# 2010 Housing Unit Shapefile
# Author: tjh
# last updated: July 18, 2021
##--------------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(ipumsr)
library(sf)

## counties included in  analysis
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)


##--------------------------------------------------------------------------------------
## Read in NHGIS Housing Unit and YSB Data
##--------------------------------------------------------------------------------------
## 2000 housing units, tract
tract_hu2000 <- read_nhgis("../nhgis0085_csv/nhgis0085_ds146_2000_tract.csv") %>%
  select(-c(2:7,9:30)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>% 
  rename(HU2000_00 = FKI001)

## 2000 Year Structure Built, tract
tract_ysb2000 <- read_nhgis("../nhgis0085_csv/nhgis0085_ds151_2000_tract.csv") %>%
  select(-c(2:7,9:31)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

## join
tracts_2000 <- left_join(tract_hu2000, tract_ysb2000)


# Create total number of 1990 HU counts from 2000 census (sum YSB 1939-1989)
tracts_2000 %>%
  mutate(
    HU90_00 = (GAJ004+GAJ005+GAJ006+GAJ007+GAJ008+GAJ009), # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
  ) %>% 
  select(-c(2,4:12)) -> tracts_hu00

# read in file with 2/3 of tracts to stay and 1/3 to remove (created in buf_2000.R)
remove_tracts_2000 <- read_csv("tracts_2000_remove.csv")

remove_tracts_2000 <- remove_tracts_2000 %>%
  select(GISJOIN, stay)

# join tract 'stay or remove' indicator to housing unit totals
final_00 <- left_join(tracts_hu00,remove_tracts_2000)

# read in 2000 CT spatial, sf
tract_2000_sf <- read_sf("../nhgis0085_shape/nhgis0085_shapefile_tl2000_us_tract_2000/US_tract_2000.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join spatial
final_00_sf <- left_join(tract_2000_sf,final_00)

# 2/3 = Y
fct_count(final_00_sf$stay)

final_00_sf         

# metadata
# GISJOIN = 2000 Census Tract
# HU2000_00 = 2000 HU counts from 2000 census
# HU90_00 = 1990 HU counts from 2000 census [YSB: “built before 1940” + “built 1940-1949” + ,,, + ”built 1980-1989”]
# stay = indicator of whether tract is in 2/3 that stay or 1/3 that get removed
           # Y indicates tract is 2/3 that stays


##--------------------------------------------------------------------------------------
# write file
# st_write(final_00_sf, dsn = "shapes/tracts_2000/shp2_tracts00.shp", layer = "shp2_tracts00.shp", driver = "ESRI Shapefile") #, append = FALSE)



