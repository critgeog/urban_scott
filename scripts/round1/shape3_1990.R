##--------------------------------------------------------------------------------------
# shape3_1990tracts.R
# 1990 Shapefile
# Author: tjh
# last updated: July 16, 2021
##--------------------------------------------------------------------------------------
# load libraries
library(tidyverse)
library(ipumsr)
library(sf)
##--------------------------------------------------------------------------------------

## counties included in analysis
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)


##--------------------------------------------------------------------------------------
## Read in NHGIS Housing Unit and YSB Data
##--------------------------------------------------------------------------------------

# 1990 housing units, tract
tracts_hu1990 <- read_nhgis("../nhgis0084_csv/nhgis0084_ds120_1990_tract.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU1990_90 = ESA001) %>%
  select(GISJOIN,HU1990_90)

# read in file with 1/3 of tracts to stay and 2/3 to remove (created in buf_1990.R)
remove_tracts_1990 <- read_csv("tracts_1990_remove.csv")

remove_tracts_1990 <- remove_tracts_1990 %>%
  select(GISJOIN, stay)

# join tract 'stay or remove' indicator to housing unit totals
final_90 <- left_join(tracts_hu1990,remove_tracts_1990)

# read in 1990 CT spatial, sf
tract_1990_sf <- read_sf("../nhgis0084_shape/nhgis0084_shapefile_tl2000_us_tract_1990/US_tract_1990.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join spatial
final_90_sf <- left_join(tract_1990_sf,final_90)
final_90_sf <- final_90_sf %>%
  select(GISJOIN, HU1990_90,stay,geometry)

final_90_sf
fct_count(final_90_sf$stay)
# 1585/2375 # 2,375 tracts in sf file

# metadata
# GISJOIN = 1990 Census Tract
# HU1990_90 = 1990 HU counts from 1990 census
# stay = indicator of whether tract is in 1/3 that stay or 2/3 that get removed
           # Y indicates tract is 1/3 that stays

##--------------------------------------------------------------------------------------
# write shapefile
# st_write(final_90_sf, dsn = "shapes/tracts_1990/shp3_tracts90.shp", layer = "shp3_tracts90.shp", driver = "ESRI Shapefile") #, append = FALSE)
