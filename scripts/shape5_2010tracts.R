##--------------------------------------------------------------------------------------
# shape5_2010tracts.R
# Creates 2010 Census Tract Shapefile
# metadata at bottom
# Author: tjh
# last updated: August 6, 2021
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
## Read in NHGIS data; ysb, 2006-2010 ACS
##--------------------------------------------------------------------------------------

# 2006-2010 ACS Year Structure Built, tract
tract_ysb2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds176_20105_2010_tract.csv") %>%
  select(-c(2:36)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2006_10 = JSDE001)

##--------------------------------------------------------------------------------------
# Sum 2000 and 1990 housing units from YSB columns in 2006-10 ACS
tracts_2010 <- tract_ysb2010 %>%
  mutate(
    HU_2000 = (JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),      # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
    HU_1990 = (JSDE004+JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010), # YSB Built 1939 or earlier + ... + YSB Built 1990-1999
  ) %>%
  select(GISJOIN, HU2006_10, HU_2000, HU_1990)

# read in file with 2/3 of tracts to stay and 1/3 to remove (created in buf_2000.R)
remove_tracts_2010 <- read_csv("csv/tracts_2010_remove.csv")

remove_tracts_2010 <- remove_tracts_2010 %>%
  select(GISJOIN, stay)

# join tract 'stay or remove' indicator to housing unit totals
final_10 <- left_join(tracts_2010,remove_tracts_2010)


##--------------------------------------------------------------------------------------
## read in 2010 housing sf, tract
tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join spatial
final_10_sf <- left_join(tract_2010_sf,final_10)

# 1/2 = Y
fct_count(final_10_sf$stay)

2052/2742
690/2742

# metadata
# GISJOIN = 2010 Census Tract
# HU2006_10 = 2006-10 ACS HU counts
# HU_2000 = 2000 Housing Units from 2006-10 ACS, Year Structure Built
# HU_1990 = 1990 Housing Units from 2006-10 ACS, Year Structure Built
# stay = indicator of whether tract is in 3/4 that stay or 1/4 that get removed
# Y indicates tract is 2/3 that stays


##--------------------------------------------------------------------------------------
# write file
st_write(final_10_sf, dsn = "shapes/tracts_2010/", layer = "shp5_tracts10.shp", driver = "ESRI Shapefile", append = TRUE) 






###(((((((((())))))))))




# join spatial
final_10_sf <- left_join(tract_2010_sf,all_data)

final_10_sf <- final_10_sf %>%
  select(GISJOIN,HU2010, HU2000_10ts, HU1990_10ts, ham_est00, ham_est90,geometry)

final_10_sf

# Metadata
# GISJOIN: 2010 GISJOIN code (from NHGIS)
# HU2010: 2010 HU from 2010 Census
# HU2000_10ts: 2000 HU (NHGIS normalized to 2010 tracts)
# HU1900_10ts: 1990 HU (NHGIS normalized to 2010 tracts)
# ham_est00: Hammer estimates for 2000 using 2010 YSB tract data [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1990-1999”] & 2000 county HU counts data
# ham_est90: Hammer estimates for 1990 using 2010 YSB data & 1990 county HU counts data

##--------------------------------------------------------------------------------------
# write shapefile
# st_write(final_10_sf, dsn = "shapes/tracts_2010/shp1_tracts10.shp", layer = "shp1_tracts10.shp", driver = "ESRI Shapefile", append = FALSE)

