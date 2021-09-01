##--------------------------------------------------------------------------------------
# shape4_2010bgroups.R
# 2010 Housing Unit Shapefile
# Author: tjh
# last updated: July 17, 2021
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
## Read in NHGIS data; 2015-19 Housing Unit and Year Structure Built data
bg_2015_19  <- read_nhgis("../nhgis0093_csv/nhgis0093_ds244_20195_2019_blck_grp.csv") %>%
  select(-c(2:43)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2015_19 = AL0DE001) # rename 2015-19 housing unit variable 

## create 2010, 2000, and 1990 Housing Unit Totals using YSB data
bg_2015_19 <- bg_2015_19 %>%
  mutate(
    HU2010 = (AL0DE004 + AL0DE005 + AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011), # housing units built before 1940...housing units built 2000-2009
    HU2000 = (AL0DE005 + AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011),# housing units built before 1940...housing units built 1990-1999
    HU1990 = (AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011)) %>% # housing units built before 1940..housing units built 1980-1989
  select(GISJOIN, HU1990, HU2000, HU2010, HU2015_19)

# AL0DE001:    Total
# AL0DE002:    Built 2014 or later
# AL0DE003:    Built 2010 to 2013
# AL0DE004:    Built 2000 to 2009
# AL0DE005:    Built 1990 to 1999
# AL0DE006:    Built 1980 to 1989
# AL0DE007:    Built 1970 to 1979
# AL0DE008:    Built 1960 to 1969
# AL0DE009:    Built 1950 to 1959
# AL0DE010:    Built 1940 to 1949
# AL0DE011:    Built 1939 or earlier


##--------------------------------------------------------------------------------------
## join time series data to shapefile
bg_2010_sf <- read_sf("../nhgis0087_shape/nhgis0087_shapefile_tl2010_us_blck_grp_2010/US_blck_grp_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join sf and tbl
final_10bg_sf <- left_join(bg_2010_sf,bg_2015_19)

final_10bg_sf

# Metadata
# GISJOIN: 2010 BGs code from NHGIS
# HU2015-19: Housing Units from 2015-19 ACS
# HU2010: 2010 HU counts from 2015-19 YSB [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 2000-2009”] 
# HU2000: 2000 HU counts from 2015-19 YSB [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1990-1999”] 
# HU1990: 1990 HU counts from 2015-19 YSB [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1980-1989”]

##--------------------------------------------------------------------------------------
# write shapefile
st_write(final_10bg_sf, dsn = "shapes/bgroups_2010", layer = "shp4_bgs10.shp", driver = "ESRI Shapefile", append = FALSE) # , append = FALSE



