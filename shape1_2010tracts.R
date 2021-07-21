##--------------------------------------------------------------------------------------
# shape1_2010tracts.Rc
# 2010 Housing Unit Shapefile
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
## Read in NHGIS data; housing unit and ysb, 2010 
##--------------------------------------------------------------------------------------

# 2010 housing units, tract
tract_hu2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds172_2010_tract.csv") %>%
  select(-c(2:39)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2010 = IFC001)

# 2006-2010 ACS Year Structure Built, tract
tract_ysb2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds176_20105_2010_tract.csv") %>%
  select(-c(2:36)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

# join housing units and YSB
tracts_2010 <- left_join(tract_hu2010, tract_ysb2010)

##--------------------------------------------------------------------------------------
# 1990 county level housing units
county_hu1990 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds120_1990_county.csv") %>%
  select(-c(2:26)) %>%
  filter(GISJOIN %in% county_fips)

# 2000 county units, read in 
county_hu2000 <- read_nhgis("../nhgis0090_csv/nhgis0090_ds146_2000_county.csv") %>%
  select(-c(2:29)) %>%
  filter(GISJOIN %in% county_fips)

# join county tbls
county_hu90_00 <- left_join(county_hu1990,county_hu2000, by = "GISJOIN") %>%
  rename(HU1990 = ESA001,
         HU2000 = FKI001)

# remove county data
rm(county_hu1990,county_hu2000)

# join county housing unit totals to census tract data
tracts_2010_county <- left_join(tracts_2010,county_hu90_00, by = c("COUNTYA" = "GISJOIN"))


##--------------------------------------------------------------------------------------
# ESA001 = 1990 Census County Level Housing Unit Totals
# FKI001 - 2000 Census County Level Housing Unit Totals
# IFC001= 2010 housing units
# JSD... = YSBJSDE001:     Total
# JSDE002:     Built 2005 or later
# JSDE003:     Built 2000 to 2004
# JSDE004:     Built 1990 to 1999
# JSDE005:     Built 1980 to 1989
# JSDE006:     Built 1970 to 1979
# JSDE007:     Built 1960 to 1969
# JSDE008:     Built 1950 to 1959
# JSDE009:     Built 1940 to 1949
# JSDE010:     Built 1939 or earlier

##--------------------------------------------------------------------------------------
# Hammer Estimates
tracts_ham_est <- tracts_2010_county %>%
  group_by(COUNTYA) %>%
  mutate(
    j90 = sum(JSDE005,JSDE006,JSDE007,JSDE008,JSDE009,JSDE010),
    i90 = (JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),
    hmadj = (HU1990/j90),
    ham_est90 = (HU1990/j90) * i90,
    j00 = sum(JSDE004,JSDE005,JSDE006,JSDE007,JSDE008,JSDE009,JSDE010),
    i00 = (JSDE004+JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),
    hamadj00 = (HU2000/j00),
    ham_est00 = (HU2000/j00) * i00
  ) %>%
  ungroup() %>%
  select(GISJOIN, HU2010, ham_est90, ham_est00)

##--------------------------------------------------------------------------------------
## Time Series NHGIS data; 1990 and 2000 in 2010 tracts
##--------------------------------------------------------------------------------------
ts_2010  <- read_nhgis("../nhgis0089_csv/nhgis0089_ts_geog2010_tract.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU1990_10ts = CM7AA1990,
         HU2000_10ts = CM7AA2000) %>%
  select(GISJOIN, HU1990_10ts, HU2000_10ts)
  
## join time series to data
all_data <- left_join(tracts_ham_est,ts_2010)

##--------------------------------------------------------------------------------------
## read in 2010 housing sf, tract
tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join spatial
final_10_sf <- left_join(tract_2010_sf,all_data)

final_10_sf



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

## TO Do LIST
# refine code, above 
# write file

st_write(final_10_sf, dsn = "shapes/tracts_2010/shp1_tracts10.shp", layer = "shp1_tracts10.shp", driver = "ESRI Shapefile", append = FALSE)

