##--------------------------------------------------------------------------------------
# shape1_2010tracts.R
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
## Read in NHGIS data; ysb 2006-10 ACS and 2015-19 ACS
##--------------------------------------------------------------------------------------

# 2015-19 ACS Year Structure Built, tract
tract_ysb19 <- read_nhgis("../nhgis0092_csv/nhgis0092_ds244_20195_2019_tract.csv") %>%
  select(-c(2:43)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2015_19 = AL0DE001)

# 2006-2010 ACS Year Structure Built, tract
tract_ysb2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds176_20105_2010_tract.csv") %>%
  select(-c(2:36,38:57)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2006_10 = JSDE001)

# join housing units and YSB
tracts_2019 <- left_join(tract_ysb19, tract_ysb2010)


##--------------------------------------------------------------------------------------
## Time Series NHGIS data; 1990 and 2000 in 2010 tracts
##--------------------------------------------------------------------------------------
ts_2010  <- read_nhgis("../nhgis0089_csv/nhgis0089_ts_geog2010_tract.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(hu1990_10ts = CM7AA1990,
         hu2000_10ts = CM7AA2000) %>%
  select(GISJOIN, hu1990_10ts, hu2000_10ts)


## join time series to data
tracts_2019_ts <- left_join(tracts_2019,ts_2010)

##--------------------------------------------------------------------------------------
# Read in 1990 county level housing units
county_hu1990 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds120_1990_county.csv") %>%
  select(-c(2:26)) %>%
  filter(GISJOIN %in% county_fips)

# Read in 2000 county units, read in 
county_hu2000 <- read_nhgis("../nhgis0090_csv/nhgis0090_ds146_2000_county.csv") %>%
  select(-c(2:29)) %>%
  filter(GISJOIN %in% county_fips)

# join county tbls
county_hu90_00 <- left_join(county_hu1990,county_hu2000, by = "GISJOIN") %>%
  rename(hu1990_cnty = ESA001,
         hu2000_cnty = FKI001)


# join county housing unit totals to census tract data
tracts_2019_county <- left_join(tracts_2019_ts,county_hu90_00, by = c("COUNTYA" = "GISJOIN"))

# clean environment
rm(county_hu1990,county_hu2000, tract_ysb19, tract_ysb2010, ts_2010, county_hu90_00, tracts_2019, tracts_2019_ts)

##--------------------------------------------------------------------------------------
# Hammer Estimates
tracts_ham_est <- tracts_2019_county %>%
  group_by(COUNTYA) %>%
  mutate(
    j90 = sum(AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # sum of housing units built before 1989 in county j
    i90 = (AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011), # housing units built before 1989 in tract i in county j
    hmadj = (hu1990_cnty/j90),
    ham_est90 = (hu1990_cnty/j90) * i90,
    j00 = sum(AL0DE005, AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # housing units built before 1999 inin county j
    i00 = (AL0DE005 + AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011),# housing units built before 1999 in tract i in county j
    hamadj00 = (hu2000_cnty/j00),
    ham_est00 = (hu2000_cnty/j00) * i00
  ) %>%
  ungroup() %>%
  select(GISJOIN, HU2015_19, HU2006_10, hu1990_10ts, hu2000_10ts, hu1990_cnty, hu2000_cnty, ham_est90, ham_est00)

tracts_ham_est 


##--------------------------------------------------------------------------------------
## read in 2010 sf, tract
tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join spatial
final_10_sf <- left_join(tract_2010_sf,tracts_ham_est)

str(final_10_sf)
# Metadata
# GISJOIN: 2010 GISJOIN code (from NHGIS)
# HU2010: 2010 HU from 2010 Census
# HU2000_10ts: 2000 HU (NHGIS normalized to 2010 tracts)
# HU1900_10ts: 1990 HU (NHGIS normalized to 2010 tracts)
# ham_est00: Hammer estimates for 2000 using 2010 YSB tract data [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1990-1999”] & 2000 county HU counts data
# ham_est90: Hammer estimates for 1990 using 2010 YSB data & 1990 county HU counts data

##--------------------------------------------------------------------------------------
# write shapefile
st_write(final_10_sf, dsn = "shapes/tracts_2019/", layer = "shp1_tracts19.shp", driver = "ESRI Shapefile")

