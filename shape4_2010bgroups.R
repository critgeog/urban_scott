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
## Read in Time Series NHGIS data; 1990 and 2000 in 2010 geographies
ts_bg_2010  <- read_nhgis("../nhgis0091_csv/nhgis0091_ts_geog2010_blck_grp.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU1990_10 = CM7AA1990,
         HU2000_10 = CM7AA2000) %>%
  select(GISJOIN, HU1990_10, HU2000_10)


##--------------------------------------------------------------------------------------
## join time series data to shapefile
bg_2010_sf <- read_sf("../nhgis0087_shape/nhgis0087_shapefile_tl2010_us_blck_grp_2010/US_blck_grp_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN)

# join sf and tbl
final_10bg_sf <- left_join(bg_2010_sf,ts_bg_2010)

final_10bg_sf

# Metadata
# GISJOIN: 2010 BGs code from NHGIS
# HU1990_10: 1990 HU (NHGIS normalized to 2010 BGs)
# HU2000_10: 2000 HU (NHGIS normalized to 2010 BGs)

## TO DO LIST
# refine code
# write file

# st_write(final_10bg_sf, dsn = "shapes/bgroups_2010/shp4_bgs10.shp", layer = "shp4_bgs10.shp", driver = "ESRI Shapefile") # , append = FALSE



