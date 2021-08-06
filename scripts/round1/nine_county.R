# nine_county.R
# get housing unit data for nine counties selected for validation
# Author: tjh
# last updated: May 2, 2021

# load libraries
library(tidyverse)
library(ipumsr)
library(sf)

state_names <- c("Louisiana", "Michigan","Missouri",
                 "California","Florida" ,"Texas",
                 "Ohio","Pennsylvania","New Jersey")
county_fips <- c("G2200710", "G2601630", "G2905100",
                 "G0600650", "G1200950","G4804390",
                 "G3400130", "G3900610", "G4200030")
# counties: 
# decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
# growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
# stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)

# Essex county, Riverside County, and Orange County (Fl) are not tracted in 1950
# All are tracted in 1960; the entire county

#1990
tract_hu1990 <- read_nhgis("../nhgis0084_csv/nhgis0084_ds120_1990_tract.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(3:12,15:21,23:26)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

tract_1990_sf <- read_sf("../nhgis0084_shape/nhgis0084_shapefile_tl2000_us_tract_1990/US_tract_1990.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)


# tract_ysb1990<- read_nhgis("../nhgis0084_csv/nhgis0084_ds123_1990_tract.csv") %>%
#   filter((STATE %in% c("Louisiana", "Michigan","Missouri","Texas","California","Florida",
#                        "Ohio","Pennsylvania","New Jersey"))) %>%
#   select(-c(2:26))

#2000
tract_hu2000 <- read_nhgis("../nhgis0085_csv/nhgis0085_ds146_2000_tract.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(9:30)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

tract_ysb2000 <- read_nhgis("../nhgis0085_csv/nhgis0085_ds151_2000_tract.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(2:31)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)


tract_2000_sf <- read_sf("../nhgis0085_shape/nhgis0085_shapefile_tl2000_us_tract_2000/US_tract_2000.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

# 2010 tract
tract_hu2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds172_2010_tract.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(3:39)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

tract_ysb2010 <- read_nhgis("../nhgis0086_csv/nhgis0086_ds176_20105_2010_tract.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(3:35)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

#2010 block group
bg_hu2010 <- read_nhgis("../nhgis0087_csv/nhgis0087_ds172_2010_blck_grp.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

# note: Year Structure Built data is from 2006-10 ACS; Year Structure Built is not in 2010 decennial census
bg_ysb0610 <- read_nhgis("../nhgis0088_csv/nhgis0088_ds176_20105_2010_blck_grp.csv") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

bg_2010_sf <- read_sf("../nhgis0087_shape/nhgis0087_shapefile_tl2010_us_blck_grp_2010/US_blck_grp_2010.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)


# count total number oftracts or block groups (sub-geography) by each county
# create new dataset
n90 <- tract_hu1990 %>%
  group_by(COUNTY, COUNTYA) %>%
  summarise(cts90 = n())

n00 <- tract_hu2000 %>%
  group_by(COUNTYA) %>%
  summarise(cts00 = n())

n10 <- tract_hu2010 %>%
  group_by(COUNTYA) %>%
  summarise(cts10 = n())

n10b <- bg_hu2010 %>%
  group_by(COUNTYA) %>%
  summarise(bgs10 = n())

county_sub_geogs_total <- left_join(n90,n00, by = "COUNTYA") %>%
  left_join(.,n10) %>%
  left_join(.,n10b)

# write csv of total tracts (90, 00, 10) and block groups (2010) in each county
# write_csv(county_sub_geogs_total, "county_sub_geogs_total.csv")

# remove temp datasets
rm(n90,n00,n10,n10b)

# join tbls to sfs

tracts_1990 <- left_join(tract_1990_sf, tract_hu1990, by = "GISJOIN")

tracts_2000 <- left_join(tract_2000_sf, tract_hu2000, by = "GISJOIN") %>%
  left_join(.,tract_ysb2000, by = 'GISJOIN')

tracts_2010 <- left_join(tract_2010_sf, tract_hu2010, by = "GISJOIN") %>%
  left_join(.,tract_ysb2010, by='GISJOIN')


# 1990 county level housing units  
county_hu1990 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds120_1990_county.csv") %>%
  filter(STATE %in% state_names) %>%
  select(-c(2:26)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips)

tracts2_2010 <- left_join(tracts_2010,county_hu1990, by = c("COUNTYA"))


# ESA001 = 1990 Census County Level Housing Unit Totals
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

tracts2_2010 %>%
  group_by(COUNTYA) %>%
  mutate(
    j90 = sum(JSDE005,JSDE006,JSDE007,JSDE008,JSDE009,JSDE010),
    i90 = (JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),
    hmadj = (ESA001/j90),
    adj90 = (ESA001/j90) * i90
  ) %>%
  # select(GISJOIN.x, COUNTYA, ESA001, IFC001, j90, i90, adj90) %>% view()
  select(GISJOIN.x,COUNTYA,hmadj) %>% view()


  # mutate(hu90 = ESA001/IFC001) %>%
  # select(GISJOIN.x, ESA001, IFC001, hu90)
  
  
  
  
  
  # 
  # #### -------------------------------------------- ####
  # ####                HAMMER TIME!!                 ####
  # #### -------------------------------------------- ####
  # ######################################################
  # ######################################################
  # 
  # ### wd: "C:/Users/scott/Dropbox/urb_proj"
  # join_df2 <- read.dbf("tables/join_df2.dbf") %>%
  #   as_tibble() %>%
  #   ## fix NAs -- from tracts with no "successor"--all 1980 plus some w/ gap in tract years (e.g., Bibb Co. GA 1940-50)
  #   mutate(hu_vect = ifelse(is.na(hu_vect2),hu_vect1,hu_vect2)) %>%
  #   select(-hu_vect1,-hu_vect2) %>%
  #   filter(!(GISJOIN=="G0800140031100" & str_detect(COUNTY, "Adams")))  ## get rid of mistaken county (repeat of this census tract)
  # 
  # join_df2
  # 
  # ## hammer method: CO_CALC controls for county boundary changes over the decade
  # ham <- join_df2 %>%
  #   group_by(CO_CALC, yr) %>%
  #   mutate(hu_vect_sum = sum(hu_vect, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(diff_v = hu_co - hu_vect_sum)
  # 
  # ham  ## n = 361,305; NAs in hu_vect and hu_supp indicate tract-year has no vector-polygon data
  # 
  # ## ham1: filter out vector estimates; dealing only w/tract-years w/raster estimates
  # ham_rast <- ham %>%
  #   filter(is.na(hu_vect)) %>%
  #   group_by(CO_CALC, yr) %>%
  #   mutate(hu_rast_sum = sum(hu_rast)) %>%
  #   ungroup() %>%
  #   mutate(hu_v = hu_rast/hu_rast_sum*diff_v)
  # 
  # ham_rast  ## n = 180,409
  # 
  # ## ham2: include only vector estimates; dealing only w/tract-years w/vector estimates
  # ham_vect <- ham %>%
  #   filter(!is.na(hu_vect)) %>%
  #   group_by(CO_CALC, yr) %>%
  #   mutate(hu_rast_sum = sum(hu_rast)) %>%
  #   ungroup() %>%
  #   mutate(hu_v = hu_vect)
  # 
  # ham_vect  ## n = 180,896
  # 
  # ## ham2: bind rows and do group calculations for later steps
  # ham2 <- bind_rows(ham_rast,ham_vect) %>%
  #   group_by(CO_CALC, yr) %>%
  #   mutate(co_diff_v = hu_co - sum(hu_v),  ## calc. diff. between county-equivalent HU estimates and tract HU estimates
  #          n_rast = sum(is.na(hu_vect)),  ## count raster-interpolated tracts per county-year
  #          n_vect = sum(!is.na(hu_vect)),  ## county vector-polygon-interpolated tracts per county-year
  #          n_tot = n(),
  #          p_rast = n_rast/n_tot,    ## percent of tracts in county-year derived from raster-based estimates
  #          p_vect = n_vect/n_tot) %>%  ## percent of tracts in county-year derived from vector-based estimates
  #   arrange(yr, GISJOIN)
  # 
  # ham2
  
