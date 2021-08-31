##--------------------------------------------------------------------------------------
## create shapes

##--------------------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)


##--------------------------------------------------------------------------------------
## read in tables
tracts1519 <- read_csv("tables/tracts1519.csv")
tracts0610 <- read_csv("tables/tracts0610.csv")
tracts00 <- read_csv("tables/tracts00.csv")
tracts90 <- read_csv("tables/tracts90.csv")
county90_00 <- read_csv("tables/countyhu90_00.csv")
tracts_ts2010 <- read_csv("tables/tracts_ts2010.csv")

##--------------------------------------------------------------------------------------
## read in tracts to remove
remove_tracts_2010 <- read_csv("csv/tracts_2010_remove.csv") %>% # read in file with 3/4 of 2010 tracts to keep for 2006-10 data (created in buf_2010.R)
  select(GISJOIN, stay)
remove_tracts_2000 <- read_csv("csv/tracts_2000_1_2_remove.csv") %>% ## read in file with 1/2 of tracts to stay and 1/2 to remove (created in buf_2000.R)
  select(GISJOIN, stay)
remove_tracts_1990 <- read_csv("csv/tracts_1990_1_4_remove.csv") %>% # read in file with 1/4 of tracts to stay and 3/4 to remove (created in buf_1990.R)
  select(GISJOIN, stay)
  

##--------------------------------------------------------------------------------------
# shape 1

# join 2006-10 ACS housing units
tracts1519 <- tracts0610 %>%
  select(GISJOIN, HU2006_10) %>%
  left_join(.,tracts1519)

## join time series to data
tracts1519ts <- left_join(tracts1519,tracts_ts2010)

# join county housing unit totals to census tract data
tracts15195s_county <- left_join(tracts1519ts,county90_00, by = c("COUNTYA" = "GISJOIN"))

# Hammer Estimates
tracts_ham_est <- tracts15195s_county %>%
  group_by(COUNTYA) %>%
  mutate(
    j90 = sum(AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # sum of housing units built before 1989 in county j
    # hmadj = (hu1990_cnty/j90),
    ham_est90 = (hu1990_cnty/j90) * hu_90,
    j00 = sum(AL0DE005, AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # housing units built before 1999 inin county j
    # hamadj00 = (hu2000_cnty/j00),
    ham_est00 = (hu2000_cnty/j00) * hu_00
  ) %>%
  ungroup() %>%
  select(GISJOIN, HU2015_19, HU2006_10, hu1990_10ts, hu2000_10ts, hu1990_cnty, hu2000_cnty, ham_est90, ham_est00)

tracts_ham_est

# ## read in 2010 sf, tract
# tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
#   mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
#   filter(COUNTYA %in% county_fips) %>%
#   select(GISJOIN)

# # join spatial
# final_10_sf <- left_join(tract_2010_sf,tracts_ham_est)
# 
# str(final_10_sf)
# # Metadata
# # GISJOIN: 2010 GISJOIN code (from NHGIS)
# # HU2010: 2010 HU from 2010 Census
# # HU2000_10ts: 2000 HU (NHGIS normalized to 2010 tracts)
# # HU1900_10ts: 1990 HU (NHGIS normalized to 2010 tracts)
# # ham_est00: Hammer estimates for 2000 using 2010 YSB tract data [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1990-1999”] & 2000 county HU counts data
# # ham_est90: Hammer estimates for 1990 using 2010 YSB data & 1990 county HU counts data
# 
# ##--------------------------------------------------------------------------------------
# # write shapefile
# st_write(final_10_sf, dsn = "shapes/tracts_2019/", layer = "shp1_tracts19.shp", driver = "ESRI Shapefile")




##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 2
# join tract 'stay or remove' indicator to housing unit totals
final_00 <- left_join(tracts00,remove_tracts_2000)

# # read in 2000 CT spatial, sf
# tract_2000_sf <- read_sf("../nhgis0085_shape/nhgis0085_shapefile_tl2000_us_tract_2000/US_tract_2000.shp") %>%
#   mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
#   filter(COUNTYA %in% county_fips) %>%
#   select(GISJOIN)
# 
# # join spatial
# final_00_sf <- left_join(tract_2000_sf,final_00)
# 
# # 1/2 = Y
# fct_count(final_00_sf$stay)
# 
# final_00_sf         
# 
# # metadata
# # GISJOIN = 2000 Census Tract
# # HU2000_00 = 2000 HU counts from 2000 census
# # HU90_00 = 1990 HU counts from 2000 census [YSB: “built before 1940” + “built 1940-1949” + ,,, + ”built 1980-1989”]
# # stay = indicator of whether tract is in 2/3 that stay or 1/3 that get removed
# # Y indicates tract is 2/3 that stays
# 
# 
# # write file
# st_write(final_00_sf, dsn = "shapes/tracts_2000/", layer = "shp2_tracts00.shp", driver = "ESRI Shapefile", append = FALSE) 


##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 3
# join tract 'stay or remove' indicator to housing unit totals
final_90 <- left_join(tracts90,remove_tracts_1990)

# # read in 1990 CT spatial, sf
# tract_1990_sf <- read_sf("../nhgis0084_shape/nhgis0084_shapefile_tl2000_us_tract_1990/US_tract_1990.shp") %>%
#   mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
#   filter(COUNTYA %in% county_fips) %>%
#   select(GISJOIN)
# 
# # join spatial
# final_90_sf <- left_join(tract_1990_sf,final_90)
# final_90_sf <- final_90_sf %>%
#   select(GISJOIN, HU1990_90,stay,geometry)
# 
# final_90_sf
# fct_count(final_90_sf$stay)
# 1778/2375
# 
# # metadata
# # GISJOIN = 1990 Census Tract
# # HU1990_90 = 1990 HU counts from 1990 census
# # stay = indicator of whether tract is in 1/3 that stay or 2/3 that get removed
# # Y indicates tract is 1/3 that stays
# 
# # write shapefile
# st_write(final_90_sf, dsn = "shapes/tracts_1990/", layer = "shp3_tracts90.shp", driver = "ESRI Shapefile", append = FALSE) #, 


# shape 4

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 5
# join tract 'stay or remove' indicator to housing unit totals
final_10 <- left_join(tracts_2010,remove_tracts_2010)





# ## read in 2010 housing sf, tract
# tract_2010_sf <- read_sf("../nhgis0086_shape/nhgis0086_shapefile_tl2010_us_tract_2010/US_tract_2010.shp") %>%
#   mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
#   filter(COUNTYA %in% county_fips) %>%
#   select(GISJOIN)
# 
# # join spatial
# final_10_sf <- left_join(tract_2010_sf,final_10)
# 
# # 1/2 = Y
# fct_count(final_10_sf$stay)
# 
# 2052/2742
# 690/2742
# 
# # metadata
# # GISJOIN = 2010 Census Tract
# # HU2006_10 = 2006-10 ACS HU counts
# # HU_2000 = 2000 Housing Units from 2006-10 ACS, Year Structure Built
# # HU_1990 = 1990 Housing Units from 2006-10 ACS, Year Structure Built
# # stay = indicator of whether tract is in 3/4 that stay or 1/4 that get removed
# # Y indicates tract is 2/3 that stays
# 
# 
# ##--------------------------------------------------------------------------------------
# # write file
# st_write(final_10_sf, dsn = "shapes/tracts_2010/", layer = "shp5_tracts10.shp", driver = "ESRI Shapefile", append = TRUE) 

