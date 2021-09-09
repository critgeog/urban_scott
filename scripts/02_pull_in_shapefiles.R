##--------------------------------------------------------------------------------------
## 02_pull_in_shapefiles.r
## Using tigris package, create 1990, 2000, and 2010 shapefiles for census tracts, and 
## 2010 block group shapefiles. These files are used in the buffering script '03_buffer.r'
## and again in combining the final datasets '04_combine.r'
## these shapefiles are saved at 'gis_files/input/'
## Coordinate Reference System is "ESRI:102003"
##--------------------------------------------------------------------------------------


# load libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')
library(sf)

## API KEY - use census api key: https://api.census.gov/data/key_signup.html
# my_acs_key <- 'YOUR KEY HERE'
my_acs_key <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

## -----------------------------------------------------------------------------------------
# county fips for NHGIS and Census Data Formats for nine  counties included in validation
# county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
#                  "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390),
#                  "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)
acs_county_fips <- c("22071", "26163", "29510", "06065", "12095","48439","34013", "39061", "42003")

# State Fips and Abbreviations of nine counties in validation
# my_states <- c(22,26,29,06,12,48,34,39,42)
state_codes <- c("LA", "MI", "MO", "CA", "FL", "TX", "NJ", "OH", "PA")

## -----------------------------------------------------------------------------------------
# pull in 1990 census tracts from tigris for nine states included in validation
cts90 <- map_dfr(
  state_codes, 
  ~tracts(
    state = .x, 
    cb = TRUE, 
    year = 1990
  )
)

tracts90_sf <- cts90 %>%
  unite(COUNTYFIPS, STATEFP, COUNTYFP, sep = "", remove = FALSE) %>%
  filter(COUNTYFIPS %in% acs_county_fips) %>%
  mutate(GISJOIN = '',
         GISJOIN = ifelse(TRACTSUF== '00',paste0("G",STATEFP,0,COUNTYFP,0,TRACTBASE), paste0("G",STATEFP,0,COUNTYFP,0,TRACTBASE,TRACTSUF))) %>%
  select(GISJOIN)

tracts90_sf <- tracts90_sf %>%
  st_transform(crs = "ESRI:102003")


## -----------------------------------------------------------------------------------------
# pull in 2000 census tracts from tigris for nine states included in validation
cts00 <- map_dfr(
  state_codes, 
  ~tracts(
    state = .x, 
    cb = TRUE, 
    year = 2000
  )
)

tracts00_sf <- cts00 %>%
  unite(COUNTYFIPS, STATEFP, COUNTYFP, sep = "") %>%
  filter(COUNTYFIPS %in% acs_county_fips) %>%
  mutate(GISJOIN = paste0("G",STATE,0,COUNTY,0,TRACT)) %>%
  select(GISJOIN)

tracts00_sf <- tracts00_sf %>%
  st_transform(crs = "ESRI:102003")

## -----------------------------------------------------------------------------------------
# pull in census tracts from tigris for nine states included in validation
cts10 <- map_dfr(
  state_codes, 
  ~tracts(
    state = .x, 
    cb = TRUE, 
    year = 2018
  )
)

# clean data, filter to nine counties, create GISJOIN variable to match NHGIS formatting
tracts10_sf <- cts10 %>%
  unite(COUNTYFIPS, STATEFP, COUNTYFP, sep = "") %>%
  filter(COUNTYFIPS %in% acs_county_fips) %>%
  mutate(GISJOIN = paste0("G",str_sub(GEOID,1,2),0,str_sub(GEOID,3,5),0,str_sub(GEOID,6,11))) %>%
  select(GISJOIN)

tracts10_sf <- tracts10_sf %>%
  st_transform(crs = "ESRI:102003")


# pull in census tracts from tigris for nine states included in validation (sf can't read rbind() in tidycensus call above)
bgs10 <- map_dfr(
  state_codes,
  ~block_groups(
    state = .x,
    cb = TRUE,
    year = 2018
  )
)

# clean spatial file; create GISJOIN
bgs10_sf <- bgs10 %>%
  unite(COUNTYFIPS, STATEFP, COUNTYFP, sep = "", remove = FALSE) %>%
  filter(COUNTYFIPS %in% acs_county_fips) %>%
  mutate(GISJOIN = paste0("G",STATEFP,0,COUNTYFP,0,TRACTCE,BLKGRPCE )) %>%
  select(GISJOIN)


# write shapefiles
st_write(tracts10_sf, dsn = "gis_files/input/tracts_2010/", layer = "tracts10.shp", driver = "ESRI Shapefile", append = TRUE)
st_write(tracts00_sf, dsn = "gis_files/input/tracts_2000/", layer = "tracts00.shp", driver = "ESRI Shapefile", append = FALSE)
st_write(tracts90_sf, dsn = "gis_files/input/tracts_1990/", layer = "tracts90.shp", driver = "ESRI Shapefile", append = FALSE) #,
st_write(bgs10_sf, dsn = "gis_files/input/bgroups_2010", layer = "bgs10.shp", driver = "ESRI Shapefile", append = FALSE) # , append = FALSE
