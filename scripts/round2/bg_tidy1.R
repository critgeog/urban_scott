library(tidyverse)
library(tidycensus)
# library(purrr)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)

## API KEY - use census api key: https://api.census.gov/data/key_signup.html
my_acs_key <- 'YOUR KEY HERE'
# my_acs_key <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

# county fips for NHGIS and Census Data Formats for nine  counties included in validation
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390),
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)
acs_county_fips <- c("22071", "26163", "29510", "06065", "12095","48439","34013", "39061", "42003")

# State Fips and Abbreviations of nine counties in validation
my_states <- c(22,26,29,06,12,48,34,39,42)
state_codes <- c("LA", "MI", "MO", "CA", "FL", "TX", "NJ", "OH", "PA")


my_vars <- c("B25034_001","B25034_002","B25034_003","B25034_004","B25034_005",":B25034_006","B25034_007","B25034_008","B25034_009","B25034_010","B25034_011")

ysb19bg <- map_dfr(
  state_codes,
  ~ get_acs(
    geography = "block group",
    variables = my_vars,
    state = .,
    year = 2019,
    survey = "acs5",
    geometry = FALSE,
    key = my_acs_key,
    output = 'wide'
  )
)

ysb19bg %>%
  select(-(ends_with("M"))) %>% # remove MOE columns 
  mutate(county_fips = str_sub(GEOID,1,5), # create a county fips column
         GISJOIN = paste0("G",str_sub(GEOID,1,2),0,str_sub(GEOID,3,5),0,str_sub(GEOID,6,11)), # create GISJOIN to match NHGIS codes
         HU2010 = rowSums(.[,6:12]), # sum of YSB 1939...YSB 2009
         HU2000 = rowSums(.[,7:12]), # sum of YSB 1939...YSB 1999
         HU1990 = rowSums(.[,8:12])) %>% # sum of YSB 1939...YSB 1989
  filter(county_fips %in% acs_county_fips) %>% # filter dataset to counties used in validation
  rename(HU2015_19 = B25034_001E) %>% # rename 2015-19 ACS Housing Units variable
  select(GISJOIN, HU2015_19, HU2010, HU2000, HU1990) # select final columns for dataset


# ysb19bg <- get_acs(geography = "block groups", state = my_states,
#         variables = my_vars, year = 2019, geometry = FALSE, output = 'wide',
#         key = my_acs_key)


# ysb19bg2 <- ysb19bg %>%
#   mutate(county_fips = str_sub(GEOID,1,5)) %>%
#   filter(county_fips %in% acs_county_fips)

# ysb19bg2 %>%
#   select(-(ends_with("M"))) %>%
#   rename(HU2015_19 = B25034_001E) %>%
#   summarise(HU2010 = rowSums(B25034_004E:B25034_011E)
#          # HU2000 = colSums(B25034_005E:B25034_011E),
#          # HU1990 = colSums(B25034_006E:B25034_011E)
#   )




# GISJOIN: 2010 BGs code from NHGIS
# HU2015-19: Housing Units from 2015-19 ACS
# HU2010: 2010 HU counts from 2015-19 YSB
# HU2000: 2000 HU counts from 2015-19 YSB
# HU1990: 1990 HU counts from 2015-19 YSB
  
# # pull in census tracts from tigris for nine states included in validation
# bgs10 <- map_dfr(
#   state_codes, 
#   ~block_groups(
#     state = .x, 
#     cb = TRUE, 
#     year = 2018
#   )
# )
# 
# 
# # ysb19bg2 %>%
# #   select(-(ends_with("M"))) %>%
# #   rename(HU2015_19 = B25034_001E) %>%
# #   summarise(HU2010 = rowSums(B25034_004E:B25034_011E)
# #          # HU2000 = colSums(B25034_005E:B25034_011E),
# #          # HU1990 = colSums(B25034_006E:B25034_011E)
# #   )
# 
# bgs10b <- bgs10 %>%
#   mutate(county_fips = str_sub(GEOID,1,5)) %>%
#   filter(county_fips %in% acs_county_fips)





multi_state_bg_geo_list <- map2(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "block group",
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2016,
    survey = "acs5",
    geometry = TRUE,
    key = my_acs_key,
    output = "wide"  # get data in wide format for easier mapping
  )
)




