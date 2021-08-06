# county_hu.R
# this script reads in data from NHGIS to calculate the difference in housing units 
# between 1990 and 2010 in US counties
# This creates a file called county_9010_validation.csv
# Based on the exported csv, we discussed which counties were appropriate for selection in the validation procedure

library(tidyverse)
library(ipumsr)

## counties included in  analysis
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)

county_hu1990 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds120_1990_county.csv") %>%
  filter(!(STATE %in% c("Puerto Rico", "Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:26)) 
# %>%
#   filter(GISJOIN %in% county_fips)  

county_hu2000 <- read_nhgis("../nhgis0090_csv/nhgis0090_ds146_2000_county.csv") %>%
  filter(!(STATE %in% c("Puerto Rico", "Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:29)) 
 # %>%
 #  filter(GISJOIN %in% county_fips)  

county_hu2010 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds172_2010_county.csv") %>%
  filter(!(STATE %in% c("Puerto Rico", "Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:4,6,8:39)) 
# %>%


county_hu2019 <- read_nhgis("../nhgis0095_csv/nhgis0095_ds244_20195_2019_county.csv") %>%
  filter(!(STATE %in% c("Puerto Rico", "Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:43,45:46)) 

  

county_hu <- left_join(county_hu2010,county_hu2000, by = "GISJOIN") %>%
  left_join(.,county_hu1990) %>%
  left_join(.,county_hu2019)


county_validation <- county_hu %>%
  filter(!(GISJOIN %in% c("G1200860", "G0800140"))) %>% # remove Miami-Dade County and Broomfield County
  rename("1990" = ESA001,
         "2000" = FKI001,
         "2010" = IFC001,
         "2019" = ALZJE001) %>%
  mutate(diff10_90 = `2010` - `1990`,
         diff19_90 = `2019` - `1990`) %>%
  arrange(diff10_90) %>%
  select(GISJOIN, STATE, COUNTY, `1990`, `2000`, `2010`, `2019`, diff10_90, diff19_90 ) 

county_validation


# write_csv(county_validation, "csv/county_validation.csv")

county_validation %>%
  filter(GISJOIN %in% county_fips) -> validate_nine
county_validation  

# write_csv(validate_nine, "csv/validate_nine.csv")



