# county_hu.R
# this script reads in data from NHGIS to calculate the difference in housing units 
# between 1990 and 2010 in US counties
# This creates a file called county_9010_validation.csv
# Based on the exported csv, we discussed which counties were appropriate for selection in the validation procedure

library(tidyverse)
library(ipumsr)

county_hu1990 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds120_1990_county.csv") %>%
  filter(!(STATE %in% c("Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:26))

  
county_hu2010 <- read_nhgis("../nhgis0079_csv/nhgis0079_ds172_2010_county.csv") %>%
  filter(!(STATE %in% c("Puerto Rico", "Alaska", "Hawaii","Virginia"))) %>%
  select(-c(2:4,6,8:39))
  


county_9010 <- left_join(county_hu1990,county_hu2010, by = "GISJOIN")

county_1090 <- left_join(county_hu2010,county_hu1990, by = "GISJOIN")

# county_1090 %>%
#   filter(GISJOIN == "G1200860") %>%
#   mutate(ESA001 = 771288))

county_1090 %>%
  mutate(ESA001=replace(ESA001, GISJOIN == "G1200860", 771288)) -> county_1090


# county_9010 %>%
#   rename("1990" = ESA001,
#          "2010" = IFC001) %>%
#   mutate(diff = `2010` - `1990`) %>%
#   arrange(diff) %>%
#   view()

county_1090 %>%
  rename("1990" = ESA001,
         "2010" = IFC001) %>%
  mutate(diff = `2010` - `1990`) %>%
  arrange(diff) -> county_9010_validation

# write_csv(county_9010_validation, "county_9010_validation.csv")

