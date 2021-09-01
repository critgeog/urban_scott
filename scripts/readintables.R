#########################################################################
#########################################################################
###                                                                   ###
###        PULL in HISTORICAL HOUSING UNIT (1990-2019) AND            ###
###         YEAR STRUCTURE BUILT (YSB) TABLES (1940-1980)             ###
###                                                                   ###
#########################################################################
#########################################################################

## Load or install packages
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])
  
  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

packages(tidyverse)
packages(httr)  # for NHGIS API
packages(jsonlite)  # for NHGIS API
packages(ipumsr)  # for NHGIS tables
packages(tidycensus)  # for getting state/county names/abbrevs.


## Check Workspace
getwd()  # D:/HIST_HU_URB

## Create temp folder
dir.create("temp")


#########################################################
## SET API KEY and EXPLORE NHGIS SHAPEFILES            ##
#########################################################

### NHGIS: https://www.nhgis.org/

## API KEY
# Set personalized API key:  https://account.ipums.org/api_keys
my_ipums_key <- "<YOUR KEY HERE>"

#########################################################
## LOAD HU & YSB TABLES                                ##
#########################################################

# Check out available datasets
url <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1"
result <- GET(url, add_headers(Authorization = my_ipums_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

# Look at single year (1980) example
url <- "https://api.ipums.org/metadata/nhgis/datasets/1980_STF3/data_tables/NT109A?version=v1"
result <- GET(url, add_headers(Authorization = my_ipums_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, 20L)


# Pull in Year Structure Built (YSB) data by decade at tract level, 2000, 2006-10, 2015-19
# Pull in Housing Unit Data at tract level 1990, 2000, 2006-10, 2015-19
# pull in 1990 and 2000 housing units at county level
# pull in 1990 and 2000 housing units in 2010 boundaries (time series data)
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '
  {
    "datasets": {
      "2015_2019_ACS5a": {
        "data_tables": ["B25034","B25001"],
        "geog_levels": ["tract"]
      },
      "2006_2010_ACS5a": {
        "data_tables": ["B25034", "B25001"],
        "geog_levels": ["tract"]
      },
      "2000_SF3a": {
        "data_tables": ["NH034A"],
        "geog_levels": ["tract"]
      },
      "2000_SF1a": {
        "data_tables": ["NH001A"],
        "geog_levels": ["tract", "county"]
      },
      "1990_STF1": {
        "data_tables": ["NH1"],
        "geog_levels": ["tract", "county"]
        }
      },
      "time_series_tables": {
      "CM7": {
        "years": ["1990","2000"],
        "geog_levels": ["tract"]
        }
      },
    "time_series_table_layout": "time_by_column_layout",
    "data_format": "csv_no_header",
    "breakdown_and_data_type_layout": "single_file"
  }
'
  
# # # Pull in Year Structure Built for Block group level
# # url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
# mybody <- '
#   {
#     "datasets": {
#     "2015_2019_ACS5a": {
#     "data_tables": ["B25034"],
#     "geog_levels": ["group"]
#         }
#       },
#     "data_format": "csv_no_header",
#     "breakdown_and_data_type_layout": "single_file"
#   }
#   '

## Request data extract from NHGIS
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_ipums_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number
my_number


## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~5+ MINUTES for EXTRACT to be PREPARED (check email) !!!!!! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Import Extract when it's ready
csv_url <- paste0("https://api.ipums.org/extracts/", my_number, "?product=nhgis&version=v1")
data_extract_status_res <- GET(csv_url, add_headers(Authorization = my_ipums_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links

# Destination file
zip_file <- "temp/NHGIS_tables.zip"

# Download extract to destination file
download.file(
  url = des_df$download_links$table_data, 
  destfile = zip_file, 
  headers = c(Authorization = my_ipums_key)
)

# List extract files in ZIP archive
unzip(zip_file, list=TRUE)


# Read CSV files into a data frame

ysb1519_table <- read_nhgis(zip_file, data_layer = contains("ds244_20195_2019_tract.csv"))
ysb0610_table <- read_nhgis(zip_file, data_layer = contains("ds176_20105_2010_tract.csv"))

hu00_table <- read_nhgis(zip_file, data_layer = contains("ds146_2000_tract.csv"))
ysb00_table <- read_nhgis(zip_file, data_layer = contains("ds151_2000_tract.csv"))

hu90_table <- read_nhgis(zip_file, data_layer = contains("ds120_1990_tract.csv"))

cntyhu90_table <- read_nhgis(zip_file, data_layer = contains("ds120_1990_county.csv"))
cntyhu00_table <- read_nhgis(zip_file, data_layer = contains("ds146_2000_county.csv"))

ct10ts_table <- read_nhgis(zip_file, data_layer = contains("ts_geog2010_tract.csv"))


###########################################################
##  CLEAN UP data frames, 1940-80                        ##
###########################################################

county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)


##--------------------------------------------------------------------------------------
# 2015-19

# 2015-19 ACS Housing Units and Year Structure Built, tract
ysb19 <-  ysb1519_table %>%
  select(-c(2:43)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2015_19 = ALZJE001)

ysb19 <- ysb19 %>%
  mutate(
    hu_90 = (AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011), # housing units built before 1989 in tract i in county j
    hu_00 = (AL0DE005 + AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011)# housing units built before 1999 in tract i in county j
  )

# count hu 90 and hu00 for Hammer estimate

## write
write_csv(ysb19, "tables/tracts1519.csv")

##--------------------------------------------------------------------------------------
# 2006-10

# 2006-2010 ACS Year Structure Built, tract
ysb2010 <- ysb0610_table %>%
  # select(-c(2:36,38:57)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2006_10 = JRIE001)

##--------------------------------------------------------------------------------------
# Sum 2000 and 1990 housing units from YSB columns in 2006-10 ACS
ysb10 <- ysb2010 %>%
  mutate(
    ysb1990 = (JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),      # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
    ysb2000 = (JSDE004+JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010), # YSB Built 1939 or earlier + ... + YSB Built 1990-1999
  ) %>%
  select(GISJOIN, HU2006_10, HU_2000, HU_1990)

## write
write_csv(ysb10, "tables/tracts0610.csv")


##--------------------------------------------------------------------------------------
## 2000 housing units, tract
tract_hu2000 <- hu00_table %>%
  select(-c(2:7,9:30)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>% 
  rename(HU2000_00 = FKI001)

## 2000 Year Structure Built, tract
tract_ysb2000 <- ysb00_table %>%
  select(-c(2:7,9:31)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  mutate(
    HU1990_00 = (GAJ004+GAJ005+GAJ006+GAJ007+GAJ008+GAJ009), # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
  )

# ## join
tracts_2000 <- left_join(tract_hu2000, tract_ysb2000)

## write
write_csv(tracts_2000, "tables/tracts00.csv")

##--------------------------------------------------------------------------------------
## Read in NHGIS Housing Unit and YSB Data, 1990

# 1990 housing units, tract
tracts_hu1990 <- hu90_table %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU1990_90 = ESA001) %>%
  select(GISJOIN,HU1990_90)

## write
write_csv(tracts_hu1990, "tables/tracts90.csv")


##--------------------------------------------------------------------------------------
## 1990 and 2000 county
# Read in 1990 county level housing units
county_hu1990 <- cntyhu90_table %>%
  select(-c(2:26)) %>%
  filter(GISJOIN %in% county_fips)

# Read in 2000 county units, read in 
county_hu2000 <- cntyhu00_table %>%
  select(-c(2:29)) %>%
  filter(GISJOIN %in% county_fips)

# join county tbls
county_hu90_00 <- left_join(county_hu1990,county_hu2000, by = "GISJOIN") %>%
  rename(hu1990_cnty = ESA001,
         hu2000_cnty = FKI001)

## write
write_csv(county_hu90_00, "tables/countyhu90_00.csv")

##--------------------------------------------------------------------------------------
## Time Series NHGIS data; 1990 and 2000 in 2010 tracts
ts_2010  <- ct10ts_table %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(hu1990_10ts = CM7AA1990,
         hu2000_10ts = CM7AA2000) %>%
  select(GISJOIN, hu1990_10ts, hu2000_10ts)

## write
write_csv(ts_2010, "tables/tracts_ts2010.csv")


