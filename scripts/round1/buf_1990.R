## --------------------------------------------------------------------------
# buf_1990.R
# identify 1/3 of 1990 tracts to keep for 1990 data
# author: tjh
# last updated: July 20, 2021
### ------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(ipumsr)
library(sf)

# Create a Spatial Buffer Script
### ------------------------------------------------------------------------------
## counties included in analysis
county_fips <- c("G2200710", "G2601630", "G2905100",
                 "G0600650", "G1200950","G4804390",
                 "G3400130", "G3900610", "G4200030")

# a 1990 tract file
tract_1990_sf <- read_sf("../nhgis0084_shape/nhgis0084_shapefile_tl2000_us_tract_1990/US_tract_1990.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  select(GISJOIN, COUNTYA)


##--------------------------------------------------------------------------------------
orleans = st_point(c(569207, -822594))
wayne = st_point(c(1049928, 617021))
stl = st_point(c(497637, 142084))
riverside = st_point(c(-1950000, -197000))
orange = st_point(c(1430000, -890000))
tarrant = st_point(c(-115000, -529192))
essex = st_point(c(1810935, 571906))
hamilton = st_point(c(980710, 244401))
allegheny = st_point(c(1340792, 443919))

### 1/3 tracts to keep
# Orleans (1)
orleans_ctr <- st_sfc(orleans, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G2200710") -> orleans_sf

orleans_buf <- st_buffer(orleans_ctr, 3300)
orleans_buf_intersects <- st_intersects(orleans_buf, orleans_sf)
orleans_sel_sf <- orleans_sf[orleans_buf_intersects[[1]],]

count(orleans_sel_sf)/count(orleans_sf)

plot(st_geometry(orleans_sf), border="#aaaaaa", main="Census tracts that fall within 3.35km of city center")
plot(st_geometry(orleans_sel_sf), add=T, col="red")
plot(st_geometry(orleans_buf), add=T, lwd = 2)


# Wayne (2)
wayne_ctr <- st_sfc(wayne, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G2601630") -> wayne_sf

wayne_buf <- st_buffer(wayne_ctr, 7800)
wayne_buf_intersects <- st_intersects(wayne_buf, wayne_sf)
wayne_sel_sf <- wayne_sf[wayne_buf_intersects[[1]],]

count(wayne_sel_sf)/count(wayne_sf)

plot(st_geometry(wayne_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(wayne_sel_sf), add=T, col="red")
plot(st_geometry(wayne_buf), add=T, lwd = 2)


# STL (3)
stl_ctr <- st_sfc(stl, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G2905100") -> stl_sf

stl_buf <- st_buffer(stl_ctr, 3100)
stl_buf_intersects <- st_intersects(stl_buf, stl_sf)
stl_sel_sf <- stl_sf[stl_buf_intersects[[1]],]

count(stl_sel_sf)/count(stl_sf)

plot(st_geometry(stl_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(stl_sel_sf), add=T, col="red")
plot(st_geometry(stl_buf), add=T, lwd = 2)


# Riverside (4)#
riverside_ctr <- st_sfc(riverside, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G0600650") -> riverside_sf

riverside_buf <- st_buffer(riverside_ctr, 19500)
riverside_buf_intersects <- st_intersects(riverside_buf, riverside_sf)
riverside_sel_sf <- riverside_sf[riverside_buf_intersects[[1]],]

count(riverside_sel_sf)/count(riverside_sf)

plot(st_geometry(riverside_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(riverside_sel_sf), add=T, col="red")
plot(st_geometry(riverside_buf), add=T, lwd = 2)

# Orange (5)
orange_ctr <- st_sfc(orange, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G1200950") -> orange_sf

orange_buf <- st_buffer(orange_ctr, 5050)
orange_buf_intersects <- st_intersects(orange_buf, orange_sf)
orange_sel_sf <- orange_sf[orange_buf_intersects[[1]],]

count(orange_sel_sf)/count(orange_sf)

plot(st_geometry(orange_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(orange_sel_sf), add=T, col="red")
plot(st_geometry(orange_buf), add=T, lwd = 2)

# Tarrant (6)
tarrant_ctr <- st_sfc(tarrant, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G4804390") -> tarrant_sf

tarrant_buf <- st_buffer(tarrant_ctr, 9400)
tarrant_buf_intersects <- st_intersects(tarrant_buf, tarrant_sf)
tarrant_sel_sf <- tarrant_sf[tarrant_buf_intersects[[1]],]

count(tarrant_sel_sf)/count(tarrant_sf)

plot(st_geometry(tarrant_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(tarrant_sel_sf), add=T, col="red")
plot(st_geometry(tarrant_buf), add=T, lwd = 2)


# Essex (7)
essex_ctr <- st_sfc(essex, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G3400130") -> essex_sf

essex_buf <- st_buffer(essex_ctr, 2700)
essex_buf_intersects <- st_intersects(essex_buf, essex_sf)
essex_sel_sf <- essex_sf[essex_buf_intersects[[1]],]

count(essex_sel_sf)/count(essex_sf)

plot(st_geometry(essex_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(essex_sel_sf), add=T, col="red")
plot(st_geometry(essex_buf), add=T, lwd = 2)

# Allegheny (8)
allegheny_ctr <- st_sfc(allegheny, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G4200030") -> allegheny_sf

allegheny_buf <- st_buffer(allegheny_ctr, 5500)
allegheny_buf_intersects <- st_intersects(allegheny_buf, allegheny_sf)
allegheny_sel_sf <- allegheny_sf[allegheny_buf_intersects[[1]],]

count(allegheny_sel_sf)/count(allegheny_sf)

plot(st_geometry(allegheny_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(allegheny_sel_sf), add=T, col="red")
plot(st_geometry(allegheny_buf), add=T, lwd = 2)

# Hamilton (9)
hamilton_ctr <- st_sfc(hamilton, crs = "ESRI:102003")

tract_1990_sf %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  filter(COUNTYA == "G3900610") -> hamilton_sf

hamilton_buf <- st_buffer(hamilton_ctr, 4900)
hamilton_buf_intersects <- st_intersects(hamilton_buf, hamilton_sf)
hamilton_sel_sf <- hamilton_sf[hamilton_buf_intersects[[1]],]

count(hamilton_sel_sf)/count(hamilton_sf)

plot(st_geometry(hamilton_sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(hamilton_sel_sf), add=T, col="red")
plot(st_geometry(hamilton_buf), add=T, lwd = 2)



all_sel_sf <- bind_rows(allegheny_sel_sf,essex_sel_sf, hamilton_sel_sf, orange_sel_sf, orleans_sel_sf, riverside_sel_sf,
                        stl_sel_sf, tarrant_sel_sf, wayne_sel_sf)

all_sel_sf <- all_sel_sf %>%
  mutate(
    stay = ("Y")
  ) %>%
  as_tibble() %>%select(-(geometry))

tracts_1990_all <- left_join(tract_1990_sf, all_sel_sf) %>%
  mutate(stay = replace_na(stay,"N"))

tracts_1990_all$stay <- factor(tracts_1990_all$stay, levels = c('Y','N'))

fct_count(tracts_1990_all$stay)

# write_csv(tracts_1990_all, "tracts_1990_remove.csv")


1585/2375
790/2375
