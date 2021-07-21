## uhh_db

### Data Validation for snmarkley
[See this Link for More Information](https://github.com/snmarkley1/HIST_HU_URB)


scripts:

shapes:

*SHAPEFILE 1*
final_10_sf\
<metadata>\
GISJOIN: 2010 GISJOIN code (from NHGIS)\
HU2010: 2010 HU from 2010 Census\
HU2000_10ts: 2000 HU (NHGIS normalized to 2010 tracts)\
HU1900_10ts: 1990 HU (NHGIS normalized to 2010 tracts)\
ham_est00: Hammer estimates for 2000 using 2010 YSB tract data [“built before 1940” + “built 1940-1949” + “built 1950 + 1959” + ,,, + ”built 1990-1999”] & 2000 county HU counts data\
ham_est90: Hammer estimates for 1990 using 2010 YSB data & 1990 county HU counts data\

*SHAPEFILE 2*
final_00_sf 
<metadata>
GISJOIN = 2000 Census Tract
HU2000_00 = 2000 HU counts from 2000 census
HU90_00 = 1990 HU counts from 2000 census [YSB: “built before 1940” + “built 1940-1949” + ,,, + ”built 1980-1989”]
stay = indicator of whether tract is in 2/3 that stay or 1/3 that get removed
Y indicates tract is 2/3 that stays


*SHAPEFILE 3*
final_90_sf
<metadata>
GISJOIN = 1990 Census Tract
HU1990_90 = 1990 HU counts from 1990 census
stay = indicator of whether tract is in 1/3 that stay or 2/3 that get removed (Y indicates tract is 1/3 that stays)
           

*SHAPEFILE 4*
<metadata>
GISJOIN: 2010 BGs code from NHGIS
HU1990_10: 1990 HU (NHGIS normalized to 2010 BGs)
HU2000_10: 2000 HU (NHGIS normalized to 2010 BGs)


csv: 