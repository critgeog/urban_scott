## uhh_db

### Data Validation Files for HHUDB
[More Information on the Historical Housing and Urbanization Database](https://github.com/snmarkley1/HIST_HU_URB)

**scripts**: Folder contains scripts required to create the shapefiles.

**shapefiles**: Folder contains four shapefiles described below

#### SHAPEFILE 1
[Metadata_shp1](https://github.com/critgeog/urban_scott/tree/master/shapes/tracts_2010)
* GISJOIN: 2010 GISJOIN code (from NHGIS)
* HU2015-19: Housing Units from 2015-19 ACS
* HU2006-10: Housing Units from 2006-10 ACS
* HU2000_10ts: 2000 HU (NHGIS normalized to 2010 tracts -- time series)
* HU1900_10ts: 1990 HU (NHGIS normalized to 2010 tracts -- time series)
* COL05: 2000 County HUs
* COL06: 1990 County HUs
* ham_est00: Hammer estimates for 2000 using 2015-19 YSB tract data & 2000 county HU counts data
* ham_est90: Hammer estimates for 1990 using 2015-19 YSB data & 1990 county HU counts data


#### SHAPEFILE 2
[Metadata_shp2](https://github.com/critgeog/urban_scott/tree/master/shapes/tracts_2000)
* GISJOIN = 2000 Census Tract
* HU2000_00 = 2000 HU counts from 2000 census
* HU90_00 = 1990 HU counts from 2000 census [YSB: “built before 1940” + “built 1940-1949” + ,,, + ”built 1980-1989”]
* stay = indicator of whether tract is in 1/2 that stay or 1/2 that get removed (Y indicates tract is 1/2 that stays)


#### SHAPEFILE 3
[Metadata_shp3](https://github.com/critgeog/urban_scott/tree/master/shapes/tracts_1990)
* GISJOIN = 1990 Census Tract
* HU1990_90 = 1990 HU counts from 1990 census
* stay = indicator of whether tract is in 1/4 that stay or 3/4 that get removed (Y indicates tract is 1/4 that stays)

 
#### SHAPEFILE 4
[Metadata_shp4](https://github.com/critgeog/urban_scott/tree/master/shapes/bgroups_2010)
* GISJOIN: 2010 BGs code from NHGIS
* HU2015-19: Housing Units from 2015-19 ACS
* HU2010: 2010 HU counts from 2015-19 YSB
* HU2000: 2000 HU counts from 2015-19 YSB
* HU1990: 1990 HU counts from 2015-19 YSB


#### SHAPEFILE 5:
[Geography: 2010 tracts]()
* GISJOIN: 2010 GISJOIN code from NHGIS
* HU2006-10: Housing Units from 2006-10 ACS
* HU 2000: Housing Units built before 2000 from 2006-10 ACS YSB
* HU 1990: Housing Units built before 1990 from 2006-10 ACS YSB
* stay: indicator of whether tract is in the 3/4 that stay or the 1/4 that get removed

## Git LFS Resources

[Git LFS 2.2.0 released](https://github.blog/2017-06-27-git-lfs-2-2-0-released/)

[Installing Git Large File Storage](https://docs.github.com/en/github/managing-large-files/versioning-large-files/installing-git-large-file-storage)

[Configuring Git Large File Storage](https://docs.github.com/en/github/managing-large-files/versioning-large-files/configuring-git-large-file-storage)

[Failing to Push Large Files](https://github.com/git-lfs/git-lfs/issues/1933#issuecomment-351275765)

##### steps\
git lfs track “*.gdbtable”\
git lfs track “*.gdb”\
git lfs track “*.csv”\
git add HHUUD_shp.gdb\
git commit -m “add gdb”\
git push
