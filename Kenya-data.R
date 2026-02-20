library(tidyverse)
library(data.table)
library(sf)
library(dplyr)
library(rnaturalearth)
library(terra)


#### 1. ---- Precipitation & Temperature Monthly Anomalies Data ---- ####
##### A. Precip data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Precip-and-Temp-Monthly-Anomalies')
precip_df <- fread(paste(path, 'Precipitation_Monthly_Anom.csv',sep = '/'))
colnames(precip_df)[1:2] <- c("long", "lat")

##--- 2. convert data to points
precip_pts <- st_as_sf(precip_df,
                coords = c("long", "lat"),
                crs = 4326,
                remove = FALSE)

##--- 3. bbox for precip pts & kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya <- st_transform(kenya, st_crs(precip_pts))
bbox_ke <- st_bbox(kenya) #geting bbox

precip_bbox <- precip_pts %>%
  filter(
    .data[["long"]] >= bbox_ke["xmin"], .data[["long"]] <= bbox_ke["xmax"],
    .data[["lat"]] >= bbox_ke["ymin"], .data[["lat"]] <= bbox_ke["ymax"]
  )

##--- 4. Get points inside kenya
precip_in_ke <- precip_bbox[lengths(st_within(precip_bbox, st_make_valid(kenya))) > 0, ]

##--- 5. Save subset
file_name = paste(path, 'precip_in_ke.csv',sep = '/')
fwrite(precip_in_ke,file_name)

##### B. Temp data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Precip-and-Temp-Monthly-Anomalies')
temp_df <- fread(paste(path, 'Temp_Monthly_Anom.csv',sep = '/'))
colnames(temp_df)[1:2] <- c("long", "lat")

##--- 2. convert data to points
temp_pts <- st_as_sf(temp_df,
                       coords = c("long", "lat"),
                       crs = 4326,
                       remove = FALSE)

##--- 3. bbox for temp pts & kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya <- st_transform(kenya, st_crs(temp_pts))
bbox_ke <- st_bbox(kenya) #geting bbox

temp_bbox <- temp_pts %>%
  filter(
    .data[["long"]] >= bbox_ke["xmin"], .data[["long"]] <= bbox_ke["xmax"],
    .data[["lat"]] >= bbox_ke["ymin"], .data[["lat"]] <= bbox_ke["ymax"]
  )

##--- 4. Get points inside kenya
temp_in_ke <- temp_bbox[lengths(st_within(temp_bbox, st_make_valid(kenya))) > 0, ]

##--- 5. Save subset
file_name = paste(path, 'temp_in_ke.csv',sep = '/')
fwrite(temp_in_ke,file_name)

#-----------------------------------------------------------------------------#
#### 2. ---- Land Cover Classification Data ---- ####
##### A. 2000 data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Land-Cover-Classification-2000-through-2020')
lc_2000 <- rast(paste(path, "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc", sep = '/'))        

##--- 2. Intersect with kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya_v <- vect(kenya)
kenya_v <- project(kenya_v, crs(lc_2000)) #make same CRS

crop_2000 <- crop(lc_2000, kenya_v)  # quick spatial crop (bounding box)
kenya_2000 <- mask(crop_2000, kenya_v)     # exact mask to country outline


## extract lc values 
lc <- fread(paste(path, 'land_cover_classification.csv',sep = '/'))
colnames(lc)[1] <- names(kenya_2000)[1]

lcdat <- kenya_2000[["lccs_class"]]
lcdat_f <- as.factor(lcdat)

levels(lcdat_f) <- list(lc)
plot(lcdat_f)
writeRaster(lcdat_f, paste(path,"kenya_2000_labeled.tif",sep = '/'), overwrite = TRUE)

##### B. 2008 data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Land-Cover-Classification-2000-through-2020')
lc_2008 <- rast(paste(path, "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2008-v2.0.7cds.nc", sep = '/'))        

##--- 2. Intersect with kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya_v <- vect(kenya)
kenya_v <- project(kenya_v, crs(lc_2008)) #make same CRS

crop_2008 <- crop(lc_2008, kenya_v)  # quick spatial crop (bounding box)
kenya_2008 <- mask(crop_2008, kenya_v)     # exact mask to country outline

##--- 3. testing data to try to make this data set the same spatial scope as the temp/precip
kenya_2008_wgs84 <- project(kenya_2008, "EPSG:4326")
df2008_lc <- as.data.frame(kenya_2008_wgs84, xy = TRUE, na.rm = TRUE)
colnames(df2008_lc)[1:3] <- c("long", "lat", "lc_class_2008")

##---4. Write CSV
out_csv <- file.path(path, "lc_2008.csv")
fwrite(df_lc, out_csv)

##### C. 2015 data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Land-Cover-Classification-2000-through-2020')
lc_2015 <- rast(paste(path, "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.nc", sep = '/'))        

##--- 2. Intersect with kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya_v <- vect(kenya)
kenya_v <- project(kenya_v, crs(lc_2015)) #make same CRS

crop_2015 <- crop(lc_2015, kenya_v)  # quick spatial crop (bounding box)
kenya_2015 <- mask(crop_2015, kenya_v)     # exact mask to country outline


##### D. 2016 data ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Land-Cover-Classification-2000-through-2020')
lc_2016 <- rast(paste(path, "C3S-LC-L4-LCCS-Map-300m-P1Y-2016-v2.1.1.nc", sep = '/'))        

##--- 2. Intersect with kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya_v <- vect(kenya)
kenya_v <- project(kenya_v, crs(lc_2016)) #make same CRS

crop_2016 <- crop(lc_2016, kenya_v)  # quick spatial crop (bounding box)
kenya_2016 <- mask(crop_2016, kenya_v)     # exact mask to country outline


#quick look at data classes
vals2000 <- values(kenya_2000$lccs_class, mat = FALSE)  
vals2000 <- sort(unique(na.omit(vals2000)))

vals2008 <- values(kenya_2008$lccs_class, mat = FALSE)  
vals2008 <- sort(unique(na.omit(vals2008)))

vals2015 <- values(kenya_2016$lccs_class, mat = FALSE)  
vals2015 <- sort(unique(na.omit(vals2015)))

vals2016 <- values(kenya_2016$lccs_class, mat = FALSE)  
vals2016 <- sort(unique(na.omit(vals2016)))

#quick thought: could look at change in classes each year amount %cropland lost?

#-----------------------------------------------------------------------------#
#### 3. ---- Conflict Events Data ---- ####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
conflict_df <- fread(paste(path, 'Africa_1997-2023_Aug04-2.csv',sep = '/'))

##--- 2. Filter Kenya
conflict_kenya <- conflict_df %>% filter(COUNTRY == "Kenya")

##--- 3. Save subset
file_name = paste(path, 'conflict_kenya.csv',sep = '/')
fwrite(conflict_kenya,file_name)

# # #how much of this data is conflict regarding farmers/herders?
# pattern <- "(farm|herd|cattle|crop|livestock|flock|harvest|graz|pasture|plantation|stock|agricult|cultivat|plants|animal|ranch|water)"
# 
# farmconflict_kenya <- conflict_kenya %>%
#   filter(
#     if_any(everything(), ~ str_detect(as.character(.x), regex(pattern, ignore_case = TRUE)))
#   )

#-----------------------------------------------------------------------------#
#### 4. ---- Rainfed area ---- ####
##### A. Crop ha #####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Rainfed-area')
cropha_df <- fread(paste(path, 'annual_area_harvested_rainfed_allcrops_ha.csv',sep = '/'))
colnames(cropha_df)[1:2] <- c("long", "lat")

##--- 2. convert data to points
cropha_pts <- st_as_sf(cropha_df,
                       coords = c("long", "lat"),
                       crs = 4326,
                       remove = FALSE)

##--- 3. bbox for precip pts & kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya <- st_transform(kenya, st_crs(cropha_pts))
bbox_ke <- st_bbox(kenya) #geting bbox

cropha_bbox <- cropha_pts %>%
  filter(
    .data[["long"]] >= bbox_ke["xmin"], .data[["long"]] <= bbox_ke["xmax"],
    .data[["lat"]] >= bbox_ke["ymin"], .data[["lat"]] <= bbox_ke["ymax"]
  )

##--- 4. Get points inside kenya
cropha_in_ke <- cropha_bbox[lengths(st_within(cropha_bbox, st_make_valid(kenya))) > 0, ]

##--- 5. Save subset
file_name = paste(path, 'cropha_in_ke.csv',sep = '/')
fwrite(cropha_in_ke,file_name)

##### B. Crop ha 30m #####
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Rainfed-area')
cropha30_df <- fread(paste(path, 'annual_area_harvested_rainfed_allcrops_ha_30mn.csv',sep = '/'))
colnames(cropha30_df)[1:2] <- c("long", "lat")

##--- 2. convert data to points
cropha30_pts <- st_as_sf(cropha30_df,
                       coords = c("long", "lat"),
                       crs = 4326,
                       remove = FALSE)

##--- 3. bbox for precip pts & kenya
kenya <- ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
kenya <- st_transform(kenya, st_crs(cropha30_pts))
bbox_ke <- st_bbox(kenya) #geting bbox

cropha30_bbox <- cropha30_pts %>%
  filter(
    .data[["long"]] >= bbox_ke["xmin"], .data[["long"]] <= bbox_ke["xmax"],
    .data[["lat"]] >= bbox_ke["ymin"], .data[["lat"]] <= bbox_ke["ymax"]
  )

##--- 4. Get points inside kenya
cropha30_in_ke <- cropha30_bbox[lengths(st_within(cropha30_bbox, st_make_valid(kenya))) > 0, ]

plot(cropha30_in_ke)

##--- 5. Save subset
file_name = paste(path, 'cropha_in_ke30.csv',sep = '/')
fwrite(cropha30_in_ke,file_name)


