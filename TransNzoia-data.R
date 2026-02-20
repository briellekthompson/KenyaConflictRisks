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

##--- 3. bbox for precip pts & TN
TransNzoia <- read_sf(here::here('GWSC-Interview-Data', 'Shapefile', 'TransNzoia.shp'))
TransNzoia <- st_transform(TransNzoia, st_crs(precip_pts))
bbox_tn <- st_bbox(TransNzoia) #geting bbox

precip_bbox <- precip_pts %>%
  filter(
    .data[["long"]] >= bbox_tn["xmin"], .data[["long"]] <= bbox_tn["xmax"],
    .data[["lat"]] >= bbox_tn["ymin"], .data[["lat"]] <= bbox_tn["ymax"]
  )

##--- 4. Get points inside kenya
precip_in_tn <- precip_bbox[lengths(st_within(precip_bbox, st_make_valid(TransNzoia))) > 0, ]

##--- 5. Save subset
file_name = paste(path, 'precip_in_tn.csv',sep = '/')
fwrite(precip_in_tn,file_name)

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
TransNzoia <- read_sf(here::here('GWSC-Interview-Data', 'Shapefile', 'TransNzoia.shp'))
TransNzoia <- st_transform(TransNzoia, st_crs(precip_pts))
bbox_tn <- st_bbox(TransNzoia) #geting bbox

temp_bbox <- temp_pts %>%
  filter(
    .data[["long"]] >= bbox_tn["xmin"], .data[["long"]] <= bbox_tn["xmax"],
    .data[["lat"]] >= bbox_tn["ymin"], .data[["lat"]] <= bbox_tn["ymax"]
  )

##--- 4. Get points inside kenya
temp_in_tn <- temp_bbox[lengths(st_within(temp_bbox, st_make_valid(TransNzoia))) > 0, ]

##--- 5. Save subset
file_name = paste(path, 'temp_in_tn.csv',sep = '/')
fwrite(temp_in_tn,file_name)

#-----------------------------------------------------------------------------#

#### 2. ---- Conflict Events Data ---- ####
#I did this in ArcGIS 
##--- 1. Read in data
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
conflict_TN_df <- fread(paste(path, 'conflict_tn.csv',sep = '/'))

length(conflict_TN_df$EVENT_ID_CNTY) #should be 169

# # #how much of this data is conflict regarding farmers/herders?
pattern <- "(farm|herd|cattle|crop|livestock|flock|harvest|graz|pasture|plantation|stock|agricult|cultivat|plants|animal|ranch|water)"

farmconflict_tn <- conflict_TN_df %>%
  filter(
    if_any(everything(), ~ str_detect(as.character(.x), regex(pattern, ignore_case = TRUE)))
  )

length(farmconflict_tn$EVENT_ID_CNTY) #should be 35

#-----------------------------------------------------------------------------#





