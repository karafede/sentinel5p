
rm(list = ls())

library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(gdalUtils)
library(rgdal)
library(gstat)
library(sf)
# devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)


### check also this one
## http://students.eagle-science.org/r-processing-toolbox-for-sentinel-5p-data/

dir <- "D:/ENEA_CAS_WORK/sentinel5p/sicily_shapefile"
### shapefile for Sicily
shp_SICILY <- readOGR(dsn = dir, layer = "sicily")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_SICILY <- spTransform(shp_SICILY, CRS("+init=epsg:4326"))

shp_SICILY@data$name <- 1:nrow(shp_SICILY)
plot(shp_SICILY)
e <- extent(shp_SICILY)

plot(e)

# make a spatial polygon from the extent
p <- as(e, "SpatialPolygons")
plot(p)
proj4string(p) <- CRS("+proj=longlat +datum=WGS84")
# crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN
setwd("D:/ENEA_CAS_WORK/sentinel5p/sicily_shapefile")
shapefile(p, "rectangular_domain.shp", overwrite=TRUE)

# reload and plot domain

dir <- "D:/ENEA_CAS_WORK/sentinel5p/sicily_shapefile"
shp_rect <- readOGR(dsn = dir, layer = "rectangular_domain")
# ----- Transform to EPSG 4326 - WGS84 (required)
shp_rect <- spTransform(shp_rect, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shp_rect@data$name <- 1:nrow(shp_rect)
plot(shp_rect)

# Define an area of interest (AOI):
# Use the example AOI or draw an AOI by calling set_aoi():
set_aoi(shp_rect)
view_aoi()


################################################################################################
################################################################################################
#### download SENTINEL 5p data from ESA ########################################################

login_CopHub(username = "karafede")
services()
get_products()
# Query all available records for multiple products and a given time range at once,
# for example for Sentinel5p:
records <- get_records(time_range = c("2019-02-01", "2019-02-28"),
                       products = c("sentinel-5p"))
records <- get_records(time_range = c("2019-05-01", "2019-05-31"),
                       products = c("sentinel-5p"))
records <- get_records(time_range = c("2019-08-01", "2019-08-31"),
                       products = c("sentinel-5p"))
records <- get_records(time_range = c("2019-11-01", "2019-11-30"),
                       products = c("sentinel-5p"))

# records <- as.data.frame(records)
records <- records[records$level == "L2",]
records <- records[records$product_type == "L2__NO2___",]


# to_keep <- as.vector(records$record_id[grep(pattern  = "_OFFL_", x = records$record_id)])
# records <- records %>%
#   filter(record_id %in% to_keep)


# view_records(records)
###### download all records:
get_data(records)

###################################################################################
###################################################################################
###################################################################################
###################################################################################

output_dir <- "D:/ENEA_CAS_WORK/sentinel5p/outputs"

### setup month to analyze
MONTH <- "NOVEMBER"

# load all nc files
setwd(paste0("D:/ENEA_CAS_WORK/sentinel5p/ncfiles_new_datasets_", MONTH, "_2019/sentinel-5p"))
# setwd("D:/ENEA_CAS_WORK/sentinel5p/ncfiles_new_datasets_May_2019/sentinel-5p")
# setwd("D:/ENEA_CAS_WORK/sentinel5p/ncfiles_new_datasets_August_2019/sentinel-5p")
# setwd("D:/ENEA_CAS_WORK/sentinel5p/ncfiles_new_datasets_November_2019/sentinel-5p")


## REFERENCE raster
reference <- raster("D:/ENEA_CAS_WORK/sentinel5p/NO2_Sentinel5p_REF.tif")
plot(reference)


patt <- ".nc"
filenames <- list.files(pattern = patt)

# filenames <- filenames[1:3]


#### initialize and empty stacked raster
all_rasters <- stack()  

# i <- 2

for (i in 1:length(filenames)) { 

seviri_file <- nc_open(filenames[i])
name_vari <- names(seviri_file$var)
  print(i)
  LON <- ncvar_get(seviri_file, "PRODUCT/longitude")
  LAT <- ncvar_get(seviri_file, "PRODUCT/latitude")
  NO2 <- ncvar_get(seviri_file, "PRODUCT/nitrogendioxide_tropospheric_column")
  ncol <- ncol(LON)
  nrow <- nrow(LON)
  LON <- as.vector(LON)
  LAT <- as.vector(LAT)
  NO2 <- as.vector((NO2))

  LAT <- matrix(LAT,nrow=nrow,ncol=ncol,byrow=TRUE)
  # LAT <- t(LAT)
  LON <- matrix(LON,nrow=nrow,ncol=ncol,byrow=TRUE)
  NO2 <- matrix(NO2,nrow=nrow,ncol=ncol,byrow=TRUE)
  
  xmn= min(LON)
  xmx=max(LON)
  ymn=min(LAT)
  ymx=max(LAT)
     


  LAT <- c(LAT)
  LON <- c(LON)
  M <- c(NO2)
  
  # make a vector with lat, lon and data
  data <- cbind(LON, LAT, M)
  data <- as.data.frame(data)
  # subset data for the emirates
  # data <- subset(data, LON <= 58 & LON >= 50 & LAT >= 21 & LAT <= 28)
   colnames(data) <- c("Lon", "Lat","M")

   xmn= min(data$Lon)
   xmx=max(data$Lon)
   ymn=min(data$Lat)
   ymx=max(data$Lat)
   
   resl_ras <- 0.1  ## 10 km
   colnames(data) <- c('x', 'y', 'z')

  x.range <- as.numeric(c(xmn, xmx))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(ymn, ymx))  # min/max latitude of the interpolation area
     
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                        y = seq(from = y.range[1]*0.95, to = y.range[2]*0.95, by = resl_ras))  # expand points to grid
     
  grd_1<- dplyr::filter(grd, grd$x == grd$x[1])
  nrow(grd_1)
  grd_2<- dplyr::filter(grd, grd$y == grd$y[1])
  nrow(grd_2)

  r <- raster(xmn=min(data$x), xmx=max(data$x), ymn=min(data$y),
              ymx=max(data$y), ncol=nrow(grd_2), nrow= nrow(grd_1))

  r <- rasterize(data[, 1:2], r, data[,3], fun=mean)
  projection(r) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(r)
  ### crop all over the SICILY only
  # r <- crop(r, extent(shp_SICILY))
  # r <- mask(r, shp_SICILY) 
  r <- crop(r, extent(shp_rect))
  r <- mask(r, shp_rect) 
  # plot(r)
  # plot(shp_SICILY, add=TRUE, lwd=1)
  
  # # reproject each raster with the same extent and resolution of the reference raster above
  r = projectRaster(r, reference)
  all_rasters<- stack(all_rasters,r)
  ## save stacked plot
  writeRaster(all_rasters, paste0(output_dir,"/", "stacked_NO2_Sentinel5p_", MONTH, ".tif") , options= "INTERLEAVE=BAND", overwrite=T)
  sum_rasters <- sum(all_rasters, na.rm = TRUE)
  # plot(sum_rasters)
  writeRaster(sum_rasters, paste0(output_dir,"/", "NO2_Sentinel5p_", MONTH, ".tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  # return(sum_rasters)
  # writeRaster(r, "NO2_Sentinel5p.tif", options= "INTERLEAVE=BAND", overwrite=T)
}


### reload file

stacked_pots <- stack(paste0(output_dir,"/","stacked_NO2_Sentinel5p_", MONTH, ".tif"))


 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 #######################################################################
 
MONTH <- "FEBRUARY" ## FEBRUARY, NOVEMBER, MAY, AUGUST
r <- raster(paste0(output_dir,"/", "NO2_Sentinel5p_", MONTH, ".tif"))
MAX <- r@data@max  # FEBRUARY:0.001625379 , MAY:0.003895839, AUGUST:0.01805374  , NOVEMBER:0.01732319
MAX <- 0.01805374
# r <- sum(stacked_pots)
# r <- crop(r, extent(shp_UAE))
# r <- mask(r, shp_UAE)
#### NORMALIZE DATA
r <- r/MAX
plot(r)
plot(shp_SICILY, add = TRUE)


library(leaflet)

map <- leaflet() %>%
 addTiles() %>%
 # addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
 addRasterImage(r, opacity = 0.4, group = "SEVIRI")
# addLayersControl(
#   baseGroups = c("Toner"),
#   overlayGroups = c("SEVIRI"))
map
 
 
#########################################################################################
 