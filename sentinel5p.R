
rm(list = ls())

library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(gdalUtils)
library(rgdal)
library(gstat)

dir <- "D:/ENEA_CAS_WORK/sentinel5p/sicily_shapefile"
### shapefile for Sicily
shp_SICILY <- readOGR(dsn = dir, layer = "sicily")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_SICILY <- spTransform(shp_SICILY, CRS("+init=epsg:4326"))

shp_SICILY@data$name <- 1:nrow(shp_SICILY)
plot(shp_SICILY)
e <- extent(shp_SICILY)


# e <- extent(50,60,20, 28)  # UAE extent
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


setwd("D:/ENEA_CAS_WORK/sentinel5p")

###################################################################################
###################################################################################
###################################################################################
###################################################################################

output_dir <- "D:/ENEA_CAS_WORK/sentinel5p/ncfiles/outputs"

# load all nc files
setwd("D:/ENEA_CAS_WORK/sentinel5p/ncfiles")


## REFERENCE raster
reference <- raster("D:/ENEA_CAS_WORK/sentinel5p/NO2_Sentinel5p_REF.tif")
plot(reference)


patt <- ".nc"
filenames <- list.files(pattern = patt)
# filenames <- "S5P_NRTI_L2__NO2____20200901T122909_20200901T123409_14955_01_010302_20200901T132207.nc"
# filenames <- "S5P_NRTI_L2__AER_AI_20200831T124909_20200831T125409_14941_01_010302_20200831T132428.nc"

filenames <- "S5P_OFFL_L2__NO2____20190214T112359_20190214T130529_06939_01_010202_20190221T100309.nc"
filenames <- "S5P_OFFL_L2__NO2____20190209T111710_20190209T125840_06868_01_010202_20190215T131301.nc"

filenames <- filenames[1:3]


#### initialize and empty stacked raster
all_rasters <- stack()  

i <- 2

for (i in 1:length(filenames)) { 

seviri_file <- nc_open(filenames[i])
name_vari <- names(seviri_file$var)

  LON <- ncvar_get(seviri_file, "PRODUCT/longitude")
  LAT <- ncvar_get(seviri_file, "PRODUCT/latitude")
  NO2 <- ncvar_get(seviri_file, "PRODUCT/nitrogendioxide_tropospheric_column")
  ncol <- ncol(LON)
  nrow <- nrow(LON)
   # AER_1 <- ncvar_get(seviri_file, "PRODUCT/aerosol_index_340_380" )   #aerosol_index_354_388                                    
  # AER_2 <- ncvar_get(seviri_file, "PRODUCT/aerosol_index_340_380") 
  LON <- as.vector(LON)
  LAT <- as.vector(LAT)
  NO2 <- as.vector((NO2))
  # AER_1 <- as.vector((AER_1))
   
     
  # LAT <- matrix(LAT,nrow=450,ncol=372,byrow=TRUE)
  # # LAT <- t(LAT)
  # LON <- matrix(LON,nrow=450,ncol=372,byrow=TRUE)
  # NO2 <- matrix(NO2,nrow=450,ncol=372,byrow=TRUE)   
  
  LAT <- matrix(LAT,nrow=nrow,ncol=ncol,byrow=TRUE)
  # LAT <- t(LAT)
  LON <- matrix(LON,nrow=nrow,ncol=ncol,byrow=TRUE)
  NO2 <- matrix(NO2,nrow=nrow,ncol=ncol,byrow=TRUE)
  
  # AER_1 <- matrix(AER_1,nrow=450,ncol=372,byrow=TRUE)   
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     


   LAT <- c(LAT)
   LON <- c(LON)
   M <- c(NO2)
   # M <- c(AER_1)
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
   
   resl_ras <- 0.1
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
  ### crop all over the UAE only
  # r <- crop(r, extent(shp_SICILY))
  # r <- mask(r, shp_SICILY) 
  r <- crop(r, extent(shp_rect))
  r <- mask(r, shp_rect) 
  plot(r)
  # plot(shp_SICILY, add=TRUE, lwd=1)
  
  # # reproject each raster with the same extent and resolution of the reference raster above
  r = projectRaster(r, reference)
  all_rasters<- stack(all_rasters,r)
  ## save stacked plot
  writeRaster(all_rasters, paste0(output_dir,"/", "stacked_NO2_Sentinel5p_FEBRUARY.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  sum_rasters <- sum(all_rasters, na.rm = TRUE)
  plot(sum_rasters)
  writeRaster(sum_rasters, paste0(output_dir,"/", "NO2_Sentinel5p_FEBRUARY.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  # return(sum_rasters)
  # writeRaster(r, "NO2_Sentinel5p.tif", options= "INTERLEAVE=BAND", overwrite=T)
}


### reload file
stacked_pots <- stack(paste0(output_dir,"/","stacked_NO2_Sentinel5p_FEBRUARY.tif"))


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
 
r <- raster(paste0(output_dir,"/", "NO2_Sentinel5p_FEBRUARY.tif"))
# r <- sum(stacked_pots)
# r <- crop(r, extent(shp_UAE))
# r <- mask(r, shp_UAE)
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
 