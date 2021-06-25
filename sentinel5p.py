
#Load library
import gdal
import folium
import rasterio as rio
from shapely.geometry import box
import geopandas as gpd
import os
os.chdir('D:/ENEA_CAS_WORK/sentinel5p/outputs')
os.getcwd()

#Open raster file
driver=gdal.GetDriverByName('GTiff')
driver.Register()

#### set month
MONTH = 'NOVEMBER'  # FEBRUARY, MAY, AUGUST, NOVEMBER
ds = gdal.Open('NO2_Sentinel5p_' + MONTH + '.tif')

if ds is None:
    print('Could not open')

#Get coordinates, cols and rows
geotransform = ds.GetGeoTransform()
cols = ds.RasterXSize
rows = ds.RasterYSize

#Get extent
xmin=geotransform[0]
ymax=geotransform[3]
xmax=xmin+cols*geotransform[1]
ymin=ymax+rows*geotransform[5]

#Get Central point
centerx=(xmin+xmax)/2
centery=(ymin+ymax)/2

#Raster convert to array in numpy
bands = ds.RasterCount
band=ds.GetRasterBand(1)
dataset= band.ReadAsArray(0,0,cols,rows)
dataimage=dataset
dataimage[dataimage[:,:]==-340282346638528859811704183484516925440.000]=0

########################################################
### create a rectangle around the extent of the shp file

# raster = rio.open("NO2_Sentinel5p_FEBRUARY.tif")
raster = rio.open('NO2_Sentinel5p_' + MONTH + '.tif')
bounds  = raster.bounds

geom = box(*bounds)
# print(geom.wkt)
# border = gpd.GeoDataFrame({"id":1,"geometry":[box(*bounds)]}, crs={'init': 'epsg:4326'})
# hex_grid = gpd.GeoDataFrame({'geometry': array_of_hexes}, crs={'init': 'epsg:4326'})


#Visualization in folium
# map= folium.Map(location=[centery, centery], zoom_start=7,tiles='Stamen Terrain')
################################################################################
# create basemap CATANIA
ave_LAT = 37.510284
ave_LON = 15.092042
map = folium.Map([ave_LAT, ave_LON], zoom_start=8, tiles='cartodbpositron')
#################################################################################
#
# style_hex = {'fillColor': '#00000000', 'color': '#00FFFFFF'}
#
# folium.GeoJson(
#     # data to plot
#     border[['id', 'geometry']].to_json(),
#     show=True,
#     style_function=lambda x:style_hex,
#     highlight_function=lambda x: {'weight': 1,
#                                   'color': 'yellow',
#                                   'fillOpacity': 1
#                                   },
#     # fields to show
#     tooltip=folium.features.GeoJsonTooltip(
#         fields=['id']
#     ),
# ).add_to(map)


folium.raster_layers.ImageOverlay(
    image=dataimage,
    opacity=0.8,
    bounds=[[ymin, xmin], [ymax, xmax]],
    colormap=lambda x: (1, 0, x, x),#R,G,B,alpha
).add_to(map)

#Save html
map.save('NO2_' + MONTH + '_2019_Sentinel5p.html')

