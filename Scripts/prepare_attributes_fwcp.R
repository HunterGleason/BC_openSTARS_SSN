####Required Libraries####
library(raster)
library(rgdal)
library(daymetr)
library(gdalUtils)
library(bcdata)
library(sf)
library(readr)
library(stars)
library(bcmaps)

####Declare Global Vars (REQUIRED)####

#Year of study
year = strtoi(readline(prompt="Enter year corresponding to stream site data: \n"))

#Month of study
month = strtoi(readline(prompt="Enter month corresponding to stream site data as integer (1-12): \n"))

#Prompt user for paths to study data, will be organized for processing with 'openSTARS' package. 

#dem_pth<-readline(prompt="Enter path to digital elevation model encompassing study area, recommended ~1000 m buffer: \n")
dem_pth <- "C:/Data/SSN/aug2020_mat_topos/parsnip.tif"
#sites_pth<-readline(prompt = "Enter path to vector layer with stream site observations, e.g., sites_2018.gpkg: \n")
sites_pth <- "C:/Data/SSN/aug2020_mat_topos/aug2020_wt_monthly_average.gpkg"
#stream_pth<-readline(prompt="Enter path of a vector stream layer, or leave blank to download the BC Freshwater Atlas Layer: \n")
stream_pth <- "C:/Data/SSN/aug2020_mat_topos_nowilliston/parsnip_streams.gpkg"
#temp_pth<-readline(prompt="Please enter the path to gridded temperature data: ")
temp_pth <- "C:/Data/SSN/aug2020_mat_topos/air_aug_monthly_average0m.augmean [Universal Kriging].tif"

#### Create directories in which to store preppred attribute layers ####
dir.create("C:/Code/BC_openSTARS_SSN/Data")
dir.create("C:/Code/BC_openSTARS_SSN/Data/DEM")
dir.create("C:/Code/BC_openSTARS_SSN/Data/Sites")
dir.create("C:/Code/BC_openSTARS_SSN/Data/FieldObs")
dir.create("C:/Code/BC_openSTARS_SSN/Data/Streams")
dir.create("C:/Code/BC_openSTARS_SSN/Data/PredVect")
dir.create("C:/Code/BC_openSTARS_SSN/DataUTM10")
#Provide study extent -> vector (length=4; order= xmin, xmax, ymin, ymax)
dem <- raster(dem_pth)
crs(dem)
e <- extent(dem)
# e<-as.double(unlist(strsplit(readline(prompt="Enter coordinates of study extent as -> xmin,xmax,ymin,ymax, must be EPSG:3005 coordinates: "),",")))

xmin<-e[1]
xmax<-e[2]
ymin<-e[3]
ymax<-e[4]

e<-raster::extent(xmin,xmax,ymin,ymax)

e_sf<- sf::st_bbox(c(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),crs = st_crs(3005))
e_sf<-sf::st_as_sfc(e_sf)

#PRJ4 for EPSG:4326 and EPSG:3005
wgs84<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
bcalb<-"+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
utm <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

####Load and write stream site observations ####

sites<-st_read(sites_pth)
st_crs(sites)!=st_crs(bcalb)
sf::write_sf(sites,"Data/Sites/sites.shp",overwrite=T)

#rewrite sites to utm10
sites <- st_read("HuntData/parsnip_sites_bcalb.shp")
sites <- st_transform(sites, crs = utm)
st_crs(sites)
st_write(sites, "DataUTM10/parsnip_sites_utm.shp")

#trophic window data ----
sites <- st_read("DataUTM10/trophic_2019_metrics.shp")
st_crs(sites)
sites <- sites %>%
  st_transform(crs = 26910)
st_write(sites, "DataUTM10/trophic_2019_metrics.shp", append = FALSE)

sites <- st_read("somedir/DataUTM10/trophic_2020_metrics.shp")
st_crs(sites)
sites <- sites %>%
  st_transform(crs = 26910)
st_write(sites, "somedir/Data/Sites/sites2020.shp")

####Load and write primary DEM####
#read in manually prepared DEM
dem <- raster::raster(dem_pth)
raster::projection(dem)
raster::compareCRS(raster::crs(dem),bcalb, unknown = T)
dem <- raster::projectRaster(dem,crs=bcalb)
plot(dem)
raster::writeRaster(dem, filename = 'Data/DEM_manual/dem.tif',overwrite=T)

# trophic window data -----
dem <- raster::raster("DataUTM10/parsnip_utm.tif") 
raster::projection(dem)
dem <- raster::projectRaster(dem,crs=utm)
raster::writeRaster(dem, filename = "somedir/Data/DEM/parsnip_utm10.gtif", format = "GTiff", overwrite=T)

# extract dem using bcmaps::cded_raster

memory.limit(50000)
bbox <- st_bbox(sites)
aoi <- st_as_sfc(bbox)
aoi_buf <- st_buffer(aoi, dist = 20000)
aoi_raster <- cded_raster(aoi_buf)
  
plot(aoi_raster)
raster::crs(aoi_raster)
aoi_raster <- projectRaster(aoi_raster,crs=bcalb)
raster::compareCRS(raster::crs(x),bcalb, unknown = T)
plot(aoi_raster)
raster::writeRaster(aoi_raster, filename = 'Data/DEMgeo/dem.gtif', format = 'GTiff', overwrite=T)


x <- raster('Data/DEMgeo/dem.tif')
raster::crs(x)

# write dem to utm10
dem <- raster("HuntData/parsnip_3005.tif")
dem <- projectRaster(dem, crs = utm)
raster::crs(dem)
plot(dem)
writeRaster(dem, filename = "DataUTM10/parsnip_utm.tif", format = 'GTiff')


#### Load and write krige map - check proj
interp_temp <- raster::raster(temp_pth)
raster::projection(interp_temp)
raster::compareCRS(raster::crs(interp_temp),bcalb)==F
interp_temp<-projectRaster(interp_temp,crs=bcalb)

plot(interp_temp)
raster::writeRaster(interp_temp, filename = 'Data/FieldObs/interp_temp.tif',overwrite=T)

#write temp raster to utm10
interp_temp <- raster::raster(temp_pth)
memory.limit(50000)
interp_temp <- projectRaster(interp_temp, crs = utm)
raster::crs(interp_temp)
plot(interp_temp)
writeRaster(interp_temp, filename = "DataUTM10/airtemp_utm.tif", format = 'GTiff')

###trophic window temp raster data ----
interp_temp <- raster::raster("DataUTM10/trophic_2019_atawat [Universal Kriging].tif")
raster::projection(interp_temp)
interp_temp <- raster::projectRaster(interp_temp ,crs=utm)
raster::writeRaster(interp_temp, filename = "DataUTM10/trophic_2019_atawat [Universal Kriging].gtif", format = "GTiff", overwrite=T)


interp_temp <- raster::raster("DataUTM10/trophic_2020_atawat [Universal Kriging].tif")
raster::projection(interp_temp)
interp_temp <- raster::projectRaster(interp_temp ,crs=utm)
raster::writeRaster(interp_temp, filename = "DataUTM10/trophic_2020_atawat [Universal Kriging].gtif", format = "GTiff", overwrite=T)

####Get Stream Network using fwapgr, this layer is used for burning the DEM (REQUIRED)####
             
# Below is code I used to get streams layer - can't write to .shp because of 
# the column names.... so using .gpkg
# remotes::install_github("poissonconsulting/fwapgr")
library(sf)
library(fwapgr)
library(mapview)

# Workaround create by Seb Dalgnaro to extract Parsnip River
# using the fwapgr package while it is still in development
             
             
wshed <- fwa_collection("whse_basemapping.fwa_named_watersheds_poly", 
                                     filter = list(gnis_name = "Anzac River"))
bbox <- sf::st_bbox(wshed)
             
stream_orders <- 3:8
all <- do.call(rbind, lapply(stream_orders, function(x){
               message(glue::glue("getting stream order {x}"))
               fwa_collection("whse_basemapping.fwa_stream_networks_sp", 
                              bbox = bbox, 
                              filter = list(stream_order = x),
                              limit = 10000)
             }))
             
anzac <- sf::st_intersection(all, wshed)
             
mapview(parsnip)
             

stream_vect <- parsnip %>%
  st_transform(crs = 3005) %>%
  select(gradient, length_metre, geometry)

mapView(stream_vect)

st_crs(stream_vect)!=st_crs(bcalb)
sf::st_write(stream_vect,paste('Data/Streams/fresh_water_atlas.shp'))

####Get Stream Network from freshwater atlas, this layer is used for burning the DEM (REQUIRED)####

#Grab freshwater atlas stream data for 'Watershed_Group_Code' from BC data warehouse, crop to extent

FWA_Stream<-bcdata::bcdc_query_geodata('92344413-8035-4c08-b996-65a9b3f62fca', crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::collect() %>%
  dplyr::select(geometry)

if(st_crs(FWA_Stream)!=st_crs(bcalb))
{
  FWA_Stream<-sf::st_transform(FWA_Stream,bcalb)
}

print("Cropping stream network to extent ...")
#Crop FWA stream network to extent 
FWA_Stream<-FWA_Stream %>% sf::st_crop(e)

#Peak
#ggplot(FWA_Stream)+geom_sf()

#Write to stream network to openSTARS data dir
print("Writing stream network to 'Data/Streams/fresh_water_atlas.shp'")
sf::write_sf(FWA_Stream,paste('Data/Streams/fresh_water_atlas.shp',sep = ""))
}else{
  stream_vect<-sf::st_read(stream_pth)
  
  if(st_crs(stream_vect)!=st_crs(bcalb))
  {
    stream_vect<-sf::st_transform(stream_vect,bcalb)
  }
  
  print("Writing stream network to 'Data/Streams/fresh_water_atlas.shp'")
  sf::write_sf(stream_vect,paste('Data/Streams/fresh_water_atlas.shp',sep = ""),delete_layer=T)
}

####Generate a combined freshwater-atlas water bodies layer, using lakes, wetlands and glaciers defined by WATERBODY_TYPE (OPTIONAL)####

##Grab freshwater atlas glacier data for 'Watershed_Group_Code' from BC data warehouse, crop to extent##
#List likely glacier layers, and pull desired layer, or...
#poss_glac<-bcdata::bcdc_list()
#poss_glac[stringr::str_detect(poss_glac,"glaciers")]

print("Downloading Freshwater Atlas glaciers ...")
FWA_Glaciers<-bcdc_query_geodata('8f2aee65-9f4c-4f72-b54c-0937dbf3e6f7', crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::collect()

print("glaciers to extent ...")
FWA_Glaciers<-FWA_Glaciers %>% 
  sf::st_crop(e) %>%
  dplyr::select(WATERBODY_TYPE)



##Grab freshwater atlas wetlands data for 'Watershed_Group_Code' from BC data warehouse, crop to extent###
#List likley wetlands layers, and pull desired layer, or...
#poss_wetl<-bcdata::bcdc_list()
#poss_wetl[stringr::str_detect(poss_wetl,"wetland")]

print("Downloading Freshwater Atlas wetlands ...")
FWA_Wetlands<-bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6", crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::collect()

print("Cropping wetlands to extent ...")
FWA_Wetlands<-FWA_Wetlands %>% 
  sf::st_crop(e) %>%
  dplyr::select(WATERBODY_TYPE)



##Grab freshwater atlas lakes data for 'Watershed_Group_Code' from BC data warehouse, crop to extent##
#List likley lakes layers, and pull desired layer, or...
#poss_lake<-bcdata::bcdc_list()
#[stringr::str_detect(poss_lake,"lake")]


print("Downloading Freshwater Atlas lakes ...")
FWA_Lakes<-bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::collect()

print("Cropping lakes to extent ...")
FWA_Lakes<-FWA_Lakes %>% 
  sf::st_crop(e) %>%
  dplyr::select(WATERBODY_TYPE)


#Combine the three diffrent water body layers
print("joining waterbodies ...")
waterbods <-rbind(as.data.frame(FWA_Lakes),as.data.frame(FWA_Wetlands),as.data.frame(FWA_Glaciers))

#Rename water body type to 'WBT'
colnames(waterbods)[1]<-'WBT'

#Convert back to sf
waterbods <- st_as_sf(waterbods)

#ggplot()+geom_sf(data=FWA_Stream)+geom_sf(data=waterbods,aes(fill = WBT))


if(st_crs(waterbods)!=st_crs(bcalb))
{
  waterbods<-sf::st_transform(waterbods,bcalb)
}


#Write water bodies layer to openSTARS dir
print("Writing Freswater Atlas waterbodies layer to 'Data/PredVect/fresh_water_atlas_waterbods.shp'")
sf::write_sf(waterbods, paste('Data/PredVect/fresh_water_atlas_waterbods.shp',sep = ""))
