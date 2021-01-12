####Required Libraries####
library(raster)
library(rgdal)
library(daymetr)
library(gdalUtils)
library(bcdata)
library(sf)
library(readr)
library(stars)

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
stream_pth <- "C:/Data/SSN/aug2020_mat_topos/parsnip_streams.gpkg"
#temp_pth<-readline(prompt="Please enter the path to gridded temperature data: ")
temp_pth <- "C:/Data/SSN/aug2020_mat_topos/air_aug_monthly_average0m.augmean [Universal Kriging].tif"

#### Create directories in which to store preppred attribute layers ####
dir.create("C:/Code/BC_openSTARS_SSN/Data")
dir.create("C:/Code/BC_openSTARS_SSN/Data/DEM")
dir.create("C:/Code/BC_openSTARS_SSN/Data/Sites")
dir.create("C:/Code/BC_openSTARS_SSN/Data/FieldObs")
dir.create("C:/Code/BC_openSTARS_SSN/Data/Streams")
dir.create("C:/Code/BC_openSTARS_SSN/Data/PredVect")

#Provide study extent -> vector (length=4; order= xmin, xmax, ymin, ymax)
dem <- raster("C:/Data/SSN/aug2020_mat_topos/parsnip.tif")
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

####Load and write primary DEM####
dem <- raster::raster(dem_pth)
raster::compareCRS(raster::crs(dem),bcalb)==F
raster::writeRaster(dem, filename = 'Data/DEM/dem.tif',overwrite=T)


####Load and write stream site observations ####

sites<-st_read(sites_pth)
st_crs(sites)!=st_crs(bcalb)
sf::write_sf(sites,"Data/Sites/sites.gpkg",overwrite=T)


#### Load and write krige map - check proj
interp_temp <- read_stars(temp_pth)
st_crs(interp_temp)
interp_temp <- interp_temp %>%
  st_transform(crs = 3005)
write_stars(interp_temp,"C:/Code/BC_openSTARS_SSN/Data/FieldObs/interp_temp.tif")


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
                                     filter = list(gnis_name = "Parsnip River"))
bbox <- sf::st_bbox(wshed)
             
stream_orders <- 3:9
all <- do.call(rbind, lapply(stream_orders, function(x){
               message(glue::glue("getting stream order {x}"))
               fwa_collection("whse_basemapping.fwa_stream_networks_sp", 
                              bbox = bbox, 
                              filter = list(stream_order = x),
                              limit = 10000)
             }))
             
parsnip <- sf::st_intersection(all, wshed)
             
mapview(parsnip)
             
             
parsnip <- parsnip %>%
      st_transform(crs = 3005)
             
stream_vect <- parsnip

stream_vect <- stream_vect %>%
  select(gradient, length_metre, geometry)

mapView(stream_vect)

stream_vect<-sf::st_read(stream_pth)
st_crs(stream_vect)!=st_crs(bcalb)
sf::write_sf(stream_vect,paste('Data/Streams/fresh_water_atlas.shp'))
             
             