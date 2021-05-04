####Required Libraries####
library(raster)
library(rgdal)
library(daymetr)
library(gdalUtils)
library(bcdata)
library(sf)
library(readr)

####Declare Global Vars (REQUIRED)####

#Year of study
year = strtoi(readline(prompt="Enter year corresponding to stream site data: \n"))

#Month of study
month = strtoi(readline(prompt="Enter month corresponding to stream site data as integer (1-12): \n"))

#Prompt user for paths to study data, will be organized for processing with 'openSTARS' package. 
dem_pth<-readline(prompt="Enter path to digital elevation model encompassing study area, recommended ~1000 m buffer: \n")
sites_pth<-readline(prompt = "Enter path to vector layer with stream site observations, e.g., sites_2018.gpkg: \n")
roads_pth<-readline(prompt="Enter path of roads vector layer, or leave blank to use digital-road-atlas-dra-master-partially-attributed-roads: \n")
stream_pth<-readline(prompt="Enter path of a vector stream layer, or leave blank to download the BC Freshwater Atlas Layer: \n")
lai_pth<-readline(prompt="Enter path of effective leaf area index (LAI) raster layer (see Google Earth Engine script): \n")
LST_pth <- readline(prompt="Enter path of remotely sensed land surface temperature raster layer (see Google Earth Engine script): \n")
MODIS_SDoff_pth <- readline(prompt="Enter path of MODIS SDoff raster layer: \n")


#Provide study extent -> vector (length=4; order= xmin, xmax, ymin, ymax)
e<-as.double(unlist(strsplit(readline(prompt="Enter coordinates of study extent as -> xmin,xmax,ymin,ymax, must be EPSG:3005 coordinates: "),",")))

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


#Prompt user to use ClimateBC or Daymet or their own raster layers
climate_source<-readline(prompt="Please enter the type of climate data to be used, enter one of: 'ClimateBC', 'Daymet' or 'NONE' if provding own layers : ")

#If providing climate data, prompt for path
if(climate_source=='NONE')
{
  temp_pth<-readline(prompt="Please enter the path to gridded temperature data: ")
  prcp_pth<-readline(prompt="Please enter the path to gridded precipitation data: ")
}

#Prompt user to download RCP Climate data. 
calc_rcp<-readline(prompt="Please enter if you wish to also calculate RCP monthly climate metrics, enter TRUE or FALSE: ")


####Load and crop primary DEM####

#Load DEM from dem_pth using raster package, either from local source, or DataBC
print("Loading and cropping DEM ...")
if(dem_pth!="")
{
  aoi_dem <- raster::raster(dem_pth)
}else{
  #Waiting for CDED package 
  #dem <- cded_get(e_sf)
}

#Project DEM if necessary
if(raster::compareCRS(raster::crs(aoi_dem),bcalb)==F)
{
  aoi_dem<-projectRaster(aoi_dem,crs=bcalb)
}

#Crop to aoi extent if necessary
if(extent(aoi_dem)!=extent(e))
{
  aoi_dem<-raster::crop(aoi_dem,e)
}

#Write DEM to openSTARS DEM data directory 
raster::writeRaster(aoi_dem, filename = 'Data/DEM/dem.tif',overwrite=T)

print("DEM wrote to Data/DEM/dem.tif")


####Load and write stream site observations####

sites<-st_read(sites_pth)

#Reproject if needed 
if(st_crs(sites)!=st_crs(bcalb))
{
  sites<-sf::st_transform(sites,bcalb)
}
  

sf::write_sf(sites,"Data/Sites/sites.gpkg",overwrite=T)

print("Stream site data wrote to Data/Sites/sites.gpkg")

####Get climateBC or Daymet Data####

#If not providing climate data 
if(climate_source!='NONE')
{
  #If user chose to use ClimateBC data 
  if(climate_source=="ClimateBC")
  {
    print("Converting DEM to ClimateBC Format at 600 m resolution.")
    #Need to resample DEM to 600 m resolution (res of climate BC) 
    resam_x<-(e[2]-e[1])/600
    resam_y<-(e[4]-e[3])/600
    resam_<-raster(nrow=resam_y, ncol=resam_x)
    extent(resam_)<-extent(aoi_dem)
    crs(resam_)<-crs(aoi_dem)
    
    #Project DEM to WGS84
    bcclim_dem<-projectRaster(resample(aoi_dem,resam_,method='bilinear'), crs = wgs84)
    #plot(bcclim_dem)
    
    #Get DEM as data frame with XY
    bcclim_dem<-raster::as.data.frame(bcclim_dem,xy=T,na.rm=T,centroids=T)
    
    #Format for input into ClimateBC windows tool as CSV
    lat<-bcclim_dem$y
    lon<-bcclim_dem$x
    el<-bcclim_dem[,3]
    
    #As YXZ
    bcclim_dem<-raster::as.data.frame(cbind(lat,lon,el))
    
    id<-paste(rep('site',length(bcclim_dem$lat)),c(1:length(bcclim_dem$lat)),sep = "")
    id<-cbind(id,rep('region1',length(bcclim_dem$lat)))
    
    bcclim_dem<-base::as.data.frame(cbind(id,bcclim_dem))
    
    #Specified by ClimateBC Tool 
    colnames(bcclim_dem)<-c('ID1','ID2','lat','long','el')
    
    #Convert elevation to integer 
    bcclim_dem$el<-round(bcclim_dem$el)
    
    #Write formatted CSV to 'bcclim_dem.csv'. !!User must now run this file in the ClimateBC tool for appropriate data and time period before continuing!!
    write_csv(bcclim_dem,'Data/PredRast/ClimateBC/CSVs/bcclim_dem.csv')
    
    print("Wrote DEM to 'Data/PredRast/ClimateBC/CSVs/bcclim_dem.csv'. (1) Open and file in EXCEL and save as CSV, do not make any edits (2) then run as input in ClimateBC tool with appropriate fields for year and month and desired dataset, (3) assure ClimateBC output CSV in 'Data/PredRast/ClimateBC/CSVs/' directory. (4) If running a Climate scenerio make sure this CSV is in the 'Data/PredRast/ClimateBC/CSVs/' directory as well.")
    
    hasRun<-readline(prompt='Have you run the ClimateBC tool and put output(s) in the "Data/PredRast/ClimateBC/CSVs/" direcotry, enter TRUE or FALSE?')
    
    
    
    #Check if user has run CliamteBC tool and put output CSV in correct directory,
    if(hasRun=='TRUE')
    {
      climBCname<-readline(prompt='Please provide name of ClimateBC output CSV, e.g., bcclim_dem_Year_2018M.csv:')
      if(calc_rcp=='TRUE')
      {
        climBCRCP<- readline(prompt='Please provide name of ClimateBC scenerio output CSV, e.g., bcclim_dem_CanESM2_rcp45M.csv:')
      }
      
    }else
    {
      print('Run ClimateBC tool and put output CSV into "Data/PredRast/ClimateBC/CSVs/"')
      climBCname<-readline(prompt='Please provide name of ClimateBC output CSV, e.g., bcclim_dem_Year_2018M.csv:')
      if(calc_rcp=='TRUE')
      {
        climBCRCP<- readline(prompt='Please provide name of ClimateBC scenerio output CSV, e.g., bcclim_dem_CanESM2_rcp45M.csv:')
      }
    }
    
    
    #Read in ClimateBC CSV
    print("Reading in ClimateBC output data ...")
    clim_bc_data<-read_csv(paste('Data/PredRast/ClimateBC/CSVs/',climBCname,sep=""))
    
    #Get average temperature and total precipitation for specified month, convert to raster, and write to PredRast directory 
    print("Retreiving temperature and precipitation data for specified month ...")
    if(month<10)
    {
      tmx_stamp<-paste('Tmax0',month,sep="")
      tmn_stamp<-paste('Tmin0',month,sep="")
      tav_stamp<-paste('Tave0',month,sep="")
      p_stamp<-paste('PPT0',month,sep="")
    }else{
      tmx_stamp<-paste('Tmax',month,sep="")
      tmn_stamp<-paste('Tmin',month,sep="")
      tav_stamp<-paste('Tave',month,sep="")
      p_stamp<-t_stamp<-paste('PPT',month,sep="")
    }
    
    #Get temperature data, covert to raster, reproject and write to Data directory 
    print("Writing max temperature data to 'Data/PredRast/ClimateBC/tmax.tif'")
    tmx<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tmx_stamp)],crs=wgs84)
    tmx_p<-projectRaster(tmx, crs = bcalb)
    raster::writeRaster(tmx_p, filename = 'Data/PredRast/ClimateBC/tmax.tif')
    
    print("Writing min temperature data to 'Data/PredRast/ClimateBC/tmin.tif'")
    tmn<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tmn_stamp)],crs=wgs84)
    tmn_p<-projectRaster(tmn, crs = bcalb)
    raster::writeRaster(tmn_p, filename = 'Data/PredRast/ClimateBC/tmin.tif')
    
    print("Writing mean temperature data to 'Data/PredRast/ClimateBC/tave.tif'")
    tav<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tav_stamp)],crs=wgs84)
    tav_p<-projectRaster(tav, crs = bcalb)
    raster::writeRaster(tav_p, filename = 'Data/PredRast/ClimateBC/tave.tif')
    
    #Get total precipitation data, covert to raster, reproject and write to Data directory 
    print("Writing total precipitation data to 'Data/PredRast/ClimateBC/ppt.tif'")
    ppt<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',p_stamp)],crs=wgs84)
    ppt_p<-projectRaster(ppt, crs = bcalb)
    raster::writeRaster(ppt_p,filename = 'Data/PredRast/ClimateBC/ppt.tif')
    
    #Plot Results
    #plot(stack(resample(aoi_dem,tmn_p,method='bilinear'),tmn_p,tmx_p,tav_p,ppt_p))
    
    
    print("Finshed getting ClimateBC data.")
    
    
    if(calc_rcp==TRUE)
    { 
      #Read in ClimateBC CSV
      print("Reading in ClimateBC output data ...")
      clim_bc_data<-read_csv(paste('Data/PredRast/ClimateBC/CSVs/',climBCRCP,sep=""))
      
      #Get average temperature and total precipitation for specified month, convert to raster, and write to PredRast directory 
      print("Retreiving temperature and precipitation data for specified month ...")
      if(month<10)
      {
        tmx_stamp<-paste('Tmax0',month,sep="")
        tmn_stamp<-paste('Tmin0',month,sep="")
        tav_stamp<-paste('Tave0',month,sep="")
        p_stamp<-paste('PPT0',month,sep="")
      }else{
        tmx_stamp<-paste('Tmax',month,sep="")
        tmn_stamp<-paste('Tmin',month,sep="")
        tav_stamp<-paste('Tave',month,sep="")
        p_stamp<-t_stamp<-paste('PPT',month,sep="")
      }
      
      #Get temperature data, covert to raster, reproject and write to Data directory 
      print("Writing max RCP temperature data to 'Data/PredRast/ClimateBC/tmax_rcp.tif'")
      tmx<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tmx_stamp)],crs=wgs84)
      tmx_p<-projectRaster(tmx, crs = bcalb)
      raster::writeRaster(tmx_p, filename = 'Data/PredRast/ClimateBC/tmax_rcp.tif')
      
      print("Writing min RCP temperature data to 'Data/PredRast/ClimateBC/tmin_rcp.tif'")
      tmn<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tmn_stamp)],crs=wgs84)
      tmn_p<-projectRaster(tmn, crs = bcalb)
      raster::writeRaster(tmn_p, filename = 'Data/PredRast/ClimateBC/tmin_rcp.tif')
      
      print("Writing mean RCP temperature data to 'Data/PredRast/ClimateBC/tave_rcp.tif'")
      tav<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tav_stamp)],crs=wgs84)
      tav_p<-projectRaster(tav, crs = bcalb)
      raster::writeRaster(tav_p, filename = 'Data/PredRast/ClimateBC/tave_rcp.tif')
      
      #Get total precipitation data, covert to raster, reproject and write to Data directory 
      print("Writing total RCP precipitation data to 'Data/PredRast/ClimateBC/ppt_rcp.tif'")
      ppt<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',p_stamp)],crs=wgs84)
      ppt_p<-projectRaster(ppt, crs = bcalb)
      raster::writeRaster(ppt_p,filename = 'Data/PredRast/ClimateBC/ppt_rcp.tif')
      
      #Plot Results
      #plot(stack(resample(aoi_dem,tmn_p,method='bilinear'),tmn_p,tmx_p,tav_p,ppt_p))
      
      
      print("Finshed getting ClimateBC RCP data.")}
    
    
  }
  if(climate_source=='Daymet')
  {
    
    print("Obtaining Daymet climate data")
    
    #Daymet path
    daymet_pth<- 'Data/PredRast/DayMet/'
    
    #Get new extent bbox
    aoi_ext<-raster::extent(projectRaster(aoi_dem,crs=wgs84))
    
    tl_y<-aoi_ext[4]
    tl_x<-aoi_ext[1]
    br_y<-aoi_ext[3]
    br_x<-aoi_ext[2]
    
    loc<-c(tl_y,tl_x,br_y,br_x)
    
    print("Downloading Daymet Climate data, be patient ...")
    #Use bbox to get Daymet tmin,tmax and prcp data for given year 
    daymetr::download_daymet_ncss(location = c(aoi_ext[4],aoi_ext[1],aoi_ext[3],aoi_ext[2]), 
                                  start = year, 
                                  end = year, 
                                  path = daymet_pth, 
                                  param = 'prcp',
                                  frequency = 'monthly')
    
    daymetr::download_daymet_ncss(location = c(aoi_ext[4],aoi_ext[1],aoi_ext[3],aoi_ext[2]), 
                                  start = year, 
                                  end = year, 
                                  path = daymet_pth, 
                                  param = 'tmin',
                                  frequency = 'monthly')
    
    daymetr::download_daymet_ncss(location = c(aoi_ext[4],aoi_ext[1],aoi_ext[3],aoi_ext[2]), 
                                  start = year, 
                                  end = year, 
                                  path = daymet_pth, 
                                  param = 'tmax',
                                  frequency = 'monthly')
    
    
    #Broken for monthly data, currently have to use gdal_translate
    #daymetr::nc2tif(path = daymet_pth,overwrite = T)
    
    print("\n")
    print("Daymet tiles downloaded, translating to GeoTIF, be patient ...")
    
    print(list.files(daymet_pth,"*ncss.nc"))
    
    #Get dir path
    work_dir<-getwd()
    
    #Fudge src path for working with NETCDF
    gdal_prcp_src<-paste('NETCDF:',"\"",work_dir,'/Data/PredRast/DayMet/prcp_monttl_',year,'_ncss.nc":prcp', sep="")
    gdal_tmin_src<-paste('NETCDF:',"\"",work_dir,'/Data/PredRast/DayMet/tmin_monavg_',year,'_ncss.nc":tmin', sep="")
    gdal_tmax_src<-paste('NETCDF:',"\"",work_dir,'/Data/PredRast/DayMet/tmax_monavg_',year,'_ncss.nc":tmax', sep="")
    
    #Establsih dst file paths
    gdal_prcp_dst<-paste(work_dir,'/Data/PredRast/DayMet/prcp_monttl_ncss.tif', sep="")
    gdal_tmin_dst<-paste(work_dir,'/Data/PredRast/DayMet/tmin_monavg_ncss.tif', sep="")
    gdal_tmax_dst<-paste(work_dir,'/Data/PredRast/DayMet/tmax_monavg_ncss.tif', sep="")
    gdal_tavg_dst<-paste(work_dir,'/Data/PredRast/DayMet/tavg_monavg_ncss.tif', sep="")
    
    #Translate to geotiff using gdal 
    gdal_translate(src_dataset = gdal_prcp_src, dst_dataset = gdal_prcp_dst, r="bilinear") 
    gdal_translate(src_dataset = gdal_tmin_src,dst_dataset = gdal_tmin_dst,r="bilinear") 
    gdal_translate(src_dataset = gdal_tmax_src,dst_dataset = gdal_tmax_dst,r="bilinear") 
    
    #Read in new data at given month index (1-12)
    prcp<-raster::raster(gdal_prcp_dst, band = month)
    tmin<-raster::raster(gdal_tmin_dst, band = month)
    tmax<-raster::raster(gdal_tmax_dst, band = month)
    
    #Reproject to EPSG:3005
    prcp<-raster::projectRaster(prcp,crs=bcalb)
    tmin<-raster::projectRaster(tmin,crs=bcalb)
    tmax<-raster::projectRaster(tmax,crs=bcalb)
    
    #Crop to aoi 
    prcp<-raster::crop(prcp,aoi_dem)
    tmin<-raster::crop(tmin,aoi_dem)
    tmax<-raster::crop(tmax,aoi_dem)
    
    tavg<-(tmin+tmax)/2
    
    #Write to data dir
    print("Writing precipitation data to '/Data/PredRast/DayMet/prcp_monttl_ncss.tif'")
    raster::writeRaster(prcp,gdal_prcp_dst, overwrite = T)
    print("Writing min temperature data to '/Data/PredRast/DayMet/tmin_monavg_ncss.tif'")
    raster::writeRaster(tmin,gdal_tmin_dst, overwrite = T)
    print("Writing max temperature data to '/Data/PredRast/DayMet/tmax_monavg_ncss.tif'")
    raster::writeRaster(tmax,gdal_tmax_dst, overwrite = T)
    print("Writing average temperature data to '/Data/PredRast/DayMet/tavg_monavg_ncss.tif'")
    raster::writeRaster(tavg,gdal_tavg_dst, overwrite = T)
    
    #Peak
    #aoi_dem<-raster::resample(aoi_dem, prcp, method="bilinear")
    #s<-raster::stack(aoi_dem,prcp,tmin,tmax)
    #raster::plot(s)
    
    print("Get Daymet data is done.")
    
  }
  
}else{
  
  interp_temp<-raster::raster(temp_pth)
  if(crs(interp_temp)@projargs!=bcalb)
  {
    interp_temp<-raster::projectRaster(from=interp_temp,crs=crs(bcalb))
  }
  raster::writeRaster(interp_temp,"Data/PredRast/FieldObs/interp_temp.tif")
  
  interp_prcp<-raster::raster(prcp_pth)
  if(crs(interp_prcp)@projargs!=bcalb)
  {
    interp_prcp<-raster::projectRaster(from=interp_prcp,crs=crs(bcalb))
  }
  raster::writeRaster(interp_prcp,"Data/PredRast/FieldObs/interp_prcp.tif")
  
}

####Get Stream Network from freshwater atlas, this layer is used for burning the DEM (REQUIRED)####

#List likley stream layers, and pull desired layer, or...
#poss_strm<-bcdata::bcdc_list()
#poss_strm[stringr::str_detect(poss_strm,"stream")]

print("Downloading Freshwater Atlas Stream network for WGC ...")
if(stream_pth=="")
{
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


####Get Roads Networks####

#List likley road layers, and pull desired layer, or...
#poss_roads<-bcdata::bcdc_list()
#[stringr::str_detect(poss_roads,"road")]

print("Loading roads layer, this may take a while ...")
if(roads_pth=="")
{
  roads<-bcdata::bcdc_query_geodata('bb060417-b6e6-4548-b837-f9060d94743e') %>%
    bcdata::filter(INTERSECTS(e_sf)) %>%
    bcdata::collect()
  
  if(st_crs(roads)!=st_crs(bcalb))
  {
    roads<-sf::st_transform(roads,bcalb)
  }
  
  roads<-sf::st_as_sf(roads)
  
}else{
  
  roads<-sf::st_read(roads_pth)
  
  if(st_crs(roads)!=st_crs(bcalb))
  {
    roads<-sf::st_transform(roads,bcalb)
  }
  
  #Crop to aoi extent
  print("Cropping roads to extent ...")
  roads<-roads %>% 
    sf::st_crop(e)
  
  roads<-sf::st_as_sf(roads)
}

#ggplot(roads)+geom_sf()

#Write roads data to PredVect directory as roads_v.shp
print("Writing roads to 'Data/PredVect/roads_v.shp'")
sf::write_sf(roads %>% dplyr::select(geometry),'Data/PredVect/roads_v.shp')


#print("Plotting final vector layers...")
#ggplot()+geom_sf(data=roads,fill="black",color="black")+geom_sf(data=FWA_Stream,fill="blue",color="blue")+geom_sf(data=waterbods,aes(fill = WBT))



####Consolidated Cutblocks####


print("Downloading Consolidated Cutblocks ...")
ConsCutb<-bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7", crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::select(HARVEST_YEAR) %>%
  bcdata::collect()


if(st_crs(ConsCutb)!=st_crs(bcalb))
{
  ConsCutb<-sf::st_transform(ConsCutb,bcalb)
}


ConsCutb<-ConsCutb%>%
  sf::st_crop(e)

ConsCutb$Age<-"OldCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR>=(year-5)]<-"NewCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR<(year-5) & ConsCutb$HARVEST_YEAR>=(year-25)]<-"RgrwSCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR<(year-25) & ConsCutb$HARVEST_YEAR>=(year-45)]<-"RgrwYCB"

ConsCutb<-ConsCutb %>%
  dplyr::select(Age)

#ggplot(ConsCutb) + geom_sf(aes(fill=Age))


#Write cutblock data to PredVect directory as roads_v.shp
print("Writing cutblocks to 'Data/PredVect/ConsCutBlk.shp'")
sf::write_sf(ConsCutb,'Data/PredVect/ConsCutBlk.shp')




####Fire Perimeters####


print("Downloading Fire Perimeters - Historical")
Fires<-bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702",crs=3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::select(FIRE_YEAR) %>%
  bcdata::collect()

if(st_crs(Fires)!=st_crs(bcalb))
{
  Fires<-sf::st_transform(Fires,bcalb)
}


Fires <- Fires %>%
  sf::st_crop(e)

Fires$Age<-"OldF"
Fires$Age[Fires$FIRE_YEAR>=(year-5)]<-"NewF"
Fires$Age[Fires$FIRE_YEAR<(year-5) & Fires$FIRE_YEAR>=(year-25)]<-"RgrwSF"
Fires$Age[Fires$FIRE_YEAR<(year-25) & Fires$FIRE_YEAR>=(year-45)]<-"RgrwYF"

Fires<-Fires %>%
  dplyr::select(Age)

#ggplot(Fires) + geom_sf(aes(fill=Age))

#Write cutblock data to PredVect directory as roads_v.shp
print("Writing fires to 'Data/PredVect/Fires.shp'")
sf::write_sf(Fires,'Data/PredVect/Fires.shp')


####BEC-Zone####


#BEC<-bcdc_query_geodata("bec-zones-generalized-1-2m-",crs=3005) %>%
#  bcdata::filter(INTERSECTS(e_sf))

####LAI####

#Load Sentinel 2 derived LAI (This can be produced using SNAP Toolbox)

if(lai_pth!="")
{
  lai<-raster(lai_pth)
  
  #Project if necessary
  if(raster::compareCRS(raster::crs(lai),bcalb)==F)
  {
    lai<-projectRaster(lai,crs=bcalb)
  }
  
  lai<-raster::crop(lai,aoi_dem)
  
  #Extinction coefficient (Kd) based on Fig. 15.4 in Environmental Biophysics (Campbell & Norman, 1998), assumes leaf angle distribution x=1 
  #kd_xeq1<-read.csv('Data/PredRast/LAI/kd_coef.csv')
  #names(kd_xeq1)<-c('LAI','Kd')
  
  #Function finds best estimate of Kd as a function of LAI 
  gen_kd<-function(x)
  {
    kd<-(0.8079940*x^-0.0770034)+(-0.0006393*x^2)
    
    return(kd)
  }
  
  #raster::beginCluster(16)
  
  #Get a raster of Kd, a function of LAI 
  #Kd <- raster::clusterR(lai, calc, args=list(fun=gen_kd), export=c('kd_xeq1'))
  
  Kd <- raster::calc(lai,gen_kd)
  
  
  #Based on work of Fuchs et al. (1976), interception of beam and diffuse radiation by canopy averaged over a whole day can be approximated by 
  #the intercepted function for diffuse because the sun traverse the whole sky. Therefore the average transmission of canopies can be 
  #modeled over whole days using Eq 15.6 (Campbell & Norman, 1998) with Kbe replaced with Kd, and the daily fractional interception can
  #be computed as : f = 1-exp(-Kd*Lt)
  frac_intercept<-1-exp(-Kd*lai)
  
  #Remove negative fractions 
  frac_intercept[frac_intercept<0]<-0
  
  #End cluster 
  #raster::endCluster()
  
  #Write fractional interception raster for assumed leaf angle distribution x=1
  writeRaster(frac_intercept,'Data/PredRast/LAI/dly_frac_intercep_x1.tif',overwrite=T)
  writeRaster(lai,'Data/PredRast/LAI/LAI.tif',overwrite=T)
}



####Process Landsat Brightness Temperature layer####
if(LST_pth!="")
{
  BT<-raster(LST_pth)
  
  #Project if necessary
  if(raster::compareCRS(raster::crs(BT),bcalb)==F)
  {
    BT<-projectRaster(BT,crs=bcalb)
  }
  
  BT<-raster::crop(BT,aoi_dem)
  
  writeRaster(BT,"Data/PredRast/LST/LST.tif",overwrite=T)
}


####MODIS Snow Off Date####
if(MODIS_SDoff_pth!="")
{
  SDoff<-raster(MODIS_SDoff_pth, band = 2)
  
  raster::NAvalue(SDoff)<--9
  
  #Project if necessary
  if(raster::compareCRS(raster::crs(SDoff),bcalb)==F)
  {
    SDoff<-projectRaster(SDoff,crs=bcalb)
  }
  
  SDoff<-raster::crop(SDoff,aoi_dem)
  
  writeRaster(SDoff,"Data/PredRast/MD10A1_SDOFF/SDoff.tif")
}


