####Required Libraries####
library(raster)
library(rgdal)
library(daymetr)
library(gdalUtils)
library(bcdata)
library(sf)
library(readr)
library(blogdown)



month_climbc_to_tif<-function(climate_bc_csv,month,targ_crs,name)
{
  wgs84<-crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  #Read in ClimateBC CSV
  print("Reading in ClimateBC output data ...")
  clim_bc_data<-read_csv(climate_bc_csv)
  
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
  tmx_p<-projectRaster(tmx, crs = targ_crs)
  raster::writeRaster(tmx_p, filename = paste('Data/PredRast/ClimateBC/',name,'_tmax.tif',sep=''),overwrite=T)
  
  print("Writing min temperature data to 'Data/PredRast/ClimateBC/tmin.tif'")
  tmn<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tmn_stamp)],crs=wgs84)
  tmn_p<-projectRaster(tmn, crs = targ_crs)
  raster::writeRaster(tmn_p, filename = paste('Data/PredRast/ClimateBC/',name,'_tmin.tif',sep=''),overwrite=T)
  
  print("Writing mean temperature data to 'Data/PredRast/ClimateBC/tave.tif'")
  tav<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',tav_stamp)],crs=wgs84)
  tav_p<-projectRaster(tav, crs = targ_crs)
  raster::writeRaster(tav_p, filename = paste('Data/PredRast/ClimateBC/',name,'_tave.tif',sep=''),overwrite=T)
  
  #Get total precipitation data, covert to raster, reproject and write to Data directory 
  print("Writing total precipitation data to 'Data/PredRast/ClimateBC/ppt.tif'")
  ppt<- rasterFromXYZ(clim_bc_data[,c('Longitude','Latitude',p_stamp)],crs=wgs84)
  ppt_p<-projectRaster(ppt, crs = targ_crs)
  raster::writeRaster(ppt_p,filename = paste('Data/PredRast/ClimateBC/',name,'_ppt.tif',sep=''),overwrite=T)
  
  
}

toml_path <- '/home/rstudio/DATA/bc_openstars.toml' 

toml<-read_toml(toml_path)

setwd(toml$prepare_inputs$working_dir)

e<-extent(raster(toml$prepare_inputs$dem_path))

xmin<-e[1]
xmax<-e[2]
ymin<-e[3]
ymax<-e[4]


e_sf<- st_bbox(c(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),crs = st_crs(3005))
e_sf<-st_as_sfc(e_sf)

year=toml$prepare_inputs$year

#PRJ4 for EPSG:4326 and EPSG:3005
dem_crs<-crs(raster(toml$prepare_inputs$dem_path))

writeRaster(raster(toml$prepare_inputs$dem_path),"Data/DEM/dem.tif",overwrite=T)


#Prompt user to use ClimateBC or Daymet or their own raster layers
climate_source<-toml$prepare_inputs$climate_source

#If providing climate data, prompt for path
if(climate_source=='NULL')
{
  temp_pth<-toml$prepare_inputs$temp_path
  prcp_pth<-toml$prepare_inputs$prcp_path
}

#Prompt user to download RCP Climate data. 
calc_rcp<-toml$prepare_inputs$calc_fut_clim



####Load and write stream site observations####

sites<-st_read(toml$prepare_inputs$sites_path)

#Reproject if needed 
if(st_crs(sites)!=st_crs(dem_crs))
{
  sites<-st_transform(sites,dem_crs)
}


write_sf(sites,"Data/Sites/sites.shp",overwrite=T)


####Get climateBC or Daymet Data####


#If user chose to use ClimateBC data 
if(climate_source=="ClimateBC")
{
  
  month_climbc_to_tif(toml$prepare_inputs$clim_bc_obs,toml$prepare_inputs$month,dem_crs,toml$prepare_inputs$year)
  
  if(calc_rcp==TRUE)
  {
    fut_pths<-toml$prepare_inputs$fut_clim_pths
    fut_names<-toml$prepare_inputs$fut_clim_names
    
    for(i in c(1:length(fut_pths)))
    {
      month_climbc_to_tif(fut_pths[i],toml$prepare_inputs$month,dem_crs,fut_names[i])
    }
    
  }
}else{
  
  interp_temp<-raster::raster(toml$prepare_inputs$temp_path)
  raster::writeRaster(interp_temp,"Data/PredRast/FieldObs/interp_temp.tif")
  
  interp_prcp<-raster::raster(toml$prepare_inputs$prcp_path)
  raster::writeRaster(interp_prcp,"Data/PredRast/FieldObs/interp_prcp.tif")
  
  if(calc_rcp==TRUE)
  {
    fut_pths<-toml$prepare_inputs$fut_clim_pths
    fut_names<-toml$prepare_inputs$fut_clim_names
    
    for(i in c(1:length(fut_pths)))
    {
      month_climbc_to_tif(fut_pths[i],toml$prepare_inputs$month,dem_crs,fut_names[i])
    }
    
  }
}

####Get Stream Network from freshwater atlas, this layer is used for burning the DEM (REQUIRED)####

#List likley stream layers, and pull desired layer, or...
#poss_strm<-bcdata::bcdc_list()
#poss_strm[stringr::str_detect(poss_strm,"stream")]

print("Downloading Freshwater Atlas Stream network for WGC ...")
if(toml$prepare_inputs$stream_path=="NULL")
{
  #Grab freshwater atlas stream data for 'Watershed_Group_Code' from BC data warehouse, crop to extent
  FWA_Stream<-bcdata::bcdc_query_geodata('92344413-8035-4c08-b996-65a9b3f62fca', crs = 3005) %>%
    bcdata::filter(INTERSECTS(e_sf)) %>%
    bcdata::collect()
  
  colnames(FWA_Stream)[colnames(FWA_Stream)=='WATERSHED_GROUP_CODE']<-'WGC'
  
  FWA_Stream <- FWA_Stream %>%
    select('WGC') %>%
    st_zm()
  
  #Reproject if needed 
  if(st_crs(FWA_Stream)!=st_crs(dem_crs))
  {
    FWA_Stream<-st_transform(FWA_Stream,dem_crs)
  }
  
  #Write to stream network to openSTARS data dir
  print("Writing stream network to 'Data/Streams/streams.shp'")
  write_sf(FWA_Stream,paste('Data/Streams/streams.shp',sep = ""))
}else{
  stream_vect<-st_read(toml$prepare_inputs$stream_path)
  
  if(st_crs(stream_vect)!=st_crs(dem_crs))
  {
    stream_vect<-st_transform(stream_vect,dem_crs)
  }
  
  print("Writing stream network to 'Data/Streams/streams.shp'")
  write_sf(stream_vect,paste('Data/Streams/streams.shp',sep = ""),delete_layer=T)
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
  dplyr::select(WATERBODY_TYPE)


#Combine the three diffrent water body layers
print("joining waterbodies ...")
waterbods <-rbind(as.data.frame(FWA_Lakes),as.data.frame(FWA_Wetlands),as.data.frame(FWA_Glaciers))

#Rename water body type to 'WBT'
colnames(waterbods)[1]<-'WBT'

#Convert back to sf
waterbods <- st_as_sf(waterbods)

#ggplot()+geom_sf(data=FWA_Stream)+geom_sf(data=waterbods,aes(fill = WBT))


if(st_crs(waterbods)!=st_crs(dem_crs))
{
  waterbods<-sf::st_transform(waterbods,dem_crs)
}


#Write water bodies layer to openSTARS dir
print("Writing Freswater Atlas waterbodies layer to 'Data/PredVect/fresh_water_atlas_waterbods.shp'")
write_sf(waterbods, paste('Data/PredVect/FWAWaterbods.shp',sep = ""))


####Get Roads Networks####

#List likley road layers, and pull desired layer, or...
#poss_roads<-bcdata::bcdc_list()
#[stringr::str_detect(poss_roads,"road")]

print("Loading roads layer, this may take a while ...")
if(toml$prepare_inputs$roads_path=="NULL")
{
  roads<-bcdata::bcdc_query_geodata('bb060417-b6e6-4548-b837-f9060d94743e') %>%
    bcdata::filter(INTERSECTS(e_sf)) %>%
    bcdata::collect()
  
  if(st_crs(roads)!=st_crs(dem_crs))
  {
    roads<-st_transform(roads,dem_crs)
  }
  
  roads<-st_as_sf(roads)
  
}else{
  
  roads<-st_read(toml$prepare_inputs$roads_path)
  
  if(st_crs(roads)!=st_crs(dem_crs))
  {
    roads<-sf::st_transform(roads,dem_crs)
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
sf::write_sf(roads %>% dplyr::select(geometry),'Data/PredVect/Roads.shp')


#print("Plotting final vector layers...")
#ggplot()+geom_sf(data=roads,fill="black",color="black")+geom_sf(data=FWA_Stream,fill="blue",color="blue")+geom_sf(data=waterbods,aes(fill = WBT))



####Consolidated Cutblocks####


print("Downloading Consolidated Cutblocks ...")
ConsCutb<-bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7", crs = 3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::select(HARVEST_YEAR) %>%
  bcdata::collect()


if(st_crs(ConsCutb)!=st_crs(dem_crs))
{
  ConsCutb<-st_transform(ConsCutb,dem_crs)
}


ConsCutb$Age<-"OldCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR>=(year-5)]<-"NewCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR<(year-5) & ConsCutb$HARVEST_YEAR>=(year-25)]<-"RgrwSCB"
ConsCutb$Age[ConsCutb$HARVEST_YEAR<(year-25) & ConsCutb$HARVEST_YEAR>=(year-45)]<-"RgrwYCB"

ConsCutb<-ConsCutb %>%
  dplyr::select(Age)

#ggplot(ConsCutb) + geom_sf(aes(fill=Age))


#Write cutblock data to PredVect directory as roads_v.shp
print("Writing cutblocks to 'Data/PredVect/ConsCutBlk.shp'")
write_sf(ConsCutb,'Data/PredVect/ConsCutBlk.shp')




####Fire Perimeters####


print("Downloading Fire Perimeters - Historical")
Fires<-bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702",crs=3005) %>%
  bcdata::filter(INTERSECTS(e_sf)) %>%
  bcdata::select(FIRE_YEAR) %>%
  bcdata::collect()

if(st_crs(Fires)!=st_crs(dem_crs))
{
  Fires<-sf::st_transform(Fires,dem_crs)
}


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
