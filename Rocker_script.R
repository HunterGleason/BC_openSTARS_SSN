# reinstall dev version devtools::install_github("MiKatt/openSTARS", ref = "dev")

#Import the openSTARS library 
library(openSTARS)
library(rgrass7)
library(rgdal)
library(sf)
library(sp)
library(parallel)
library(dplyr)

####Set Global Vars####

#Prompt user for name of output SSN file.
ssn_dir <- paste("SSN/",readline(prompt="Please enter the desired name of the output SNN object, e.g., bowron.ssn: "),sep="")

#Path to DEM 
dem_path<-"DataUTM10/parsnip_utm.tif"

#Path to observed stream sites.
sites_path <- "DataUTM10/trophic_2019_metrics.shp"

#repeated_meas <- readline(prompt="Do you wish to add repeated measures to SSN object, TRUE or FALSE: ")

#Enter distance of predictions site for auto generation
pred_dist<-strtoi(readline(prompt="Please enter stream distance interval at which to generate prediction sites in meters: "))

# if(pred_sites=='interval')
# {
#   pred_dist<-strtoi(readline(prompt="Please enter stream distance interval at which to generate prediction sites in meters: "))
# }
# if(pred_sites=='provided')
# {
#   pred_url<-strtoi(readline(prompt="Please enter path to prediction sites vector file: "))
# }


#Path to freshwater-atlas stream network
#stream_path<-"Data/Streams/fresh_water_atlas.shp"

#Vector of predictive raster attribute layers, and Abbrv. vector 
#Enter path to gridded air temp
r_pred_paths<- c("DataUTM10/trophic_2019_at.awat [Universal Kriging].tif")

#Make raster covar names shp file friendly 
r_pred_names<-c('avTmp')

#Prompt user for accumulation threshold to use for deriving stream network 
#accum_thresh<-strtoi(readline(prompt = "Please enter the accumulation threshold to use (i.e. minimum flow accumulation value in cells that will initiate a new stream), 700 is a good start.: "))

#Prompt user for desired minimum stream length. 

#min_strm_lngth<-strtoi(readline(prompt = "Please enter minimum stream length in number of DEM raster cells; shorter first order stream segments are deleted, typically set to zero.: "))


#Prompt user for depth to burn DEM 
#burn_m<-strtoi(readline(prompt = "How many meters should the Freshwater Atlas streams layer be burned into the DEM? Typically 0-5m. Leave blank if streams should be derived solely from DEM.: "))

# 
# #Monthly DOY centres, used for estimating monthly average solar input 
# month<-strtoi(readline(prompt = "Please enter the month that the study data corresponds to (1-12): "))
# doy_centres<-c(15,46,74,105,135,166,196,227,258,288,319,349)
# 



#### Set up grass env. based on local TRIM DEM 
grass_location <-"parsnip_2019"

print("Setting up GRASS Env. based on DEM ...")

#Install addons
system('grass76 --tmp-location XY --exec g.extension r.stream.basins')
system('grass76 --tmp-location XY --exec g.extension r.stream.distance')
system('grass76 --tmp-location XY --exec g.extension r.stream.order')
system('grass76 --tmp-location XY --exec g.extension r.stream.slope')
system('grass76 --tmp-location XY --exec g.extension r.hydrodem')

rgrass7::use_sp()

#gisBase <- "C:/Program Files/GRASS GIS 7.6"
#grass_db_path <- file.path("C:/Code/BC_openSTARS_SSN", "grassDB")
#initGRASS(gisBase = gisBase, location=grass_location, remove_GISRC = T, override = T)
#setup_grass_environment(dem = dem_path, gisBase = gisBase, gisDbase = grass_db_path, location = grass_location, remove_GISRC = T, override = T)

dem_grid <- rgdal::readGDAL(dem_path, silent = TRUE)
initGRASS(gisBase = '/usr/lib/grass76/',
          mapset = 'PERMANENT',
          location = grass_location,
          override=T)


rgrass7::writeRAST(dem_grid,'region_dem')

execGRASS("g.region", flags = c("c", "quiet"),
          parameters = list(
            raster ='region_dem'
          ))

execGRASS("g.proj", flags = c("c", "quiet"),
          parameters = list(
            georef = dem_path
          ))

gmeta()


#Check projections
#print("Checking projections ... ")
check_projection(r_pred_paths)


#This function loads a DEM (digital elevation model) and sites data (both required) into the 'GRASS' session. 
#Optionally, prediction sites and streams data can be loaded and the streams may be corrected by snapping to 
#prevent lose ends. Likewise, potential predictor maps (raster or vector format) can be loaded.
#See params.

# print("Importing data into GRASS ...")
# if(pred_sites=="" | pred_sites=="interval")
# {
import_data(dem = dem_path, 
            sites = sites_path,
            predictor_raster = r_pred_paths, 
            predictor_r_names = r_pred_names)
# streams = stream_path, 
# snap_streams = T)
# }else{
#   import_data(dem = dem_path, 
#               sites = sites_path,
#               pred_sites = pred_url,
#               predictor_raster =r_pred_paths, 
#               predictor_r_names =r_pred_names,
#               streams = stream_path, 
#               snap_streams = T)
# }




#Streams are derived from a digital elevation model (DEM) using the GRASS function r.stream.extract. 
#If a stream network is available (see import_data) and burn > 0 it will be first burnt into DEM. 
#Stream topology is derived using the GRASS function r.stream.order.
#See params.
print("Deriving streams, this may take a while ...")
# if(burn_m=="")
# {
derive_streams(burn = 0, accum_threshold = 2000, min_stream_length = 0, condition = TRUE, clean = TRUE)
# }else{
#derive_streams(burn = burn_m, accum_threshold = accum_thresh, condition = T, clean = TRUE)
# }

# dem <- readRAST("dem", ignore.stderr = TRUE)
# streams <- readVECT("streams_v", ignore.stderr = TRUE)
# sites <- readVECT("sites_o", ignore.stderr = TRUE) 
# plot(dem, col = terrain.colors(20))
# lines(streams, col = "blue")
# cols <- colorRampPalette(c("blue", "red"))(length(sites$value))[rank(sites$value)]
# points(sites, pch = 16, col = "red")

# Check and correct complex junctions 
print("Checking for and correcting complex junctions, this may take a while ...")
cj <- check_compl_confluences()
# if(cj){
print("Correcting Stream Junctions ...")

correct_compl_confluences()
# }

# lakes <- readVECT("watrbod", ignore.stderr = TRUE)
# lakes <- st_as_sf(lakes)
# lakes <- lakes[lakes$WBT=='L',]
# lakes<- as_Spatial(lakes)
# writeVECT(lakes,vname = 'lakes')

#!!In progress!!
#delete_lakes('lakes')

#A vector (lines) map 'edges' is derived from 'streams_v' and several attributes are assigned.
#No params.
print("Computing stream edge attributes, this may take a while ...")
calc_edges()


edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data, n = 4)

#### Calculate DEM derivative layers for use and model inputs using rgrass7 ####
#if(burn_m==0)
#{
dem_name<-'dem_cond'
#}else{
#  dem_name<-paste('dem_cond_burn',burn_m,sep="")
#}

# calculate slope and aspect from DEM as an input attribute

#print("Computing slope and aspect from DEM ...")
execGRASS("r.slope.aspect", flags = c("overwrite"),
          parameters = list(
            elevation = dem_name,
            slope = "slope",
            aspect = "aspect"
          ))

# 
# #calculate total solar irradiance for the ~15th of month, should ~ average solar input for the month ...
# print("Computing total solar irradiance for the 15th day of month, this may take a while ...")
# procs<-round(detectCores(all.tests = FALSE, logical = TRUE)*(cpu/100))
# execGRASS("r.sun",flags = c("overwrite"),
#           parameters = list(elevation=dem_name,
#                             aspect="aspect",
#                             slope="slope",
#                             glob_rad="totirra",
#                             day=doy_centres[month],
#                             nprocs=procs))

# #Adjust incoming solar radiation for canopy effects based on Sentinel 2 derived daily fractional interception by canopy.
# #Assumes leaf angle distribution x=1.
# print("Adjusting solar input for daily fractional interception from canopy ...")
# execGRASS("r.mapcalc",flags=c("overwrite"),
#           parameters = list(
#             expression = "totirra_adj = totirra - ( totirra * frcItrc * 0.8)"
#           ))
# 
# #Buffer stream network by 120m 
# print("Buffering Stream Network ...")
# execGRASS("v.buffer",flags = c("overwrite"),
#           parameters = list(
#             input="streams_v",
#             output="streams_buff",
#             distance=150
#           ))
# 
# #Set mask to stream buffer
# print("Masking to buffered streams ...")
# execGRASS("r.mask",flags = c("overwrite"),
#           parameters = list(
#             vector = "streams_buff"
#           ))
# 
# #Generate adjusted total solar input layer masked to stream buffer
# print("Generate solar input along streams buffer ...")
# execGRASS("r.mapcalc",flags = c("overwrite"),
#           parameters = list(
#             expression="totirra_strm = totirra_adj"
#           ))
# 
# 
# #Deactivate stream mask
# print("Deactivate stream mask ...")
# execGRASS("r.mask", flags = c("r"))
# 
# # calculate eastness from aspect as an input attribute
# print("Computing Eastness and Northness from aspect ...")
# execGRASS("r.mapcalc",flags = c("overwrite"),
#           parameters = list(
#             expression = "eastness = cos(aspect)"
#           ))
# 
# # calculate northness from aspect as an input attribute
# execGRASS("r.mapcalc",flags = c("overwrite"),
#           parameters = list(
#             expression = "northnes = sin(aspect)"
#           ))

# calculate drainage from dem
print("Calculating drainage from DEM ...")
execGRASS("r.watershed",flags = c("overwrite"),
          parameters = list(
            elevation = dem_name,
            drainage = "drain"
          ))
# 
# 
# # calculate down stream gradient from dem and drainage
# print("Calculating stream gradient from drainage ...")
execGRASS("r.stream.slope",flags = c("overwrite"),
          parameters = list(
            direction = "drain",
            elevation = dem_name,
            gradient = "gradt"
          ))

# 
# #Mask gradient data to vector stream network
execGRASS("r.mask",flags = c("overwrite"),
          parameters = list(
            raster = "streams_r"
          ))
# 
# 
# #Run mapcalc on gradiant raster, and Landsat Brightness Temp with stream mask active 
execGRASS("r.mapcalc", flags = c("overwrite"),
          parameters = list(
            expression = "gradt_ds = gradt"
          ))
# 
# execGRASS("r.mapcalc", flags = c("overwrite"),
#           parameters = list(
#             expression = "lstst = lst"
#           ))
# 
# #Deactivate stream mask
execGRASS("r.mask", flags = c("r"))
# 
# 
# #Mask Landsat-8 Brightness Temp to lakes 
# execGRASS("r.mask",flags = c("overwrite"),
#           parameters = list(
#             vector = "lakes"
#           ))
# 
# #Run mapcalc on gradiant raster with stream mask active 
# execGRASS("r.mapcalc", flags = c("overwrite"),
#           parameters = list(
#             expression = "lstlk = lst"
#           ))
# 
# #Deactivate lake mask
# execGRASS("r.mask", flags = c("r"))
# 
# 
# print("Converting roads to raster ...")
# dem <- readRAST("dem", ignore.stderr = TRUE)
# execGRASS("v.to.rast", flags = c("overwrite"),
#           parameters = list(
#             input = "roads",
#             type = "line",
#             output = "roads_r",
#             use = "val",
#             value =dem@grid@cellsize[1]/1000
#           ))
# 


#### Aggregate attributes by segment, add external attributes as needed below ####

#A vector (points) map 'sites' is derived and several attributes are assigned.
#See params.
# print("Computing site attributes, this may take a while ...")
# if(pred_sites=="interval")
# {
calc_sites(maxdist = 200)

sites <- readVECT("sites", ignore.stderr = TRUE)

head(sites@data, n = 4)


restrict_network("sites", keep_netIDs = unique(sites$netID))


#Need to specify netIDs, for if sites exist in other basins
#See params
print("Computing prediction site attributes, this may take a while ...")
calc_prediction_sites(predictions = "preds_o", dist = pred_dist, netIDs = unique(sites$netID))


# }else if(pred_sites=="provided"){
#   #Compute the local pred_sites
#   print("Computing prediction site attributes, this may take a while ...")
#   calc_sites(maxdist=500, predictions="preds_o")
#   sites <- readVECT("sites", ignore.stderr = TRUE)
#   restrict_network("sites",keep_netIDs = unique(sites$netID))
#   
# } else {
#   print("Not computing prediction sites ...")
#   calc_sites(maxdist = 500)
#   
#   sites <- readVECT("sites", ignore.stderr = TRUE)
#   
#   restrict_network("sites",keep_netIDs = unique(sites$netID))
# }
# 


print("Computing covariate edge attributes, this might take a while ...")

# calc_attributes_edges(input_raster = c('dem','slope','mavTmp'), 
#                       stat_rast = c('mean','mean','mean'), 
#                       attr_name_rast = c('avEle','avSlo','mavTmp')
#                       #input_vector = c("watrbod","cutblk","fires"),
#                       #stat_vect = c('percent','percent','percent'),
#                       #attr_name_vect = c('WBT','Age','Age'),
#                       #round_dig = 5
#                  )

calc_attributes_edges(input_raster = c('dem','slope','gradt_ds', 'avTmp'), 
                      stat_rast = c('mean','mean','mean', 'mean'), 
                      attr_name_rast = c('avEle','avSlo','avGrdt', 'avTmp'),
                      round_dig = 5
)

#!!!!!!Need to address issue of when no glaciers, lakes or wetlands are present, also issue with cutblocks and fires !!!!!!!!
# 
# wtrbods <- readVECT('watrbod', ignore.stderr = TRUE)
# wtrbods <- c('L','W','G') %in% as.vector(unique(wtrbods@data$WBT))
# 
# cutblks <- readVECT('cutblk', ignore.stderr = TRUE)
# cutblks <- c('NewCB','OldCB','RgrwSCB','RgrwYCB') %in% as.vector(unique(cutblks@data$Age))
# 
# fires <- readVECT('fires', ignore.stderr = TRUE)
# fires <- c('NewF','OldF','RgrwSF','RgrwYF') %in% as.vector(unique(fires@data$Age))
# 
# vec_att_names<-c('L','W','G','NewCB','OldCB','RgrwSCB','RgrwYCB','NewF','OldF','RgrwSF','RgrwYF')[c(wtrbods,cutblks,fires)]
# vec_out_names<-c('LA','WA','GA','NewCBA','OldCBA','RgrwSCBA','RgrwYCBA','NewFA','OldFA','RgrwSFA','RgrwYFA')[c(wtrbods,cutblks,fires)]
# vec_stats<-c('percent','percent','percent','percent','percent','percent','percent','percent','percent','percent','percent')[c(wtrbods,cutblks,fires)]
# 

#### Compute basin covariate attributes by site####

#if(pred_sites!="")
#{
#  print("Computing covariate prediction site attributes, this might take a while ...")
calc_attributes_sites_approx(sites_map = "preds_o", 
                             input_attr_name = c('avEle','avSlo','avGrdt', 'avTmp'),
                             output_attr_name = c('avEleA','avSloA','avGrdtA', 'avTmpA'),
                             stat = c('mean','mean','mean', 'mean'),
                             calc_basin_area = TRUE,
                             round_dig = 5)
#}

#print("Computing covariate site attributes, this might take a while ...")
calc_attributes_sites_approx(sites_map = "sites", 
                             input_attr_name = c('avEle','avSlo','avGrdt', 'avTmp'),
                             output_attr_name = c('avEleA','avSloA','avGrdtA', 'avTmpA'),
                             stat = c('mean','mean','mean','mean'),
                             calc_basin_area = TRUE,
                             round_dig = 5)


#!!Uncomment if necessary to compute exact statistics!!
# calc_attributes_sites_exact(sites_map = "preds_o",
#                             input_raster = c('dem','slope','eastness','northnes','totirra_adj','totirra_strm','avTmp','totPpt','gradt_ds','roads_r'),
#                             stat_rast = c('mean','mean','mean','mean','mean','mean','mean','mean','mean','sum'),
#                             attr_name_rast = c('avEleE','avSloE','avEasE','avNorE','avIrradE','avIrrStE','avTmpE','avTotPpE','avGrdtE','smRdsE'),
#                             input_vector = c("watrbod","cutblk","fires"),
#                             stat_vect = c('percent','percent','percent'),
#                             attr_name_vect = c('WBT','Age','Age'),
#                             round_dig = 5)

####Hunters custom merge repeated measures function####
# merge_sites_measurements_hg<-function(ssn_v)
# {
#   #Get SSN data.frame and coordinates 
#   #Get current SSN object as data.frame and coordinates 
#   cur_sites <- readVECT("sites", ignore.stderr = TRUE)
#   cur_sites_df<-as.data.frame(cur_sites@data)
#   cur_sites_coords<-as.data.frame(cur_sites@coords)
#   cur_sites_coords$site<-cur_sites_df$site
#   
#   
#   
#   merged_df<-cur_sites_df
#   merged_coords<-cur_sites_coords
#   
#   for(i in c(1:length(ssn_v)))
#   {
#     ssn_ssn<-SSN::importSSN(ssn_v[i])
#     ssn_df<-as.data.frame(SSN::getSSNdata.frame(ssn_ssn))
#     ssn_coords<-as.data.frame(slot(ssn_ssn@obspoints@SSNPoints[[1]],"point.coords"))
#     ssn_coords$site<-ssn_df$site
#     
#     merged_df<-as.data.frame(rbind(merged_df,ssn_df))
#     merged_coords<-as.data.frame(rbind(merged_coords,ssn_coords))
#   }
#   
#   #Get unique site locations 
#   uniq_site_coords<-as.data.frame(unique(merged_coords))
#   
#   
#   uniq_site_coords$locID<-c(1:nrow(uniq_site_coords))
#   
#   merged_df$coords.x1<--9999
#   merged_df$coords.x2<--9999
#   
#   for(rw in c(1:nrow(merged_df)))
#   {
#     cur_site<-merged_df$site[rw]
#     
#     merged_df$coords.x1[rw]<-uniq_site_coords$coords.x1[uniq_site_coords$site %in% cur_site]
#     merged_df$coords.x2[rw]<-uniq_site_coords$coords.x2[uniq_site_coords$site %in% cur_site]
#     merged_df$locID[rw]<-uniq_site_coords$locID[uniq_site_coords$site %in% cur_site]
#     
#   }
#   
#   merged_coords<-merged_df[,c("coords.x1","coords.x2")]
#   merged_df[,c("coords.x1","coords.x2")]<-NULL
#   
#   #Update pid and cat 
#   merged_df$pid<-c(1:nrow(merged_df))
#   merged_df$cat<-merged_df$pid
#   
#   #Replace current SSN data and coordinates with merged
#   cur_sites@data<-merged_df
#   cur_sites@coords<-as.matrix(merged_coords)
#   
#   #Write updated SSN to GRASS Env., replace 
#   writeVECT(cur_sites, "sites", v.in.ogr_flags = c("overwrite"), ignore.stderr = TRUE)
#   
# }
# 
# #Add repeated measure to current SSN 
# if(repeated_meas==TRUE)
# {
#   
#   print("Merging repeated measures ... ")
#   merge_sites_measurements_hg(ssn_vec)
#   
# }




#Export SSN object to SNN project directory
#print(paste("Exporting SSN object to ",ssn_dir,sep=""))
#if(pred_sites!="")
#{
export_ssn(ssn_dir,predictions='preds_o',delete_directory = TRUE)
#} else {
#  export_ssn(ssn_dir,delete_directory = TRUE)
#}

print("Checking SSN object ...")
print(paste("SSN onject is good: ",check_ssn(ssn_dir),sep=""))