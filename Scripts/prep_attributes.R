#air temp 
awat <- raster::raster("/home/rstudio/DataUTM10/trophic_2019_at.awat_[Universal Kriging].tif")
raster::projection(awat)
awat <- raster::projectRaster(awat, crs = utm)
raster::writeRaster(awat, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2019_at.awat [Universal Kriging].tif", format = "GTiff", overwrite=T)

awcv <- raster::raster("/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2019_at.awcoef [Universal Kriging].tif")
raster::projection(awcv)
awcv <- raster::projectRaster(awcv, crs = utm)
raster::writeRaster(awcv, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2019_at.awcoef [Universal Kriging].tif", format = "GTiff", overwrite=T)

mwat <- raster::raster("/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2019_at.mwat [Universal Kriging].tif")
raster::projection(mwat)
mwat <- raster::projectRaster(mwat, crs = utm)
raster::writeRaster(mwat, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2019_at.mwat [Universal Kriging].tif", format = "GTiff", overwrite=T)


awat <- raster::raster("/home/rstudio/DataUTM10/trophic_2020_at.awat [Universal Kriging].tif")
raster::projection(awat)
awat <- raster::projectRaster(awat, crs = utm)
raster::writeRaster(awat, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2020_at.awat [Universal Kriging].tif", format = "GTiff", overwrite=T)

awcv <- raster::raster("/home/rstudio/DataUTM10/trophic_2020_at.awcoef [Universal Kriging].tif")
raster::projection(awcv)
awcv <- raster::projectRaster(awcv, crs = utm)
raster::writeRaster(awcv, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2020_at.awcoef [Universal Kriging].tif", format = "GTiff", overwrite=T)

mwat <- raster::raster("/home/rstudio/DataUTM10/trophic_2020_at.mwat [Universal Kriging].tif")
raster::projection(mwat)
mwat <- raster::projectRaster(mwat, crs = utm)
raster::writeRaster(mwat, filename = "/home/rstudio/BC_openSTARS_SSN/DataUTM10/trophic_2020_at.mwat [Universal Kriging].tif", format = "GTiff", overwrite=T)

