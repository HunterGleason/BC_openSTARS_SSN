library(SSN)
library(dplyr)
library(readr)
library(sf)

trophic2019 <- importSSN("SSN/parsnip_trophic2019.ssn", predpts = "preds_o")
names(trophic2019@obspoints@SSNPoints[[1]]@point.data)[names(trophic2019@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   
names(trophic2019)

trophic2020 <- importSSN("SSN/parsnip_trophic2020.ssn", predpts = "preds_o")
names(trophic2020@obspoints@SSNPoints[[1]]@point.data)[names(trophic2020@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   
names(trophic2020)

#get data from SSN object ----
data <- SSN::getSSNdata.frame(trophic2019, Name = "preds_o")
data <- trophic2019@data
data <- trophic2019@predpoints@SSNPoints[[1]]@network.point.coords

#get stream network from object ------
trophic2019 <- importSSN("SSN/parsnip_trophic2019.ssn", predpts = "preds_o")

#extract lines using sp package
network <- trophic2019@lines
sp::spplot(network@Lines)
sl <- sp::SpatialLines(network)
plot(sl)

#extract using SSN package much simpler
spatial_trophic2019 <- SSN::as.SpatialLinesDataFrame(trophic2019, data = "preds_o")
str(spatial_trophic2019)
plot(spatial_trophic2019)
plot(data, add = TRUE)

#change to data frame
spatialdata <- broom::tidy(spatial_trophic2019)

##### need to learn how to connect the lines and the data
# best guess is @network.line.coords$SegmentID = @data$rid = @lines@ID

##extract predictions-----
SSN::getPreds()

##place data in the object ---
SSN::putSSNdata.frame()

#add daily observations SSN Object ----
obs <- getSSNdata.frame(trophic2019)
day <- st_read("DataUTM10/trophic_2019_metrics.shp") %>%
  dplyr::select(-c(awat,awvar,mwat,mwvar,mwvar,mwcoef,site_cd,geometry))

#select out pid column
x <- obs$pid

#bind new data
obs <- obs %>%
  dplyr::left_join(day, by = "wypnt_n")

#fix rownames to match the pid column.... not sure why this works
rownames(obs) <- x

trophic2019 <- putSSNdata.frame(obs, trophic2019)
x <- getSSNdata.frame(trophic2019)



