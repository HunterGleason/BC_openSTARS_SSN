library(SSN)
library(dplyr)


##  Iterative SSN build 

trophic2019 <- importSSN("SSN/parsnip_trophic2019.ssn", predpts = "preds_o")
names(trophic2019@obspoints@SSNPoints[[1]]@point.data)[names(trophic2019@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   

#create additive function and distance map
trophic2019 <- additive.function(trophic2019, "H2OArea", "computed.afv")
createDistMat(trophic2019, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)
names(trophic2019)

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

#single prediction plot
plot(trophic2019, "X2019.05.15", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     breaktype = "user", brks = c(4,8,10,12,14,16),
     lwdLineEx = 15, lineCol = "black", asp = 1,)
ssnm <- glmssn(as.formula(paste("X2019.05.15"," ~ avTmpA + avEleA")), trophic2019,
                 CorModels = c("Spherical.tailup", "Spherical.taildown"),
                 addfunccol = "computed.afv")
summary(ssnm)
preds <- predict(ssnm, "preds_o")
plot(preds, "X2019.05.15", cex=0.5)

#extract list of daily names
names <- as.character(names(trophic2019)$Obs)
names <- na.omit(stringr::str_extract(names, "[X]\\d+\\.\\d+\\.\\d+"))

dir.create("test")

str(names)
preds <- list()
for (i in 1:length(names)) {
  
  x <- glmssn(as.formula(paste(names[i], "~ avTmpA + avEleA")), trophic2019,
         CorModels = c("Spherical.tailup", "Spherical.taildown"),
         addfunccol = "computed.afv")
  
  preds[[i]] <- predict(x, "preds_o")
  
  tiff(paste0("test/", names[i], ".tiff"), res = 100)
  plot(preds[[i]], names[i], cex=0.5)
  dev.off()
  
}

names(preds) <- names
write_rds(preds, "~/Documents/Data/SSN/summer_preds.rds")

preds$`X2019.08.01`$ssn.object@predpoints@SSNPoints[[1]]@points.data
z <- data.frame(preds$`X2019.08.01`$ssn.object@predpoints@SSNPoints[[1]]@point.data)
plot(z$NEAR_X, z$NEAR_Y)


ggplot(z, aes(NEAR_X,NEAR_Y, col = X2019.08.01)) +
  geom_point()
  
map <- st_as_sf(z, crs = 26910, coords = c("NEAR_X", "NEAR_Y"))

mapview(map)


drv <- dbDriver("PostgreSQL")
conn <- con <- dbConnect(drv, dbname = "FWCPGrayling", user = "postgres", 
                         password = "parsnipgrayling", host = "localhost",
                         port = 5432)
wpt <- dbGetQuery(conn, "SELECT 
                                wpt.waypoint_name,
                                wpt.lat,
                                wpt.lon
                          FROM 
                                lookup.waypoints AS wpt,
                                telemetry.deployment AS teldep
                          WHERE 
                                teldep.waypoint_id = wpt.waypoint_id;")

wpt <- unique(wpt)


wpt <- st_as_sf(wpt, crs = 4326, coords = c("lon", "lat")) %>%
  st_transform(crs = 26910)
  
mapview(wpt)

st_crs(wpt)
st_crs(map)

map <- st_buffer(map, dist = 500)
mapview(map)
new_points <- st_intersection(wpt, map)
new_points <- st_union(wpt, map, tolerance=500)


