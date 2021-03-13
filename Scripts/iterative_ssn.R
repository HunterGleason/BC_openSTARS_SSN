library(SSN)
library(dplyr)
library(sf)
library(mapview)
library(RPostgreSQL)
library(readr)

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
  dplyr::select(-c(awat,awvar,mwat,mwvar,mwvar,awcoef,mwcoef,site_cd,geometry), -c(X2019.05.01:X2019.06.3), -c(X2019.09.16:geometry))

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
plot(trophic2019, "X2019.09.15", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     breaktype = "user", brks = c(4,8,10,12,14,16),
     lwdLineEx = 15, lineCol = "black", asp = 1,)
ssnm <- glmssn(as.formula(paste("X2019.09.15"," ~ avTmpA + avEleA")), trophic2019,
                 CorModels = c("Spherical.tailup", "Spherical.taildown"),
                 addfunccol = "computed.afv")
summary(ssnm)
preds <- predict(ssnm, "preds_o")
plot(preds, "X2019.09.15", cex=0.5, breaktype = "user", brks = c(4,6,8,10,12,14,16))

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

z <- data.frame()

for (i in 1:length(names)) {
  ztmp <- preds[[i]]$ssn.object@predpoints@SSNPoints[[1]]@point.data %>%
    mutate(date = names[i]) %>%
    rename(temp = names[i], temp_se = paste0(names[i], ".predSE"))
  z <- bind_rows(z, ztmp)
  
}

### pull out unique PID and near_x, Near_Y
bind <- z %>%
  select(pid, NEAR_X, NEAR_Y) %>%
  unique()

bind <- st_as_sf(bind, crs = 26910, coords = c("NEAR_X", "NEAR_Y"))

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


wpt <- st_as_sf(wpt, crs = 4326, coords = c("lon", "lat"), remove = FALSE) %>%
  st_transform(crs = 26910)

ind <- st_nearest_feature(wpt, bind)

new_points <- bind$pid[ind] 

bind <- bind[new_points,]

bind <- cbind(wpt, bind) %>%
  select(-c(geometry.1))

st_geometry(bind) <- NULL

## combine with master z dataframe
str(z)
str(bind)
test <- z %>%
  left_join(bind, by = "pid") %>%
  drop_na() %>%
  select(waypoint_name, lat, lon, upDist, H2OAreaA, avEleA, avSloA, avTmpA, temp, temp_se, date)
str(test)

write_rds(test, "~/Documents/Data/SSN/pred_temps.rds")


#### now need to pull this for 2020 as well
### integrate into bayesian script
### see if anything changes 
### fix up the legend scale on the plots created by loop that creates preds



#spatial joins OLDER SCRIPT 
preds <- readRDS("~/Documents/Data/SSN/summer_preds.rds")
names(preds) <- names


preds$`X2019.08.01`$ssn.object@predpoints@SSNPoints[[1]]@point.data
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
mapview(map)


new_points <- st_nearest_feature(wpt, map)


PID <- map$pid[new_points] 

map <- filter(map, pid %in% PID)


mapview(temp)


test <- cbind(wpt, temp) %>%
  select(-c(geometry.1))
  
mapview(test)

a <- ggplot(test) +
  geom_sf(aes(color = waypoint_name), alpha=0.8)+
  theme(legend.position = "none")
b <- ggplot(wpt) +
  geom_sf(aes(color = waypoint_name)) +
    theme(legend.position = "none")
grid.arrange(a,b)





