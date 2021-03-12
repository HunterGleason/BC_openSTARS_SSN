#SSN exploration on parsnip_test.ssn
library(SSN)


#example data
mf04p <- importSSN("MiddleFork.ssn", predpts = "pred1km")
mf04p <- importPredpts(mf04p, "Knapp", "ssn")
mf04p <- importPredpts(mf04p, "CapeHorn", "ssn")
names(mf04p)

#parsnip HuntData ----
pars1 <- importSSN("SSN/parsnip_trophic2019.ssn", predpts = "preds_o")

names(pars1)

pars1 <- additive.function(pars1, "H2OArea", "computed.afv")

createDistMat(pars1, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)

names(pars1)

plot(pars1, lwdLineCol = "H2OAreaA", lwdLineEx = 10, lineCol = "blue", 
     pch = 19, xlab = "x-coordinate (m)", ylab = "y-coordinate (m)", asp = 1)

brks <- plot(pars1, "augmean", lwdLineCol = "afvArea", 
             lwdLineEx = 15, lineCol = "black", xlab = "x-coordinate" ,
             ylab = "y-coordinate", asp = 1)

#inspect odd offshoot near mouth of river
plot(pars2, xlim = c(1150000,1210000), ylim = c(1100000,1150000))

pars1.Torg <- Torgegram(pars1, "augmean", nlag = 20)
plot(pars1.Torg)

names(pars1)


pars.glmssn1 <- glmssn(X2019.08.03 ~ avEleA + avSloA, pars1,
    CorModels = c("Exponential.tailup", "Exponential.taildown"), 
    addfunccol = "computed.afv")


summary(pars.glmssn1)


pars.pred1km <- predict(pars.glmssn1, "preds_o")
pars.pred1km


plot(pars.pred1km, "X2019.08.03", cex = .1)
plot(pars.pred1km)

#add data to object after the fact
#join data
ObsDF <- getSSNdata.frame(pars1)
day <- st_read("DataUTM10/trophic_2019_metrics.shp") %>%
    dplyr::select(-c(awat,awvar,mwat,mwvar,mwvar,awcoef,mwcoef,site_cd,augmean,
    augmax,augvar,augcoef,geometry))

#select out pid column
x <- ObsDF$pid

#bind new data
testObs <- ObsDF %>%
    dplyr::left_join(day, by = "wypnt_n")

#fix rownames to match the pid column.... not sure why this works
rownames(testObs) <- x

pars1 <- putSSNdata.frame(testObs, pars1)


names(pars1)
rownames(testObs) <- 1:55
testObs$cat
testObs$locID <- 1:55
rownames(testObs) <- x
#### parsnip test 3 ----
#parsnip object by Bryce 
pars3 <- importSSN("SSN/parsnip_test3.ssn", predpts = "preds_o")

names(pars3@data)

pars3 <- additive.function(pars3, "H2OArea", "computed.afv")

createDistMat(pars3, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)

names(pars3)

brks <- plot(pars3, "augmean", lwdLineCol = "afvArea", 
             lwdLineEx = 15, lineCol = "black", xlab = "x-coordinate" ,
             ylab = "y-coordinate", asp = 1)

pars3.Torg <- Torgegram(pars3, "augmean", nlag = 20)
plot(pars3.Torg)

names(pars3)

pars.glmssn3 <- glmssn(augmean ~ avEleA, pars3,
                       CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                       addfunccol = "computed.afv")
pars.glmssn1 <- glmssn(augmean ~ avSloA + avGrdtA, pars3,
                       CorModels = c("Exponential.tailup", "Exponential.taildown", "Exponential.Euclid"), 
                       addfunccol = "computed.afv")
summary(pars.glmssn3)

pars.pred5km <- predict(pars.glmssn3, "preds_o")
pars.pred5km

plot(pars.pred5km, "augmean", cex = 0.1)

#inspect upper anzac predictions
plot(pars.pred5km, xlim = c(1220000,1260000), ylim = c(1080000,1140000))

## parsnip test 4 ----
#ssn object with air_temp
pars3 <- importSSN("SSN/parsnip_test4.ssn", predpts = "preds_o")

names(pars3@data)

pars3 <- additive.function(pars3, "H2OArea", "computed.afv")

createDistMat(pars3, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)

names(pars3)

brks <- plot(pars3, "augmean", lwdLineCol = "afvArea", 
             lwdLineEx = 15, lineCol = "black", xlab = "x-coordinate" ,
             ylab = "y-coordinate", asp = 1)

pars3.Torg <- Torgegram(pars3, "augmean", nlag = 20)
plot(pars3.Torg)

names(pars3)

#model creation
pars.glmssn3 <- glmssn(augmean ~ avTmpA, pars3,
                       CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                       addfunccol = "computed.afv",
                       EstMeth = "ML")

summary(pars.glmssn3)

#prediction for pred points
pars.pred1km <- predict(pars.glmssn3, "preds_o")


#visualize
plot(pars.pred1km, "augmean", cex = 0.1, main = "august")

plot.glmssn.predict(pars.pred1km, "augmean", breaktype = "user", 
                    brks = c(8, 10, 12, 14), cex = 0.1)
title(main = "August Mean Monthly Temperature (C)")

plot(pars3, "augmean", lwdLineCol = "afvArea", brks = c(8, 10, 12, 14), 
             lwdLineEx = 10, lineCol = "black", xlab = "x-coordinate" ,
             ylab = "y-coordinate", asp = 1, main = "August")

plot.glmssn.predict(pars.pred1km, "augmean", 
                    cex = 0.1, add = TRUE)




plot.SpatialStreamNetwork(pars3, "augmean", add = TRUE, 
                          breaktype = "user", brks = c(4, 8, 10, 12, 14),
                          main = "August 2019 Average Temperature (C)", 
                          ylab = "Northing", 
                          xlab = "Easting"
                          )
plot(pars.pred1km, "augmean", cex = 0.3, add = TRUE)


#residuals plot
pars.resid3 <- residuals(pars.glmssn3)
plot(pars.resid3)
hist(pars.resid3)
hist(pars3, "augmean")

names(pars.resid3)

#LOOCV
cv.out <- CrossValidationSSN(pars.glmssn3)
par(mfrow = c(1, 2))
plot(pars.glmssn3$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(pars3)[, "augmean"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(pars.glmssn3)
x[1,2]
x[2,2]
x[3,2]


#AIC ------
AIC(pars.glmssn3)


###Model Selection -- find more combinations after rereading pubs on spatial models
pars3.glmssn1 <- glmssn(augmean ~ avTmpA, pars3, 
                        CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                        addfunccol = "computed.afv") 
pars3.glmssn2 <- glmssn(augmean ~ avTmpA , pars3, 
                        CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                        addfunccol = "computed.afv")
pars3.glmssn3 <- glmssn(augmean ~ avTmpA, pars3, 
                        CorModels = "Exponential.Euclid", 
                        addfunccol = "computed.afv")
pars3.glmssn4 <- glmssn(augmean ~ avTmpA, pars3,
                       CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                       addfunccol = "computed.afv")


#wouldn't run
# pars3.glmssn4 <- glmssn(augmean ~ avTmpA, pars3, 
#                         CorModels = c("Spherical.tailup", "Spherical.taildown"), 
#                         addfunccol = "computed.afv")

