library(SSN)
#trophic window model evaluation ----
## TROPHIC 2019 ----
trophic2019 <- importSSN("data/parsnip_trophic2019.ssn", predpts = "preds_o")
names(trophic2019@obspoints@SSNPoints[[1]]@point.data)[names(trophic2019@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   



#create additive function and distance map
trophic2019 <- additive.function(trophic2019, "H2OArea", "computed.afv")
createDistMat(trophic2019, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)
names(trophic2019)


#awat model ----
#plot observed points
plot(trophic2019, "awat", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     breaktype = "user", brks = c(4,8,10,12,14,16),
     lwdLineEx = 15, lineCol = "black", asp = 1,
     main = "Summer 2019 Observed Points AWAT (C)")

#Torgegram
trophic2019awat_torg <- Torgegram(trophic2019, "awat", nlag = 20)
plot(trophic2019awat_torg)

#awat model selection
trophic2019awat_glmssn1 <- glmssn(awat ~ avTmpA + avEleA, trophic2019,
                                  CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                  addfunccol = "computed.afv")
summary(trophic2019awat_glmssn1)
trophic2019awat_glmssn2 <- glmssn(awat ~ avTmpA + avEleA, trophic2019, 
                                  CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                  addfunccol = "computed.afv") 
summary(trophic2019awat_glmssn2)
trophic2019awat_glmssn3 <- glmssn(awat ~ avTmpA + avEleA, trophic2019, 
                                  CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                  addfunccol = "computed.afv")
summary(trophic2019awat_glmssn3)
trophic2019awat_glmssn4 <- glmssn(awat ~ avTmpA + avEleA, trophic2019, 
                                  CorModels = "Exponential.Euclid", 
                                  addfunccol = "computed.afv")
summary(trophic2019awat_glmssn4)
trophic2019awat_glmssn5 <- glmssn(awat ~ avTmpA + avEleA, trophic2019, 
                                  CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                  addfunccol = "computed.afv") 
summary(trophic2019awat_glmssn5)
trophic2019awat_glmssn6 <- glmssn(awat ~ avTmpA + avEleA, trophic2019, 
                                  CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                  addfunccol = "computed.afv") 
summary(trophic2019awat_glmssn6)
trophic2019awat_glmssn7 <- glmssn(awat ~ avTmpA + avEleA, trophic2019,
                                  CorModels = c("Spherical.tailup", "Spherical.taildown"),
                                  addfunccol = "computed.afv")
summary(trophic2019awat_glmssn7)
ModSelawat <- InfoCritCompare(list(trophic2019awat_glmssn1, trophic2019awat_glmssn2, 
                                   trophic2019awat_glmssn3, trophic2019awat_glmssn4, 
                                   trophic2019awat_glmssn5, trophic2019awat_glmssn6,
                                   trophic2019awat_glmssn7))
View(ModSelawat)

#top model from AIC
trophic2019awat.pred1km <- predict(trophic2019awat_glmssn7, "preds_o")

plot.SpatialStreamNetwork(trophic2019, "awat", add = TRUE, 
                          breaktype = "user", brks = c(4,8,10,12,14,16),
                          main = "Summer 2019 AWAT (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2019awat.pred1km, "awat", cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2019_resid <- residuals(trophic2019awat_glmssn7)
plot(trophic2019_resid)
hist(trophic2019_resid)

#LOOCV
cv.out <- CrossValidationSSN(trophic2019awat_glmssn7)
par(mfrow = c(1, 2))
plot(trophic2019awat_glmssn7$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2019)[, "awat"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2019awat_glmssn1)
x


#awcoef ----
#plot observed points
plot(trophic2019, "awcoef", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2019awcoef_torg <- Torgegram(trophic2019, "awcoef", nlag = 20)
plot(trophic2019awcoef_torg)
names(trophic2019)

#awcoef model selection
trophic2019awcoef_glmssn1 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019,
                                    CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                    addfunccol = "computed.afv")
summary(trophic2019awcoef_glmssn1)
trophic2019awcoef_glmssn2 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019awcoef_glmssn2)
trophic2019awcoef_glmssn3 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                    addfunccol = "computed.afv")
summary(trophic2019awcoef_glmssn3)
trophic2019awcoef_glmssn4 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = "Exponential.Euclid", 
                                    addfunccol = "computed.afv")
summary(trophic2019awcoef_glmssn4)
trophic2019awcoef_glmssn5 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019awcoef_glmssn5)
trophic2019awcoef_glmssn6 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019awcoef_glmssn6)
trophic2019awcoef_glmssn7 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2019,
                                    CorModels = c("Spherical.tailup", "Spherical.taildown"),
                                    addfunccol = "computed.afv")
summary(trophic2019awcoef_glmssn7)
ModSelawcoef <- InfoCritCompare(list(trophic2019awcoef_glmssn1, trophic2019awcoef_glmssn2, 
                                     trophic2019awcoef_glmssn3, trophic2019awcoef_glmssn4, 
                                     trophic2019awcoef_glmssn5, trophic2019awcoef_glmssn6,
                                     trophic2019awcoef_glmssn7))
View(ModSelawcoef)

#top model from AIC
trophic2019awcoef.pred1km <- predict(trophic2019awcoef_glmssn7, "preds_o")

plot.SpatialStreamNetwork(trophic2019, "awcoef", add = TRUE,
                          main = "Summer 2019 awcoef (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2019awcoef.pred1km, "awcoef", cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2019_resid <- residuals(trophic2019awcoef_glmssn7)
plot(trophic2019_resid)
hist(trophic2019_resid)
hist(trophic2019, "awcoef")

#LOOCV
cv.out <- CrossValidationSSN(trophic2019awcoef_glmssn7)
par(mfrow = c(1, 2))
plot(trophic2019awcoef_glmssn7$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2019)[, "awcoef"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2019awcoef_glmssn7)
x


#augmax -----
#plot observed points
plot(trophic2019, "augmax", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2019augmax_torg <- Torgegram(trophic2019, "augmax", nlag = 20)
plot(trophic2019augmax_torg)

#augmax model selection
trophic2019augmax_glmssn1 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019,
                                    CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                    addfunccol = "computed.afv")
summary(trophic2019augmax_glmssn1)
trophic2019augmax_glmssn2 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019augmax_glmssn2)
trophic2019augmax_glmssn3 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                    addfunccol = "computed.afv")
summary(trophic2019augmax_glmssn3)
trophic2019augmax_glmssn4 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = "Exponential.Euclid", 
                                    addfunccol = "computed.afv")
summary(trophic2019augmax_glmssn4)
trophic2019augmax_glmssn5 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019augmax_glmssn5)
trophic2019augmax_glmssn6 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                    CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                    addfunccol = "computed.afv") 
summary(trophic2019augmax_glmssn6)
trophic2019augmax_glmssn7<- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2019, 
                                   CorModels = c("Spherical.tailup", "Spherical.taildown"), 
                                   addfunccol = "computed.afv")
summary(trophic2019augmax_glmssn7)
ModSelaugmax <- InfoCritCompare(list(trophic2019augmax_glmssn1, trophic2019augmax_glmssn2, 
                                     trophic2019augmax_glmssn3, trophic2019augmax_glmssn4, 
                                     trophic2019augmax_glmssn5, trophic2019augmax_glmssn6, trophic2019augmax_glmssn7))
View(ModSelaugmax)

#top model from AIC
trophic2019augmax.pred1km <- predict(trophic2019augmax_glmssn3, "preds_o")

plot.SpatialStreamNetwork(trophic2019, "augmax", add = TRUE, 
                          breaktype = "user", brks = c(4,8,10,12,14,18),
                          main = "Summer 2019 augmax (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2019augmax.pred1km, "augmax",  cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2019_resid <- residuals(trophic2019augmax_glmssn3)
plot(trophic2019_resid)
hist(trophic2019_resid)

#LOOCV
cv.out <- CrossValidationSSN(trophic2019augmax_glmssn3)
par(mfrow = c(1, 2))
plot(trophic2019augmax_glmssn3$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2019)[, "augmax"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2019augmax_glmssn1)
x



# TROPHIC 2020 -----
trophic2020 <- importSSN("data/parsnip_trophic2020.ssn", predpts = "preds_o")
names(trophic2020@obspoints@SSNPoints[[1]]@point.data)[names(trophic2020@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   
names(trophic2020)

#create additive function and distance map
trophic2020 <- additive.function(trophic2020, "H2OArea", "computed.afv")
createDistMat(trophic2020, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)
names(trophic2020)



#awat model -----
#plot observed points
plot(trophic2020, "awat", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2020awat_torg <- Torgegram(trophic2020, "awat", nlag = 20)
plot(trophic2020awat_torg)

#awat model selection
trophic2020awat_glmssn1 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020,
                                  CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                  addfunccol = "computed.afv")
summary(trophic2020awat_glmssn1)
trophic2020awat_glmssn2 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020, 
                                  CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                  addfunccol = "computed.afv") 
summary(trophic2020awat_glmssn2)
trophic2020awat_glmssn3 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020, 
                                  CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                  addfunccol = "computed.afv")
summary(trophic2020awat_glmssn3)
trophic2020awat_glmssn4 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020, 
                                  CorModels = "Exponential.Euclid", 
                                  addfunccol = "computed.afv")
summary(trophic2020awat_glmssn4)
trophic2020awat_glmssn5 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020, 
                                  CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                  addfunccol = "computed.afv") 
summary(trophic2020awat_glmssn5)
trophic2020awat_glmssn6 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020, 
                                  CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                  addfunccol = "computed.afv") 
summary(trophic2020awat_glmssn6)
trophic2020awat_glmssn7 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2020,
                                  CorModels = c("Spherical.tailup", "Spherical.taildown"),
                                  addfunccol = "computed.afv")
summary(trophic2020awat_glmssn7)
ModSelawat <- InfoCritCompare(list(trophic2020awat_glmssn1, trophic2020awat_glmssn2, 
                                   trophic2020awat_glmssn3, trophic2020awat_glmssn4, 
                                   trophic2020awat_glmssn5, trophic2020awat_glmssn6,
                                   trophic2020awat_glmssn7))
View(ModSelawat)

#top model from AIC
trophic2020awat.pred1km <- predict(trophic2020awat_glmssn1, "preds_o")

plot.SpatialStreamNetwork(trophic2020, "awat", add = TRUE, 
                          breaktype = "user", brks = c(4,8,10,12,14,16),
                          main = "Summer 2020 AWAT (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2020awat.pred1km, "awat", cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2020_resid <- residuals(trophic2020awat_glmssn1)
hist(trophic2020_resid)
plot(trophic2020_resid,
     breaktype = "user", brks = c(-3,-2,1,4,8))

#remove anzr 42 as per diagnostic issues
ObsDFr <- getSSNdata.frame(trophic2020_resid)
ObsDF <- getSSNdata.frame(trophic2020)
indOutlier <- ObsDFr["_resid_"] < -3
ObsDF[indOutlier, "awat"] <- NA
trophic2020 <- putSSNdata.frame(ObsDF, trophic2020)

#LOOCV
cv.out <- CrossValidationSSN(trophic2020awat_glmssn1)
par(mfrow = c(1, 2))
plot(trophic2020awat_glmssn1$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2020)[, "awat"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2020awat_glmssn1)
x

#awcoef ----
#plot observed points
plot(trophic2020, "awcoef", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2020awcoef_torg <- Torgegram(trophic2020, "awcoef", nlag = 20)
plot(trophic2020awcoef_torg)
names(trophic2020)

#awcoef model selection
trophic2020awcoef_glmssn1 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020,
                                    CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                    addfunccol = "computed.afv")
summary(trophic2020awcoef_glmssn1)
trophic2020awcoef_glmssn2 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020awcoef_glmssn2)
trophic2020awcoef_glmssn3 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                    addfunccol = "computed.afv")
summary(trophic2020awcoef_glmssn3)
trophic2020awcoef_glmssn4 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = "Exponential.Euclid", 
                                    addfunccol = "computed.afv")
summary(trophic2020awcoef_glmssn4)
trophic2020awcoef_glmssn5 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020awcoef_glmssn5)
trophic2020awcoef_glmssn6 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020awcoef_glmssn6)
trophic2020awcoef_glmssn7 <- glmssn(awcoef ~ avTmpA + H2OAreaA, trophic2020,
                                    CorModels = c("Spherical.tailup", "Spherical.taildown"),
                                    addfunccol = "computed.afv")
summary(trophic2020awcoef_glmssn7)
ModSelawcoef <- InfoCritCompare(list(trophic2020awcoef_glmssn1, trophic2020awcoef_glmssn2, 
                                     trophic2020awcoef_glmssn3, trophic2020awcoef_glmssn4, 
                                     trophic2020awcoef_glmssn5, trophic2020awcoef_glmssn6,
                                     trophic2020awcoef_glmssn7))
View(ModSelawcoef)

#top model from AIC
trophic2020awcoef.pred1km <- predict(trophic2020awcoef_glmssn4, "preds_o")

plot.SpatialStreamNetwork(trophic2020, "awcoef", add = TRUE,
                          main = "Summer 2020 awcoef (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2020awcoef.pred1km, "awcoef", cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2020_resid <- residuals(trophic2020awcoef_glmssn3)
hist(trophic2020_resid)
hist(trophic2020, "awcoef")
plot(trophic2020_resid, 
     breaktype = "user", brks = c(-0.1, -0.03,-0.02,-0.01,0.01,0.02,0.03, 0.1))

#residual outliers
ObsDFr <- getSSNdata.frame(trophic2020_resid) %>%
  select(site_cd, `_resid_`, awcoef)
ObsDF <- getSSNdata.frame(trophic2020)
indOutlier <- ObsDFr["site_cd"] == c('homr32')
ObsDF[indOutlier, "awcoef"] <- NA
indOutlier <- ObsDFr["site_cd"] == c('anzr42')
ObsDF[indOutlier, "awcoef"] <- NA
indOutlier <- ObsDFr["site_cd"] == c('homr37')
ObsDF[indOutlier, "awcoef"] <- NA
trophic2020 <- putSSNdata.frame(ObsDF, trophic2020)


#LOOCV
cv.out <- CrossValidationSSN(trophic2020awcoef_glmssn7)
par(mfrow = c(1, 2))
plot(trophic2020awcoef_glmssn7$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2020)[, "awcoef"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2020awcoef_glmssn7)
x


#augmax -----
#plot observed points
plot(trophic2020, "augmax", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2020augmax_torg <- Torgegram(trophic2020, "augmax", nlag = 20)
plot(trophic2020augmax_torg)

#augmax model selection
trophic2020augmax_glmssn1 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020,
                                    CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                                    addfunccol = "computed.afv")
summary(trophic2020augmax_glmssn1)
trophic2020augmax_glmssn2 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020augmax_glmssn2)
trophic2020augmax_glmssn3 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                                    addfunccol = "computed.afv")
summary(trophic2020augmax_glmssn3)
trophic2020augmax_glmssn4 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = "Exponential.Euclid", 
                                    addfunccol = "computed.afv")
summary(trophic2020augmax_glmssn4)
trophic2020augmax_glmssn5 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020augmax_glmssn5)
trophic2020augmax_glmssn6 <- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                    CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                                    addfunccol = "computed.afv") 
summary(trophic2020augmax_glmssn6)
trophic2020augmax_glmssn7<- glmssn(augmax ~ avTmpA + H2OAreaA, trophic2020, 
                                   CorModels = c("Spherical.tailup", "Spherical.taildown"), 
                                   addfunccol = "computed.afv")
summary(trophic2020augmax_glmssn7)
ModSelaugmax <- InfoCritCompare(list(trophic2020augmax_glmssn1, trophic2020augmax_glmssn2, 
                                     trophic2020augmax_glmssn3, trophic2020augmax_glmssn4, 
                                     trophic2020augmax_glmssn5, trophic2020augmax_glmssn6, trophic2020augmax_glmssn7))
View(ModSelaugmax)

#top model from AIC
trophic2020augmax.pred1km <- predict(trophic2020augmax_glmssn1, "preds_o")

plot.SpatialStreamNetwork(trophic2020, "augmax", add = TRUE, 
                          breaktype = "user", brks = c(4,8,10,12,14,18),
                          main = "Summer 2020 augmax (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2020augmax.pred1km, "augmax",  cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2020_resid <- residuals(trophic2020augmax_glmssn3)
plot(trophic2020_resid)
hist(trophic2020_resid)

#LOOCV
cv.out <- CrossValidationSSN(trophic2020augmax_glmssn3)
par(mfrow = c(1, 2))
plot(trophic2020augmax_glmssn3$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2020)[, "augmax"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2020augmax_glmssn1)
x