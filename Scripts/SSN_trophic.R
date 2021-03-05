#trophic window model evaluation ----
## trophic 2019 ----
trophic2019 <- importSSN("SSN/parsnip_trophic2019.ssn", predpts = "preds_o")
names(trophic2019@obspoints@SSNPoints[[1]]@point.data)[names(trophic2019@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   
names(trophic2019)

#create additive function and distance map
trophic2019 <- additive.function(trophic2019, "H2OArea", "computed.afv")
createDistMat(trophic2019, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)
names(trophic2019)

#plot observed points
plot(trophic2019, "awat", lwdLineCol = "afvArea", 
     xlab = "Easting", ylab = "Northing", 
     lwdLineEx = 15, lineCol = "black", asp = 1)

#Torgegram
trophic2019_torg <- Torgegram(trophic2019, "awat", nlag = 20)
plot(trophic2019_torg)


#awat model ----
names(trophic2019)
trophic2019_glmssn1 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019,
                       CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                       addfunccol = "computed.afv",
                       EstMeth = "ML")
summary(trophic2019_glmssn1)

trophic2019.pred1km <- predict(trophic2019_glmssn1, "preds_o")

#visualize
plot.SpatialStreamNetwork(trophic2019, "awat", add = TRUE, 
                          breaktype = "user", brks = c(4, 8, 10, 12, 14),
                          main = "Summer 2019 Average Temperature (C)", 
                          ylab = "Northing", 
                          xlab = "Easting",
                          cex = 0.5)

plot(trophic2019.pred1km, "awat", cex = 0.3, add = TRUE)


#residuals
#residuals plot
trophic2019_resid <- residuals(trophic2019_glmssn1)
plot(trophic2019_resid)
hist(trophic2019_resid)
hist(pars3, "augmean")

#LOOCV
cv.out <- CrossValidationSSN(trophic2019_glmssn3)
par(mfrow = c(1, 2))
plot(trophic2019_glmssn3$sampinfo$z, cv.out[, "cv.pred"], pch = 19,
     xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot(na.omit(getSSNdata.frame(trophic2019)[, "awat"]), cv.out[, "cv.se"],
     pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

#Variance composition
x <- varcomp(trophic2019_glmssn1)
x

#AIC Comparison
trophic2019_glmssn1 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019,
                              CorModels = c("Spherical.tailup", "Exponential.Euclid"),
                              addfunccol = "computed.afv")
summary(trophic2019_glmssn1)
trophic2019_glmssn2 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019, 
                        CorModels = c("Exponential.tailup", "Exponential.taildown"), 
                        addfunccol = "computed.afv") 
summary(trophic2019_glmssn2)
trophic2019_glmssn3 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019, 
                        CorModels = c("Mariah.tailup", "LinearSill.taildown"), 
                        addfunccol = "computed.afv")
summary(trophic2019_glmssn3)
trophic2019_glmssn4 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019, 
                        CorModels = "Exponential.Euclid", 
                        addfunccol = "computed.afv")
summary(trophic2019_glmssn4)
trophic2019_glmssn5 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019, 
                              CorModels = c("Exponential.tailup", "Mariah.taildown"), 
                              addfunccol = "computed.afv") 
summary(trophic2019_glmssn5)
trophic2019_glmssn6 <- glmssn(awat ~ avTmpA + H2OAreaA, trophic2019, 
                              CorModels = c("Exponential.tailup", "Exponential.Euclid"), 
                              addfunccol = "computed.afv") 
summary(trophic2019_glmssn6)
ModSel <- InfoCritCompare(list(trophic2019_glmssn1, trophic2019_glmssn2, 
                     trophic2019_glmssn3, trophic2019_glmssn4, 
                     trophic2019_glmssn5, trophic2019_glmssn6))


# trophic 2020 -----
trophic2020 <- importSSN("SSN/parsnip_trophic2020.ssn", predpts = "preds_o")
names(trophic2020@obspoints@SSNPoints[[1]]@point.data)[names(trophic2020@obspoints@SSNPoints[[1]]@point.data) == "avTmp"] <- "avTmpA"   
names(trophic2020)
