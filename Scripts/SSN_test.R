#SSN exploration on parsnip_test.ssn
library(SSN)

#example data
mf04p <- importSSN("MiddleFork.ssn", predpts = "pred1km")
mf04p <- importPredpts(mf04p, "Knapp", "ssn")
mf04p <- importPredpts(mf04p, "CapeHorn", "ssn")
names(mf04p)

#parsnip object create by Hunter
pars <- importSSN("Data/parsnip_test.ssn", predpts = "preds_o")


names(pars@data)

pars <- additive.function(pars, "H2OArea", "computed.afv")


createDistMat(pars, predpts = "preds_o", o.write = TRUE,  amongpreds = TRUE)

names(pars)

plot(pars, lwdLineCol = "H2OAreaA", lwdLineEx = 10, lineCol = "blue", 
     pch = 19, xlab = "x-coordinate (m)", ylab = "y-coordinate (m)", asp = 1)

brks <- plot(pars, "augmean", lwdLineCol = "afvArea", 
             lwdLineEx = 15, lineCol = "black", xlab = "x-coordinate" ,
             ylab = "y-coordinate", asp = 1)

pars.Torg <- Torgegram(pars, "augmean", nlag = 20)
plot(pars.Torg)


pars.glmssn1 <- glmssn(augmean ~ avEleA + avSloA, pars,
    CorModels = c("Exponential.tailup", "Exponential.taildown", "Exponential.Euclid"), 
    addfunccol = "computed.afv")


summary(pars.glmssn1)

pars.pred1km <- predict(pars.glmssn1, "preds_o")
pars.pred1km

plot(pars.pred1km, "augmean")


