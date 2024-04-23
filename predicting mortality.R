library(demography)
library(StMoMo)
#Data extraction (Female data)
USA_DATA<-hmd.mx(country="USA", username="graceshiks74@gmail.com", password="@Grace4073", label="USA")
USA_DATA
head(USA_DATA)
usa_female<-StMoMoData(data=USA_DATA, series ="female",type="central")
head(usa_female)

#Data extraction (Male data)
usa_male<-StMoMoData(data=USA_DATA, series ="male",type="central")
head(usa_male)
par(mfrow = c(1, 2))
plot(USA_DATA,series="female",main="Female death rates")
plot(USA_DATA,series="male",main="Male death rates")
par(mfrow = c(1, 2))
plot(USA_DATA,series="female",plot.type="time",main="Female death rates")
plot(USA_DATA,series="male",plot.type="time",main="Male death rates")

#Females fitting and forecasts
#Fitting Renshaw Haberman model
LCfit <- fit(lc(), data =usa_female, ages.fit = 0:80)
head(LCfit)
plot(LCfit)
wxt <- genWeightMat(0:80, usa_female$years, clip = 3)
RHfit <- fit(rh(), data = usa_female, ages.fit = 0:80,
             wxt = wxt, start.ax = LCfit$ax,
             start.bx = LCfit$bx, start.kt = LCfit$kt)
plot(RHfit,parametricbx=FALSE)
wxt

plot(LCfit)
fitted.rh <- fitted(RHfit, type = "rates")
fitted.rh
ruxt <- RHfit$Dxt / RHfit$Ext

#Comparing the fitted vs. observed rates at random ages
plot(RHfit$years, ruxt["40", ], xlab = "year", ylab = "death rate",
     main = "fitted vs. observed rates at age 40")
lines(RHfit$years, fitted.rh["40", ])
forecast.rh<-forecast(RHfit,h=61)
forecast.rh
plot(forecast.rh)
forecast.rh$rates

#Fitting CBD model
M6 <- m6()
M6fit <- fit(M6, data = central2initial(usa_female), ages.fit =0:80)
M6fit
plot(M6fit, parametricbx = FALSE)
plot(M6fit)
fit.cbd<- fitted(M6fit, type = "rates")
fit.cbd
cuxt <- M6fit$Dxt / M6fit$Ext
#Comparing the fitted vs. observed rates at random ages
plot(M6fit$years, cuxt["40", ], xlab = "year", ylab = "death rate",
     main = "fitted vs. observed rates at age 40 CBD (0-80)")
lines(M6fit$years, fit.cbd["40", ])
forecast.cbd<-forecast(M6fit,h=61,gc.order = c(0,0,1),method="ML")
forecast.cbd
forecast.cbd$rates

#Checking RMSE
mxf<-usa_female$Dxt/usa_female$Ext
mxf
#Extracting the observed data
cohort1981F<-extractCohort(mxf,cohort=1981)
cohort2002F<-extractCohort(mxf,cohort=2002)
#Extracting the fitted data(RH and CBD)
cohort1981fittedRh<-extractCohort(fitted.rh,cohort=1981)
cohort2002fittedRh<-extractCohort(fitted.rh,cohort=2002)
#CBD
cohort1981FCBD<-extractCohort(mxf,cohort=1981)
cohort2002FCBD<-extractCohort(mxf,cohort=2002)
cohort1981fittedCBD<-extractCohort(fit.cbd,cohort=1981)
cohort2002fittedCBD<-extractCohort(fit.cbd,cohort=2002)
#RMSE CBD 1981
RMSE_RH1981 <- sqrt(mean((cohort1981F-cohort1981fittedRh)^2))
RMSE_RH1981
#RMSE CBD 2002
RMSE_RH2002<-sqrt(mean((cohort2002F-cohort2002fittedRh)^2))
RMSE_RH2002
#RMSE RH  1981
RMSE_CBD1981 <- sqrt(mean((cohort1981FCBD -cohort1981fittedCBD)^2))
RMSE_CBD1981
#RMSE RH 2002
RMSE_CBD2002<-sqrt(mean((cohort2002FCBD-cohort2002fittedCBD)^2))
RMSE_CBD2002
# Create a data frame to store the RMSE values
rmse_table <- data.frame(Model = c("CBD_1981","RH_1981","CBD_2002", "RH_2002"),
                         RMSE = c(RMSE_CBD1981, RMSE_RH1981,RMSE_CBD2002, RMSE_RH2002))
# Print the table
print(rmse_table)

#Checking MAE
#MAE CBD 1981
MAE_CBD1981 <- mean(abs(cohort1981FCBD -cohort1981fittedCBD))
MAE_CBD1981

# MAE CBD 2002
MAE_CBD2002 <- mean(abs(cohort2002FCBD-cohort2002fittedCBD))
MAE_CBD2002

# MAE RH 1981
MAE_RH1981 <- mean(abs(cohort1981F-cohort1981fittedRh))
MAE_RH1981

# MAE RH 2002
MAE_RH2002 <- mean(abs(cohort2002F-cohort2002fittedRh))
MAE_RH2002

# Create a data frame to store the MAE values
Femalemae_table <- data.frame(Model = c("CBD_1981", "RH_1981", "CBD_2002", "RH_2002"),
                              MAE = c(MAE_CBD1981, MAE_RH1981, MAE_CBD2002, MAE_RH2002))
# Print the table
print(Femalemae_table)

#Female Residuals
RH_Residuals<-residuals(RHfit)
RH_Residuals
CBD_Residuals<-residuals(M6fit)
CBD_Residuals
plot(RH_Residuals,type="scatter")
plot(CBD_Residuals,type="scatter")

#Male fitting and forecasts
#Renshaw Haberman
LCfitm <- fit(lc(), data =usa_male, ages.fit = 0:80)
head(LCfitm)
plot(LCfitm)
wxtm <- genWeightMat(0:80, usa_male$years, clip = 3)
RHfitm <- fit(rh(), data = usa_male, ages.fit = 0:80,
              wxt = wxtm, start.ax = LCfitm$ax,
              start.bx = LCfitm$bx, start.kt = LCfitm$kt)
plot(RHfitm)
plot(Rhfitm,gc.order = c(0,0,1))
fitted.rhm <- fitted(RHfitm, type = "rates")
fitted.rhm
ruxtm <- RHfitm$Dxt / RHfitm$Ext
plot(RHfitm$years, ruxtm["40", ], xlab = "year", ylab = "death rate",
     main = "fitted vs. observed rates at age 40")
lines(RHfitm$years, fitted.rhm["40", ])
forecast.rhm<-forecast(RHfitm,h=61,gc.order = c(0,0,1),method="ML")
forecast.rhm
plot(forecast.rhm)
forecast.rhm$rates
tail(forecast.rhm$rates)

#CBD with cohort effect
M6m <- m6()
M6mfit <- fit(M6, data = central2initial(usa_male), ages.fit =0:80)
plot(M6mfit, parametricbx = FALSE)
plot(M6mfit)
fit.cbdm<- fitted(M6mfit, type = "rates")
fit.cbdm
cuxtm <- M6mfit$Dxt / M6mfit$Ext
plot(M6mfit$years, cuxtm["40", ], xlab = "year", ylab = "death rate",
     main = "fitted vs. observed rates at age 40 CBD (0-80)")
lines(M6mfit$years, fit.cbdm["40", ])
forecast.cbdm<-forecast(M6mfit,h=61,gc.order = c(0,0,1),method="ML")
forecast.cbdm
forecast.cbdm$rates

#New RMSE
malexf<-usa_male$Dxt/usa_male$Ext
malexf
cohort1981m<-extractCohort(malexf,cohort=1981)
cohort2002m<-extractCohort(malexf,cohort=2002)
cohort1981fittedRhm<-extractCohort(fitted.rhm,cohort=1981)
cohort2002fittedRhm<-extractCohort(fitted.rhm,cohort=2002)
#CBD
cohort1981mCBD<-extractCohort(malexf,cohort=1981)
cohort2002mCBD<-extractCohort(malexf,cohort=2002)
cohort1981fittedCBDm<-extractCohort(fit.cbdm,cohort=1981)
cohort2002fittedCBDm<-extractCohort(fit.cbdm,cohort=2002)

#RMSE CBD 1981
RMSE_RH1981 <- sqrt(mean((cohort1981m-cohort1981fittedRhm)^2))
RMSE_RH1981
#RMSE CBD 2002
RMSE_RH2002<-sqrt(mean((cohort2002m-cohort2002fittedRhm)^2))
RMSE_RH2002
#RMSE RH  1981
RMSE_CBD1981 <- sqrt(mean((cohort1981mCBD -cohort1981fittedCBDm)^2))
RMSE_CBD1981
#RMSE RH 2002
RMSE_CBD2002<-sqrt(mean((cohort2002mCBD-cohort2002fittedCBDm)^2))
RMSE_CBD2002
# Create a data frame to store the RMSE values
rmse_table <- data.frame(Model = c("CBD_1981","RH_1981","CBD_2002", "RH_2002"),
                         RMSE = c(RMSE_CBD1981, RMSE_RH1981,RMSE_CBD2002, RMSE_RH2002))
# Print the table
print(rmse_table)

#EXTRACTS
malexf<-usa_male$Dxt/usa_male$Ext
malexf
tail(malexf)
write.csv(malexf,"Malemortality.csv")
write.csv(forecast_lcm$rates,"ForcastedMalenortalityRH.csv")
write.csv(forecast.cbdm$rates,"ForcastedMalemortalityCBD.csv")

RH_Residualsm<-residuals(RHfitm)
RH_Residualsm
CBD_Residualsm<-residuals(M6mfit)
CBD_Residualsm
plot(RH_Residualsm,type="scatter",main="RH Deviance residuals Males")
plot(CBD_Residualsm,type="scatter", main="CBD Deviance residuals Males")

#MAE
# MAE CBD 1981
MAE_CBD1981m <- mean(abs(mortalityM1981O - mortalityM1981))
MAE_CBD1981m

# MAE CBD 2002
MAE_CBD2002m <- mean(abs(mortalityM2002O - mortalityM2002))
MAE_CBD2002m

# MAE RH 1981
MAE_RH1981m <- mean(abs(mortalityM1981Orh - mortalityM1981rh))
MAE_RH1981m

# MAE RH 2002
MAE_RH2002m <- mean(abs(mortalityM2002Orh - mortalityM2002rh))
MAE_RH2002m

# Create a data frame to store the MAE values
Malemae_table <- data.frame(Model = c("CBD_1981", "RH_1981", "CBD_2002", "RH_2002"),
                            MAE = c(MAE_CBD1981m, MAE_RH1981m, MAE_CBD2002m, MAE_RH2002m))

# Print the table
print(Malemae_table)
