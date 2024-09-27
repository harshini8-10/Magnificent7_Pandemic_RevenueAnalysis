# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#------------------------------------- Meta -----------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")


# CREATE DATAFRAME
Meta.data <- read.csv("MetaRevenue.csv")

# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# meta_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023)

meta_allrevenue.ts <- ts(Meta.data$Meta_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
meta_allrevenue.ts

# meta_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
meta_revenue.ts <- ts(Meta.data$Meta_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
meta_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Millions of Dollars)", 
     ylim = c(300, 45000), bty = "l",
     xaxt = "n", xlim = c(2010, 2025.25), 
     main = "Meta Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
meta_allrevenue.stl <- stl(meta_allrevenue.ts, s.window = "periodic")
autoplot(meta_allrevenue.stl, main = "Meta Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
meta_allautocor <- Acf(meta_allrevenue.ts, lag.max = 4, 
                        main = "Autocorrelation Chart for Meta")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
meta_autocor <- Acf(meta_revenue.ts, lag.max = 4, 
      main = "Autocorrelation Chart for Meta Revenue Data (Pre-Covid : 2010 - 2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
meta_lag <- round(meta_autocor$lag,0)
meta_ACF <- round(meta_autocor$acf,3)
data.frame(meta_lag,meta_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR META'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
meta_revenue.ar1 <- Arima(meta_revenue.ts, order = c(1,0,0),method = "ML")
summary(meta_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'.


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1.
ar1 <- 0.9931
s.e. <- 0.0093
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

#--------- APPROACH 2: EXAMINATION OF ACF FOR FIRST DIFFERENCED SERIES --------#

# CREATE FIRST DIFFERENCED META REVENUE DATA USING lag1
diff.meta_revenue.ts <- diff(meta_revenue.ts, lag = 1)
diff.meta_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED META REVENUE
Acf(diff.meta_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Meta Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(meta_revenue.ts) = 40 (10 YEARS)
# meta_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# meta_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

meta_nValid <- 12
meta_nTrain <- length(meta_revenue.ts) - meta_nValid
meta_train.ts <- window(meta_revenue.ts, start = c(2010, 1), end = c(2010, meta_nTrain))
meta_train.ts
meta_valid.ts <- window(meta_revenue.ts, start = c(2010, meta_nTrain + 1), end = c(2010, meta_nTrain + meta_nValid))
meta_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# meta_future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

meta_future.ts <- window(meta_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
meta_future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(meta_train.ts,
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(300,45000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(meta_valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, COVID & POST-COVID INTERVALS
lines(c(2017,2017), c(0,46000)) 
lines(c(2020,2020), c(0,46000)) 
text(2013.5,46000, "TRAINING")
text(2018.5,46000, "VALIDATION", col = "blue")
text(2022.2, 46000, "COVID & POST-COVID", col ="green")
arrows(2010,45000,2016.9, 45000,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,45000,2019.9, 45000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,45000,2023.9, 45000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
meta_revenue.naive.pred <- naive(meta_train.ts, h = meta_nValid)
meta_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_revenue.naive.pred$mean, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
meta_revenuef.naive.pred <- naive(meta_revenue.ts, h = 16)
meta_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(meta_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 1 : NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(meta_allrevenue.ts, col = "black", lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Naive Forecast: Pre-Covid Period (2010-2019)",
                              "Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(meta_revenue.ts))$fitted, meta_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
meta_revenue.snaive.pred <- snaive(meta_train.ts, h = meta_nValid)
meta_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_revenue.snaive.pred$mean, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
meta_revenuef.snaive.pred <- snaive(meta_revenue.ts, h = 16)
meta_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(meta_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 2 : SEASONAL NAIVE FORECAST", col = "green", lwd =3) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_revenuef.snaive.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, col = "black", lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                              "Seasonal Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(meta_revenue.ts))$fitted, meta_revenue.ts), 3)


#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

meta_train.lin <- tslm(meta_train.ts ~ trend)
summary(meta_train.lin)

# FORECAST FOR VALIDATION DATA
meta_train.lin.pred <- forecast(meta_train.lin, h = meta_nValid, level = 0)
meta_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.lin.pred$mean, meta_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

meta_train.quad <- tslm(meta_train.ts ~ trend + I(trend^2))
summary(meta_train.quad)

# FORECAST FOR VALIDATION DATA
meta_train.quad.pred <- forecast(meta_train.quad, h = meta_nValid, level = 0)
meta_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.quad.pred$mean, meta_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

meta_train.season <- tslm(meta_train.ts ~ season)
summary(meta_train.season)

# FORECAST FOR VALIDATION DATA
meta_train.season.pred <- forecast(meta_train.season, h = meta_nValid, level = 0)
meta_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.season.pred$mean, meta_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

meta_train.lin.season <- tslm(meta_train.ts ~ trend + season)
summary(meta_train.lin.season)

# FORECAST FOR VALIDATION DATA
meta_train.lin.season.pred <- forecast(meta_train.lin.season, h = meta_nValid, level = 0)
meta_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.lin.season.pred$mean, meta_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

meta_train.quad.season <- tslm(meta_train.ts ~ trend + I(trend^2) + season)
summary(meta_train.quad.season)

# FORECAST FOR VALIDATION DATA
meta_train.quad.season.pred <- forecast(meta_train.quad.season, h = meta_nValid, level = 0)
meta_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.quad.season.pred$mean, meta_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

meta_lin.trend <- tslm(meta_revenue.ts ~ trend)
summary(meta_lin.trend)

# FORECAST FOR POST-COVID PERIOD
meta_lin.trend.pred <- forecast(meta_lin.trend, h = 16, level = 0)
meta_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(meta_lin.trend.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_lin.trend.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_lin.trend.pred$fitted, meta_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

meta_quad.trend <- tslm(meta_revenue.ts ~ trend + I(trend^2))
summary(meta_quad.trend)

# FORECAST FOR POST-COVID PERIOD
meta_quad.trend.pred <- forecast(meta_quad.trend, h = 16, level = 0)
meta_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(meta_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     ylim = c(300, 45000), bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_quad.trend.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_quad.trend.pred$fitted, meta_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

meta_revenue.season <- tslm(meta_revenue.ts ~ season)
summary(meta_revenue.season)

# FORECAST FOR POST-COVID PERIOD
meta_revenue.season.pred <- forecast(meta_revenue.season, h = 16, level = 0)
meta_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(meta_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_revenue.season.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,44000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_revenue.season.pred$fitted, meta_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

meta_lin.season <- tslm(meta_revenue.ts ~ trend + season)
summary(meta_lin.season)

# FORECAST FOR POST-COVID PERIOD
meta_lin.season.pred <- forecast(meta_lin.season, h = 16, level = 0)
meta_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(meta_lin.season.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_lin.season.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-CVOID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_lin.season.pred$fitted, meta_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

meta_quad.season <- tslm(meta_revenue.ts ~ trend + I(trend^2) + season)
summary(meta_quad.season)

# FORECAST FOR POST-COVID PERIOD
meta_quad.season.pred <- forecast(meta_quad.season, h = 16, level = 0)
meta_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(meta_quad.season.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_quad.season.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_quad.season.pred$fitted, meta_revenue.ts),3)


#-------------------- MODEL 4 : TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
meta_trend.seas <- tslm(meta_train.ts ~ trend + season)
summary(meta_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
meta_trend.seas.res <- meta_trend.seas$residuals
meta_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
meta_ma.trail.res_2 <- rollmean(meta_trend.seas.res, k = 2, align = "right")
meta_ma.trail.res_2
meta_ma.trail.res_3 <- rollmean(meta_trend.seas.res, k = 3, align = "right")
meta_ma.trail.res_3
meta_ma.trail.res_4 <- rollmean(meta_trend.seas.res, k = 4, align = "right")
meta_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
meta_trend.seas.pred <- forecast(meta_trend.seas, h = meta_nValid, level = 0)
meta_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
meta_trend.seas.res.valid <- meta_valid.ts - meta_trend.seas.pred$mean
meta_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
meta_ma.trail.res.pred_2 <- forecast(meta_ma.trail.res_2, h = meta_nValid, level = 0)
meta_ma.trail.res.pred_2
meta_ma.trail.res.pred_3 <- forecast(meta_ma.trail.res_3, h = meta_nValid, level = 0)
meta_ma.trail.res.pred_3
meta_ma.trail.res.pred_4 <- forecast(meta_ma.trail.res_4, h = meta_nValid, level = 0)
meta_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
meta_fst.2level_2 <- meta_trend.seas.pred$mean + meta_ma.trail.res.pred_2$mean
meta_fst.2level_2
meta_fst.2level_3 <- meta_trend.seas.pred$mean + meta_ma.trail.res.pred_3$mean
meta_fst.2level_3
meta_fst.2level_4 <- meta_trend.seas.pred$mean + meta_ma.trail.res.pred_4$mean
meta_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
meta_valid_2.df <- round(data.frame(meta_valid.ts, meta_trend.seas.pred$mean, 
                               meta_ma.trail.res.pred_2$mean, 
                               meta_fst.2level_2), 3)
names(meta_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
meta_valid_2.df

meta_valid_3.df <- round(data.frame(meta_valid.ts, meta_trend.seas.pred$mean, 
                               meta_ma.trail.res.pred_3$mean, 
                               meta_fst.2level_3), 3)
names(meta_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
meta_valid_3.df

meta_valid_4.df <- round(data.frame(meta_valid.ts, meta_trend.seas.pred$mean, 
                               meta_ma.trail.res.pred_4$mean, 
                               meta_fst.2level_4), 3)
names(meta_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
meta_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(meta_fst.2level_2, meta_valid.ts), 3)
round(accuracy(meta_fst.2level_3, meta_valid.ts), 3)
round(accuracy(meta_fst.2level_4, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
meta_tot.trend.seas <- tslm(meta_revenue.ts ~ trend  + season)
summary(meta_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
meta_tot.trend.seas.pred <- forecast(meta_tot.trend.seas, h = 16, level = 0)
meta_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
meta_tot.trend.seas.res <- meta_tot.trend.seas$residuals
meta_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
meta_tot.ma.trail.res_2 <- rollmean(meta_tot.trend.seas.res, k = 2, align = "right")
meta_tot.ma.trail.res_2
meta_tot.ma.trail.res_3 <- rollmean(meta_tot.trend.seas.res, k = 3, align = "right")
meta_tot.ma.trail.res_3
meta_tot.ma.trail.res_4 <- rollmean(meta_tot.trend.seas.res, k = 4, align = "right")
meta_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
meta_tot.ma.trail.res_2.pred <- forecast(meta_tot.ma.trail.res_2, h = 16, level = 0)
meta_tot.ma.trail.res_2.pred
meta_tot.ma.trail.res_3.pred <- forecast(meta_tot.ma.trail.res_3, h = 16, level = 0)
meta_tot.ma.trail.res_3.pred
meta_tot.ma.trail.res_4.pred <- forecast(meta_tot.ma.trail.res_4, h = 16, level = 0)
meta_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
meta_tot.fst.2level_2 <- meta_tot.trend.seas.pred$mean + meta_tot.ma.trail.res_2.pred$mean
meta_tot.fst.2level_2
meta_tot.fst.2level_3 <- meta_tot.trend.seas.pred$mean + meta_tot.ma.trail.res_3.pred$mean
meta_tot.fst.2level_3
meta_tot.fst.2level_4 <- meta_tot.trend.seas.pred$mean + meta_tot.ma.trail.res_4.pred$mean
meta_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
meta_future_2.df <- round(data.frame(meta_tot.trend.seas.pred$mean, meta_tot.ma.trail.res_2.pred$mean, 
                                meta_tot.fst.2level_2), 3)
names(meta_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_future_2.df

meta_future_3.df <- round(data.frame(meta_tot.trend.seas.pred$mean, meta_tot.ma.trail.res_3.pred$mean, 
                                meta_tot.fst.2level_3), 3)
names(meta_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_future_3.df

meta_future_4.df <- round(data.frame(meta_tot.trend.seas.pred$mean, meta_tot.ma.trail.res_4.pred$mean, 
                                meta_tot.fst.2level_4), 3)
names(meta_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), lwd =2, xaxt = "n",
     main = "MODEL 4A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(meta_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(meta_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"),
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,45000)) 
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(meta_tot.trend.seas.res, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(-4000, 20000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(meta_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(meta_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(meta_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(meta_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(meta_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 15000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-4000,20000)) 
text(2015,20000, "PRE-COVID", col = "blue")
text(2022.2, 20000, "COVID & POST-COVID", col ="green")
arrows(2010.1,19000,2019.9, 19000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,19000,2024.1, 19000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     ylim = c(300,45000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(meta_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,38000, legend = c("Meta Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2,45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,40000,2019.9,40000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,40000,2024.1, 40000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_2, meta_revenue.ts), 3)
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_3, meta_revenue.ts), 3)
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_4, meta_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
meta_quad.seas <- tslm(meta_train.ts ~ trend + I(trend^2) + season)
summary(meta_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
meta_quad.seas.res <- meta_quad.seas$residuals
meta_quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
meta_ma.trail.qres_2 <- rollmean(meta_quad.seas.res, k = 2, align = "right")
meta_ma.trail.qres_2
meta_ma.trail.qres_3 <- rollmean(meta_quad.seas.res, k = 3, align = "right")
meta_ma.trail.qres_3
meta_ma.trail.qres_4 <- rollmean(meta_quad.seas.res, k = 4, align = "right")
meta_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
meta_quad.seas.pred <- forecast(meta_quad.seas, h = meta_nValid, level = 0)
meta_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
meta_quad.seas.res.valid <- meta_valid.ts - meta_quad.seas.pred$mean
meta_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
meta_ma.trail.qres.pred_2 <- forecast(meta_ma.trail.qres_2, h = meta_nValid, level = 0)
meta_ma.trail.qres.pred_2
meta_ma.trail.qres.pred_3 <- forecast(meta_ma.trail.qres_3, h = meta_nValid, level = 0)
meta_ma.trail.qres.pred_3
meta_ma.trail.qres.pred_4 <- forecast(meta_ma.trail.qres_4, h = meta_nValid, level = 0)
meta_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
meta_qfst.2level_2 <- meta_quad.seas.pred$mean + meta_ma.trail.qres.pred_2$mean
meta_qfst.2level_2
meta_qfst.2level_3 <- meta_quad.seas.pred$mean + meta_ma.trail.qres.pred_3$mean
meta_qfst.2level_3
meta_qfst.2level_4 <- meta_quad.seas.pred$mean + meta_ma.trail.qres.pred_4$mean
meta_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
meta_qvalid_2.df <- round(data.frame(meta_valid.ts, meta_quad.seas.pred$mean, 
                                meta_ma.trail.qres.pred_2$mean, 
                                meta_qfst.2level_2), 3)
names(meta_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
meta_qvalid_2.df

meta_qvalid_3.df <- round(data.frame(meta_valid.ts, meta_quad.seas.pred$mean, 
                                meta_ma.trail.qres.pred_3$mean, 
                                meta_qfst.2level_3), 3)
names(meta_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
meta_qvalid_3.df

meta_qvalid_4.df <- round(data.frame(meta_valid.ts, meta_quad.seas.pred$mean, 
                                meta_ma.trail.qres.pred_4$mean, 
                                meta_qfst.2level_4), 3)
names(meta_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
meta_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(meta_qfst.2level_2, meta_valid.ts), 3)
round(accuracy(meta_qfst.2level_3, meta_valid.ts), 3)
round(accuracy(meta_qfst.2level_4, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
meta_tot.quad.seas <- tslm(meta_revenue.ts ~ trend + I(trend^2) + season)
summary(meta_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
meta_tot.quad.seas.pred <- forecast(meta_tot.quad.seas, h = 16, level = 0)
meta_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
meta_tot.quad.seas.res <- meta_tot.quad.seas$residuals
meta_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
meta_quad.ma.trail.res_2 <- rollmean(meta_tot.quad.seas.res, k = 2, align = "right")
meta_quad.ma.trail.res_2
meta_quad.ma.trail.res_3 <- rollmean(meta_tot.quad.seas.res, k = 3, align = "right")
meta_quad.ma.trail.res_3
meta_quad.ma.trail.res_4 <- rollmean(meta_tot.quad.seas.res, k = 4, align = "right")
meta_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
meta_quad.ma.trail.res_2.pred <- forecast(meta_quad.ma.trail.res_2, h = 16, level = 0)
meta_quad.ma.trail.res_2.pred$mean
meta_quad.ma.trail.res_3.pred <- forecast(meta_quad.ma.trail.res_3, h = 16, level = 0)
meta_quad.ma.trail.res_3.pred$mean
meta_quad.ma.trail.res_4.pred <- forecast(meta_quad.ma.trail.res_4, h = 16, level = 0)
meta_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
meta_quad.fst.2level_2 <- meta_tot.quad.seas.pred$mean + meta_quad.ma.trail.res_2.pred$mean
meta_quad.fst.2level_2
meta_quad.fst.2level_3 <- meta_tot.quad.seas.pred$mean + meta_quad.ma.trail.res_3.pred$mean
meta_quad.fst.2level_3
meta_quad.fst.2level_4 <- meta_tot.quad.seas.pred$mean + meta_quad.ma.trail.res_4.pred$mean
meta_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
meta_futureq_2.df <- round(data.frame(meta_tot.quad.seas.pred$mean, meta_quad.ma.trail.res_2.pred$mean, 
                                 meta_quad.fst.2level_2), 3)
names(meta_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_futureq_2.df

meta_futureq_3.df <- round(data.frame(meta_tot.quad.seas.pred$mean, meta_quad.ma.trail.res_3.pred$mean, 
                                 meta_quad.fst.2level_3), 3)
names(meta_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_futureq_3.df

meta_futureq_4.df <- round(data.frame(meta_tot.quad.seas.pred$mean, meta_quad.ma.trail.res_4.pred$mean, 
                                 meta_quad.fst.2level_4), 3)
names(meta_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
meta_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(meta_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(meta_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,45000)) 
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(meta_tot.quad.seas.res, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(-2000, 4000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(meta_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(meta_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(meta_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(meta_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(meta_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 3000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-4000,4000)) 
text(2015,4000, "PRE-COVID", col = "blue")
text(2022.2, 4000, "COVID & POST-COVID", col ="green")
arrows(2010.1,3800,2019.9, 3800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,3800,2024.1, 3800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     ylim = c(300,45000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(meta_quad.fst.2level_2, col = "green", lwd = 3)
legend(2010,150000, legend = c("Meta Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2,45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,43000,2019.9,43000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,43000,2024.1, 43000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_2, meta_revenue.ts), 3)
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_3, meta_revenue.ts), 3)
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_4, meta_revenue.ts), 3)


  #------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL ------------------#
  
  #--------------------------- FOR VALIDATION PERIOD ----------------------------#
  
  # AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
  meta_hw.ZZZ <- ets(meta_train.ts, model = "ZZZ")
  meta_hw.ZZZ 
  # MODEL : (M, A, M); alpha = 0.9773, beta = 0.2965, gamma = 0.0227
  
  # AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
  meta_hw.ZZZ.pred <- forecast(meta_hw.ZZZ, h = meta_nValid, level = 0)
  meta_hw.ZZZ.pred
  
  # FORECAST ACCURACY FOR VALIDATION DATA
  round(accuracy(meta_hw.ZZZ.pred$mean, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
meta_HW.ZZZ <- ets(meta_revenue.ts, model = "ZZZ")
meta_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.9974, beta = 0.347, gamma = 0.0026

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
meta_HW.ZZZ.pred <- forecast(meta_HW.ZZZ, h = 16 , level = 0)
meta_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(meta_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 3) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_HW.ZZZ.pred$fitted, col = "yellow", lwd = 3)
lines(meta_allrevenue.ts, lwd = 2)
legend(2010,43000, 
       legend = c("Revenue (2010-2023)", 
                  "Holt-Winter's Forecast: Pre-Covid Period (2010-2019)",
                  "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-COVID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_HW.ZZZ.pred$fitted, meta_revenue.ts), 3)


#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(meta_train.ts, lag.max = 8, main = "Autocorrelation for Meta's Revenue Training Data Set")
Acf(meta_valid.ts, lag.max = 8, main = "Autocorrelation for Meta's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
meta_hw.ZZZ <- ets(meta_train.ts, model = "ZZZ")
meta_hw.ZZZ 
# MODEL : (M, A, M); alpha = 0.9773, beta = 0.2965, gamma = 0.0227

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
meta_hw.ZZZ.pred <- forecast(meta_hw.ZZZ, h = meta_nValid, level = 0)
meta_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
meta_train.residuals <- meta_hw.ZZZ.pred$residuals
meta_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(meta_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Meta's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
meta_res.ar1 <- Arima(meta_hw.ZZZ$residuals, order = c(1,0,0))
summary(meta_res.ar1)

# FORECAST FOR VALIDATION DATA
meta_res.ar1.pred <- forecast(meta_res.ar1, h = meta_nValid, level = 0)
meta_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(meta_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Meta's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
meta_valid.two.level.pred <- meta_hw.ZZZ.pred$mean + meta_res.ar1.pred$mean
meta_valid.df <- round(data.frame(meta_valid.ts, meta_hw.ZZZ.pred$mean, 
                             meta_res.ar1.pred$mean, meta_valid.two.level.pred),3)
names(meta_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
meta_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_valid.two.level.pred, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
meta_HW.ZZZ <- ets(meta_revenue.ts, model = "ZZZ")
meta_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.9974, beta = 0.347, gamma = 0.0026

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
meta_HW.ZZZ.pred <- forecast(meta_HW.ZZZ, h = 16 , level = 0)
meta_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
meta_residual.ar1 <- Arima(meta_HW.ZZZ$residuals, order = c(1,0,0))
meta_residual.ar1.pred <- forecast(meta_residual.ar1, h = 16, level = 0)
summary(meta_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(meta_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
meta_HW.ZZZ.ar1.pred <- meta_HW.ZZZ.pred$mean + meta_residual.ar1.pred$mean
meta_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
meta_table.df <- round(data.frame(meta_HW.ZZZ.pred$mean, 
                             meta_residual.ar1.pred$mean, meta_HW.ZZZ.ar1.pred),3)
names(meta_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
meta_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", ylim = c(300, 45000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 6 : TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_HW.ZZZ$fitted + meta_residual.ar1$fitted, col = "yellow", lwd = 3)
lines(meta_HW.ZZZ.ar1.pred, col = "green", lwd = 3)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                              "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) 
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_HW.ZZZ$fitted + meta_residual.ar1$fitted, meta_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
meta_train.auto.arima <- auto.arima(meta_train.ts)
summary(meta_train.auto.arima)

# FORECAST FOR VALIDATION DATA
meta_train.auto.arima.pred <- forecast(meta_train.auto.arima, h = meta_nValid, level = 0)
meta_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(meta_train.auto.arima.pred$mean, meta_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
meta_revenue.auto.arima <- arima(meta_revenue.ts,
                            order = c(2,1,2),
                            seasonal = c(2,2,2),
                            method = "ML")
summary(meta_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
meta_revenue.auto.arima.pred <- forecast(meta_revenue.auto.arima, h = 16, level = 0)
meta_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR ARIMA MODEL
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     ylim = c(300, 45000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 7 : ARIMA MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta_revenue.auto.arima$fitted, col = "yellow", lwd = 3)
lines(meta_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,45000)) # FOR PRE-CVOID DATA
text(2015,45000, "PRE-COVID", col = "blue")
text(2022.2, 45000, "COVID & POST-COVID", col ="green")
arrows(2010.1,44000,2019.9, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,44000,2024.1, 44000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(meta_revenue.auto.arima.pred$fitted, meta_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON meta_revenue.ts (2010-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(meta_revenue.ts))$fitted, meta_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(meta_revenue.ts))$fitted, meta_revenue.ts), 3)

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(meta_lin.trend.pred$fitted, meta_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(meta_quad.trend.pred$fitted, meta_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(meta_revenue.season.pred$fitted, meta_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(meta_lin.season.pred$fitted, meta_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(meta_quad.season.pred$fitted, meta_revenue.ts),3)

#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_2, meta_revenue.ts), 3)
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_3, meta_revenue.ts), 3)
round(accuracy(meta_tot.trend.seas.pred$fitted + meta_tot.ma.trail.res_4, meta_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_2, meta_revenue.ts), 3)
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_3, meta_revenue.ts), 3)
round(accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_4, meta_revenue.ts), 3)

#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(meta_HW.ZZZ.pred$fitted, meta_revenue.ts), 3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(meta_HW.ZZZ$fitted + meta_residual.ar1$fitted, meta_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(meta_revenue.auto.arima.pred$fitted, meta_revenue.ts), 3)

#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
meta_accuracy_model_1 <- accuracy(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_2, meta_revenue.ts)
meta_accuracy_model_2 <- accuracy(meta_HW.ZZZ.pred$fitted, meta_revenue.ts)
meta_accuracy_model_3 <- accuracy(meta_revenue.auto.arima.pred$fitted, meta_revenue.ts)

# Extract RMSE and MAPE for each model
meta_rmse_model_1 <- meta_accuracy_model_1[1, "RMSE"]
meta_mape_model_1 <- meta_accuracy_model_1[1, "MAPE"]

meta_rmse_model_2 <- meta_accuracy_model_2[1, "RMSE"]
meta_mape_model_2 <- meta_accuracy_model_2[1, "MAPE"]

meta_rmse_model_3 <- meta_accuracy_model_3[1, "RMSE"]
meta_mape_model_3 <- meta_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
meta_inv_rmse_model_1 <- 1 / meta_rmse_model_1
meta_inv_mape_model_1 <- 1 / meta_mape_model_1

meta_inv_rmse_model_2 <- 1 / meta_rmse_model_2
meta_inv_mape_model_2 <- 1 / meta_mape_model_2

meta_inv_rmse_model_3 <- 1 / meta_rmse_model_3
meta_inv_mape_model_3 <- 1 / meta_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
meta_total_inv_rmse <- meta_inv_rmse_model_1 + meta_inv_rmse_model_2 + meta_inv_rmse_model_3
meta_total_inv_mape <- meta_inv_mape_model_1 + meta_inv_mape_model_2 + meta_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
meta_inv_rmse_weight_1 <- meta_inv_rmse_model_1 / meta_total_inv_rmse
meta_inv_rmse_weight_2 <- meta_inv_rmse_model_2 / meta_total_inv_rmse
meta_inv_rmse_weight_3 <- meta_inv_rmse_model_3 / meta_total_inv_rmse

meta_inv_mape_weight_1 <- meta_inv_mape_model_1 / meta_total_inv_mape
meta_inv_mape_weight_2 <- meta_inv_mape_model_2 / meta_total_inv_mape
meta_inv_mape_weight_3 <- meta_inv_mape_model_3 / meta_total_inv_mape

# Calculate the total weight for each model
meta_total_weight_1 <- (meta_inv_rmse_weight_1 + meta_inv_mape_weight_1) / 2
meta_total_weight_2 <- (meta_inv_rmse_weight_2 + meta_inv_mape_weight_2) / 2
meta_total_weight_3 <- (meta_inv_rmse_weight_3 + meta_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", meta_rmse_model_1, "MAPE:", meta_mape_model_1, 
    "Inverse RMSE:", meta_inv_rmse_model_1, "Inverse MAPE:", meta_inv_mape_model_1, 
    "Inv. RMSE Weight:", meta_inv_rmse_weight_1, "Inv. MAPE Weight:", meta_inv_mape_weight_1, 
    "Total Weight:", meta_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", meta_rmse_model_2, "MAPE:", meta_mape_model_2, 
    "Inverse RMSE:", meta_inv_rmse_model_2, "Inverse MAPE:", meta_inv_mape_model_2, 
    "Inv. RMSE Weight:", meta_inv_rmse_weight_2, "Inv. MAPE Weight:", meta_inv_mape_weight_2, 
    "Total Weight:", meta_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", meta_rmse_model_3, "MAPE:", meta_mape_model_3, 
    "Inverse RMSE:", meta_inv_rmse_model_3, "Inverse MAPE:", meta_inv_mape_model_3, 
    "Inv. RMSE Weight:", meta_inv_rmse_weight_3, "Inv. MAPE Weight:", meta_inv_mape_weight_3, 
    "Total Weight:", meta_total_weight_3, "\n\n")

# Calculate the ensemble model weights
meta_total_inv_rmse_all <- meta_inv_rmse_model_1 + meta_inv_rmse_model_2 + meta_inv_rmse_model_3
meta_total_inv_mape_all <- meta_inv_mape_model_1 + meta_inv_mape_model_2 + meta_inv_mape_model_3

meta_ensemble_inv_rmse_weight <- meta_total_inv_rmse_all / meta_total_inv_rmse_all
meta_ensemble_inv_mape_weight <- meta_total_inv_mape_all / meta_total_inv_mape_all

# Create a dataframe-like structure
meta_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(meta_rmse_model_1, meta_rmse_model_2, meta_rmse_model_3, ""),
  MAPE = c(meta_mape_model_1, meta_mape_model_2, meta_mape_model_3, ""),
  `Inverse RMSE` = c(meta_inv_rmse_model_1, meta_inv_rmse_model_2, meta_inv_rmse_model_3, meta_total_inv_rmse_all),
  `Inverse MAPE` = c(meta_inv_mape_model_1, meta_inv_mape_model_2, meta_inv_mape_model_3, meta_total_inv_mape_all),
  `Inv. RMSE Weight` = c(meta_inv_rmse_weight_1, meta_inv_rmse_weight_2, meta_inv_rmse_weight_3, meta_ensemble_inv_rmse_weight),
  `Inv. MAPE Weight` = c(meta_inv_mape_weight_1, meta_inv_mape_weight_2, meta_inv_mape_weight_3, meta_ensemble_inv_mape_weight),
  `Total Weight` = c(meta_total_weight_1, meta_total_weight_2, meta_total_weight_3, 1)
)


# Print the dataframe.
print(meta_results, right = F)


#--------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)--------------#

# Create ensemble forecast for pre-COVID period.
meta.ensemble.pre_covid <-( 
  (meta_total_weight_1*(meta_tot.quad.seas.pred$fitted + meta_quad.ma.trail.res_2))
  + (meta_total_weight_2*meta_HW.ZZZ.pred$fitted)
  + (meta_total_weight_3*meta_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
meta.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(meta.ensemble.pre_covid, meta_revenue.ts), 3)

#----------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------#

# Create ensemble forecast for COVID & post-COVID periods.
meta.ensemble.covid <-( 
  (meta_total_weight_1*meta_quad.fst.2level_2)
  + (meta_total_weight_2*meta_HW.ZZZ.pred$mean)
  + (meta_total_weight_3*meta_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
meta.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(meta_allrevenue.ts, 
     xlab = "Time", ylab = "Meta's Revenue (in Million $)", 
     ylim = c(0, 45000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(meta.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(meta.ensemble.covid, col = "green", lwd = 3)
legend(2010,43000, legend = c("Revenue (2010-2023)", 
                               "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                               "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,45000)) # FOR PRE-COVID DATA
text(2015, 44500, "PRE-COVID", col = "blue")
text(2022.2, 44500, "COVID & POST-COVID", col ="green")
arrows(2010.1,43000,2019.9, 43000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,43000,2024.1, 43000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#--------------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 


meta.revenue.change <- meta_future.ts - meta.ensemble.covid

meta_revenue_covid.df <- round(data.frame(
  meta_future.ts, 
  meta.ensemble.covid, 
  meta.revenue.change), 3)
names(meta_revenue_covid.df) <- c(
  "Actual_Revenue", 
  "Ensemble_Fst", 
  "Difference")
meta_revenue_covid.df

library("writexl")
write_xlsx(meta_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_meta_20_23_actual_fst.xlsx")







