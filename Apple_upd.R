# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#------------------------------------- Apple -----------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")


# CREATE DATAFRAME
Apple.data <- read.csv("AppleRevenue.csv")

# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# apple_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023)

apple_allrevenue.ts <- ts(Apple.data$Apple_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
apple_allrevenue.ts

# apple_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
apple_revenue.ts <- ts(Apple.data$Apple_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
apple_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Millions of Dollars)", 
     ylim = c(13000, 130000), bty = "l",
     xaxt = "n", xlim = c(2010, 2025.25), 
     main = "Apple Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
apple_allrevenue.stl <- stl(apple_allrevenue.ts, s.window = "periodic")
autoplot(apple_allrevenue.stl, main = "Apple Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
apple_allautocor <- Acf(apple_allrevenue.ts, lag.max = 4, 
                          main = "Autocorrelation Chart for Apple")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
apple_autocor <- Acf(apple_revenue.ts, lag.max = 4, 
      main = "Autocorrelation Chart for Apple Revenue Data (Pre-Covid : 2010 - 2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
apple_lag <- round(apple_autocor$lag,0)
apple_ACF <- round(apple_autocor$acf,3)
data.frame(apple_lag,apple_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR APPLE'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
apple_revenue.ar1 <- Arima(apple_revenue.ts, order = c(1,0,0),method = "ML")
summary(apple_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'.


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1
ar1 <- 0.6437
s.e. <- 0.1390
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

# CREATE FIRST DIFFERENCED APPLE REVENUE DATA USING lag1
diff.apple_revenue.ts <- diff(apple_revenue.ts, lag = 1)
diff.apple_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED APPLE REVENUE
Acf(diff.apple_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Apple Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(apple_revenue.ts) = 40 (10 YEARS)
# apple_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# apple_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

apple_nValid <- 12
apple_nTrain <- length(apple_revenue.ts) - apple_nValid
apple_train.ts <- window(apple_revenue.ts, start = c(2010, 1), end = c(2010, apple_nTrain))
apple_train.ts
apple_valid.ts <- window(apple_revenue.ts, start = c(2010, apple_nTrain + 1), end = c(2010, apple_nTrain + apple_nValid))
apple_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# apple_future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

apple_future.ts <- window(apple_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
apple_future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(apple_train.ts,
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(13000,130000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(apple_valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND COVID & POST-COVID PREDICTION INTERVALS
lines(c(2017,2017), c(0,130500)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,130500)) # FOR VALIDATION DATASET
text(2013.5,130500, "TRAINING")
text(2018.5,130500, "VALIDATION", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010,125000,2016.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2023.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
apple_revenue.naive.pred <- naive(apple_train.ts, h = apple_nValid)
apple_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_revenue.naive.pred$mean, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
apple_revenuef.naive.pred <- naive(apple_revenue.ts, h = 16)
apple_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(apple_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts, col = "black", lwd = 2)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Naive Forecast: Covid and Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")


# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(apple_revenue.ts))$fitted, apple_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
apple_revenue.snaive.pred <- snaive(apple_train.ts, h = apple_nValid)
apple_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_revenue.snaive.pred$mean, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
apple_revenuef.snaive.pred <- snaive(apple_revenue.ts, h = 16)
apple_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(apple_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 2 : SEASONAL NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_revenuef.snaive.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts, col = "black", lwd = 2)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Seasonal Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(apple_revenue.ts))$fitted, apple_revenue.ts), 3)

#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

apple_train.lin <- tslm(apple_train.ts ~ trend)
summary(apple_train.lin)

# FORECAST FOR VALIDATION DATA
apple_train.lin.pred <- forecast(apple_train.lin, h = apple_nValid, level = 0)
apple_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.lin.pred$mean, apple_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

apple_train.quad <- tslm(apple_train.ts ~ trend + I(trend^2))
summary(apple_train.quad)

# FORECAST FOR VALIDATION DATA
apple_train.quad.pred <- forecast(apple_train.quad, h = apple_nValid, level = 0)
apple_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.quad.pred$mean, apple_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

apple_train.season <- tslm(apple_train.ts ~ season)
summary(apple_train.season)

# FORECAST FOR VALIDATION DATA
apple_train.season.pred <- forecast(apple_train.season, h = apple_nValid, level = 0)
apple_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.season.pred$mean, apple_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

apple_train.lin.season <- tslm(apple_train.ts ~ trend + season)
summary(apple_train.lin.season)

# FORECAST FOR VALIDATION DATA
apple_train.lin.season.pred <- forecast(apple_train.lin.season, h = apple_nValid, level = 0)
apple_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.lin.season.pred$mean, apple_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

apple_train.quad.season <- tslm(apple_train.ts ~ trend + I(trend^2) + season)
summary(apple_train.quad.season)

# FORECAST FOR VALIDATION DATA
apple_train.quad.season.pred <- forecast(apple_train.quad.season, h = apple_nValid, level = 0)
apple_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.quad.season.pred$mean, apple_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

apple_lin.trend <- tslm(apple_revenue.ts ~ trend)
summary(apple_lin.trend)

# FORECAST FOR POST-COVID PERIOD
apple_lin.trend.pred <- forecast(apple_lin.trend, h = 16, level = 0)
apple_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(apple_lin.trend.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_lin.trend.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_lin.trend.pred$fitted, apple_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

apple_quad.trend <- tslm(apple_revenue.ts ~ trend + I(trend^2))
summary(apple_quad.trend)

# FORECAST FOR POST-COVID PERIOD
apple_quad.trend.pred <- forecast(apple_quad.trend, h = 16, level = 0)
apple_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(apple_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     ylim = c(13000, 130000), bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_quad.trend.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_quad.trend.pred$fitted, apple_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

apple_revenue.season <- tslm(apple_revenue.ts ~ season)
summary(apple_revenue.season)

# FORECAST FOR POST-COVID PERIOD
apple_revenue.season.pred <- forecast(apple_revenue.season, h = 16, level = 0)
apple_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(apple_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_revenue.season.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_revenue.season.pred$fitted, apple_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

apple_lin.season <- tslm(apple_revenue.ts ~ trend + season)
summary(apple_lin.season)

# FORECAST FOR POST-COVID PERIOD
apple_lin.season.pred <- forecast(apple_lin.season, h = 16, level = 0)
apple_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(apple_lin.season.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_lin.season.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_lin.season.pred$fitted, apple_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

apple_quad.season <- tslm(apple_revenue.ts ~ trend + I(trend^2) + season)
summary(apple_quad.season)

# FORECAST FOR POST-COVID PERIOD
apple_quad.season.pred <- forecast(apple_quad.season, h = 16, level = 0)
apple_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(apple_quad.season.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_quad.season.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_quad.season.pred$fitted, apple_revenue.ts),3)


#-------------------- MODEL 4 : TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
apple_trend.seas <- tslm(apple_train.ts ~ trend + season)
summary(apple_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
apple_trend.seas.res <- apple_trend.seas$residuals
apple_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
apple_ma.trail.res_2 <- rollmean(apple_trend.seas.res, k = 2, align = "right")
apple_ma.trail.res_2
apple_ma.trail.res_3 <- rollmean(apple_trend.seas.res, k = 3, align = "right")
apple_ma.trail.res_3
apple_ma.trail.res_4 <- rollmean(apple_trend.seas.res, k = 4, align = "right")
apple_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
apple_trend.seas.pred <- forecast(apple_trend.seas, h = apple_nValid, level = 0)
apple_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
apple_trend.seas.res.valid <- apple_valid.ts - apple_trend.seas.pred$mean
apple_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
apple_ma.trail.res.pred_2 <- forecast(apple_ma.trail.res_2, h = apple_nValid, level = 0)
apple_ma.trail.res.pred_2
apple_ma.trail.res.pred_3 <- forecast(apple_ma.trail.res_3, h = apple_nValid, level = 0)
apple_ma.trail.res.pred_3
apple_ma.trail.res.pred_4 <- forecast(apple_ma.trail.res_4, h = apple_nValid, level = 0)
apple_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
apple_fst.2level_2 <- apple_trend.seas.pred$mean + apple_ma.trail.res.pred_2$mean
apple_fst.2level_2
apple_fst.2level_3 <- apple_trend.seas.pred$mean + apple_ma.trail.res.pred_3$mean
apple_fst.2level_3
apple_fst.2level_4 <- apple_trend.seas.pred$mean + apple_ma.trail.res.pred_4$mean
apple_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
apple_valid_2.df <- round(data.frame(apple_valid.ts, apple_trend.seas.pred$mean, 
                               apple_ma.trail.res.pred_2$mean, 
                               apple_fst.2level_2), 3)
names(apple_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
apple_valid_2.df

apple_valid_3.df <- round(data.frame(apple_valid.ts, apple_trend.seas.pred$mean, 
                               apple_ma.trail.res.pred_3$mean, 
                               apple_fst.2level_3), 3)
names(apple_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
apple_valid_3.df

apple_valid_4.df <- round(data.frame(apple_valid.ts, apple_trend.seas.pred$mean, 
                               apple_ma.trail.res.pred_4$mean, 
                               apple_fst.2level_4), 3)
names(apple_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
apple_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(apple_fst.2level_2, apple_valid.ts), 3)
round(accuracy(apple_fst.2level_3, apple_valid.ts), 3)
round(accuracy(apple_fst.2level_4, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
apple_tot.trend.seas <- tslm(apple_revenue.ts ~ trend  + season)
summary(apple_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
apple_tot.trend.seas.pred <- forecast(apple_tot.trend.seas, h = 16, level = 0)
apple_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
apple_tot.trend.seas.res <- apple_tot.trend.seas$residuals
apple_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
apple_tot.ma.trail.res_2 <- rollmean(apple_tot.trend.seas.res, k = 2, align = "right")
apple_tot.ma.trail.res_2
apple_tot.ma.trail.res_3 <- rollmean(apple_tot.trend.seas.res, k = 3, align = "right")
apple_tot.ma.trail.res_3
apple_tot.ma.trail.res_4 <- rollmean(apple_tot.trend.seas.res, k = 4, align = "right")
apple_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
apple_tot.ma.trail.res_2.pred <- forecast(apple_tot.ma.trail.res_2, h = 16, level = 0)
apple_tot.ma.trail.res_2.pred
apple_tot.ma.trail.res_3.pred <- forecast(apple_tot.ma.trail.res_3, h = 16, level = 0)
apple_tot.ma.trail.res_3.pred
apple_tot.ma.trail.res_4.pred <- forecast(apple_tot.ma.trail.res_4, h = 16, level = 0)
apple_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
apple_tot.fst.2level_2 <- apple_tot.trend.seas.pred$mean + apple_tot.ma.trail.res_2.pred$mean
apple_tot.fst.2level_2
apple_tot.fst.2level_3 <- apple_tot.trend.seas.pred$mean + apple_tot.ma.trail.res_3.pred$mean
apple_tot.fst.2level_3
apple_tot.fst.2level_4 <- apple_tot.trend.seas.pred$mean + apple_tot.ma.trail.res_4.pred$mean
apple_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
apple_future_2.df <- round(data.frame(apple_tot.trend.seas.pred$mean, apple_tot.ma.trail.res_2.pred$mean, 
                                apple_tot.fst.2level_2), 3)
names(apple_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_future_2.df

apple_future_3.df <- round(data.frame(apple_tot.trend.seas.pred$mean, apple_tot.ma.trail.res_3.pred$mean, 
                                apple_tot.fst.2level_3), 3)
names(apple_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_future_3.df

apple_future_4.df <- round(data.frame(apple_tot.trend.seas.pred$mean, apple_tot.ma.trail.res_4.pred$mean, 
                                apple_tot.fst.2level_4), 3)
names(apple_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(apple_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(apple_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,130500)) 
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(apple_tot.trend.seas.res, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(-19000, 17000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(apple_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(apple_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(apple_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(apple_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(apple_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 16500, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-19000,17000)) # FOR PRE-COVID DATA
text(2015,17000, "PRE-COVID", col = "blue")
text(2022.2, 17000, "COVID & POST-COVID", col ="green")
arrows(2010.1,16000,2019.9, 16000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,16000,2024.1, 16000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     ylim = c(13000,130000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(apple_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,115000, legend = c("Apple Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130000)) # FOR PRE-COVID DATA
text(2015,130000, "PRE-COVID", col = "blue")
text(2022.2,130000, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9,125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_2, apple_revenue.ts), 3)
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_3, apple_revenue.ts), 3)
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_4, apple_revenue.ts), 3)

#-- MODEL 4B: REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
apple_quad.seas <- tslm(apple_train.ts ~ trend + I(trend^2) + season)
summary(apple_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
apple_quad.seas.res <- apple_quad.seas$residuals
apple_quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
apple_ma.trail.qres_2 <- rollmean(apple_quad.seas.res, k = 2, align = "right")
apple_ma.trail.qres_2
apple_ma.trail.qres_3 <- rollmean(apple_quad.seas.res, k = 3, align = "right")
apple_ma.trail.qres_3
apple_ma.trail.qres_4 <- rollmean(apple_quad.seas.res, k = 4, align = "right")
apple_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
apple_quad.seas.pred <- forecast(apple_quad.seas, h = apple_nValid, level = 0)
apple_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
apple_quad.seas.res.valid <- apple_valid.ts - apple_quad.seas.pred$mean
apple_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
apple_ma.trail.qres.pred_2 <- forecast(apple_ma.trail.qres_2, h = apple_nValid, level = 0)
apple_ma.trail.qres.pred_2
apple_ma.trail.qres.pred_3 <- forecast(apple_ma.trail.qres_3, h = apple_nValid, level = 0)
apple_ma.trail.qres.pred_3
apple_ma.trail.qres.pred_4 <- forecast(apple_ma.trail.qres_4, h = apple_nValid, level = 0)
apple_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
apple_qfst.2level_2 <- apple_quad.seas.pred$mean + apple_ma.trail.qres.pred_2$mean
apple_qfst.2level_2
apple_qfst.2level_3 <- apple_quad.seas.pred$mean + apple_ma.trail.qres.pred_3$mean
apple_qfst.2level_3
apple_qfst.2level_4 <- apple_quad.seas.pred$mean + apple_ma.trail.qres.pred_4$mean
apple_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
apple_qvalid_2.df <- round(data.frame(apple_valid.ts, apple_quad.seas.pred$mean, 
                                apple_ma.trail.qres.pred_2$mean, 
                                apple_qfst.2level_2), 3)
names(apple_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
apple_qvalid_2.df

apple_qvalid_3.df <- round(data.frame(apple_valid.ts, apple_quad.seas.pred$mean, 
                                apple_ma.trail.qres.pred_3$mean, 
                                apple_qfst.2level_3), 3)
names(apple_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
apple_qvalid_3.df

apple_qvalid_4.df <- round(data.frame(apple_valid.ts, apple_quad.seas.pred$mean, 
                                apple_ma.trail.qres.pred_4$mean, 
                                apple_qfst.2level_4), 3)
names(apple_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
apple_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(apple_qfst.2level_2, apple_valid.ts), 3)
round(accuracy(apple_qfst.2level_3, apple_valid.ts), 3)
round(accuracy(apple_qfst.2level_4, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
apple_tot.quad.seas <- tslm(apple_revenue.ts ~ trend + I(trend^2) + season)
summary(apple_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
apple_tot.quad.seas.pred <- forecast(apple_tot.quad.seas, h = 16, level = 0)
apple_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
apple_tot.quad.seas.res <- apple_tot.quad.seas$residuals
apple_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
apple_quad.ma.trail.res_2 <- rollmean(apple_tot.quad.seas.res, k = 2, align = "right")
apple_quad.ma.trail.res_2
apple_quad.ma.trail.res_3 <- rollmean(apple_tot.quad.seas.res, k = 3, align = "right")
apple_quad.ma.trail.res_3
apple_quad.ma.trail.res_4 <- rollmean(apple_tot.quad.seas.res, k = 4, align = "right")
apple_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
apple_quad.ma.trail.res_2.pred <- forecast(apple_quad.ma.trail.res_2, h = 16, level = 0)
apple_quad.ma.trail.res_2.pred$mean
apple_quad.ma.trail.res_3.pred <- forecast(apple_quad.ma.trail.res_3, h = 16, level = 0)
apple_quad.ma.trail.res_3.pred$mean
apple_quad.ma.trail.res_4.pred <- forecast(apple_quad.ma.trail.res_4, h = 16, level = 0)
apple_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
apple_quad.fst.2level_2 <- apple_tot.quad.seas.pred$mean + apple_quad.ma.trail.res_2.pred$mean
apple_quad.fst.2level_2
apple_quad.fst.2level_3 <- apple_tot.quad.seas.pred$mean + apple_quad.ma.trail.res_3.pred$mean
apple_quad.fst.2level_3
apple_quad.fst.2level_4 <- apple_tot.quad.seas.pred$mean + apple_quad.ma.trail.res_4.pred$mean
apple_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
apple_futureq_2.df <- round(data.frame(apple_tot.quad.seas.pred$mean, apple_quad.ma.trail.res_2.pred$mean, 
                                 apple_quad.fst.2level_2), 3)
names(apple_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_futureq_2.df

apple_futureq_3.df <- round(data.frame(apple_tot.quad.seas.pred$mean, apple_quad.ma.trail.res_3.pred$mean, 
                                 apple_quad.fst.2level_3), 3)
names(apple_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_futureq_3.df

apple_futureq_4.df <- round(data.frame(apple_tot.quad.seas.pred$mean, apple_quad.ma.trail.res_4.pred$mean, 
                                 apple_quad.fst.2level_4), 3)
names(apple_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
apple_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(apple_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(apple_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,130500))
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(apple_tot.quad.seas.res, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(-16000, 15000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(apple_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(apple_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(apple_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(apple_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(apple_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 14000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-16000,15000)) 
text(2015,15000, "PRE-COVID", col = "blue")
text(2022.2, 15000, "COVID & POST-COVID", col ="green")
arrows(2010.1,14000,2019.9, 14000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,14000,2024.1, 14000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     ylim = c(13000,130000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(apple_quad.fst.2level_2, col = "green", lwd = 3)
legend(2010,110000, legend = c("Apple Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130000)) # FOR PRE-COVID DATA
text(2015,130000, "PRE-COVID", col = "blue")
text(2022.2,130000, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9,125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_2, apple_revenue.ts), 3)
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_3, apple_revenue.ts), 3)
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_4, apple_revenue.ts), 3)


#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL ------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
apple_hw.ZZZ <- ets(apple_train.ts, model = "ZZZ")
apple_hw.ZZZ 
# MODEL : (M, A, M); alpha = 0.6597, beta = 0.1113, gamma = 0.3403

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
apple_hw.ZZZ.pred <- forecast(apple_hw.ZZZ, h = apple_nValid, level = 0)
apple_hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_hw.ZZZ.pred$mean, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
apple_HW.ZZZ <- ets(apple_revenue.ts, model = "MMM")
apple_HW.ZZZ 
# MODEL : (M, M, M); alpha = 0.5506, beta = 0.0004, gamma = 0.3869,phi = 0.8732

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
apple_HW.ZZZ.pred <- forecast(apple_HW.ZZZ, h = 16 , level = 0)
apple_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(apple_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 5: HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_HW.ZZZ.pred$fitted, col = "yellow", lwd = 2)
lines(apple_allrevenue.ts)
legend(2010,115000, 
       legend = c("Revenue (2010-2023)", 
                  "Holt-Winter's Forecast: Pre-Covid Period (2010-2019)",
                  "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_HW.ZZZ.pred$fitted, apple_revenue.ts), 3)


#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(apple_train.ts, lag.max = 8, main = "Autocorrelation for Apple's Revenue Training Data Set")
Acf(apple_valid.ts, lag.max = 8, main = "Autocorrelation for Apple's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
apple_hw.ZZZ <- ets(apple_train.ts, model = "ZZZ")
apple_hw.ZZZ 
# MODEL : (M, A, M); alpha = 0.6597, beta = 0.1113, gamma = 0.3403

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
apple_hw.ZZZ.pred <- forecast(apple_hw.ZZZ, h = apple_nValid, level = 0)
apple_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
apple_train.residuals <- apple_hw.ZZZ.pred$residuals
apple_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(apple_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Apple's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
apple_res.ar1 <- Arima(apple_hw.ZZZ$residuals, order = c(1,0,0))
summary(apple_res.ar1)

# FORECAST FOR VALIDATION DATA
apple_res.ar1.pred <- forecast(apple_res.ar1, h = apple_nValid, level = 0)
apple_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(apple_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Apple's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
apple_valid.two.level.pred <- apple_hw.ZZZ.pred$mean + apple_res.ar1.pred$mean
apple_valid.df <- round(data.frame(apple_valid.ts, apple_hw.ZZZ.pred$mean, 
                             apple_res.ar1.pred$mean, apple_valid.two.level.pred),3)
names(apple_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
apple_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_valid.two.level.pred, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
apple_HW.ZZZ <- ets(apple_revenue.ts, model = "MMM")
apple_HW.ZZZ 
# MODEL : (M, M, M); alpha = 0.5506, beta = 0.0004, gamma = 0.3869, phi=0.8732

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
apple_HW.ZZZ.pred <- forecast(apple_HW.ZZZ, h = 16 , level = 0)
apple_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
apple_residual.ar1 <- Arima(apple_HW.ZZZ$residuals, order = c(1,0,0))
apple_residual.ar1.pred <- forecast(apple_residual.ar1, h = 16, level = 0)
summary(apple_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(apple_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
apple_HW.ZZZ.ar1.pred <- apple_HW.ZZZ.pred$mean + apple_residual.ar1.pred$mean
apple_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
apple_table.df <- round(data.frame(apple_HW.ZZZ.pred$mean, 
                             apple_residual.ar1.pred$mean, apple_HW.ZZZ.ar1.pred),3)
names(apple_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
apple_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", ylim = c(13000, 130000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 6: TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_HW.ZZZ$fitted + apple_residual.ar1$fitted, col = "yellow", lwd = 2)
lines(apple_HW.ZZZ.ar1.pred, col = "green", lwd = 2)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                               "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) 
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_HW.ZZZ$fitted + apple_residual.ar1$fitted, apple_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
apple_train.auto.arima <- auto.arima(apple_train.ts)
summary(apple_train.auto.arima)

# FORECAST FOR VALIDATION DATA
apple_train.auto.arima.pred <- forecast(apple_train.auto.arima, h = apple_nValid, level = 0)
apple_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(apple_train.auto.arima.pred$mean, apple_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
apple_revenue.auto.arima <- arima(apple_revenue.ts,
                            order = c(2,1,2),
                            seasonal = c(2,2,3),
                            method = "ML")
summary(apple_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
apple_revenue.auto.arima.pred <- forecast(apple_revenue.auto.arima, h = 16, level = 0)
apple_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR AUTO-ARIMA MODEL
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     ylim = c(13000, 130000), bty = "l", xlim = c(2010, 2024),
     xaxt = "n",  lwd = 2,
     main = "MODEL 7 : ARIMA MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple_revenue.auto.arima.pred$fitted, col = "yellow", lwd = 3)
lines(apple_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2010,115000, legend = c("Revenue (2010-2023)", 
                               "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                               "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,130500)) # FOR PRE-COVID DATA
text(2015,130500, "PRE-COVID", col = "blue")
text(2022.2, 130500, "COVID & POST-COVID", col ="green")
arrows(2010.1,125000,2019.9, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,125000,2024.1, 125000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(apple_revenue.auto.arima.pred$fitted, apple_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON apple_revenue.ts (2010-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(apple_revenue.ts))$fitted, apple_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(apple_revenue.ts))$fitted, apple_revenue.ts), 3)

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(apple_lin.trend.pred$fitted, apple_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(apple_quad.trend.pred$fitted, apple_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(apple_revenue.season.pred$fitted, apple_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(apple_lin.season.pred$fitted, apple_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(apple_quad.season.pred$fitted, apple_revenue.ts),3)

#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_2, apple_revenue.ts), 3)
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_3, apple_revenue.ts), 3)
round(accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_4, apple_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_2, apple_revenue.ts), 3)
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_3, apple_revenue.ts), 3)
round(accuracy(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_4, apple_revenue.ts), 3)

#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(apple_HW.ZZZ.pred$fitted, apple_revenue.ts), 3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(apple_HW.ZZZ$fitted + apple_residual.ar1$fitted, apple_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(apple_revenue.auto.arima.pred$fitted, apple_revenue.ts), 3)

#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
apple_accuracy_model_1 <- accuracy(apple_tot.trend.seas.pred$fitted + apple_tot.ma.trail.res_2, apple_revenue.ts)
apple_accuracy_model_2 <- accuracy(apple_HW.ZZZ.pred$fitted, apple_revenue.ts)
apple_accuracy_model_3 <- accuracy(apple_revenue.auto.arima.pred$fitted, apple_revenue.ts)

# Extract RMSE and MAPE for each model
apple_rmse_model_1 <- apple_accuracy_model_1[1, "RMSE"]
apple_mape_model_1 <- apple_accuracy_model_1[1, "MAPE"]

apple_rmse_model_2 <- apple_accuracy_model_2[1, "RMSE"]
apple_mape_model_2 <- apple_accuracy_model_2[1, "MAPE"]

apple_rmse_model_3 <- apple_accuracy_model_3[1, "RMSE"]
apple_mape_model_3 <- apple_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
apple_inv_rmse_model_1 <- 1 / apple_rmse_model_1
apple_inv_mape_model_1 <- 1 / apple_mape_model_1

apple_inv_rmse_model_2 <- 1 / apple_rmse_model_2
apple_inv_mape_model_2 <- 1 / apple_mape_model_2

apple_inv_rmse_model_3 <- 1 / apple_rmse_model_3
apple_inv_mape_model_3 <- 1 / apple_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
apple_total_inv_rmse <- apple_inv_rmse_model_1 + apple_inv_rmse_model_2 + apple_inv_rmse_model_3
apple_total_inv_mape <- apple_inv_mape_model_1 + apple_inv_mape_model_2 + apple_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
apple_inv_rmse_weight_1 <- apple_inv_rmse_model_1 / apple_total_inv_rmse
apple_inv_rmse_weight_2 <- apple_inv_rmse_model_2 / apple_total_inv_rmse
apple_inv_rmse_weight_3 <- apple_inv_rmse_model_3 / apple_total_inv_rmse

apple_inv_mape_weight_1 <- apple_inv_mape_model_1 / apple_total_inv_mape
apple_inv_mape_weight_2 <- apple_inv_mape_model_2 / apple_total_inv_mape
apple_inv_mape_weight_3 <- apple_inv_mape_model_3 / apple_total_inv_mape

# Calculate the total weight for each model
apple_total_weight_1 <- (apple_inv_rmse_weight_1 + apple_inv_mape_weight_1) / 2
apple_total_weight_2 <- (apple_inv_rmse_weight_2 + apple_inv_mape_weight_2) / 2
apple_total_weight_3 <- (apple_inv_rmse_weight_3 + apple_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", apple_rmse_model_1, "MAPE:", apple_mape_model_1, 
    "Inverse RMSE:", apple_inv_rmse_model_1, "Inverse MAPE:", apple_inv_mape_model_1, 
    "Inv. RMSE Weight:", apple_inv_rmse_weight_1, "Inv. MAPE Weight:", apple_inv_mape_weight_1, 
    "Total Weight:", apple_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", apple_rmse_model_2, "MAPE:", apple_mape_model_2, 
    "Inverse RMSE:", apple_inv_rmse_model_2, "Inverse MAPE:", apple_inv_mape_model_2, 
    "Inv. RMSE Weight:", apple_inv_rmse_weight_2, "Inv. MAPE Weight:", apple_inv_mape_weight_2, 
    "Total Weight:", apple_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", apple_rmse_model_3, "MAPE:", apple_mape_model_3, 
    "Inverse RMSE:", apple_inv_rmse_model_3, "Inverse MAPE:", apple_inv_mape_model_3, 
    "Inv. RMSE Weight:", apple_inv_rmse_weight_3, "Inv. MAPE Weight:", apple_inv_mape_weight_3, 
    "Total Weight:", apple_total_weight_3, "\n\n")

# Calculate the ensemble model weights
apple_total_inv_rmse_all <- apple_inv_rmse_model_1 + apple_inv_rmse_model_2 + apple_inv_rmse_model_3
apple_total_inv_mape_all <- apple_inv_mape_model_1 + apple_inv_mape_model_2 + apple_inv_mape_model_3

apple_ensemble_inv_rmse_weight <- apple_total_inv_rmse_all / apple_total_inv_rmse_all
apple_ensemble_inv_mape_weight <- apple_total_inv_mape_all / apple_total_inv_mape_all

# Create a dataframe-like structure
apple_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(apple_rmse_model_1, apple_rmse_model_2, apple_rmse_model_3, ""),
  MAPE = c(apple_mape_model_1, apple_mape_model_2, apple_mape_model_3, ""),
  `Inverse RMSE` = c(apple_inv_rmse_model_1, apple_inv_rmse_model_2, apple_inv_rmse_model_3, apple_total_inv_rmse_all),
  `Inverse MAPE` = c(apple_inv_mape_model_1, apple_inv_mape_model_2, apple_inv_mape_model_3, apple_total_inv_mape_all),
  `Inv. RMSE Weight` = c(apple_inv_rmse_weight_1, apple_inv_rmse_weight_2, apple_inv_rmse_weight_3, apple_ensemble_inv_rmse_weight),
  `Inv. MAPE Weight` = c(apple_inv_mape_weight_1, apple_inv_mape_weight_2, apple_inv_mape_weight_3, apple_ensemble_inv_mape_weight),
  `Total Weight` = c(apple_total_weight_1, apple_total_weight_2, apple_total_weight_3, 1)
)



# Print the dataframe.
print(apple_results, right = F)

#-----------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)----------------------#

# Create ensemble forecast for pre-COVID period.
apple.ensemble.pre_covid <-( 
  (apple_total_weight_1*(apple_tot.quad.seas.pred$fitted + apple_quad.ma.trail.res_2))
  + (apple_total_weight_2*apple_HW.ZZZ.pred$fitted)
  + (apple_total_weight_3*apple_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
apple.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(apple.ensemble.pre_covid, apple_revenue.ts), 3)

#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Create ensemble forecast for COVID & post-COVID periods.
apple.ensemble.covid <-( 
  (apple_total_weight_1*apple_quad.fst.2level_2)
  + (apple_total_weight_2*apple_HW.ZZZ.pred$mean)
  + (apple_total_weight_3*apple_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
apple.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(apple_allrevenue.ts, 
     xlab = "Time", ylab = "Apple's Revenue (in Million $)", 
     ylim = c(6000, 180000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(apple.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(apple.ensemble.covid, col = "green", lwd = 3)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                               "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,183500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2, 180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#--------------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 

apple.revenue.change <- apple_future.ts - apple.ensemble.covid

apple_revenue_covid.df <- round(data.frame(
  apple_future.ts, 
  apple.ensemble.covid, 
  apple.revenue.change), 3)
names(apple_revenue_covid.df) <- c(
  "Actual_Revenue", 
  "Ensemble_Fst", 
  "Difference")
apple_revenue_covid.df

library("writexl")
write_xlsx(apple_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_apple_20_23_actual_fst.xlsx")





