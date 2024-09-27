# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#------------------------------------- NVIDIA ---------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")

# CREATE DATAFRAME
NVIDIA.data <- read.csv("NVIDIARevenue.csv")


# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# NVIDIA_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023)

NVIDIA_allrevenue.ts <- ts(NVIDIA.data$NVIDIA_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
NVIDIA_allrevenue.ts

# NVIDIA_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
NVIDIA_revenue.ts <- ts(NVIDIA.data$NVIDIA_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
NVIDIA_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Millions of Dollars)", 
     ylim = c(900, 20000), bty = "l",
     xaxt = "n", xlim = c(2010, 2025.25), 
     main = "NVIDIA Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
NVIDIA_allrevenue.stl <- stl(NVIDIA_allrevenue.ts, s.window = "periodic")
autoplot(NVIDIA_allrevenue.stl, main = "NVIDIA Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
NVIDIA_allautocor <- Acf(NVIDIA_allrevenue.ts, lag.max = 4, 
                           main = "Autocorrelation Chart for NVIDIA")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
NVIDIA_autocor <- Acf(NVIDIA_revenue.ts, lag.max = 4, 
      main = "Autocorrelation Chart for NVIDIA Revenue Data (Pre-Covid : 2010 - 2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
NVIDIA_lag <- round(NVIDIA_autocor$lag,0)
NVIDIA_ACF <- round(NVIDIA_autocor$acf,3)
data.frame(NVIDIA_lag,NVIDIA_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR APPLE'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
NVIDIA_revenue.ar1 <- Arima(NVIDIA_revenue.ts, order = c(1,0,0), method="ML")
summary(NVIDIA_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'.

# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1
ar1 <- 0.9619
s.e. <- 0.0377
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
diff.NVIDIA_revenue.ts <- diff(NVIDIA_revenue.ts, lag = 1)
diff.NVIDIA_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED APPLE REVENUE
Acf(diff.NVIDIA_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced NVIDIA Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(NVIDIA_revenue.ts) = 40 (10 YEARS)
# NVIDIA_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# NVIDIA_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

NVIDIA_nValid <- 12
NVIDIA_nTrain <- length(NVIDIA_revenue.ts) - NVIDIA_nValid
NVIDIA_train.ts <- window(NVIDIA_revenue.ts, start = c(2010, 1), end = c(2010, NVIDIA_nTrain))
NVIDIA_train.ts
NVIDIA_valid.ts <- window(NVIDIA_revenue.ts, start = c(2010, NVIDIA_nTrain + 1), end = c(2010, NVIDIA_nTrain + NVIDIA_nValid))
NVIDIA_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# NVIDIA_future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

NVIDIA_future.ts <- window(NVIDIA_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
NVIDIA_future.ts
  
# ******** PLOT OF DATA PARTITION *********
  
# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(NVIDIA_train.ts,
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(900, 5000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
  
# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(NVIDIA_valid.ts, col = "blue", lwd = "2")
  
# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,5000)) 
lines(c(2020,2020), c(0,5000)) 
text(2013.5,5000, "TRAINING")
text(2018.5,5000, "VALIDATION", col = "blue")
text(2022.2,5000, "COVID & POST-COVID", col ="green")
arrows(2010,4800,2016.9,4800,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,4800,2019.9, 4800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,4800,2024.1, 4800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
NVIDIA_revenue.naive.pred <- naive(NVIDIA_train.ts, h = NVIDIA_nValid)
NVIDIA_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_revenue.naive.pred$mean, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
NVIDIA_revenuef.naive.pred <- naive(NVIDIA_revenue.ts, h = 16)
NVIDIA_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(NVIDIA_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts, col = "black", lwd = 2)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Naive Forecast: Pre-Covid Period (2010-2019)",
                              "Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(NVIDIA_revenue.ts))$fitted, NVIDIA_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
NVIDIA_revenue.snaive.pred <- snaive(NVIDIA_train.ts, h = NVIDIA_nValid)
NVIDIA_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_revenue.snaive.pred$mean, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
NVIDIA_revenuef.snaive.pred <- snaive(NVIDIA_revenue.ts, h = 16)
NVIDIA_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(NVIDIA_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 2: SEASONAL NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_revenuef.snaive.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts, col = "black", lwd = 2)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                              "Seasonal Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVIF DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(NVIDIA_revenue.ts))$fitted, NVIDIA_revenue.ts), 3)


#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

NVIDIA_train.lin <- tslm(NVIDIA_train.ts ~ trend)
summary(NVIDIA_train.lin)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.lin.pred <- forecast(NVIDIA_train.lin, h = NVIDIA_nValid, level = 0)
NVIDIA_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.lin.pred$mean, NVIDIA_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

NVIDIA_train.quad <- tslm(NVIDIA_train.ts ~ trend + I(trend^2))
summary(NVIDIA_train.quad)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.quad.pred <- forecast(NVIDIA_train.quad, h = NVIDIA_nValid, level = 0)
NVIDIA_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.quad.pred$mean, NVIDIA_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

NVIDIA_train.season <- tslm(NVIDIA_train.ts ~ season)
summary(NVIDIA_train.season)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.season.pred <- forecast(NVIDIA_train.season, h = NVIDIA_nValid, level = 0)
NVIDIA_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.season.pred$mean, NVIDIA_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

NVIDIA_train.lin.season <- tslm(NVIDIA_train.ts ~ trend + season)
summary(NVIDIA_train.lin.season)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.lin.season.pred <- forecast(NVIDIA_train.lin.season, h = NVIDIA_nValid, level = 0)
NVIDIA_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.lin.season.pred$mean, NVIDIA_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

NVIDIA_train.quad.season <- tslm(NVIDIA_train.ts ~ trend + I(trend^2) + season)
summary(NVIDIA_train.quad.season)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.quad.season.pred <- forecast(NVIDIA_train.quad.season, h = NVIDIA_nValid, level = 0)
NVIDIA_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.quad.season.pred$mean, NVIDIA_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

NVIDIA_lin.trend <- tslm(NVIDIA_revenue.ts ~ trend)
summary(NVIDIA_lin.trend)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_lin.trend.pred <- forecast(NVIDIA_lin.trend, h = 16, level = 0)
NVIDIA_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(NVIDIA_lin.trend.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_lin.trend.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVIF DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_lin.trend.pred$fitted, NVIDIA_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

NVIDIA_quad.trend <- tslm(NVIDIA_revenue.ts ~ trend + I(trend^2))
summary(NVIDIA_quad.trend)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_quad.trend.pred <- forecast(NVIDIA_quad.trend, h = 16, level = 0)
NVIDIA_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(NVIDIA_quad.trend.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_quad.trend.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_quad.trend.pred$fitted, NVIDIA_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

NVIDIA_revenue.season <- tslm(NVIDIA_revenue.ts ~ season)
summary(NVIDIA_revenue.season)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_revenue.season.pred <- forecast(NVIDIA_revenue.season, h = 16, level = 0)
NVIDIA_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(NVIDIA_revenue.season.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_revenue.season.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2,20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_revenue.season.pred$fitted, NVIDIA_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

NVIDIA_lin.season <- tslm(NVIDIA_revenue.ts ~ trend + season)
summary(NVIDIA_lin.season)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_lin.season.pred <- forecast(NVIDIA_lin.season, h = 16, level = 0)
NVIDIA_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(NVIDIA_lin.season.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_lin.season.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_lin.season.pred$fitted, NVIDIA_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

NVIDIA_quad.season <- tslm(NVIDIA_revenue.ts ~ trend + I(trend^2) + season)
summary(NVIDIA_quad.season)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_quad.season.pred <- forecast(NVIDIA_quad.season, h = 16, level = 0)
NVIDIA_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(NVIDIA_quad.season.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_quad.season.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_quad.season.pred$fitted, NVIDIA_revenue.ts),3)


#-------------------- MODEL 4 : TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
NVIDIA_trend.seas <- tslm(NVIDIA_train.ts ~ trend + season)
summary(NVIDIA_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
NVIDIA_trend.seas.res <- NVIDIA_trend.seas$residuals
NVIDIA_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
NVIDIA_ma.trail.res_2 <- rollmean(NVIDIA_trend.seas.res, k = 2, align = "right")
NVIDIA_ma.trail.res_2
NVIDIA_ma.trail.res_3 <- rollmean(NVIDIA_trend.seas.res, k = 3, align = "right")
NVIDIA_ma.trail.res_3
NVIDIA_ma.trail.res_4 <- rollmean(NVIDIA_trend.seas.res, k = 4, align = "right")
NVIDIA_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
NVIDIA_trend.seas.pred <- forecast(NVIDIA_trend.seas, h = NVIDIA_nValid, level = 0)
NVIDIA_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
NVIDIA_trend.seas.res.valid <- NVIDIA_valid.ts - NVIDIA_trend.seas.pred$mean
NVIDIA_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
NVIDIA_ma.trail.res.pred_2 <- forecast(NVIDIA_ma.trail.res_2, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.res.pred_2
NVIDIA_ma.trail.res.pred_3 <- forecast(NVIDIA_ma.trail.res_3, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.res.pred_3
NVIDIA_ma.trail.res.pred_4 <- forecast(NVIDIA_ma.trail.res_4, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
NVIDIA_fst.2level_2 <- NVIDIA_trend.seas.pred$mean + NVIDIA_ma.trail.res.pred_2$mean
NVIDIA_fst.2level_2
NVIDIA_fst.2level_3 <- NVIDIA_trend.seas.pred$mean + NVIDIA_ma.trail.res.pred_3$mean
NVIDIA_fst.2level_3
NVIDIA_fst.2level_4 <- NVIDIA_trend.seas.pred$mean + NVIDIA_ma.trail.res.pred_4$mean
NVIDIA_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
NVIDIA_valid_2.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_trend.seas.pred$mean, 
                               NVIDIA_ma.trail.res.pred_2$mean, 
                               NVIDIA_fst.2level_2), 3)
names(NVIDIA_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_valid_2.df

NVIDIA_valid_3.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_trend.seas.pred$mean, 
                               NVIDIA_ma.trail.res.pred_3$mean, 
                               NVIDIA_fst.2level_3), 3)
names(NVIDIA_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_valid_3.df

NVIDIA_valid_4.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_trend.seas.pred$mean, 
                               NVIDIA_ma.trail.res.pred_4$mean, 
                               NVIDIA_fst.2level_4), 3)
names(NVIDIA_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(NVIDIA_fst.2level_2, NVIDIA_valid.ts), 3)
round(accuracy(NVIDIA_fst.2level_3, NVIDIA_valid.ts), 3)
round(accuracy(NVIDIA_fst.2level_4, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
NVIDIA_tot.trend.seas <- tslm(NVIDIA_revenue.ts ~ trend  + season)
summary(NVIDIA_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
NVIDIA_tot.trend.seas.pred <- forecast(NVIDIA_tot.trend.seas, h = 16, level = 0)
NVIDIA_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
NVIDIA_tot.trend.seas.res <- NVIDIA_tot.trend.seas$residuals
NVIDIA_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
NVIDIA_tot.ma.trail.res_2 <- rollmean(NVIDIA_tot.trend.seas.res, k = 2, align = "right")
NVIDIA_tot.ma.trail.res_2
NVIDIA_tot.ma.trail.res_3 <- rollmean(NVIDIA_tot.trend.seas.res, k = 3, align = "right")
NVIDIA_tot.ma.trail.res_3
NVIDIA_tot.ma.trail.res_4 <- rollmean(NVIDIA_tot.trend.seas.res, k = 4, align = "right")
NVIDIA_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
NVIDIA_tot.ma.trail.res_2.pred <- forecast(NVIDIA_tot.ma.trail.res_2, h = 16, level = 0)
NVIDIA_tot.ma.trail.res_2.pred
NVIDIA_tot.ma.trail.res_3.pred <- forecast(NVIDIA_tot.ma.trail.res_3, h = 16, level = 0)
NVIDIA_tot.ma.trail.res_3.pred
NVIDIA_tot.ma.trail.res_4.pred <- forecast(NVIDIA_tot.ma.trail.res_4, h = 16, level = 0)
NVIDIA_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
NVIDIA_tot.fst.2level_2 <- NVIDIA_tot.trend.seas.pred$mean + NVIDIA_tot.ma.trail.res_2.pred$mean
NVIDIA_tot.fst.2level_2
NVIDIA_tot.fst.2level_3 <- NVIDIA_tot.trend.seas.pred$mean + NVIDIA_tot.ma.trail.res_3.pred$mean
NVIDIA_tot.fst.2level_3
NVIDIA_tot.fst.2level_4 <- NVIDIA_tot.trend.seas.pred$mean + NVIDIA_tot.ma.trail.res_4.pred$mean
NVIDIA_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
NVIDIA_future_2.df <- round(data.frame(NVIDIA_tot.trend.seas.pred$mean, NVIDIA_tot.ma.trail.res_2.pred$mean, 
                                NVIDIA_tot.fst.2level_2), 3)
names(NVIDIA_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_future_2.df

NVIDIA_future_3.df <- round(data.frame(NVIDIA_tot.trend.seas.pred$mean, NVIDIA_tot.ma.trail.res_3.pred$mean, 
                                NVIDIA_tot.fst.2level_3), 3)
names(NVIDIA_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_future_3.df

NVIDIA_future_4.df <- round(data.frame(NVIDIA_tot.trend.seas.pred$mean, NVIDIA_tot.ma.trail.res_4.pred$mean, 
                                NVIDIA_tot.fst.2level_4), 3)
names(NVIDIA_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2009,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,20500)) 
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(NVIDIA_tot.trend.seas.res, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(-700, 1000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(NVIDIA_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(NVIDIA_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(NVIDIA_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(NVIDIA_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(NVIDIA_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 990, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-700,1000)) 
text(2015,1000, "PRE-COVID", col = "blue")
text(2022.2, 1000, "COVID & POST-COVID", col ="green")
arrows(2010.1,950,2019.9, 950,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,950,2024.1, 950,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", 
     ylim = c(900,20000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n", 
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(NVIDIA_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,180000, legend = c("NVIDIA Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20000)) # FOR PRE-COVID DATA
text(2015,20000, "PRE-COVID", col = "blue")
text(2022.2,20000, "COVID & POST-COVID", col ="green")
arrows(2010.1,18500,2019.9,18500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,18500,2024.1, 18500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_2, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_3, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_4, NVIDIA_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
NVIDIA_quad.seas <- tslm(NVIDIA_train.ts ~ trend + I(trend^2) + season)
summary(NVIDIA_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
NVIDIA_quad.seas.res <- NVIDIA_quad.seas$residuals
NVIDIA_quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
NVIDIA_ma.trail.qres_2 <- rollmean(NVIDIA_quad.seas.res, k = 2, align = "right")
NVIDIA_ma.trail.qres_2
NVIDIA_ma.trail.qres_3 <- rollmean(NVIDIA_quad.seas.res, k = 3, align = "right")
NVIDIA_ma.trail.qres_3
NVIDIA_ma.trail.qres_4 <- rollmean(NVIDIA_quad.seas.res, k = 4, align = "right")
NVIDIA_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
NVIDIA_quad.seas.pred <- forecast(NVIDIA_quad.seas, h = NVIDIA_nValid, level = 0)
NVIDIA_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
NVIDIA_quad.seas.res.valid <- NVIDIA_valid.ts - NVIDIA_quad.seas.pred$mean
NVIDIA_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
NVIDIA_ma.trail.qres.pred_2 <- forecast(NVIDIA_ma.trail.qres_2, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.qres.pred_2
NVIDIA_ma.trail.qres.pred_3 <- forecast(NVIDIA_ma.trail.qres_3, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.qres.pred_3
NVIDIA_ma.trail.qres.pred_4 <- forecast(NVIDIA_ma.trail.qres_4, h = NVIDIA_nValid, level = 0)
NVIDIA_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
NVIDIA_qfst.2level_2 <- NVIDIA_quad.seas.pred$mean + NVIDIA_ma.trail.qres.pred_2$mean
NVIDIA_qfst.2level_2
NVIDIA_qfst.2level_3 <- NVIDIA_quad.seas.pred$mean + NVIDIA_ma.trail.qres.pred_3$mean
NVIDIA_qfst.2level_3
NVIDIA_qfst.2level_4 <- NVIDIA_quad.seas.pred$mean + NVIDIA_ma.trail.qres.pred_4$mean
NVIDIA_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
NVIDIA_qvalid_2.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_quad.seas.pred$mean, 
                                NVIDIA_ma.trail.qres.pred_2$mean, 
                                NVIDIA_qfst.2level_2), 3)
names(NVIDIA_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_qvalid_2.df

NVIDIA_qvalid_3.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_quad.seas.pred$mean, 
                                NVIDIA_ma.trail.qres.pred_3$mean, 
                                NVIDIA_qfst.2level_3), 3)
names(NVIDIA_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_qvalid_3.df

NVIDIA_qvalid_4.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_quad.seas.pred$mean, 
                                NVIDIA_ma.trail.qres.pred_4$mean, 
                                NVIDIA_qfst.2level_4), 3)
names(NVIDIA_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(NVIDIA_qfst.2level_2, NVIDIA_valid.ts), 3)
round(accuracy(NVIDIA_qfst.2level_3, NVIDIA_valid.ts), 3)
round(accuracy(NVIDIA_qfst.2level_4, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
NVIDIA_tot.quad.seas <- tslm(NVIDIA_revenue.ts ~ trend + I(trend^2) + season)
summary(NVIDIA_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
NVIDIA_tot.quad.seas.pred <- forecast(NVIDIA_tot.quad.seas, h = 16, level = 0)
NVIDIA_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
NVIDIA_tot.quad.seas.res <- NVIDIA_tot.quad.seas$residuals
NVIDIA_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
NVIDIA_quad.ma.trail.res_2 <- rollmean(NVIDIA_tot.quad.seas.res, k = 2, align = "right")
NVIDIA_quad.ma.trail.res_2
NVIDIA_quad.ma.trail.res_3 <- rollmean(NVIDIA_tot.quad.seas.res, k = 3, align = "right")
NVIDIA_quad.ma.trail.res_3
NVIDIA_quad.ma.trail.res_4 <- rollmean(NVIDIA_tot.quad.seas.res, k = 4, align = "right")
NVIDIA_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
NVIDIA_quad.ma.trail.res_2.pred <- forecast(NVIDIA_quad.ma.trail.res_2, h = 16, level = 0)
NVIDIA_quad.ma.trail.res_2.pred$mean
NVIDIA_quad.ma.trail.res_3.pred <- forecast(NVIDIA_quad.ma.trail.res_3, h = 16, level = 0)
NVIDIA_quad.ma.trail.res_3.pred$mean
NVIDIA_quad.ma.trail.res_4.pred <- forecast(NVIDIA_quad.ma.trail.res_4, h = 16, level = 0)
NVIDIA_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
NVIDIA_quad.fst.2level_2 <- NVIDIA_tot.quad.seas.pred$mean + NVIDIA_quad.ma.trail.res_2.pred$mean
NVIDIA_quad.fst.2level_2
NVIDIA_quad.fst.2level_3 <- NVIDIA_tot.quad.seas.pred$mean + NVIDIA_quad.ma.trail.res_3.pred$mean
NVIDIA_quad.fst.2level_3
NVIDIA_quad.fst.2level_4 <- NVIDIA_tot.quad.seas.pred$mean + NVIDIA_quad.ma.trail.res_4.pred$mean
NVIDIA_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
NVIDIA_futureq_2.df <- round(data.frame(NVIDIA_tot.quad.seas.pred$mean, NVIDIA_quad.ma.trail.res_2.pred$mean, 
                                 NVIDIA_quad.fst.2level_2), 3)
names(NVIDIA_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_futureq_2.df

NVIDIA_futureq_3.df <- round(data.frame(NVIDIA_tot.quad.seas.pred$mean, NVIDIA_quad.ma.trail.res_3.pred$mean, 
                                 NVIDIA_quad.fst.2level_3), 3)
names(NVIDIA_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_futureq_3.df

NVIDIA_futureq_4.df <- round(data.frame(NVIDIA_tot.quad.seas.pred$mean, NVIDIA_quad.ma.trail.res_4.pred$mean, 
                                 NVIDIA_quad.fst.2level_4), 3)
names(NVIDIA_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
NVIDIA_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,20500)) 
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(NVIDIA_tot.quad.seas.res, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(-1300, 900), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(NVIDIA_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(NVIDIA_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(NVIDIA_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(NVIDIA_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(NVIDIA_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 870, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-1300,900)) 
text(2015,900,"PRE-COVID", col = "blue")
text(2022.2, 900, "COVID & POST-COVID", col ="green")
arrows(2010.1,850,2019.9, 850,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,850,2024.1, 850,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", 
     ylim = c(900,20000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(NVIDIA_quad.fst.2level_2, col = "green", lwd = 3)
legend(2010,17000, legend = c("NVIDIA Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20000)) # FOR PRE-COVID DATA
text(2015,20000, "PRE-COVID", col = "blue")
text(2022.2,20000, "COVID & POST-COVID", col ="green")
arrows(2010.1,19500,2019.9,19500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,19500,2024.1, 19500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_2, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_3, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_4, NVIDIA_revenue.ts), 3)


#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL ------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
NVIDIA_hw.ZZZ <- ets(NVIDIA_train.ts, model = "ZZZ")
NVIDIA_hw.ZZZ 
# MODEL : (M, N, N); alpha = 0.9999

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
NVIDIA_hw.ZZZ.pred <- forecast(NVIDIA_hw.ZZZ, h = NVIDIA_nValid, level = 0)
NVIDIA_hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_hw.ZZZ.pred$mean, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
NVIDIA_HW.ZZZ <- ets(NVIDIA_revenue.ts, model = "MAM")
NVIDIA_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.9967, beta = 0.0004, gamma = 0.0033

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
NVIDIA_HW.ZZZ.pred <- forecast(NVIDIA_HW.ZZZ, h = 16 , level = 0)
NVIDIA_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(NVIDIA_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 5: HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_HW.ZZZ.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_allrevenue.ts)
legend(2010,19800, 
       legend = c("Revenue (2010-2023)", 
                  "Holt-Winter's Forecast: Pre-Covid Period (2010-2019)",
                  "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_HW.ZZZ.pred$fitted, NVIDIA_revenue.ts), 3)


#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(NVIDIA_train.ts, lag.max = 8, main = "Autocorrelation for NVIDIA's Revenue Training Data Set")
Acf(NVIDIA_valid.ts, lag.max = 8, main = "Autocorrelation for NVIDIA's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
NVIDIA_hw.ZZZ <- ets(NVIDIA_train.ts, model = "ZZZ")
NVIDIA_hw.ZZZ 
# MODEL : (M, N, N); alpha = 0.9999

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
NVIDIA_hw.ZZZ.pred <- forecast(NVIDIA_hw.ZZZ, h = NVIDIA_nValid, level = 0)
NVIDIA_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
NVIDIA_train.residuals <- NVIDIA_hw.ZZZ.pred$residuals
NVIDIA_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(NVIDIA_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of NVIDIA's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
NVIDIA_res.ar1 <- Arima(NVIDIA_hw.ZZZ$residuals, order = c(1,0,0))
summary(NVIDIA_res.ar1)

# FORECAST FOR VALIDATION DATA
NVIDIA_res.ar1.pred <- forecast(NVIDIA_res.ar1, h = NVIDIA_nValid, level = 0)
NVIDIA_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(NVIDIA_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for NVIDIA's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
NVIDIA_valid.two.level.pred <- NVIDIA_hw.ZZZ.pred$mean + NVIDIA_res.ar1.pred$mean
NVIDIA_valid.df <- round(data.frame(NVIDIA_valid.ts, NVIDIA_hw.ZZZ.pred$mean, 
                             NVIDIA_res.ar1.pred$mean, NVIDIA_valid.two.level.pred),3)
names(NVIDIA_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
NVIDIA_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_valid.two.level.pred, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
NVIDIA_HW.ZZZ <- ets(NVIDIA_revenue.ts, model = "MAM")
NVIDIA_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.9967, beta = 0.0001, gamma = 0.0033

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
NVIDIA_HW.ZZZ.pred <- forecast(NVIDIA_HW.ZZZ, h = 16 , level = 0)
NVIDIA_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
NVIDIA_residual.ar1 <- Arima(NVIDIA_HW.ZZZ$residuals, order = c(1,0,0))
NVIDIA_residual.ar1.pred <- forecast(NVIDIA_residual.ar1, h = 16, level = 0)
summary(NVIDIA_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(NVIDIA_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
NVIDIA_HW.ZZZ.ar1.pred <- NVIDIA_HW.ZZZ.pred$mean + NVIDIA_residual.ar1.pred$mean
NVIDIA_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
NVIDIA_table.df <- round(data.frame(NVIDIA_HW.ZZZ.pred$mean, 
                             NVIDIA_residual.ar1.pred$mean, NVIDIA_HW.ZZZ.ar1.pred),3)
names(NVIDIA_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
NVIDIA_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", ylim = c(900, 20000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 6: TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_HW.ZZZ$fitted + NVIDIA_residual.ar1$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_HW.ZZZ.ar1.pred, col = "green", lwd = 2)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                              "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) 
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_HW.ZZZ$fitted + NVIDIA_residual.ar1$fitted, NVIDIA_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
NVIDIA_train.auto.arima <- auto.arima(NVIDIA_train.ts)
summary(NVIDIA_train.auto.arima)

# FORECAST FOR VALIDATION DATA
NVIDIA_train.auto.arima.pred <- forecast(NVIDIA_train.auto.arima, h = NVIDIA_nValid, level = 0)
NVIDIA_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(NVIDIA_train.auto.arima.pred$mean, NVIDIA_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
NVIDIA_revenue.auto.arima <- arima(NVIDIA_revenue.ts,
                              order = c(5,1,3),
                              seasonal = c(5,1,2),
                              method = "ML")
summary(NVIDIA_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
NVIDIA_revenue.auto.arima.pred <- forecast(NVIDIA_revenue.auto.arima, h = 16, level = 0)
NVIDIA_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR ARIMA MODEL
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", 
     ylim = c(900, 20000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n",
     main = "MODEL 7 : ARIMA MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA_revenue.auto.arima.pred$fitted, col = "yellow", lwd = 2)
lines(NVIDIA_revenue.auto.arima.pred$mean, col = "green", lwd = 2)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,20500)) # FOR VALIDATION DATASET
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(NVIDIA_revenue.auto.arima.pred$fitted, NVIDIA_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON NVIDIA_revenue.ts (2010-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(NVIDIA_revenue.ts))$fitted, NVIDIA_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(NVIDIA_revenue.ts))$fitted, NVIDIA_revenue.ts), 3)

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(NVIDIA_lin.trend.pred$fitted, NVIDIA_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(NVIDIA_quad.trend.pred$fitted, NVIDIA_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(NVIDIA_revenue.season.pred$fitted, NVIDIA_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(NVIDIA_lin.season.pred$fitted, NVIDIA_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(NVIDIA_quad.season.pred$fitted, NVIDIA_revenue.ts),3)

#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_2, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_3, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.trend.seas.pred$fitted + NVIDIA_tot.ma.trail.res_4, NVIDIA_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_2, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_3, NVIDIA_revenue.ts), 3)
round(accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_4, NVIDIA_revenue.ts), 3)

#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(NVIDIA_HW.ZZZ.pred$fitted, NVIDIA_revenue.ts), 3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(NVIDIA_HW.ZZZ$fitted + NVIDIA_residual.ar1$fitted, NVIDIA_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(NVIDIA_revenue.auto.arima.pred$fitted, NVIDIA_revenue.ts), 3)

#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
NVIDIA_accuracy_model_1 <- accuracy(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_2, NVIDIA_revenue.ts)
NVIDIA_accuracy_model_2 <- accuracy(NVIDIA_HW.ZZZ.pred$fitted, NVIDIA_revenue.ts)
NVIDIA_accuracy_model_3 <- accuracy(NVIDIA_revenue.auto.arima.pred$fitted, NVIDIA_revenue.ts)

# Extract RMSE and MAPE for each model
NVIDIA_rmse_model_1 <- NVIDIA_accuracy_model_1[1, "RMSE"]
NVIDIA_mape_model_1 <- NVIDIA_accuracy_model_1[1, "MAPE"]

NVIDIA_rmse_model_2 <- NVIDIA_accuracy_model_2[1, "RMSE"]
NVIDIA_mape_model_2 <- NVIDIA_accuracy_model_2[1, "MAPE"]

NVIDIA_rmse_model_3 <- NVIDIA_accuracy_model_3[1, "RMSE"]
NVIDIA_mape_model_3 <- NVIDIA_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
NVIDIA_inv_rmse_model_1 <- 1 / NVIDIA_rmse_model_1
NVIDIA_inv_mape_model_1 <- 1 / NVIDIA_mape_model_1

NVIDIA_inv_rmse_model_2 <- 1 / NVIDIA_rmse_model_2
NVIDIA_inv_mape_model_2 <- 1 / NVIDIA_mape_model_2

NVIDIA_inv_rmse_model_3 <- 1 / NVIDIA_rmse_model_3
NVIDIA_inv_mape_model_3 <- 1 / NVIDIA_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
NVIDIA_total_inv_rmse <- NVIDIA_inv_rmse_model_1 + NVIDIA_inv_rmse_model_2 + NVIDIA_inv_rmse_model_3
NVIDIA_total_inv_mape <- NVIDIA_inv_mape_model_1 + NVIDIA_inv_mape_model_2 + NVIDIA_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
NVIDIA_inv_rmse_weight_1 <- NVIDIA_inv_rmse_model_1 / NVIDIA_total_inv_rmse
NVIDIA_inv_rmse_weight_2 <- NVIDIA_inv_rmse_model_2 / NVIDIA_total_inv_rmse
NVIDIA_inv_rmse_weight_3 <- NVIDIA_inv_rmse_model_3 / NVIDIA_total_inv_rmse

NVIDIA_inv_mape_weight_1 <- NVIDIA_inv_mape_model_1 / NVIDIA_total_inv_mape
NVIDIA_inv_mape_weight_2 <- NVIDIA_inv_mape_model_2 / NVIDIA_total_inv_mape
NVIDIA_inv_mape_weight_3 <- NVIDIA_inv_mape_model_3 / NVIDIA_total_inv_mape

# Calculate the total weight for each model
NVIDIA_total_weight_1 <- (NVIDIA_inv_rmse_weight_1 + NVIDIA_inv_mape_weight_1) / 2
NVIDIA_total_weight_2 <- (NVIDIA_inv_rmse_weight_2 + NVIDIA_inv_mape_weight_2) / 2
NVIDIA_total_weight_3 <- (NVIDIA_inv_rmse_weight_3 + NVIDIA_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", NVIDIA_rmse_model_1, "MAPE:", NVIDIA_mape_model_1, 
    "Inverse RMSE:", NVIDIA_inv_rmse_model_1, "Inverse MAPE:", NVIDIA_inv_mape_model_1, 
    "Inv. RMSE Weight:", NVIDIA_inv_rmse_weight_1, "Inv. MAPE Weight:", NVIDIA_inv_mape_weight_1, 
    "Total Weight:", NVIDIA_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", NVIDIA_rmse_model_2, "MAPE:", NVIDIA_mape_model_2, 
    "Inverse RMSE:", NVIDIA_inv_rmse_model_2, "Inverse MAPE:", NVIDIA_inv_mape_model_2, 
    "Inv. RMSE Weight:", NVIDIA_inv_rmse_weight_2, "Inv. MAPE Weight:", NVIDIA_inv_mape_weight_2, 
    "Total Weight:", NVIDIA_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", NVIDIA_rmse_model_3, "MAPE:", NVIDIA_mape_model_3, 
    "Inverse RMSE:", NVIDIA_inv_rmse_model_3, "Inverse MAPE:", NVIDIA_inv_mape_model_3, 
    "Inv. RMSE Weight:", NVIDIA_inv_rmse_weight_3, "Inv. MAPE Weight:", NVIDIA_inv_mape_weight_3, 
    "Total Weight:", NVIDIA_total_weight_3, "\n\n")

# Calculate the ensemble model weights
NVIDIA_total_inv_rmse_all <- NVIDIA_inv_rmse_model_1 + NVIDIA_inv_rmse_model_2 + NVIDIA_inv_rmse_model_3
NVIDIA_total_inv_mape_all <- NVIDIA_inv_mape_model_1 + NVIDIA_inv_mape_model_2 + NVIDIA_inv_mape_model_3

NVIDIA_ensemble_inv_rmse_weight <- NVIDIA_total_inv_rmse_all / NVIDIA_total_inv_rmse_all
NVIDIA_ensemble_inv_mape_weight <- NVIDIA_total_inv_mape_all / NVIDIA_total_inv_mape_all

# Create a dataframe-like structure
NVIDIA_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(NVIDIA_rmse_model_1, NVIDIA_rmse_model_2, NVIDIA_rmse_model_3, ""),
  MAPE = c(NVIDIA_mape_model_1, NVIDIA_mape_model_2, NVIDIA_mape_model_3, ""),
  `Inverse RMSE` = c(NVIDIA_inv_rmse_model_1, NVIDIA_inv_rmse_model_2, NVIDIA_inv_rmse_model_3, NVIDIA_total_inv_rmse_all),
  `Inverse MAPE` = c(NVIDIA_inv_mape_model_1, NVIDIA_inv_mape_model_2, NVIDIA_inv_mape_model_3, NVIDIA_total_inv_mape_all),
  `Inv. RMSE Weight` = c(NVIDIA_inv_rmse_weight_1, NVIDIA_inv_rmse_weight_2, NVIDIA_inv_rmse_weight_3, NVIDIA_ensemble_inv_rmse_weight),
  `Inv. MAPE Weight` = c(NVIDIA_inv_mape_weight_1, NVIDIA_inv_mape_weight_2, NVIDIA_inv_mape_weight_3, NVIDIA_ensemble_inv_mape_weight),
  `Total Weight` = c(NVIDIA_total_weight_1, NVIDIA_total_weight_2, NVIDIA_total_weight_3, 1)
)


# Print the dataframe
print(NVIDIA_results, right = F)


#-----------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)----------------------#

# Create ensemble forecast for pre-COVID period.
NVIDIA.ensemble.pre_covid <-( 
  (NVIDIA_total_weight_1*(NVIDIA_tot.quad.seas.pred$fitted + NVIDIA_quad.ma.trail.res_2))
  + (NVIDIA_total_weight_2*NVIDIA_HW.ZZZ.pred$fitted)
  + (NVIDIA_total_weight_3*NVIDIA_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
NVIDIA.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(NVIDIA.ensemble.pre_covid, NVIDIA_revenue.ts), 3)

#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Create ensemble forecast for COVID & post-COVID periods.
NVIDIA.ensemble.covid <-( 
  (NVIDIA_total_weight_1*NVIDIA_quad.fst.2level_2)
  + (NVIDIA_total_weight_2*NVIDIA_HW.ZZZ.pred$mean)
  + (NVIDIA_total_weight_3*NVIDIA_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
NVIDIA.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(NVIDIA_allrevenue.ts, 
     xlab = "Time", ylab = "NVIDIA's Revenue (in Million $)", 
     ylim = c(0, 20500), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(NVIDIA.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(NVIDIA.ensemble.covid, col = "green", lwd = 3)
legend(2010,19800, legend = c("Revenue (2010-2023)", 
                              "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                              "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,20500, "PRE-COVID", col = "blue")
text(2022.2, 20500, "COVID & POST-COVID", col ="green")
arrows(2010.1,20000,2019.9, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,20000,2024.1, 20000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 

NVIDIA.revenue.change <- NVIDIA_future.ts - NVIDIA.ensemble.covid

NVIDIA_revenue_covid.df <- round(data.frame(
                           NVIDIA_future.ts, 
                           NVIDIA.ensemble.covid, 
                           NVIDIA.revenue.change), 3)
names(NVIDIA_revenue_covid.df) <- c(
            "Actual_Revenue", 
            "Ensemble_Fst", 
             "Difference")
NVIDIA_revenue_covid.df

library("writexl")
write_xlsx(NVIDIA_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_NVIDIA_20_23_actual_fst.xlsx")




