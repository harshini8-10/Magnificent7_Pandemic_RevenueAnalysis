# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#--------------------------------- Microsoft ----------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")

# CREATE DATAFRAME
Microsoft.data <- read.csv("MicrosoftRevenue.csv")


# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# microsoft_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023)

microsoft_allrevenue.ts <- ts(Microsoft.data$Microsoft_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
microsoft_allrevenue.ts

# microsoft_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
microsoft_revenue.ts <- ts(Microsoft.data$Microsoft_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
microsoft_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Millions of Dollars)", 
     ylim = c(14000, 65000), bty = "l",
     xaxt = "n", xlim = c(2010, 2025.25), 
     main = "Microsoft Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
microsoft_allrevenue.stl <- stl(microsoft_allrevenue.ts, s.window = "periodic")
autoplot(microsoft_allrevenue.stl, main = "Microsoft Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
microsoft_allautocor <- Acf(microsoft_allrevenue.ts, lag.max = 4, 
                         main = "Autocorrelation Chart for Microsoft")

  # ** AUTOCORRELATION FOR PRE-COVID PERIOD **
  
  # PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
  microsoft_autocor <- Acf(microsoft_revenue.ts, lag.max = 4, 
            main = "Autocorrelation Chart for Microsoft Revenue Data (Pre-Covid : 2010 - 2019)")
  
  # AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
  microsoft_lag <- round(microsoft_autocor$lag,0)
  microsoft_ACF <- round(microsoft_autocor$acf,3)
  data.frame(microsoft_lag,microsoft_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR APPLE'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
microsoft_revenue.ar1 <- Arima(microsoft_revenue.ts, order = c(1,0,0), method = "ML")
summary(microsoft_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'. 


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1
ar1 <- 0.8959
s.e. <- 0.0826
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
diff.microsoft_revenue.ts <- diff(microsoft_revenue.ts, lag = 1)
diff.microsoft_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED APPLE REVENUE
Acf(diff.microsoft_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Microsoft Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(microsoft_revenue.ts) = 40 (10 YEARS)
# microsoft_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# microsoft_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

microsoft_nValid <- 12
microsoft_nTrain <- length(microsoft_revenue.ts) - microsoft_nValid
microsoft_train.ts <- window(microsoft_revenue.ts, start = c(2010, 1), end = c(2010, microsoft_nTrain))
microsoft_train.ts
microsoft_valid.ts <- window(microsoft_revenue.ts, start = c(2010, microsoft_nTrain + 1), end = c(2010, microsoft_nTrain + microsoft_nValid))
microsoft_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# microsoft_future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

microsoft_future.ts <- window(microsoft_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
microsoft_future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(microsoft_train.ts,
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(14000,65000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(microsoft_valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, COVID & POST-COVID INTERVALS
lines(c(2017,2017), c(0,65500)) 
lines(c(2020,2020), c(0,65500))
text(2013.5,65500, "TRAINING")
text(2018.5,65500, "VALIDATION", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010,64000,2016.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2023.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
microsoft_revenue.naive.pred <- naive(microsoft_train.ts, h = microsoft_nValid)
microsoft_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_revenue.naive.pred$mean, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
microsoft_revenuef.naive.pred <- naive(microsoft_revenue.ts, h = 16)
microsoft_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(microsoft_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts, col = "black", lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                             "Naive Forecast: Pre-Covid Period (2010-2019)",
                             "Naive Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(microsoft_revenue.ts))$fitted, microsoft_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
microsoft_revenue.snaive.pred <- snaive(microsoft_train.ts, h = microsoft_nValid)
microsoft_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_revenue.snaive.pred$mean, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
microsoft_revenuef.snaive.pred <- snaive(microsoft_revenue.ts, h = 16)
microsoft_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(microsoft_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 2: SEASONAL NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_revenuef.snaive.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts, col = "black", lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                               "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Seasonal Naive Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID PERIODS", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(microsoft_revenue.ts))$fitted, microsoft_revenue.ts), 3)


#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

microsoft_train.lin <- tslm(microsoft_train.ts ~ trend)
summary(microsoft_train.lin)

# FORECAST FOR VALIDATION DATA
microsoft_train.lin.pred <- forecast(microsoft_train.lin, h = microsoft_nValid, level = 0)
microsoft_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.lin.pred$mean, microsoft_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

microsoft_train.quad <- tslm(microsoft_train.ts ~ trend + I(trend^2))
summary(microsoft_train.quad)

# FORECAST FOR VALIDATION DATA
microsoft_train.quad.pred <- forecast(microsoft_train.quad, h = microsoft_nValid, level = 0)
microsoft_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.quad.pred$mean, microsoft_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

microsoft_train.season <- tslm(microsoft_train.ts ~ season)
summary(microsoft_train.season)

# FORECAST FOR VALIDATION DATA
microsoft_train.season.pred <- forecast(microsoft_train.season, h = microsoft_nValid, level = 0)
microsoft_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.season.pred$mean, microsoft_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

microsoft_train.lin.season <- tslm(microsoft_train.ts ~ trend + season)
summary(microsoft_train.lin.season)

# FORECAST FOR VALIDATION DATA
microsoft_train.lin.season.pred <- forecast(microsoft_train.lin.season, h = microsoft_nValid, level = 0)
microsoft_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.lin.season.pred$mean, microsoft_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

microsoft_train.quad.season <- tslm(microsoft_train.ts ~ trend + I(trend^2) + season)
summary(microsoft_train.quad.season)

# FORECAST FOR VALIDATION DATA
microsoft_train.quad.season.pred <- forecast(microsoft_train.quad.season, h = microsoft_nValid, level = 0)
microsoft_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.quad.season.pred$mean, microsoft_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

microsoft_lin.trend <- tslm(microsoft_revenue.ts ~ trend)
summary(microsoft_lin.trend)

# FORECAST FOR POST-COVID PERIOD
microsoft_lin.trend.pred <- forecast(microsoft_lin.trend, h = 16, level = 0)
microsoft_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(microsoft_lin.trend.pred$mean, 
      xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
      bty = "l", xlim = c(2010, 2024), xaxt = "n",
      main = "MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ", 
      lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_lin.trend.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_lin.trend.pred$fitted, microsoft_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

microsoft_quad.trend <- tslm(microsoft_revenue.ts ~ trend + I(trend^2))
summary(microsoft_quad.trend)

# FORECAST FOR POST-COVID PERIOD
microsoft_quad.trend.pred <- forecast(microsoft_quad.trend, h = 16, level = 0)
microsoft_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(microsoft_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_quad.trend.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_quad.trend.pred$fitted, microsoft_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

microsoft_revenue.season <- tslm(microsoft_revenue.ts ~ season)
summary(microsoft_revenue.season)

# FORECAST FOR POST-COVID PERIOD
microsoft_revenue.season.pred <- forecast(microsoft_revenue.season, h = 16, level = 0)
microsoft_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(microsoft_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_revenue.season.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts, lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Model: Pre-Covid Period (2010-2019)",
                              "Regression Model: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_revenue.season.pred$fitted, microsoft_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

microsoft_lin.season <- tslm(microsoft_revenue.ts ~ trend + season)
summary(microsoft_lin.season)

# FORECAST FOR POST-COVID PERIOD
microsoft_lin.season.pred <- forecast(microsoft_lin.season, h = 16, level = 0)
microsoft_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(microsoft_lin.season.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_lin.season.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_lin.season.pred$fitted, microsoft_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

microsoft_quad.season <- tslm(microsoft_revenue.ts ~ trend + I(trend^2) + season)
summary(microsoft_quad.season)

# FORECAST FOR POST-COVID PERIOD
microsoft_quad.season.pred <- forecast(microsoft_quad.season, h = 16, level = 0)
microsoft_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(microsoft_quad.season.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_quad.season.pred$fitted, col = "yellow", lwd = 3)
lines(microsoft_allrevenue.ts, lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_quad.season.pred$fitted, microsoft_revenue.ts),3)


#-------------------- MODEL 4 : TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
microsoft_trend.seas <- tslm(microsoft_train.ts ~ trend + season)
summary(microsoft_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
microsoft_trend.seas.res <- microsoft_trend.seas$residuals
microsoft_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
microsoft_ma.trail.res_2 <- rollmean(microsoft_trend.seas.res, k = 2, align = "right")
microsoft_ma.trail.res_2
microsoft_ma.trail.res_3 <- rollmean(microsoft_trend.seas.res, k = 3, align = "right")
microsoft_ma.trail.res_3
microsoft_ma.trail.res_4 <- rollmean(microsoft_trend.seas.res, k = 4, align = "right")
microsoft_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
microsoft_trend.seas.pred <- forecast(microsoft_trend.seas, h = microsoft_nValid, level = 0)
microsoft_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
microsoft_trend.seas.res.valid <- microsoft_valid.ts - microsoft_trend.seas.pred$mean
microsoft_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
microsoft_ma.trail.res.pred_2 <- forecast(microsoft_ma.trail.res_2, h = microsoft_nValid, level = 0)
microsoft_ma.trail.res.pred_2
microsoft_ma.trail.res.pred_3 <- forecast(microsoft_ma.trail.res_3, h = microsoft_nValid, level = 0)
microsoft_ma.trail.res.pred_3
microsoft_ma.trail.res.pred_4 <- forecast(microsoft_ma.trail.res_4, h = microsoft_nValid, level = 0)
microsoft_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
microsoft_fst.2level_2 <- microsoft_trend.seas.pred$mean + microsoft_ma.trail.res.pred_2$mean
microsoft_fst.2level_2
microsoft_fst.2level_3 <- microsoft_trend.seas.pred$mean + microsoft_ma.trail.res.pred_3$mean
microsoft_fst.2level_3
microsoft_fst.2level_4 <- microsoft_trend.seas.pred$mean + microsoft_ma.trail.res.pred_4$mean
microsoft_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
microsoft_valid_2.df <- round(data.frame(microsoft_valid.ts, microsoft_trend.seas.pred$mean, 
                               microsoft_ma.trail.res.pred_2$mean, 
                               microsoft_fst.2level_2), 3)
names(microsoft_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
microsoft_valid_2.df

microsoft_valid_3.df <- round(data.frame(microsoft_valid.ts, microsoft_trend.seas.pred$mean, 
                               microsoft_ma.trail.res.pred_3$mean, 
                               microsoft_fst.2level_3), 3)
names(microsoft_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
microsoft_valid_3.df

microsoft_valid_4.df <- round(data.frame(microsoft_valid.ts, microsoft_trend.seas.pred$mean, 
                               microsoft_ma.trail.res.pred_4$mean, 
                               microsoft_fst.2level_4), 3)
names(microsoft_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
microsoft_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(microsoft_fst.2level_2, microsoft_valid.ts), 3)
round(accuracy(microsoft_fst.2level_3, microsoft_valid.ts), 3)
round(accuracy(microsoft_fst.2level_4, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
microsoft_tot.trend.seas <- tslm(microsoft_revenue.ts ~ trend  + season)
summary(microsoft_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
microsoft_tot.trend.seas.pred <- forecast(microsoft_tot.trend.seas, h = 16, level = 0)
microsoft_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
microsoft_tot.trend.seas.res <- microsoft_tot.trend.seas$residuals
microsoft_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
microsoft_tot.ma.trail.res_2 <- rollmean(microsoft_tot.trend.seas.res, k = 2, align = "right")
microsoft_tot.ma.trail.res_2
microsoft_tot.ma.trail.res_3 <- rollmean(microsoft_tot.trend.seas.res, k = 3, align = "right")
microsoft_tot.ma.trail.res_3
microsoft_tot.ma.trail.res_4 <- rollmean(microsoft_tot.trend.seas.res, k = 4, align = "right")
microsoft_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
microsoft_tot.ma.trail.res_2.pred <- forecast(microsoft_tot.ma.trail.res_2, h = 16, level = 0)
microsoft_tot.ma.trail.res_2.pred
microsoft_tot.ma.trail.res_3.pred <- forecast(microsoft_tot.ma.trail.res_3, h = 16, level = 0)
microsoft_tot.ma.trail.res_3.pred
microsoft_tot.ma.trail.res_4.pred <- forecast(microsoft_tot.ma.trail.res_4, h = 16, level = 0)
microsoft_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
microsoft_tot.fst.2level_2 <- microsoft_tot.trend.seas.pred$mean + microsoft_tot.ma.trail.res_2.pred$mean
microsoft_tot.fst.2level_2
microsoft_tot.fst.2level_3 <- microsoft_tot.trend.seas.pred$mean + microsoft_tot.ma.trail.res_3.pred$mean
microsoft_tot.fst.2level_3
microsoft_tot.fst.2level_4 <- microsoft_tot.trend.seas.pred$mean + microsoft_tot.ma.trail.res_4.pred$mean
microsoft_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
microsoft_future_2.df <- round(data.frame(microsoft_tot.trend.seas.pred$mean, microsoft_tot.ma.trail.res_2.pred$mean, 
                                microsoft_tot.fst.2level_2), 3)
names(microsoft_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_future_2.df

microsoft_future_3.df <- round(data.frame(microsoft_tot.trend.seas.pred$mean, microsoft_tot.ma.trail.res_3.pred$mean, 
                                microsoft_tot.fst.2level_3), 3)
names(microsoft_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_future_3.df

microsoft_future_4.df <- round(data.frame(microsoft_tot.trend.seas.pred$mean, microsoft_tot.ma.trail.res_4.pred$mean, 
                                microsoft_tot.fst.2level_4), 3)
names(microsoft_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), lwd =2, xaxt = "n",
     main = "MODEL 4A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(microsoft_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,65500)) 
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(microsoft_tot.trend.seas.res, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(-4000, 7000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(microsoft_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(microsoft_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(microsoft_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(microsoft_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(microsoft_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 6500, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-4000,7300)) 
text(2015,6700, "PRE-COVID", col = "blue")
text(2022.2, 6700, "COVID & POST-COVID", col ="green")
arrows(2010.1,6300,2019.9, 6300,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,6300,2024.1, 6300,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", 
     ylim = c(14000,65000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(microsoft_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,60000, legend = c("Microsoft Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65000)) # FOR PRE-COVID DATA
text(2015,65000, "PRE-COVID", col = "blue")
text(2022.2,65000, "COVID & POST-COVID", col ="green")
arrows(2010.1,63000,2019.9,63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,63000,2024.1, 63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_2, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_3, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_4, microsoft_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
microsoft_quad.seas <- tslm(microsoft_train.ts ~ trend + I(trend^2) + season)
summary(microsoft_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
microsoft_quad.seas.res <- microsoft_quad.seas$residuals
microsoft_quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
microsoft_ma.trail.qres_2 <- rollmean(microsoft_quad.seas.res, k = 2, align = "right")
microsoft_ma.trail.qres_2
microsoft_ma.trail.qres_3 <- rollmean(microsoft_quad.seas.res, k = 3, align = "right")
microsoft_ma.trail.qres_3
microsoft_ma.trail.qres_4 <- rollmean(microsoft_quad.seas.res, k = 4, align = "right")
microsoft_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
microsoft_quad.seas.pred <- forecast(microsoft_quad.seas, h = microsoft_nValid, level = 0)
microsoft_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
microsoft_quad.seas.res.valid <- microsoft_valid.ts - microsoft_quad.seas.pred$mean
microsoft_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
microsoft_ma.trail.qres.pred_2 <- forecast(microsoft_ma.trail.qres_2, h = microsoft_nValid, level = 0)
microsoft_ma.trail.qres.pred_2
microsoft_ma.trail.qres.pred_3 <- forecast(microsoft_ma.trail.qres_3, h = microsoft_nValid, level = 0)
microsoft_ma.trail.qres.pred_3
microsoft_ma.trail.qres.pred_4 <- forecast(microsoft_ma.trail.qres_4, h = microsoft_nValid, level = 0)
microsoft_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
microsoft_qfst.2level_2 <- microsoft_quad.seas.pred$mean + microsoft_ma.trail.qres.pred_2$mean
microsoft_qfst.2level_2
microsoft_qfst.2level_3 <- microsoft_quad.seas.pred$mean + microsoft_ma.trail.qres.pred_3$mean
microsoft_qfst.2level_3
microsoft_qfst.2level_4 <- microsoft_quad.seas.pred$mean + microsoft_ma.trail.qres.pred_4$mean
microsoft_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
microsoft_qvalid_2.df <- round(data.frame(microsoft_valid.ts, microsoft_quad.seas.pred$mean, 
                                microsoft_ma.trail.qres.pred_2$mean, 
                                microsoft_qfst.2level_2), 3)
names(microsoft_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
microsoft_qvalid_2.df

microsoft_qvalid_3.df <- round(data.frame(microsoft_valid.ts, microsoft_quad.seas.pred$mean, 
                                microsoft_ma.trail.qres.pred_3$mean, 
                                microsoft_qfst.2level_3), 3)
names(microsoft_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
microsoft_qvalid_3.df

microsoft_qvalid_4.df <- round(data.frame(microsoft_valid.ts, microsoft_quad.seas.pred$mean, 
                                microsoft_ma.trail.qres.pred_4$mean, 
                                microsoft_qfst.2level_4), 3)
names(microsoft_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
microsoft_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(microsoft_qfst.2level_2, microsoft_valid.ts), 3)
round(accuracy(microsoft_qfst.2level_3, microsoft_valid.ts), 3)
round(accuracy(microsoft_qfst.2level_4, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
microsoft_tot.quad.seas <- tslm(microsoft_revenue.ts ~ trend + I(trend^2) + season)
summary(microsoft_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
microsoft_tot.quad.seas.pred <- forecast(microsoft_tot.quad.seas, h = 16, level = 0)
microsoft_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
microsoft_tot.quad.seas.res <- microsoft_tot.quad.seas$residuals
microsoft_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
microsoft_quad.ma.trail.res_2 <- rollmean(microsoft_tot.quad.seas.res, k = 2, align = "right")
microsoft_quad.ma.trail.res_2
microsoft_quad.ma.trail.res_3 <- rollmean(microsoft_tot.quad.seas.res, k = 3, align = "right")
microsoft_quad.ma.trail.res_3
microsoft_quad.ma.trail.res_4 <- rollmean(microsoft_tot.quad.seas.res, k = 4, align = "right")
microsoft_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
microsoft_quad.ma.trail.res_2.pred <- forecast(microsoft_quad.ma.trail.res_2, h = 16, level = 0)
microsoft_quad.ma.trail.res_2.pred$mean
microsoft_quad.ma.trail.res_3.pred <- forecast(microsoft_quad.ma.trail.res_3, h = 16, level = 0)
microsoft_quad.ma.trail.res_3.pred$mean
microsoft_quad.ma.trail.res_4.pred <- forecast(microsoft_quad.ma.trail.res_4, h = 16, level = 0)
microsoft_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
microsoft_quad.fst.2level_2 <- microsoft_tot.quad.seas.pred$mean + microsoft_quad.ma.trail.res_2.pred$mean
microsoft_quad.fst.2level_2
microsoft_quad.fst.2level_3 <- microsoft_tot.quad.seas.pred$mean + microsoft_quad.ma.trail.res_3.pred$mean
microsoft_quad.fst.2level_3
microsoft_quad.fst.2level_4 <- microsoft_tot.quad.seas.pred$mean + microsoft_quad.ma.trail.res_4.pred$mean
microsoft_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
microsoft_futureq_2.df <- round(data.frame(microsoft_tot.quad.seas.pred$mean, microsoft_quad.ma.trail.res_2.pred$mean, 
                                 microsoft_quad.fst.2level_2), 3)
names(microsoft_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_futureq_2.df

microsoft_futureq_3.df <- round(data.frame(microsoft_tot.quad.seas.pred$mean, microsoft_quad.ma.trail.res_3.pred$mean, 
                                 microsoft_quad.fst.2level_3), 3)
names(microsoft_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_futureq_3.df

microsoft_futureq_4.df <- round(data.frame(microsoft_tot.quad.seas.pred$mean, microsoft_quad.ma.trail.res_4.pred$mean, 
                                 microsoft_quad.fst.2level_4), 3)
names(microsoft_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
microsoft_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), lwd =2, xaxt = "n",
     main = "MODEL 4B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(microsoft_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                             "Regression Forecast: Pre-Covid Period (2010-2019)",
                             "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,65500)) 
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(microsoft_tot.quad.seas.res, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(-3000, 5300), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(microsoft_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(microsoft_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(microsoft_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(microsoft_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(microsoft_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 4800, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-3000,5300))
text(2015,5100, "PRE-COVID", col = "blue")
text(2022.2, 5100, "COVID & POST-COVID", col ="green")
arrows(2010.1,4800,2019.9, 4800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,4800,2024.1, 4800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", 
     ylim = c(14000,65000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(microsoft_quad.fst.2level_2, col = "green", lwd = 3)
legend(2010,60000, legend = c("Microsoft Revenue (2010-2023)", 
                               "Two-level Forecast: Pre-Covid Period (2010-2019)",
                               "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65000)) # FOR PRE-COVID DATA
text(2015,65000, "PRE-COVID", col = "blue")
text(2022.2,65000, "COVID & POST-COVID", col ="green")
arrows(2010.1,63000,2019.9,63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,63000,2024.1, 63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_2, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_3, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_4, microsoft_revenue.ts), 3)



#------------------- MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL ------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
microsoft_hw.ZZZ <- ets(microsoft_train.ts, model = "ZZZ")
microsoft_hw.ZZZ 
# MODEL : (M, A, A); alpha = 0.0001, beta = 0.0001, gamma = 0.0001

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
microsoft_hw.ZZZ.pred <- forecast(microsoft_hw.ZZZ, h = microsoft_nValid, level = 0)
microsoft_hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_hw.ZZZ.pred$mean, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
microsoft_HW.ZZZ <- ets(microsoft_revenue.ts, model = "AAA")
microsoft_HW.ZZZ 
# MODEL : (A, A, A); alpha = 0.7217, beta = 0.0001, gamma = 0.0001

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
microsoft_HW.ZZZ.pred <- forecast(microsoft_HW.ZZZ, h = 16 , level = 0)
microsoft_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(microsoft_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 5 : HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_HW.ZZZ.pred$fitted, col = "yellow", lwd = 2)
lines(microsoft_allrevenue.ts, lwd = 2)
legend(2010,64000, 
       legend = c("Revenue (2010-2023)", 
                  "Holt-Winter's Forecast: Pre-Covid Period (2010-2019)",
                  "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_HW.ZZZ.pred$fitted, microsoft_revenue.ts), 3)


#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(microsoft_train.ts, lag.max = 8, main = "Autocorrelation for Microsoft's Revenue Training Data Set")
Acf(microsoft_valid.ts, lag.max = 8, main = "Autocorrelation for Microsoft's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
microsoft_hw.ZZZ <- ets(microsoft_train.ts, model = "ZZZ")
microsoft_hw.ZZZ 
# MODEL : (M, A, A); alpha = 0.0001, beta = 0.0001, gamma = 0.0001

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
microsoft_hw.ZZZ.pred <- forecast(microsoft_hw.ZZZ, h = microsoft_nValid, level = 0)
microsoft_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
microsoft_train.residuals <- microsoft_hw.ZZZ.pred$residuals
microsoft_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(microsoft_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Microsoft's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
microsoft_res.ar1 <- Arima(microsoft_hw.ZZZ$residuals, order = c(1,0,0))
summary(microsoft_res.ar1)

# FORECAST FOR VALIDATION DATA
microsoft_res.ar1.pred <- forecast(microsoft_res.ar1, h = microsoft_nValid, level = 0)
microsoft_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(microsoft_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Microsoft's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
microsoft_valid.two.level.pred <- microsoft_hw.ZZZ.pred$mean + microsoft_res.ar1.pred$mean
microsoft_valid.df <- round(data.frame(microsoft_valid.ts, microsoft_hw.ZZZ.pred$mean, 
                             microsoft_res.ar1.pred$mean, microsoft_valid.two.level.pred),3)
names(microsoft_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
microsoft_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_valid.two.level.pred, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
microsoft_HW.ZZZ <- ets(microsoft_revenue.ts, model = "ZZZ")
microsoft_HW.ZZZ 
# MODEL : (A, A, A); alpha = 0.7217, beta = 0.0001, gamma = 0.0001

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
microsoft_HW.ZZZ.pred <- forecast(microsoft_HW.ZZZ, h = 16 , level = 0)
microsoft_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
microsoft_residual.ar1 <- Arima(microsoft_HW.ZZZ$residuals, order = c(1,0,0))
microsoft_residual.ar1.pred <- forecast(microsoft_residual.ar1, h = 16, level = 0)
summary(microsoft_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(microsoft_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
microsoft_HW.ZZZ.ar1.pred <- microsoft_HW.ZZZ.pred$mean + microsoft_residual.ar1.pred$mean
microsoft_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
microsoft_table.df <- round(data.frame(microsoft_HW.ZZZ.pred$mean, 
                             microsoft_residual.ar1.pred$mean, microsoft_HW.ZZZ.ar1.pred),3)
names(microsoft_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
microsoft_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", ylim = c(14000, 65000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 6 : TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_HW.ZZZ$fitted + microsoft_residual.ar1$fitted, col = "yellow", lwd = 2)
lines(microsoft_HW.ZZZ.ar1.pred, col = "green", lwd = 2)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                              "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) 
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_HW.ZZZ$fitted + microsoft_residual.ar1$fitted, microsoft_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
microsoft_train.auto.arima <- auto.arima(microsoft_train.ts)
summary(microsoft_train.auto.arima)

# FORECAST FOR VALIDATION DATA
microsoft_train.auto.arima.pred <- forecast(microsoft_train.auto.arima, h = microsoft_nValid, level = 0)
microsoft_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(microsoft_train.auto.arima.pred$mean, microsoft_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
microsoft_revenue.auto.arima <- arima(microsoft_revenue.ts,
                                order = c(2,1,2),
                                seasonal = c(2,1,1),
                                method = "ML")
summary(microsoft_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
microsoft_revenue.auto.arima.pred <- forecast(microsoft_revenue.auto.arima, h = 16, level = 0)
microsoft_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR AUTO-ARIMA MODEL
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "Microsoft's Revenue (in Million $)", 
     ylim = c(14000, 65000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 7: ARIMA MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft_revenue.auto.arima$fitted, col = "yellow", lwd = 3)
lines(microsoft_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2010,64000, legend = c("Revenue (2010-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,65500)) # FOR PRE-COVID DATA
text(2015,65500, "PRE-COVID", col = "blue")
text(2022.2, 65500, "COVID & POST-COVID", col ="green")
arrows(2010.1,64000,2019.9, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,64000,2024.1, 64000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(microsoft_revenue.auto.arima.pred$fitted, microsoft_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON microsoft_revenue.ts (2010-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(microsoft_revenue.ts))$fitted, microsoft_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(microsoft_revenue.ts))$fitted, microsoft_revenue.ts), 3)

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(microsoft_lin.trend.pred$fitted, microsoft_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(microsoft_quad.trend.pred$fitted, microsoft_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(microsoft_revenue.season.pred$fitted, microsoft_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(microsoft_lin.season.pred$fitted, microsoft_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(microsoft_quad.season.pred$fitted, microsoft_revenue.ts),3)

#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_2, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_3, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.trend.seas.pred$fitted + microsoft_tot.ma.trail.res_4, microsoft_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_2, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_3, microsoft_revenue.ts), 3)
round(accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_4, microsoft_revenue.ts), 3)

#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(microsoft_HW.ZZZ.pred$fitted, microsoft_revenue.ts), 3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(microsoft_HW.ZZZ$fitted + microsoft_residual.ar1$fitted, microsoft_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(microsoft_revenue.auto.arima.pred$fitted, microsoft_revenue.ts), 3)

#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
microsoft_accuracy_model_1 <- accuracy(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_2, microsoft_revenue.ts)
microsoft_accuracy_model_2 <- accuracy(microsoft_HW.ZZZ.pred$fitted, microsoft_revenue.ts)
microsoft_accuracy_model_3 <- accuracy(microsoft_revenue.auto.arima.pred$fitted, microsoft_revenue.ts)

# Extract RMSE and MAPE for each model
microsoft_rmse_model_1 <- microsoft_accuracy_model_1[1, "RMSE"]
microsoft_mape_model_1 <- microsoft_accuracy_model_1[1, "MAPE"]

microsoft_rmse_model_2 <- microsoft_accuracy_model_2[1, "RMSE"]
microsoft_mape_model_2 <- microsoft_accuracy_model_2[1, "MAPE"]

microsoft_rmse_model_3 <- microsoft_accuracy_model_3[1, "RMSE"]
microsoft_mape_model_3 <- microsoft_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
microsoft_inv_rmse_model_1 <- 1 / microsoft_rmse_model_1
microsoft_inv_mape_model_1 <- 1 / microsoft_mape_model_1

microsoft_inv_rmse_model_2 <- 1 / microsoft_rmse_model_2
microsoft_inv_mape_model_2 <- 1 / microsoft_mape_model_2

microsoft_inv_rmse_model_3 <- 1 / microsoft_rmse_model_3
microsoft_inv_mape_model_3 <- 1 / microsoft_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
microsoft_total_inv_rmse <- microsoft_inv_rmse_model_1 + microsoft_inv_rmse_model_2 + microsoft_inv_rmse_model_3
microsoft_total_inv_mape <- microsoft_inv_mape_model_1 + microsoft_inv_mape_model_2 + microsoft_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
microsoft_inv_rmse_weight_1 <- microsoft_inv_rmse_model_1 / microsoft_total_inv_rmse
microsoft_inv_rmse_weight_2 <- microsoft_inv_rmse_model_2 / microsoft_total_inv_rmse
microsoft_inv_rmse_weight_3 <- microsoft_inv_rmse_model_3 / microsoft_total_inv_rmse

microsoft_inv_mape_weight_1 <- microsoft_inv_mape_model_1 / microsoft_total_inv_mape
microsoft_inv_mape_weight_2 <- microsoft_inv_mape_model_2 / microsoft_total_inv_mape
microsoft_inv_mape_weight_3 <- microsoft_inv_mape_model_3 / microsoft_total_inv_mape

# Calculate the total weight for each model
microsoft_total_weight_1 <- (microsoft_inv_rmse_weight_1 + microsoft_inv_mape_weight_1) / 2
microsoft_total_weight_2 <- (microsoft_inv_rmse_weight_2 + microsoft_inv_mape_weight_2) / 2
microsoft_total_weight_3 <- (microsoft_inv_rmse_weight_3 + microsoft_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", microsoft_rmse_model_1, "MAPE:", microsoft_mape_model_1, 
    "Inverse RMSE:", microsoft_inv_rmse_model_1, "Inverse MAPE:", microsoft_inv_mape_model_1, 
    "Inv. RMSE Weight:", microsoft_inv_rmse_weight_1, "Inv. MAPE Weight:", microsoft_inv_mape_weight_1, 
    "Total Weight:", microsoft_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", microsoft_rmse_model_2, "MAPE:", microsoft_mape_model_2, 
    "Inverse RMSE:", microsoft_inv_rmse_model_2, "Inverse MAPE:", microsoft_inv_mape_model_2, 
    "Inv. RMSE Weight:", microsoft_inv_rmse_weight_2, "Inv. MAPE Weight:", microsoft_inv_mape_weight_2, 
    "Total Weight:", microsoft_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", microsoft_rmse_model_3, "MAPE:", microsoft_mape_model_3, 
    "Inverse RMSE:", microsoft_inv_rmse_model_3, "Inverse MAPE:", microsoft_inv_mape_model_3, 
    "Inv. RMSE Weight:", microsoft_inv_rmse_weight_3, "Inv. MAPE Weight:", microsoft_inv_mape_weight_3, 
    "Total Weight:", microsoft_total_weight_3, "\n\n")

# Calculate the ensemble model weights
microsoft_total_inv_rmse_all <- microsoft_inv_rmse_model_1 + microsoft_inv_rmse_model_2 + microsoft_inv_rmse_model_3
microsoft_total_inv_mape_all <- microsoft_inv_mape_model_1 + microsoft_inv_mape_model_2 + microsoft_inv_mape_model_3

microsoft_ensemble_inv_rmse_weight <- microsoft_total_inv_rmse_all / microsoft_total_inv_rmse_all
microsoft_ensemble_inv_mape_weight <- microsoft_total_inv_mape_all / microsoft_total_inv_mape_all

# Create a dataframe-like structure
microsoft_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(microsoft_rmse_model_1, microsoft_rmse_model_2, microsoft_rmse_model_3, ""),
  MAPE = c(microsoft_mape_model_1, microsoft_mape_model_2, microsoft_mape_model_3, ""),
  `Inverse RMSE` = c(microsoft_inv_rmse_model_1, microsoft_inv_rmse_model_2, microsoft_inv_rmse_model_3, microsoft_total_inv_rmse_all),
  `Inverse MAPE` = c(microsoft_inv_mape_model_1, microsoft_inv_mape_model_2, microsoft_inv_mape_model_3, microsoft_total_inv_mape_all),
  `Inv. RMSE Weight` = c(microsoft_inv_rmse_weight_1, microsoft_inv_rmse_weight_2, microsoft_inv_rmse_weight_3, microsoft_ensemble_inv_rmse_weight),
  `Inv. MAPE Weight` = c(microsoft_inv_mape_weight_1, microsoft_inv_mape_weight_2, microsoft_inv_mape_weight_3, microsoft_ensemble_inv_mape_weight),
  `Total Weight` = c(microsoft_total_weight_1, microsoft_total_weight_2, microsoft_total_weight_3, 1)
)


# Print the dataframe.
print(microsoft_results, right = F)


#---------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)-------------#

# Create ensemble forecast for pre-COVID period.
microsoft.ensemble.pre_covid <-( 
  (microsoft_total_weight_1*(microsoft_tot.quad.seas.pred$fitted + microsoft_quad.ma.trail.res_2))
  + (microsoft_total_weight_2*microsoft_HW.ZZZ.pred$fitted)
  + (microsoft_total_weight_3*microsoft_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
microsoft.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(microsoft.ensemble.pre_covid, microsoft_revenue.ts), 3)

#----------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------#

# Create ensemble forecast for COVID & post-COVID periods.
microsoft.ensemble.covid <-( 
  (microsoft_total_weight_1*microsoft_quad.fst.2level_2)
  + (microsoft_total_weight_2*microsoft_HW.ZZZ.pred$mean)
  + (microsoft_total_weight_3*microsoft_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
microsoft.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(microsoft_allrevenue.ts, 
     xlab = "Time", ylab = "microsoft's Revenue (in Million $)", 
     ylim = c(0, 65000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(microsoft.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(microsoft.ensemble.covid, col = "green", lwd = 3)
legend(2010,63000, legend = c("Revenue (2010-2023)", 
                              "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                              "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,65000)) # FOR PRE-COVID DATA
text(2015, 64500, "PRE-COVID", col = "blue")
text(2022.2, 64500, "COVID & POST-COVID", col ="green")
arrows(2010.1,63000,2019.9, 63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,63000,2024.1, 63000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#--------------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 


microsoft.revenue.change <- microsoft_future.ts - microsoft.ensemble.covid

microsoft_revenue_covid.df <- round(data.frame(
  microsoft_future.ts, 
  microsoft.ensemble.covid, 
  microsoft.revenue.change), 3)
names(microsoft_revenue_covid.df) <- c(
  "Actual_Revenue", 
  "Ensemble_Fst", 
  "Difference")
microsoft_revenue_covid.df

library("writexl")
write_xlsx(microsoft_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_microsoft_20_23_actual_fst.xlsx")













