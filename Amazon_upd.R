# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#------------------------------------- Amazon ---------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")


# CREATE DATAFRAME
Amazon.data <- read.csv("AmazonRevenue.csv")

# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# amazon_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021) AND POST-COVID PERIODS (2022-2023)

amazon_allrevenue.ts <- ts(Amazon.data$Amazon_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
amazon_allrevenue.ts

# amazon_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
amazon_revenue.ts <- ts(Amazon.data$Amazon_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
amazon_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Millions of Dollars)", 
     ylim = c(6000,180000), bty = "l",
     xaxt = "n", xlim = c(2010,2025.25), 
     main = "Amazon Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
amazon_allrevenue.stl <- stl(amazon_allrevenue.ts, s.window = "periodic")
autoplot(amazon_allrevenue.stl, main = "Amazon Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
amazon_allautocor <- Acf(amazon_allrevenue.ts, lag.max = 4, 
                            main = "Autocorrelation Chart for Amazon")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
amazon_autocor <- Acf(amazon_revenue.ts, lag.max = 8, 
      main = "Autocorrelation Chart for Amazon Revenue Data (Pre-Covid: 2010-2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
amazon_lag <- round(amazon_autocor$lag,0)
amazon_ACF <- round(amazon_autocor$acf,3)
data.frame(amazon_lag,amazon_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR AMAZON'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
amazon_revenue.ar1 <- Arima(amazon_revenue.ts, order = c(1,0,0),method = "ML")
summary(amazon_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'. 


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1.
ar1 <- 0.9774
s.e. <- 0.0279
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

# CREATE FIRST DIFFERENCED AMAZON REVENUE DATA USING lag1
diff.amazon_revenue.ts <- diff(amazon_revenue.ts, lag = 1)
diff.amazon_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED AMAZON REVENUE
Acf(diff.amazon_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Amazon Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(amazon_revenue.ts) = 40 (10 YEARS)
# amazon_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# amazon_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

amazon_nValid <- 12
amazon_nTrain <- length(amazon_revenue.ts) - amazon_nValid
amazon_train.ts <- window(amazon_revenue.ts, start = c(2010, 1), end = c(2010, amazon_nTrain))
amazon_train.ts
amazon_valid.ts <- window(amazon_revenue.ts, start = c(2010, amazon_nTrain + 1), end = c(2010, amazon_nTrain + amazon_nValid))
amazon_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# amazon_future.ts IS FOR COVID (2020-2021) AND POST-COVID PERIODS (2022-2023)

amazon_future.ts <- window(amazon_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
amazon_future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(amazon_train.ts,
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(6000,180000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(amazon_valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, COVID & POST-COVID INTERVALS
lines(c(2017,2017), c(0,180500)) 
lines(c(2020,2020), c(0,180500)) 
text(2013.5,180500, "TRAINING")
text(2018.5,180500, "VALIDATION", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010,175000,2016.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2023.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
amazon_revenue.naive.pred <- naive(amazon_train.ts, h = amazon_nValid)
amazon_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_revenue.naive.pred$mean, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
amazon_revenuef.naive.pred <- naive(amazon_revenue.ts, h = 16)
amazon_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(amazon_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =3) 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_revenuef.naive.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, col = "black", lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(amazon_revenue.ts))$fitted, amazon_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
amazon_revenue.snaive.pred <- snaive(amazon_train.ts, h = amazon_nValid)
amazon_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_revenue.snaive.pred$mean, amazon_valid.ts), 3)

#--------------------------- FOR POST-COVID PERIOD --------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
amazon_revenuef.snaive.pred <- snaive(amazon_revenue.ts, h = 16)
amazon_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(amazon_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 2: SEASONAL NAIVE FORECAST", col = "green", lwd =3) 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_revenuef.naive.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, col = "black", lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Seasonal Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(amazon_revenue.ts))$fitted, amazon_revenue.ts), 3)


#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

amazon_train.lin <- tslm(amazon_train.ts ~ trend)
summary(amazon_train.lin)

# FORECAST FOR VALIDATION DATA
amazon_train.lin.pred <- forecast(amazon_train.lin, h = amazon_nValid, level = 0)
amazon_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.lin.pred$mean, amazon_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

amazon_train.quad <- tslm(amazon_train.ts ~ trend + I(trend^2))
summary(amazon_train.quad)

# FORECAST FOR VALIDATION DATA
amazon_train.quad.pred <- forecast(amazon_train.quad, h = amazon_nValid, level = 0)
amazon_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.quad.pred$mean, amazon_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

amazon_train.season <- tslm(amazon_train.ts ~ season)
summary(amazon_train.season)

# FORECAST FOR VALIDATION DATA
amazon_train.season.pred <- forecast(amazon_train.season, h = amazon_nValid, level = 0)
amazon_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.season.pred$mean, amazon_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

amazon_train.lin.season <- tslm(amazon_train.ts ~ trend + season)
summary(amazon_train.lin.season)

# FORECAST FOR VALIDATION DATA
amazon_train.lin.season.pred <- forecast(amazon_train.lin.season, h = amazon_nValid, level = 0)
amazon_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.lin.season.pred$mean, amazon_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

amazon_train.quad.season <- tslm(amazon_train.ts ~ trend + I(trend^2) + season)
summary(amazon_train.quad.season)

# FORECAST FOR VALIDATION DATA
amazon_train.quad.season.pred <- forecast(amazon_train.quad.season, h = amazon_nValid, level = 0)
amazon_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.quad.season.pred$mean, amazon_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#---------------- MODEL 3A: REGRESSION MODEL WITH LINEAR TREND ----------------#

amazon_lin.trend <- tslm(amazon_revenue.ts ~ trend)
summary(amazon_lin.trend)

# FORECAST FOR POST-COVID PERIOD
amazon_lin.trend.pred <- forecast(amazon_lin.trend, h = 16, level = 0)
amazon_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(amazon_lin.trend.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 3A: REGRESSION MODEL WITH LINEAR TREND", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_lin.trend.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_lin.trend.pred$fitted, amazon_revenue.ts),3)

#-------------- MODEL 3B: REGRESSION MODEL WITH QUADRATIC TREND --------------#

amazon_quad.trend <- tslm(amazon_revenue.ts ~ trend + I(trend^2))
summary(amazon_quad.trend)

# FORECAST FOR POST-COVID PERIOD
amazon_quad.trend.pred <- forecast(amazon_quad.trend, h = 16, level = 0)
amazon_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(amazon_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     ylim = c(6000,180000), bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 3B: REGRESSION MODEL WITH QUADRATIC TREND", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_quad.trend.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_quad.trend.pred$fitted, amazon_revenue.ts),3)

#----------------- MODEL 3C: REGRESSION MODEL WITH SEASONALITY ----------------#

amazon_revenue.season <- tslm(amazon_revenue.ts ~ season)
summary(amazon_revenue.season)

# FORECAST FOR POST-COVID PERIOD
amazon_revenue.season.pred <- forecast(amazon_revenue.season, h = 16, level = 0)
amazon_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(amazon_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 3C: REGRESSION MODEL WITH SEASON", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_revenue.season.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_revenue.season.pred$fitted, amazon_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

amazon_lin.season <- tslm(amazon_revenue.ts ~ trend + season)
summary(amazon_lin.season)

# FORECAST FOR POST-COVID PERIOD
amazon_lin.season.pred <- forecast(amazon_lin.season, h = 16, level = 0)
amazon_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY.
plot(amazon_lin.season.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 3D: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_lin.season.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_lin.season.pred$fitted, amazon_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

amazon_quad.season <- tslm(amazon_revenue.ts ~ trend + I(trend^2) + season)
summary(amazon_quad.season)

# FORECAST FOR POST-COVID PERIOD
amazon_quad.season.pred <- forecast(amazon_quad.season, h = 16, level = 0)
amazon_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY.
plot(amazon_quad.season.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 3E: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_quad.season.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_quad.season.pred$fitted, amazon_revenue.ts),3)


#-------------------- MODEL 4: TWO-LEVEL FORECASTING MODEL --------------------#


#--- MODEL 4A: REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ----#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
amazon_trend.seas <- tslm(amazon_train.ts ~ trend + season)
summary(amazon_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
amazon_trend.seas.res <- amazon_trend.seas$residuals
amazon_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
amazon_ma.trail.res_2 <- rollmean(amazon_trend.seas.res, k = 2, align = "right")
amazon_ma.trail.res_2
amazon_ma.trail.res_3 <- rollmean(amazon_trend.seas.res, k = 3, align = "right")
amazon_ma.trail.res_3
amazon_ma.trail.res_4 <- rollmean(amazon_trend.seas.res, k = 4, align = "right")
amazon_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
amazon_trend.seas.pred <- forecast(amazon_trend.seas, h = amazon_nValid, level = 0)
amazon_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
amazon_trend.seas.res.valid <- amazon_valid.ts - amazon_trend.seas.pred$mean
amazon_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
amazon_ma.trail.res.pred_2 <- forecast(amazon_ma.trail.res_2, h = amazon_nValid, level = 0)
amazon_ma.trail.res.pred_2
amazon_ma.trail.res.pred_3 <- forecast(amazon_ma.trail.res_3, h = amazon_nValid, level = 0)
amazon_ma.trail.res.pred_3
amazon_ma.trail.res.pred_4 <- forecast(amazon_ma.trail.res_4, h = amazon_nValid, level = 0)
amazon_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
amazon_fst.2level_2 <- amazon_trend.seas.pred$mean + amazon_ma.trail.res.pred_2$mean
amazon_fst.2level_2
amazon_fst.2level_3 <- amazon_trend.seas.pred$mean + amazon_ma.trail.res.pred_3$mean
amazon_fst.2level_3
amazon_fst.2level_4 <- amazon_trend.seas.pred$mean + amazon_ma.trail.res.pred_4$mean
amazon_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
amazon_valid_2.df <- round(data.frame(amazon_valid.ts, amazon_trend.seas.pred$mean, 
                               amazon_ma.trail.res.pred_2$mean, 
                               amazon_fst.2level_2), 3)
names(amazon_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
amazon_valid_2.df

amazon_valid_3.df <- round(data.frame(amazon_valid.ts, amazon_trend.seas.pred$mean, 
                               amazon_ma.trail.res.pred_3$mean, 
                               amazon_fst.2level_3), 3)
names(amazon_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
amazon_valid_3.df

amazon_valid_4.df <- round(data.frame(amazon_valid.ts, amazon_trend.seas.pred$mean, 
                               amazon_ma.trail.res.pred_4$mean, 
                               amazon_fst.2level_4), 3)
names(amazon_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
amazon_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(amazon_fst.2level_2, amazon_valid.ts), 3)
round(accuracy(amazon_fst.2level_3, amazon_valid.ts), 3)
round(accuracy(amazon_fst.2level_4, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
amazon_tot.trend.seas <- tslm(amazon_revenue.ts ~ trend  + season)
summary(amazon_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
amazon_tot.trend.seas.pred <- forecast(amazon_tot.trend.seas, h = 16, level = 0)
amazon_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
amazon_tot.trend.seas.res <- amazon_tot.trend.seas$residuals
amazon_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
amazon_tot.ma.trail.res_2 <- rollmean(amazon_tot.trend.seas.res, k = 2, align = "right")
amazon_tot.ma.trail.res_2
amazon_tot.ma.trail.res_3 <- rollmean(amazon_tot.trend.seas.res, k = 3, align = "right")
amazon_tot.ma.trail.res_3
amazon_tot.ma.trail.res_4 <- rollmean(amazon_tot.trend.seas.res, k = 4, align = "right")
amazon_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
amazon_tot.ma.trail.res_2.pred <- forecast(amazon_tot.ma.trail.res_2, h = 16, level = 0)
amazon_tot.ma.trail.res_2.pred
amazon_tot.ma.trail.res_3.pred <- forecast(amazon_tot.ma.trail.res_3, h = 16, level = 0)
amazon_tot.ma.trail.res_3.pred
amazon_tot.ma.trail.res_4.pred <- forecast(amazon_tot.ma.trail.res_4, h = 16, level = 0)
amazon_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
amazon_tot.fst.2level_2 <- amazon_tot.trend.seas.pred$mean + amazon_tot.ma.trail.res_2.pred$mean
amazon_tot.fst.2level_2
amazon_tot.fst.2level_3 <- amazon_tot.trend.seas.pred$mean + amazon_tot.ma.trail.res_3.pred$mean
amazon_tot.fst.2level_3
amazon_tot.fst.2level_4 <- amazon_tot.trend.seas.pred$mean + amazon_tot.ma.trail.res_4.pred$mean
amazon_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
amazon_future_2.df <- round(data.frame(amazon_tot.trend.seas.pred$mean, amazon_tot.ma.trail.res_2.pred$mean, 
                                amazon_tot.fst.2level_2), 3)
names(amazon_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_future_2.df

amazon_future_3.df <- round(data.frame(amazon_tot.trend.seas.pred$mean, amazon_tot.ma.trail.res_3.pred$mean, 
                                amazon_tot.fst.2level_3), 3)
names(amazon_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_future_3.df

amazon_future_4.df <- round(data.frame(amazon_tot.trend.seas.pred$mean, amazon_tot.ma.trail.res_4.pred$mean, 
                                amazon_tot.fst.2level_4), 3)
names(amazon_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), lwd =2, xaxt = "n",
     main = "MODEL 4A: LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(amazon_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-10000,180500)) 
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,170500,2019.9,170500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,170500,2024.1,170500,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(amazon_tot.trend.seas.res, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(-8000,50000), 
     bty = "l", xaxt = "n", xlim = c(2010,2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(amazon_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(amazon_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(amazon_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(amazon_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(amazon_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010,35000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1,1,1,1), lwd =c(2,2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-8000,50000)) 
text(2015,50000, "PRE-COVID", col = "blue")
text(2022.2,50000, "COVID & POST-COVID", col ="green")
arrows(2010.1,47000,2019.9,47000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,47000,2024.1,47000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     ylim = c(6000,180000), bty = "l", xlim = c(2010,2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_2, 
                lwd=3, col = "yellow")
lines(amazon_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,165000, legend = c("Amazon Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2010-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_2, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_3, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_4, amazon_revenue.ts), 3)

#--MODEL 4B: REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
amazon_quad.seas <- tslm(amazon_train.ts ~ trend + I(trend^2) + season)
summary(amazon_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
amazon_quad.seas.res <- amazon_quad.seas$residuals
amazon_quad.seas.res

# LEVEL 2: TRAILING MA MODEL TO FORECAST RESIDUALS
amazon_ma.trail.qres_2 <- rollmean(amazon_quad.seas.res, k = 2, align = "right")
amazon_ma.trail.qres_2
amazon_ma.trail.qres_3 <- rollmean(amazon_quad.seas.res, k = 3, align = "right")
amazon_ma.trail.qres_3
amazon_ma.trail.qres_4 <- rollmean(amazon_quad.seas.res, k = 4, align = "right")
amazon_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
amazon_quad.seas.pred <- forecast(amazon_quad.seas, h = amazon_nValid, level = 0)
amazon_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
amazon_quad.seas.res.valid <- amazon_valid.ts - amazon_quad.seas.pred$mean
amazon_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
amazon_ma.trail.qres.pred_2 <- forecast(amazon_ma.trail.qres_2, h = amazon_nValid, level = 0)
amazon_ma.trail.qres.pred_2
amazon_ma.trail.qres.pred_3 <- forecast(amazon_ma.trail.qres_3, h = amazon_nValid, level = 0)
amazon_ma.trail.qres.pred_3
amazon_ma.trail.qres.pred_4 <- forecast(amazon_ma.trail.qres_4, h = amazon_nValid, level = 0)
amazon_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
amazon_qfst.2level_2 <- amazon_quad.seas.pred$mean + amazon_ma.trail.qres.pred_2$mean
amazon_qfst.2level_2
amazon_qfst.2level_3 <- amazon_quad.seas.pred$mean + amazon_ma.trail.qres.pred_3$mean
amazon_qfst.2level_3
amazon_qfst.2level_4 <- amazon_quad.seas.pred$mean + amazon_ma.trail.qres.pred_4$mean
amazon_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
amazon_qvalid_2.df <- round(data.frame(amazon_valid.ts, amazon_quad.seas.pred$mean, 
                                amazon_ma.trail.qres.pred_2$mean, 
                                amazon_qfst.2level_2), 3)
names(amazon_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
amazon_qvalid_2.df

amazon_qvalid_3.df <- round(data.frame(amazon_valid.ts, amazon_quad.seas.pred$mean, 
                                amazon_ma.trail.qres.pred_3$mean, 
                                amazon_qfst.2level_3), 3)
names(amazon_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
amazon_qvalid_3.df

amazon_qvalid_4.df <- round(data.frame(amazon_valid.ts, amazon_quad.seas.pred$mean, 
                                amazon_ma.trail.qres.pred_4$mean, 
                                amazon_qfst.2level_4), 3)
names(amazon_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
amazon_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(amazon_qfst.2level_2, amazon_valid.ts), 3)
round(accuracy(amazon_qfst.2level_3, amazon_valid.ts), 3)
round(accuracy(amazon_qfst.2level_4, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
amazon_tot.quad.seas <- tslm(amazon_revenue.ts ~ trend + I(trend^2) + season)
summary(amazon_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
amazon_tot.quad.seas.pred <- forecast(amazon_tot.quad.seas, h = 16, level = 0)
amazon_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
amazon_tot.quad.seas.res <- amazon_tot.quad.seas$residuals
amazon_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
amazon_quad.ma.trail.res_2 <- rollmean(amazon_tot.quad.seas.res, k = 2, align = "right")
amazon_quad.ma.trail.res_2
amazon_quad.ma.trail.res_3 <- rollmean(amazon_tot.quad.seas.res, k = 3, align = "right")
amazon_quad.ma.trail.res_3
amazon_quad.ma.trail.res_4 <- rollmean(amazon_tot.quad.seas.res, k = 4, align = "right")
amazon_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
amazon_quad.ma.trail.res_2.pred <- forecast(amazon_quad.ma.trail.res_2, h = 16, level = 0)
amazon_quad.ma.trail.res_2.pred$mean
amazon_quad.ma.trail.res_3.pred <- forecast(amazon_quad.ma.trail.res_3, h = 16, level = 0)
amazon_quad.ma.trail.res_3.pred$mean
amazon_quad.ma.trail.res_4.pred <- forecast(amazon_quad.ma.trail.res_4, h = 16, level = 0)
amazon_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
amazon_quad.fst.2level_2 <- amazon_tot.quad.seas.pred$mean + amazon_quad.ma.trail.res_2.pred$mean
amazon_quad.fst.2level_2
amazon_quad.fst.2level_3 <- amazon_tot.quad.seas.pred$mean + amazon_quad.ma.trail.res_3.pred$mean
amazon_quad.fst.2level_3
amazon_quad.fst.2level_4 <- amazon_tot.quad.seas.pred$mean + amazon_quad.ma.trail.res_4.pred$mean
amazon_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
amazon_futureq_2.df <- round(data.frame(amazon_tot.quad.seas.pred$mean, amazon_quad.ma.trail.res_2.pred$mean, 
                                 amazon_quad.fst.2level_2), 3)
names(amazon_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_futureq_2.df

amazon_futureq_3.df <- round(data.frame(amazon_tot.quad.seas.pred$mean, amazon_quad.ma.trail.res_3.pred$mean, 
                                 amazon_quad.fst.2level_3), 3)
names(amazon_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_futureq_3.df

amazon_futureq_4.df <- round(data.frame(amazon_tot.quad.seas.pred$mean, amazon_quad.ma.trail.res_4.pred$mean, 
                                 amazon_quad.fst.2level_4), 3)
names(amazon_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
amazon_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), lwd =2, xaxt = "n",
     main = "MODEL 4B: LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(amazon_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,180500)) 
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(amazon_tot.quad.seas.res, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(-5000,6500), 
     bty = "l", xaxt = "n", xlim = c(2010,2024), lwd =2, col = "brown", 
     main = "MODEL 4B: LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(amazon_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(amazon_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(amazon_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(amazon_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(amazon_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010,6200, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1,1,1,1), lwd =c(2,2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-5000,6500)) 
text(2015,6500, "PRE-COVID", col = "blue")
text(2022.2, 6500, "COVID & POST-COVID", col ="green")
arrows(2010.1,6200,2019.9, 6200,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,6200,2024.1, 6200,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     ylim = c(6500,180000), bty = "l", xlim = c(2014,2024), lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2014,2024,1), labels = format(seq(2014,2024,1)))
lines(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(amazon_quad.fst.2level_2, col = "green", lwd = 3)
legend(2014,150000, legend = c("Amazon Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2010-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_2, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_3, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_4, amazon_revenue.ts), 3)


#------------------- MODEL 5: AUTOMATED HOLT-WINTER'S MODEL -------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
amazon_hw.ZZZ <- ets(amazon_train.ts, model = "ZZZ")
amazon_hw.ZZZ 
# MODEL : (M, A, M); alpha = 0.3382, beta = 0.2157, gamma = 0.6616

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
amazon_hw.ZZZ.pred <- forecast(amazon_hw.ZZZ, h = amazon_nValid, level = 0)
amazon_hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_hw.ZZZ.pred$mean, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
amazon_HW.ZZZ <- ets(amazon_revenue.ts, model = "ZZZ")
amazon_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.5247, beta = 0.1949, gamma = 0.4753

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
amazon_HW.ZZZ.pred <- forecast(amazon_HW.ZZZ, h = 16 , level = 0)
amazon_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(amazon_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2014,2024), xaxt = "n",
     main = "MODEL 5: HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 3) 
axis(1, at = seq(2014,2024,1), labels = format(seq(2014,2024,1)))
lines(amazon_HW.ZZZ.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_allrevenue.ts, lwd = 2)
legend(2014,165000, 
       legend = c("Revenue (2014-2023)", 
        "Holt-Winter's Forecast: Pre-Covid Period (2014-2019)",
        "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_HW.ZZZ.pred$fitted, amazon_revenue.ts), 3)


#-------------- MODEL 6: AUTOCORRELATION & AUTOREGRESSIVE MODEL ---------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(amazon_train.ts, lag.max = 8, main = "Autocorrelation for Amazon's Revenue Training Data Set")
Acf(amazon_valid.ts, lag.max = 8, main = "Autocorrelation for Amazon's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
amazon_hw.ZZZ <- ets(amazon_train.ts, model = "ZZZ")
amazon_hw.ZZZ 
# MODEL : (M, A, M); alpha = 0.3382, beta = 0.2157, gamma = 0.6616

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
amazon_hw.ZZZ.pred <- forecast(amazon_hw.ZZZ, h = amazon_nValid, level = 0)
amazon_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
amazon_train.residuals <- amazon_hw.ZZZ.pred$residuals
amazon_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(amazon_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Amazon's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
amazon_res.ar1 <- Arima(amazon_hw.ZZZ$residuals, order = c(1,0,0))
summary(amazon_res.ar1)

# FORECAST FOR VALIDATION DATA
amazon_res.ar1.pred <- forecast(amazon_res.ar1, h = amazon_nValid, level = 0)
amazon_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(amazon_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Amazon's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
amazon_valid.two.level.pred <- amazon_hw.ZZZ.pred$mean + amazon_res.ar1.pred$mean
amazon_valid.df <- round(data.frame(amazon_valid.ts, amazon_hw.ZZZ.pred$mean, 
                             amazon_res.ar1.pred$mean, amazon_valid.two.level.pred),3)
names(amazon_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
amazon_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_valid.two.level.pred, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
amazon_HW.ZZZ <- ets(amazon_revenue.ts, model = "ZZZ")
amazon_HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.5247, beta = 0.1949, gamma = 0.4753

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
amazon_HW.ZZZ.pred <- forecast(amazon_HW.ZZZ, h = 16 , level = 0)
amazon_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
amazon_residual.ar1 <- Arima(amazon_HW.ZZZ$residuals, order = c(1,0,0))
amazon_residual.ar1.pred <- forecast(amazon_residual.ar1, h = 16, level = 0)
summary(amazon_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(amazon_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
amazon_HW.ZZZ.ar1.pred <- amazon_HW.ZZZ.pred$mean + amazon_residual.ar1.pred$mean
amazon_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
amazon_table.df <- round(data.frame(amazon_HW.ZZZ.pred$mean, 
                             amazon_residual.ar1.pred$mean, amazon_HW.ZZZ.ar1.pred),3)
names(amazon_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
amazon_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", ylim = c(6000,180000), 
     bty = "l", xlim = c(2010,2024), xaxt = "n",
     main = "MODEL 6: TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_HW.ZZZ$fitted + amazon_residual.ar1$fitted, 
      col = "yellow", lwd = 3)
lines(amazon_HW.ZZZ.ar1.pred, col = "green", lwd = 3)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                               "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) 
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_HW.ZZZ$fitted + amazon_residual.ar1$fitted, amazon_revenue.ts),3) 

#- MODEL 7: AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
amazon_train.auto.arima <- auto.arima(amazon_train.ts)
summary(amazon_train.auto.arima)

# FORECAST FOR VALIDATION DATA
amazon_train.auto.arima.pred <- forecast(amazon_train.auto.arima, h = amazon_nValid, level = 0)
amazon_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(amazon_train.auto.arima.pred$mean, amazon_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
amazon_revenue.auto.arima <- arima(amazon_revenue.ts, 
                order = c(2,1,2), 
                seasonal = c(2,1,1), 
                method = "ML")
summary(amazon_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
amazon_revenue.auto.arima.pred <- forecast(amazon_revenue.auto.arima, h = 16, level = 0)
amazon_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR ARIMA MODEL.
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     ylim = c(6000,180000), bty = "l", xlim = c(2010,2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 7: ARIMA MODEL")  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon_revenue.auto.arima.pred$fitted, col = "yellow", lwd = 3)
lines(amazon_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(amazon_revenue.auto.arima.pred$fitted, amazon_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON amazon_revenue.ts (2010-2019)

#---------------------------- MODEL 1: NAIVE MODEL ---------------------------#
round(accuracy((naive(amazon_revenue.ts))$fitted, amazon_revenue.ts), 3)

#------------------------ MODEL 2: SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(amazon_revenue.ts))$fitted, amazon_revenue.ts), 3)

#--------------- MODEL 3A: REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(amazon_lin.trend.pred$fitted, amazon_revenue.ts),3)

#-------------- MODEL 3B: REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(amazon_quad.trend.pred$fitted, amazon_revenue.ts),3)

#---------------- MODEL 3C: REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(amazon_revenue.season.pred$fitted, amazon_revenue.ts),3)

#-------- MODEL 3D: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(amazon_lin.season.pred$fitted, amazon_revenue.ts),3)

#------- MODEL 3E: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(amazon_quad.season.pred$fitted, amazon_revenue.ts),3)

#--- MODEL 4A: REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_2, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_3, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.trend.seas.pred$fitted + amazon_tot.ma.trail.res_4, amazon_revenue.ts), 3)

#--MODEL 4B: REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_2, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_3, amazon_revenue.ts), 3)
round(accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_4, amazon_revenue.ts), 3)

#------------------ MODEL 5: AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(amazon_HW.ZZZ.pred$fitted, amazon_revenue.ts), 3)

#---------------- MODEL 6: AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(amazon_HW.ZZZ$fitted + amazon_residual.ar1$fitted, amazon_revenue.ts),3) 

#- MODEL 7: AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(amazon_revenue.auto.arima.pred$fitted, amazon_revenue.ts), 3)

#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
amazon_accuracy_model_1 <- accuracy(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_2, amazon_revenue.ts)
amazon_accuracy_model_2 <- accuracy(amazon_HW.ZZZ.pred$fitted, amazon_revenue.ts)
amazon_accuracy_model_3 <- accuracy(amazon_revenue.auto.arima.pred$fitted, amazon_revenue.ts)

# Extract RMSE and MAPE for each model
amazon_rmse_model_1 <- amazon_accuracy_model_1[1, "RMSE"]
amazon_mape_model_1 <- amazon_accuracy_model_1[1, "MAPE"]

amazon_rmse_model_2 <- amazon_accuracy_model_2[1, "RMSE"]
amazon_mape_model_2 <- amazon_accuracy_model_2[1, "MAPE"]

amazon_rmse_model_3 <- amazon_accuracy_model_3[1, "RMSE"]
amazon_mape_model_3 <- amazon_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
amazon_inv_rmse_model_1 <- 1 / amazon_rmse_model_1
amazon_inv_mape_model_1 <- 1 / amazon_mape_model_1

amazon_inv_rmse_model_2 <- 1 / amazon_rmse_model_2
amazon_inv_mape_model_2 <- 1 / amazon_mape_model_2

amazon_inv_rmse_model_3 <- 1 / amazon_rmse_model_3
amazon_inv_mape_model_3 <- 1 / amazon_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
amazon_total_inv_rmse <- amazon_inv_rmse_model_1 + amazon_inv_rmse_model_2 + amazon_inv_rmse_model_3
amazon_total_inv_mape <- amazon_inv_mape_model_1 + amazon_inv_mape_model_2 + amazon_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
amazon_inv_rmse_weight_1 <- amazon_inv_rmse_model_1 / amazon_total_inv_rmse
amazon_inv_rmse_weight_2 <- amazon_inv_rmse_model_2 / amazon_total_inv_rmse
amazon_inv_rmse_weight_3 <- amazon_inv_rmse_model_3 / amazon_total_inv_rmse

amazon_inv_mape_weight_1 <- amazon_inv_mape_model_1 / amazon_total_inv_mape
amazon_inv_mape_weight_2 <- amazon_inv_mape_model_2 / amazon_total_inv_mape
amazon_inv_mape_weight_3 <- amazon_inv_mape_model_3 / amazon_total_inv_mape

# Calculate the total weight for each model
amazon_total_weight_1 <- (amazon_inv_rmse_weight_1 + amazon_inv_mape_weight_1) / 2
amazon_total_weight_2 <- (amazon_inv_rmse_weight_2 + amazon_inv_mape_weight_2) / 2
amazon_total_weight_3 <- (amazon_inv_rmse_weight_3 + amazon_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", amazon_rmse_model_1, "MAPE:", amazon_mape_model_1, 
    "Inverse RMSE:", amazon_inv_rmse_model_1, "Inverse MAPE:", amazon_inv_mape_model_1, 
    "Inv. RMSE Weight:", amazon_inv_rmse_weight_1, "Inv. MAPE Weight:", amazon_inv_mape_weight_1, 
    "Total Weight:", amazon_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", amazon_rmse_model_2, "MAPE:", amazon_mape_model_2, 
    "Inverse RMSE:", amazon_inv_rmse_model_2, "Inverse MAPE:", amazon_inv_mape_model_2, 
    "Inv. RMSE Weight:", amazon_inv_rmse_weight_2, "Inv. MAPE Weight:", amazon_inv_mape_weight_2, 
    "Total Weight:", amazon_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", amazon_rmse_model_3, "MAPE:", amazon_mape_model_3, 
    "Inverse RMSE:", amazon_inv_rmse_model_3, "Inverse MAPE:", amazon_inv_mape_model_3, 
    "Inv. RMSE Weight:", amazon_inv_rmse_weight_3, "Inv. MAPE Weight:", amazon_inv_mape_weight_3, 
    "Total Weight:", amazon_total_weight_3, "\n\n")

# Calculate the ensemble model weights
amazon_total_inv_rmse_all <- amazon_inv_rmse_model_1 + amazon_inv_rmse_model_2 + amazon_inv_rmse_model_3
amazon_total_inv_mape_all <- amazon_inv_mape_model_1 + amazon_inv_mape_model_2 + amazon_inv_mape_model_3

amazon_ensemble_inv_rmse_weight <- amazon_total_inv_rmse_all / amazon_total_inv_rmse_all
amazon_ensemble_inv_mape_weight <- amazon_total_inv_mape_all / amazon_total_inv_mape_all

# Create a dataframe-like structure
amazon_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(amazon_rmse_model_1, amazon_rmse_model_2, amazon_rmse_model_3, ""),
  MAPE = c(amazon_mape_model_1, amazon_mape_model_2, amazon_mape_model_3, ""),
  `Inverse RMSE` = c(amazon_inv_rmse_model_1, amazon_inv_rmse_model_2, amazon_inv_rmse_model_3, amazon_total_inv_rmse_all),
  `Inverse MAPE` = c(amazon_inv_mape_model_1, amazon_inv_mape_model_2, amazon_inv_mape_model_3, amazon_total_inv_mape_all),
  `RMSE Weight` = c(amazon_inv_rmse_weight_1, amazon_inv_rmse_weight_2, amazon_inv_rmse_weight_3, amazon_ensemble_inv_rmse_weight),
  `MAPE Weight` = c(amazon_inv_mape_weight_1, amazon_inv_mape_weight_2, amazon_inv_mape_weight_3, amazon_ensemble_inv_mape_weight),
  `Total Weight` = c(amazon_total_weight_1, amazon_total_weight_2, amazon_total_weight_3, 1)
)


# Print the dataframe.
print(amazon_results, right = F)


#---------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)-------------#

# Create ensemble forecast for pre-COVID period.
amazon.ensemble.pre_covid <-( 
    (amazon_total_weight_1*(amazon_tot.quad.seas.pred$fitted + amazon_quad.ma.trail.res_2))
  + (amazon_total_weight_2*amazon_HW.ZZZ.pred$fitted)
  + (amazon_total_weight_3*amazon_revenue.auto.arima.pred$fitted)
  )
# Display ensemble forecast for pre-COVID period.     
amazon.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(amazon.ensemble.pre_covid, amazon_revenue.ts), 3)

#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Create ensemble forecast for COVID & post-COVID periods.
amazon.ensemble.covid <-( 
    (amazon_total_weight_1*amazon_quad.fst.2level_2)
  + (amazon_total_weight_2*amazon_HW.ZZZ.pred$mean)
  + (amazon_total_weight_3*amazon_revenue.auto.arima.pred$mean)
  )

# Display ensemble forecast for COVID and post-COVID periods.     
amazon.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(amazon_allrevenue.ts, 
     xlab = "Time", ylab = "Amazon's Revenue (in Million $)", 
     ylim = c(6000,180000), bty = "l", xlim = c(2010,2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010,2024,1), labels = format(seq(2010,2024,1)))
lines(amazon.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(amazon.ensemble.covid, col = "green", lwd = 3)
legend(2010,165000, legend = c("Revenue (2010-2023)", 
                               "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                               "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1,1,1), lwd =c(2,2,2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,180500)) # FOR PRE-COVID DATA
text(2015,180500, "PRE-COVID", col = "blue")
text(2022.2,180500, "COVID & POST-COVID", col ="green")
arrows(2010.1,175000,2019.9,175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,175000,2024.1, 175000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#----------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 


amazon.revenue.change <- amazon_future.ts - amazon.ensemble.covid

amazon_revenue_covid.df <- round(data.frame(
              amazon_future.ts, 
              amazon.ensemble.covid, 
              amazon.revenue.change), 3)
names(amazon_revenue_covid.df) <- c(
             "Actual_Revenue", 
             "Ensemble_Fst", 
             "Difference")
amazon_revenue_covid.df

library("writexl")
write_xlsx(amazon_revenue_covid.df, 
"C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_amazon_20_23_actual_fst.xlsx")



