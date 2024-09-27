# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#--------------------------------- Alphabet -----------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")


# CREATE DATAFRAME
Alphabet.data <- read.csv("AlphabetRevenue.csv")

# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# alphabet_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2010-2019), COVID (2020-2021) AND POST-COVID PERIODS (2022-2023)

alphabet_allrevenue.ts <- ts(Alphabet.data$Alphabet_Revenue, start = c(2010,1), end = c(2023,4), freq = 4)
alphabet_allrevenue.ts

# alphabet_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
alphabet_revenue.ts <- ts(Alphabet.data$Alphabet_Revenue, start = c(2010,1), end = c(2019,4), freq = 4)
alphabet_revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Millions of Dollars)", 
     ylim = c(6000, 100000), bty = "l",
     xaxt = "n", xlim = c(2010, 2024.25), 
     main = "Alphabet Revenue Data (2010-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2010 TO 2023
alphabet_allrevenue.stl <- stl(alphabet_allrevenue.ts, s.window = "periodic")
autoplot(alphabet_allrevenue.stl, main = "Alphabet Revenue - Time Series Components (2010-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
alphabet_allautocor<- Acf(alphabet_allrevenue.ts, lag.max = 4, 
                        main = "Autocorrelation Chart for Alphabet")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2010 - 2019)
alphabet_autocor <- Acf(alphabet_revenue.ts, lag.max = 4, 
        main = "Autocorrelation Chart for Alphabet Revenue Data (Pre-Covid : 2010 - 2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
alphabet_lag <- round(alphabet_autocor$lag,0)
alphabet_ACF <- round(alphabet_autocor$acf,3)
data.frame(alphabet_lag,alphabet_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR AMAZON'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
alphabet_revenue.ar1 <- Arima(alphabet_revenue.ts, order = c(1,0,0),method = "ML")
summary(alphabet_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'.


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1
ar1 <- 0.9936
s.e. <- 0.0087
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
diff.alphabet_revenue.ts <- diff(alphabet_revenue.ts, lag = 1)
diff.alphabet_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED AMAZON REVENUE
Acf(diff.alphabet_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Alphabet Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(alphabet_revenue.ts) = 40 (10 YEARS)
# alphabet_nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# alphabet_nTrain = 28 QUARTERS (7 YEARS), FROM Q1-2010 TO Q4-2016

alphabet_nValid <- 12
alphabet_nTrain <- length(alphabet_revenue.ts) - alphabet_nValid
alphabet_train.ts <- window(alphabet_revenue.ts, start = c(2010, 1), end = c(2010, alphabet_nTrain))
alphabet_train.ts
alphabet_valid.ts <- window(alphabet_revenue.ts, start = c(2010, alphabet_nTrain + 1), end = c(2010, alphabet_nTrain + alphabet_nValid))
alphabet_valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# alphabet_future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

alphabet_future.ts <- window(alphabet_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
alphabet_future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(alphabet_train.ts,
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     xlim = c(2010, 2024.25), ylim = c(6000,100000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(alphabet_valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, COVID & POST-COVID INTERVALS
lines(c(2017,2017), c(0,100500)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,100500)) # FOR VALIDATION DATASET
text(2013.5,100500, "TRAINING")
text(2018.5,100500, "VALIDATION", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010,95000,2016.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2023.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
alphabet_revenue.naive.pred <- naive(alphabet_train.ts, h = alphabet_nValid)
alphabet_revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_revenue.naive.pred$mean, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
alphabet_revenuef.naive.pred <- naive(alphabet_revenue.ts, h = 16)
alphabet_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(alphabet_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts, col = "black", lwd = 2)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                             "Naive Forecast : Pre-Covid Period (2010-2019)",
                             "Naive Forecast : Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(alphabet_revenue.ts))$fitted, alphabet_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
alphabet_revenue.snaive.pred <- snaive(alphabet_train.ts, h = alphabet_nValid)
alphabet_revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_revenue.snaive.pred$mean, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
alphabet_revenuef.snaive.pred <- snaive(alphabet_revenue.ts, h = 16)
alphabet_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(alphabet_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 2 : SEASONAL NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_revenuef.snaive.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts, col = "black", lwd = 2)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                               "Seasonal Naive Forecast: Pre-Covid Period (2010-2019)",
                               "Seasonal Naive Forecast: Covid and Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9,95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(alphabet_revenue.ts))$fitted, alphabet_revenue.ts), 3)


#------------------------- MODEL 3 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

alphabet_train.lin <- tslm(alphabet_train.ts ~ trend)
summary(alphabet_train.lin)

# FORECAST FOR VALIDATION DATA
alphabet_train.lin.pred <- forecast(alphabet_train.lin, h = alphabet_nValid, level = 0)
alphabet_train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.lin.pred$mean, alphabet_valid.ts), 3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

alphabet_train.quad <- tslm(alphabet_train.ts ~ trend + I(trend^2))
summary(alphabet_train.quad)

# FORECAST FOR VALIDATION DATA
alphabet_train.quad.pred <- forecast(alphabet_train.quad, h = alphabet_nValid, level = 0)
alphabet_train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.quad.pred$mean, alphabet_valid.ts), 3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

alphabet_train.season <- tslm(alphabet_train.ts ~ season)
summary(alphabet_train.season)

# FORECAST FOR VALIDATION DATA
alphabet_train.season.pred <- forecast(alphabet_train.season, h = alphabet_nValid, level = 0)
alphabet_train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.season.pred$mean, alphabet_valid.ts), 3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

alphabet_train.lin.season <- tslm(alphabet_train.ts ~ trend + season)
summary(alphabet_train.lin.season)

# FORECAST FOR VALIDATION DATA
alphabet_train.lin.season.pred <- forecast(alphabet_train.lin.season, h = alphabet_nValid, level = 0)
alphabet_train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.lin.season.pred$mean, alphabet_valid.ts),3)

#------ MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

alphabet_train.quad.season <- tslm(alphabet_train.ts ~ trend + I(trend^2) + season)
summary(alphabet_train.quad.season)

# FORECAST FOR VALIDATION DATA
alphabet_train.quad.season.pred <- forecast(alphabet_train.quad.season, h = alphabet_nValid, level = 0)
alphabet_train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.quad.season.pred$mean, alphabet_valid.ts),3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#

alphabet_lin.trend <- tslm(alphabet_revenue.ts ~ trend)
summary(alphabet_lin.trend)

# FORECAST FOR POST-COVID PERIOD
alphabet_lin.trend.pred <- forecast(alphabet_lin.trend, h = 16, level = 0)
alphabet_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(alphabet_lin.trend.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_lin.trend.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_lin.trend.pred$fitted, alphabet_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

alphabet_quad.trend <- tslm(alphabet_revenue.ts ~ trend + I(trend^2))
summary(alphabet_quad.trend)

# FORECAST FOR POST-COVID PERIOD
alphabet_quad.trend.pred <- forecast(alphabet_quad.trend, h = 16, level = 0)
alphabet_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(alphabet_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     ylim = c(6000, 100000), bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_quad.trend.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_quad.trend.pred$fitted, alphabet_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#

alphabet_revenue.season <- tslm(alphabet_revenue.ts ~ season)
summary(alphabet_revenue.season)

# FORECAST FOR POST-COVID PERIOD
alphabet_revenue.season.pred <- forecast(alphabet_revenue.season, h = 16, level = 0)
alphabet_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(alphabet_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_revenue.season.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_revenue.season.pred$fitted, alphabet_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

alphabet_lin.season <- tslm(alphabet_revenue.ts ~ trend + season)
summary(alphabet_lin.season)

# FORECAST FOR POST-COVID PERIOD
alphabet_lin.season.pred <- forecast(alphabet_lin.season, h = 16, level = 0)
alphabet_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(alphabet_lin.season.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_lin.season.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_lin.season.pred$fitted, alphabet_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

alphabet_quad.season <- tslm(alphabet_revenue.ts ~ trend + I(trend^2) + season)
summary(alphabet_quad.season)

# FORECAST FOR POST-COVID PERIOD
alphabet_quad.season.pred <- forecast(alphabet_quad.season, h = 16, level = 0)
alphabet_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(alphabet_quad.season.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_quad.season.pred$fitted, col = "yellow", lwd = 2)
lines(alphabet_allrevenue.ts)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Regression Forecast: Pre-Covid Period (2010-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_quad.season.pred$fitted, alphabet_revenue.ts),3)


#-------------------- MODEL 4: TWO-LEVEL FORECASTING MODEL --------------------#


#--- MODEL 4A: REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ----#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
alphabet_trend.seas <- tslm(alphabet_train.ts ~ trend + season)
summary(alphabet_trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
alphabet_trend.seas.res <- alphabet_trend.seas$residuals
alphabet_trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
alphabet_ma.trail.res_2 <- rollmean(alphabet_trend.seas.res, k = 2, align = "right")
alphabet_ma.trail.res_2
alphabet_ma.trail.res_3 <- rollmean(alphabet_trend.seas.res, k = 3, align = "right")
alphabet_ma.trail.res_3
alphabet_ma.trail.res_4 <- rollmean(alphabet_trend.seas.res, k = 4, align = "right")
alphabet_ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
alphabet_trend.seas.pred <- forecast(alphabet_trend.seas, h = alphabet_nValid, level = 0)
alphabet_trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
alphabet_trend.seas.res.valid <- alphabet_valid.ts - alphabet_trend.seas.pred$mean
alphabet_trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
alphabet_ma.trail.res.pred_2 <- forecast(alphabet_ma.trail.res_2, h = alphabet_nValid, level = 0)
alphabet_ma.trail.res.pred_2
alphabet_ma.trail.res.pred_3 <- forecast(alphabet_ma.trail.res_3, h = alphabet_nValid, level = 0)
alphabet_ma.trail.res.pred_3
alphabet_ma.trail.res.pred_4 <- forecast(alphabet_ma.trail.res_4, h = alphabet_nValid, level = 0)
alphabet_ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
alphabet_fst.2level_2 <- alphabet_trend.seas.pred$mean + alphabet_ma.trail.res.pred_2$mean
alphabet_fst.2level_2
alphabet_fst.2level_3 <- alphabet_trend.seas.pred$mean + alphabet_ma.trail.res.pred_3$mean
alphabet_fst.2level_3
alphabet_fst.2level_4 <- alphabet_trend.seas.pred$mean + alphabet_ma.trail.res.pred_4$mean
alphabet_fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
alphabet_valid_2.df <- round(data.frame(alphabet_valid.ts, alphabet_trend.seas.pred$mean, 
                               alphabet_ma.trail.res.pred_2$mean, 
                               alphabet_fst.2level_2), 3)
names(alphabet_valid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
alphabet_valid_2.df

alphabet_valid_3.df <- round(data.frame(alphabet_valid.ts, alphabet_trend.seas.pred$mean, 
                               alphabet_ma.trail.res.pred_3$mean, 
                               alphabet_fst.2level_3), 3)
names(alphabet_valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
alphabet_valid_3.df

alphabet_valid_4.df <- round(data.frame(alphabet_valid.ts, alphabet_trend.seas.pred$mean, 
                               alphabet_ma.trail.res.pred_4$mean, 
                               alphabet_fst.2level_4), 3)
names(alphabet_valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
alphabet_valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(alphabet_fst.2level_2, alphabet_valid.ts), 3)
round(accuracy(alphabet_fst.2level_3, alphabet_valid.ts), 3)
round(accuracy(alphabet_fst.2level_4, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
alphabet_tot.trend.seas <- tslm(alphabet_revenue.ts ~ trend  + season)
summary(alphabet_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
alphabet_tot.trend.seas.pred <- forecast(alphabet_tot.trend.seas, h = 16, level = 0)
alphabet_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
alphabet_tot.trend.seas.res <- alphabet_tot.trend.seas$residuals
alphabet_tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
alphabet_tot.ma.trail.res_2 <- rollmean(alphabet_tot.trend.seas.res, k = 2, align = "right")
alphabet_tot.ma.trail.res_2
alphabet_tot.ma.trail.res_3 <- rollmean(alphabet_tot.trend.seas.res, k = 3, align = "right")
alphabet_tot.ma.trail.res_3
alphabet_tot.ma.trail.res_4 <- rollmean(alphabet_tot.trend.seas.res, k = 4, align = "right")
alphabet_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
alphabet_tot.ma.trail.res_2.pred <- forecast(alphabet_tot.ma.trail.res_2, h = 16, level = 0)
alphabet_tot.ma.trail.res_2.pred
alphabet_tot.ma.trail.res_3.pred <- forecast(alphabet_tot.ma.trail.res_3, h = 16, level = 0)
alphabet_tot.ma.trail.res_3.pred
alphabet_tot.ma.trail.res_4.pred <- forecast(alphabet_tot.ma.trail.res_4, h = 16, level = 0)
alphabet_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
alphabet_tot.fst.2level_2 <- alphabet_tot.trend.seas.pred$mean + alphabet_tot.ma.trail.res_2.pred$mean
alphabet_tot.fst.2level_2
alphabet_tot.fst.2level_3 <- alphabet_tot.trend.seas.pred$mean + alphabet_tot.ma.trail.res_3.pred$mean
alphabet_tot.fst.2level_3
alphabet_tot.fst.2level_4 <- alphabet_tot.trend.seas.pred$mean + alphabet_tot.ma.trail.res_4.pred$mean
alphabet_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
alphabet_future_2.df <- round(data.frame(alphabet_tot.trend.seas.pred$mean, alphabet_tot.ma.trail.res_2.pred$mean, 
                                alphabet_tot.fst.2level_2), 3)
names(alphabet_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_future_2.df

alphabet_future_3.df <- round(data.frame(alphabet_tot.trend.seas.pred$mean, alphabet_tot.ma.trail.res_3.pred$mean, 
                                alphabet_tot.fst.2level_3), 3)
names(alphabet_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_future_3.df

alphabet_future_4.df <- round(data.frame(alphabet_tot.trend.seas.pred$mean, alphabet_tot.ma.trail.res_4.pred$mean, 
                                alphabet_tot.fst.2level_4), 3)
names(alphabet_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(alphabet_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                             "Regression Forecast: Pre-Covid Period (2010-2019)",
                             "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,100500)) # FOR VALIDATION DATASET
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(alphabet_tot.trend.seas.res, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(-5000, 20000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(alphabet_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(alphabet_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(alphabet_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(alphabet_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(alphabet_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 19500, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-5000,20000)) 
text(2015,20000, "PRE-COVID", col = "blue")
text(2022.2, 20000, "COVID & POST-COVID", col ="green")
arrows(2010.1,19550,2019.9, 19550,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,19550,2024.1, 19550,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     ylim = c(6000, 100000), bty = "l", xlim = c(2010, 2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL MODEL WITH LINEAR TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS")
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_2, 
                  lwd=3, col = "yellow")
lines(alphabet_tot.fst.2level_2, col = "green", lwd = 3)
legend(2010,85000, legend = c("Alpahbet Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2010-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_2, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_3, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_4, alphabet_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
alphabet_quad.seas <- tslm(alphabet_train.ts ~ trend + I(trend^2) + season)
summary(alphabet_quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
alphabet_quad.seas.res <- alphabet_quad.seas$residuals
alphabet_quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
alphabet_ma.trail.qres_2 <- rollmean(alphabet_quad.seas.res, k = 2, align = "right")
alphabet_ma.trail.qres_2
alphabet_ma.trail.qres_3 <- rollmean(alphabet_quad.seas.res, k = 3, align = "right")
alphabet_ma.trail.qres_3
alphabet_ma.trail.qres_4 <- rollmean(alphabet_quad.seas.res, k = 4, align = "right")
alphabet_ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
alphabet_quad.seas.pred <- forecast(alphabet_quad.seas, h = alphabet_nValid, level = 0)
alphabet_quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
alphabet_quad.seas.res.valid <- alphabet_valid.ts - alphabet_quad.seas.pred$mean
alphabet_quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
alphabet_ma.trail.qres.pred_2 <- forecast(alphabet_ma.trail.qres_2, h = alphabet_nValid, level = 0)
alphabet_ma.trail.qres.pred_2
alphabet_ma.trail.qres.pred_3 <- forecast(alphabet_ma.trail.qres_3, h = alphabet_nValid, level = 0)
alphabet_ma.trail.qres.pred_3
alphabet_ma.trail.qres.pred_4 <- forecast(alphabet_ma.trail.qres_4, h = alphabet_nValid, level = 0)
alphabet_ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
alphabet_qfst.2level_2 <- alphabet_quad.seas.pred$mean + alphabet_ma.trail.qres.pred_2$mean
alphabet_qfst.2level_2
alphabet_qfst.2level_3 <- alphabet_quad.seas.pred$mean + alphabet_ma.trail.qres.pred_3$mean
alphabet_qfst.2level_3
alphabet_qfst.2level_4 <- alphabet_quad.seas.pred$mean + alphabet_ma.trail.qres.pred_4$mean
alphabet_qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
alphabet_qvalid_2.df <- round(data.frame(alphabet_valid.ts, alphabet_quad.seas.pred$mean, 
                                alphabet_ma.trail.qres.pred_2$mean, 
                                alphabet_qfst.2level_2), 3)
names(alphabet_qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
alphabet_qvalid_2.df

alphabet_qvalid_3.df <- round(data.frame(alphabet_valid.ts, alphabet_quad.seas.pred$mean, 
                                alphabet_ma.trail.qres.pred_3$mean, 
                                alphabet_qfst.2level_3), 3)
names(alphabet_qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
alphabet_qvalid_3.df

alphabet_qvalid_4.df <- round(data.frame(alphabet_valid.ts, alphabet_quad.seas.pred$mean, 
                                alphabet_ma.trail.qres.pred_4$mean, 
                                alphabet_qfst.2level_4), 3)
names(alphabet_qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                        "MA.Residuals.Fst", "Combined.Fst")
alphabet_qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(alphabet_qfst.2level_2, alphabet_valid.ts), 3)
round(accuracy(alphabet_qfst.2level_3, alphabet_valid.ts), 3)
round(accuracy(alphabet_qfst.2level_4, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
alphabet_tot.quad.seas <- tslm(alphabet_revenue.ts ~ trend + I(trend^2) + season)
summary(alphabet_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
alphabet_tot.quad.seas.pred <- forecast(alphabet_tot.quad.seas, h = 16, level = 0)
alphabet_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
alphabet_tot.quad.seas.res <- alphabet_tot.quad.seas$residuals
alphabet_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
alphabet_quad.ma.trail.res_2 <- rollmean(alphabet_tot.quad.seas.res, k = 2, align = "right")
alphabet_quad.ma.trail.res_2
alphabet_quad.ma.trail.res_3 <- rollmean(alphabet_tot.quad.seas.res, k = 3, align = "right")
alphabet_quad.ma.trail.res_3
alphabet_quad.ma.trail.res_4 <- rollmean(alphabet_tot.quad.seas.res, k = 4, align = "right")
alphabet_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
alphabet_quad.ma.trail.res_2.pred <- forecast(alphabet_quad.ma.trail.res_2, h = 16, level = 0)
alphabet_quad.ma.trail.res_2.pred$mean
alphabet_quad.ma.trail.res_3.pred <- forecast(alphabet_quad.ma.trail.res_3, h = 16, level = 0)
alphabet_quad.ma.trail.res_3.pred$mean
alphabet_quad.ma.trail.res_4.pred <- forecast(alphabet_quad.ma.trail.res_4, h = 16, level = 0)
alphabet_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
alphabet_quad.fst.2level_2 <- alphabet_tot.quad.seas.pred$mean + alphabet_quad.ma.trail.res_2.pred$mean
alphabet_quad.fst.2level_2
alphabet_quad.fst.2level_3 <- alphabet_tot.quad.seas.pred$mean + alphabet_quad.ma.trail.res_3.pred$mean
alphabet_quad.fst.2level_3
alphabet_quad.fst.2level_4 <- alphabet_tot.quad.seas.pred$mean + alphabet_quad.ma.trail.res_4.pred$mean
alphabet_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
alphabet_futureq_2.df <- round(data.frame(alphabet_tot.quad.seas.pred$mean, alphabet_quad.ma.trail.res_2.pred$mean, 
                                 alphabet_quad.fst.2level_2), 3)
names(alphabet_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_futureq_2.df

alphabet_futureq_3.df <- round(data.frame(alphabet_tot.quad.seas.pred$mean, alphabet_quad.ma.trail.res_3.pred$mean, 
                                 alphabet_quad.fst.2level_3), 3)
names(alphabet_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_futureq_3.df

alphabet_futureq_4.df <- round(data.frame(alphabet_tot.quad.seas.pred$mean, alphabet_quad.ma.trail.res_4.pred$mean, 
                                 alphabet_quad.fst.2level_4), 3)
names(alphabet_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
alphabet_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), lwd =1, xaxt = "n",
     main = "MODEL 4B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(alphabet_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                             "Regression Forecast: Pre-Covid Period (2010-2019)",
                             "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,100500)) 
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(alphabet_tot.quad.seas.res, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(-5000, 6000), 
     bty = "l", xaxt = "n", xlim = c(2010, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(alphabet_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(alphabet_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(alphabet_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(alphabet_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(alphabet_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2010, 5800, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-5000,6000)) 
text(2018.5,6000, "PRE-COVID", col = "blue")
text(2022.2, 6000, "COVID & POST-COVID", col ="green")
arrows(2010.1,5600,2019.9, 5600,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,5600,2024.1, 5600,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     ylim = c(6000, 100000), bty = "l", xlim = c(2010, 2024), 
     lwd =2, xaxt = "n",
     main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC TREND 
     AND SEASONALITY REGRESSION AND TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2010, 2024,1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(alphabet_quad.fst.2level_2, col = "green", lwd = 3)
legend(2010,85000, legend = c("Alpahbet Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2010-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"),
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_2, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_3, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_4, alphabet_revenue.ts), 3)


#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
alphabet_hw.ZZZ <- ets(alphabet_train.ts, model = "ZZZ")
alphabet_hw.ZZZ 
# MODEL : (M, A, N); alpha = 0.076, beta = 0.0716

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
alphabet_hw.ZZZ.pred <- forecast(alphabet_hw.ZZZ, h = alphabet_nValid, level = 0)
alphabet_hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_hw.ZZZ.pred$mean, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
alphabet_HW.ZZZ <- ets(alphabet_revenue.ts, model = "MMM")
alphabet_HW.ZZZ 
# MODEL : (M, M, M); alpha = 0.2984, beta = 0.0918, gamma = 0.0004

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
alphabet_HW.ZZZ.pred <- forecast(alphabet_HW.ZZZ, h = 16 , level = 0)
alphabet_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(alphabet_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 5 : HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 3) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_HW.ZZZ.pred$fitted, col = "yellow", lwd = 3)
lines(alphabet_allrevenue.ts, lwd = 2)
legend(2010,85000, 
       legend = c("Revenue (2010-2023)", 
                  "Holt-Winter's Forecast: Pre-Covid Period (2010-2019)",
                  "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100500, "PRE-COVID", col = "blue")
text(2022.2, 100500, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_HW.ZZZ.pred$fitted, alphabet_revenue.ts), 3)


#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(alphabet_train.ts, lag.max = 8, main = "Autocorrelation for Alphabet's Revenue Training Data Set")
Acf(alphabet_valid.ts, lag.max = 8, main = "Autocorrelation for Alphabet's Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
alphabet_hw.ZZZ <- ets(alphabet_train.ts, model = "ZZZ")
alphabet_hw.ZZZ 
# MODEL : (M, A, N); alpha = 0.076, beta = 0.0716

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
alphabet_hw.ZZZ.pred <- forecast(alphabet_hw.ZZZ, h = alphabet_nValid, level = 0)
alphabet_hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
alphabet_train.residuals <- alphabet_hw.ZZZ.pred$residuals
alphabet_train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(alphabet_train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Alphabet's Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
alphabet_res.ar1 <- Arima(alphabet_hw.ZZZ$residuals, order = c(1,0,0))
summary(alphabet_res.ar1)

# FORECAST FOR VALIDATION DATA
alphabet_res.ar1.pred <- forecast(alphabet_res.ar1, h = alphabet_nValid, level = 0)
alphabet_res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(alphabet_res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Alphabet's Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
alphabet_valid.two.level.pred <- alphabet_hw.ZZZ.pred$mean + alphabet_res.ar1.pred$mean
alphabet_valid.df <- round(data.frame(alphabet_valid.ts, alphabet_hw.ZZZ.pred$mean, 
                             alphabet_res.ar1.pred$mean, alphabet_valid.two.level.pred),3)
names(alphabet_valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
alphabet_valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_valid.two.level.pred, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
alphabet_HW.ZZZ <- ets(alphabet_revenue.ts, model = "MMM")
alphabet_HW.ZZZ 
# MODEL : (M, M, M); alpha = 0.2984, beta = 0.0918, gamma = 0.0004

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
alphabet_HW.ZZZ.pred <- forecast(alphabet_HW.ZZZ, h = 16 , level = 0)
alphabet_HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
alphabet_residual.ar1 <- Arima(alphabet_HW.ZZZ$residuals, order = c(1,0,0))
alphabet_residual.ar1.pred <- forecast(alphabet_residual.ar1, h = 16, level = 0)
summary(alphabet_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(alphabet_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
alphabet_HW.ZZZ.ar1.pred <- alphabet_HW.ZZZ.pred$mean + alphabet_residual.ar1.pred$mean
alphabet_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
alphabet_table.df <- round(data.frame(alphabet_HW.ZZZ.pred$mean, 
                             alphabet_residual.ar1.pred$mean, alphabet_HW.ZZZ.ar1.pred),3)
names(alphabet_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
alphabet_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", ylim = c(6000, 100000), 
     bty = "l", xlim = c(2010, 2024), xaxt = "n",
     main = "MODEL 6 : TWO LEVEL MODEL (HW'S MODEL + AR(1) MODEL)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_HW.ZZZ$fitted + alphabet_residual.ar1$fitted, col = "yellow", lwd = 3)
lines(alphabet_HW.ZZZ.ar1.pred, col = "green", lwd = 3)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "Two-Level Forecast: Pre-Covid Period (2010-2019)", 
                              "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) 
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_HW.ZZZ$fitted + alphabet_residual.ar1$fitted, alphabet_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
alphabet_train.auto.arima <- auto.arima(alphabet_train.ts)
summary(alphabet_train.auto.arima)

# FORECAST FOR VALIDATION DATA
alphabet_train.auto.arima.pred <- forecast(alphabet_train.auto.arima, h = alphabet_nValid, level = 0)
alphabet_train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(alphabet_train.auto.arima.pred$mean, alphabet_valid.ts), 3)

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
alphabet_revenue.auto.arima <- arima(alphabet_revenue.ts,
                  order = c(1,1,2), 
                  seasonal = c(2,1,2), 
                  method="ML")
summary(alphabet_revenue.auto.arima)

# FORECAST FOR POST-COVID PERIOD
alphabet_revenue.auto.arima.pred <- forecast(alphabet_revenue.auto.arima, h = 16, level = 0)
alphabet_revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR AUTO-ARIMA MODEL
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     ylim = c(6000, 100000), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 7: AUTO-ARIMA MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet_revenue.auto.arima.pred$fitted, col = "yellow", lwd = 3)
lines(alphabet_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2010,85000, legend = c("Revenue (2010-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2010-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"),  
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100000)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,95000,2019.9, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,95000,2024.1, 95000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(alphabet_revenue.auto.arima.pred$fitted, alphabet_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON alphabet_revenue.ts (2010-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(alphabet_revenue.ts))$fitted, alphabet_revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(alphabet_revenue.ts))$fitted, alphabet_revenue.ts), 3)

#--------------- MODEL 3A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(alphabet_lin.trend.pred$fitted, alphabet_revenue.ts),3)

#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(alphabet_quad.trend.pred$fitted, alphabet_revenue.ts),3)

#---------------- MODEL 3C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(alphabet_revenue.season.pred$fitted, alphabet_revenue.ts),3)

#-------- MODEL 3D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(alphabet_lin.season.pred$fitted, alphabet_revenue.ts),3)

#------- MODEL 3E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(alphabet_quad.season.pred$fitted, alphabet_revenue.ts),3)

#--- MODEL 4A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_2, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_3, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.trend.seas.pred$fitted + alphabet_tot.ma.trail.res_4, alphabet_revenue.ts), 3)

#--MODEL 4B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_2, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_3, alphabet_revenue.ts), 3)
round(accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_4, alphabet_revenue.ts), 3)

#------------------ MODEL 5 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(alphabet_HW.ZZZ.pred$fitted, alphabet_revenue.ts), 3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(alphabet_HW.ZZZ$fitted + alphabet_residual.ar1$fitted, alphabet_revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(alphabet_revenue.auto.arima.pred$fitted, alphabet_revenue.ts), 3)


#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
alphabet_accuracy_model_1 <- accuracy(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_2, alphabet_revenue.ts)
alphabet_accuracy_model_2 <- accuracy(alphabet_HW.ZZZ.pred$fitted, alphabet_revenue.ts)
alphabet_accuracy_model_3 <- accuracy(alphabet_revenue.auto.arima.pred$fitted, alphabet_revenue.ts)

# Extract RMSE and MAPE for each model
alphabet_rmse_model_1 <- alphabet_accuracy_model_1[1, "RMSE"]
alphabet_mape_model_1 <- alphabet_accuracy_model_1[1, "MAPE"]

alphabet_rmse_model_2 <- alphabet_accuracy_model_2[1, "RMSE"]
alphabet_mape_model_2 <- alphabet_accuracy_model_2[1, "MAPE"]

alphabet_rmse_model_3 <- alphabet_accuracy_model_3[1, "RMSE"]
alphabet_mape_model_3 <- alphabet_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
alphabet_inv_rmse_model_1 <- 1 / alphabet_rmse_model_1
alphabet_inv_mape_model_1 <- 1 / alphabet_mape_model_1

alphabet_inv_rmse_model_2 <- 1 / alphabet_rmse_model_2
alphabet_inv_mape_model_2 <- 1 / alphabet_mape_model_2

alphabet_inv_rmse_model_3 <- 1 / alphabet_rmse_model_3
alphabet_inv_mape_model_3 <- 1 / alphabet_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
alphabet_total_inv_rmse <- alphabet_inv_rmse_model_1 + alphabet_inv_rmse_model_2 + alphabet_inv_rmse_model_3
alphabet_total_inv_mape <- alphabet_inv_mape_model_1 + alphabet_inv_mape_model_2 + alphabet_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
alphabet_inv_rmse_weight_1 <- alphabet_inv_rmse_model_1 / alphabet_total_inv_rmse
alphabet_inv_rmse_weight_2 <- alphabet_inv_rmse_model_2 / alphabet_total_inv_rmse
alphabet_inv_rmse_weight_3 <- alphabet_inv_rmse_model_3 / alphabet_total_inv_rmse

alphabet_inv_mape_weight_1 <- alphabet_inv_mape_model_1 / alphabet_total_inv_mape
alphabet_inv_mape_weight_2 <- alphabet_inv_mape_model_2 / alphabet_total_inv_mape
alphabet_inv_mape_weight_3 <- alphabet_inv_mape_model_3 / alphabet_total_inv_mape

# Calculate the total weight for each model
alphabet_total_weight_1 <- (alphabet_inv_rmse_weight_1 + alphabet_inv_mape_weight_1) / 2
alphabet_total_weight_2 <- (alphabet_inv_rmse_weight_2 + alphabet_inv_mape_weight_2) / 2
alphabet_total_weight_3 <- (alphabet_inv_rmse_weight_3 + alphabet_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", alphabet_rmse_model_1, "MAPE:", alphabet_mape_model_1, 
    "Inverse RMSE:", alphabet_inv_rmse_model_1, "Inverse MAPE:", alphabet_inv_mape_model_1, 
    "Inv. RMSE Weight:", alphabet_inv_rmse_weight_1, "Inv. MAPE Weight:", alphabet_inv_mape_weight_1, 
    "Total Weight:", alphabet_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", alphabet_rmse_model_2, "MAPE:", alphabet_mape_model_2, 
    "Inverse RMSE:", alphabet_inv_rmse_model_2, "Inverse MAPE:", alphabet_inv_mape_model_2, 
    "Inv. RMSE Weight:", alphabet_inv_rmse_weight_2, "Inv. MAPE Weight:", alphabet_inv_mape_weight_2, 
    "Total Weight:", alphabet_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", alphabet_rmse_model_3, "MAPE:", alphabet_mape_model_3, 
    "Inverse RMSE:", alphabet_inv_rmse_model_3, "Inverse MAPE:", alphabet_inv_mape_model_3, 
    "Inv. RMSE Weight:", alphabet_inv_rmse_weight_3, "Inv. MAPE Weight:", alphabet_inv_mape_weight_3, 
    "Total Weight:", alphabet_total_weight_3, "\n\n")

# Calculate the ensemble model weights
alphabet_total_inv_rmse_all <- alphabet_inv_rmse_model_1 + alphabet_inv_rmse_model_2 + alphabet_inv_rmse_model_3
alphabet_total_inv_mape_all <- alphabet_inv_mape_model_1 + alphabet_inv_mape_model_2 + alphabet_inv_mape_model_3

alphabet_ensemble_inv_rmse_weight <- alphabet_total_inv_rmse_all / alphabet_total_inv_rmse_all
alphabet_ensemble_inv_mape_weight <- alphabet_total_inv_mape_all / alphabet_total_inv_mape_all

# Create a dataframe-like structure
alphabet_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(alphabet_rmse_model_1, alphabet_rmse_model_2, alphabet_rmse_model_3, ""),
  MAPE = c(alphabet_mape_model_1, alphabet_mape_model_2, alphabet_mape_model_3, ""),
  `Inverse RMSE` = c(alphabet_inv_rmse_model_1, alphabet_inv_rmse_model_2, alphabet_inv_rmse_model_3, alphabet_total_inv_rmse_all),
  `Inverse MAPE` = c(alphabet_inv_mape_model_1, alphabet_inv_mape_model_2, alphabet_inv_mape_model_3, alphabet_total_inv_mape_all),
  `RMSE Weight` = c(alphabet_inv_rmse_weight_1, alphabet_inv_rmse_weight_2, alphabet_inv_rmse_weight_3, alphabet_ensemble_inv_rmse_weight),
  `MAPE Weight` = c(alphabet_inv_mape_weight_1, alphabet_inv_mape_weight_2, alphabet_inv_mape_weight_3, alphabet_ensemble_inv_mape_weight),
  `Total Weight` = c(alphabet_total_weight_1, alphabet_total_weight_2, alphabet_total_weight_3, 1)
)


# Print the dataframe.
print(alphabet_results, right = F)


#--------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)--------------#

# Create ensemble forecast for pre-COVID period.
alphabet.ensemble.pre_covid <-( 
  (alphabet_total_weight_1*(alphabet_tot.quad.seas.pred$fitted + alphabet_quad.ma.trail.res_2))
  + (alphabet_total_weight_2*alphabet_HW.ZZZ.pred$fitted)
  + (alphabet_total_weight_3*alphabet_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
alphabet.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(alphabet.ensemble.pre_covid, alphabet_revenue.ts), 3)

#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Create ensemble forecast for COVID & post-COVID periods.
alphabet.ensemble.covid <-( 
  (alphabet_total_weight_1*alphabet_quad.fst.2level_2)
  + (alphabet_total_weight_2*alphabet_HW.ZZZ.pred$mean)
  + (alphabet_total_weight_3*alphabet_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
alphabet.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(alphabet_allrevenue.ts, 
     xlab = "Time", ylab = "Alphabet's Revenue (in Million $)", 
     ylim = c(6000, 100500), bty = "l", xlim = c(2010, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(alphabet.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(alphabet.ensemble.covid, col = "green", lwd = 3)
legend(2010,88000, legend = c("Revenue (2010-2023)", 
                               "Ensemble Forecast: Pre-Covid Period (2010-2019)", 
                               "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINE# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(0,100500)) # FOR PRE-COVID DATA
text(2015,100000, "PRE-COVID", col = "blue")
text(2022.2, 100000, "COVID & POST-COVID", col ="green")
arrows(2010.1,97000,2019.9, 97000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,97000,2024.1, 97000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#--------------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 

alphabet.revenue.change <- alphabet_future.ts - alphabet.ensemble.covid

alphabet_revenue_covid.df <- round(data.frame(
  alphabet_future.ts, 
  alphabet.ensemble.covid, 
  alphabet.revenue.change), 3)
names(alphabet_revenue_covid.df) <- c(
  "Actual_Revenue", 
  "Ensemble_Fst", 
  "Difference")
alphabet_revenue_covid.df

library("writexl")
write_xlsx(alphabet_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_alphabet_20_23_actual_fst.xlsx")

