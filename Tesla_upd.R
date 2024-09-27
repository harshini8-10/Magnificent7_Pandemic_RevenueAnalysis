# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)


#------------------------------------- Tesla ----------------------------------#

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files")


# CREATE DATAFRAME
Tesla.data <- read.csv("TeslaRevenue_upd.csv")

# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# tesla_allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2014-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023).

tesla_allrevenue.ts <- ts(Tesla.data$Tesla_Revenue, start = c(2014,1), end = c(2023,4), freq = 4)
tesla_allrevenue.ts

# tesla_revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
tesla_revenue.ts <- ts(Tesla.data$Tesla_Revenue, start = c(2014,1),end = c(2019,4), freq = 4)
tesla_revenue.ts

tesla_future.ts <- window(tesla_allrevenue.ts, start = c(2020,1), end = c(2023,4), freq = 4)
tesla_future.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2010 TO 2023 USING plot() FUNCTION
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Millions of Dollars)", 
     ylim = c(0, 27000), bty = "l",
     xaxt = "n", xlim = c(2014, 2025.25), 
     main = "Tesla Revenue Data (2014-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2014 TO 2023
tesla_allrevenue.stl <- stl(tesla_allrevenue.ts, s.window = "periodic")
autoplot(tesla_allrevenue.stl, main = "Tesla Revenue - Time Series Components (2014-2023)") 

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (ALL PERIODS 2010 - 2023)
tesla_allautocor <- Acf(tesla_allrevenue.ts, lag.max = 4, 
                            main = "Autocorrelation Chart for Tesla")

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID: 2014-2019)
tesla_autocor <- Acf(tesla_revenue.ts, lag.max = 8, 
    main = "Autocorrelation Chart for Tesla Revenue Data (Pre-Covid: 2014-2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
tesla_lag <- round(tesla_autocor$lag,0)
tesla_ACF <- round(tesla_autocor$acf,3)
data.frame(tesla_lag,tesla_ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR APPLE'S REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
tesla_revenue.ar1 <- Arima(tesla_revenue.ts, order = c(1,0,0),method = "ML")
summary(tesla_revenue.ar1)

# The autoregressive (AR) component of the model is non-stationary. 
# This implies that the relationships between the observations 
# are changing over time, which violates a key assumption of ARIMA models.
# To overcome this issue in Arima() function, apply 'method = "ML"'.   


# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1.
ar1 <- 0.9383
s.e. <- 0.0650
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
diff.tesla_revenue.ts <- diff(tesla_revenue.ts, lag = 1)
diff.tesla_revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED APPLE REVENUE
Acf(diff.tesla_revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Tesla Revenue Data")



# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1: NAIVE MODEL ---------------------------#


#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# NAIVE FORECAST FOR POST-COVID PERIOD
tesla_revenuef.naive.pred <- naive(tesla_revenue.ts, h = 16)
tesla_revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(tesla_revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(20, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 1: NAIVE FORECAST", col = "green", lwd =3) 
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_revenuef.naive.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, col = "black", lwd = 2)
legend(2014,25000, legend = c("Revenue (2014-2023)", 
                              "Naive Forecast: Pre-Covid Period (2014-2019)",
                              "Naive Forecast: Covid & Post-Covid (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")


# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(tesla_revenue.ts))$fitted, tesla_revenue.ts), 3)


#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# SEASONAL NAIVE FORECAST FOR POST-COVID PERIOD 
tesla_revenuef.snaive.pred <- snaive(tesla_revenue.ts, h = 16)
tesla_revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(tesla_revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(-1000, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 2: SEASONAL NAIVE FORECAST", col = "green", lwd = 3) 
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_revenuef.snaive.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, col = "black", lwd = 2)
legend(2014,23000, legend = c("Revenue (2010-2023)", 
                               "Seasonal Naive Forecast: Pre-Covid Period (2014-2019)",
                               "Seasonal Naive Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(tesla_revenue.ts))$fitted, tesla_revenue.ts), 3)

#------------------------- MODEL 3: REGRESSION MODELS ------------------------#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

#--------------- MODEL 3A: REGRESSION MODEL WITH LINEAR TREND ----------------#

tesla_lin.trend <- tslm(tesla_revenue.ts ~ trend)
summary(tesla_lin.trend)

# FORECAST FOR POST-COVID PERIOD
tesla_lin.trend.pred <- forecast(tesla_lin.trend, h = 16, level = 0)
tesla_lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(tesla_lin.trend.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 3A: REGRESSION MODEL WITH LINEAR TREND", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_lin.trend.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,23000, legend = c("Revenue (2014-2023)", 
                              "Regression Forecast: Pre-Covid Period (2014-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_lin.trend.pred$fitted, tesla_revenue.ts),3)


#-------------- MODEL 3B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

tesla_quad.trend <- tslm(tesla_revenue.ts ~ trend + I(trend^2))
summary(tesla_quad.trend)

# FORECAST FOR POST-COVID PERIOD
tesla_quad.trend.pred <- forecast(tesla_quad.trend, h = 16, level = 0)
tesla_quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(tesla_quad.trend.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", 
     ylim = c(0, 27000), bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 3B: REGRESSION MODEL WITH QUADRATIC TREND", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_quad.trend.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,23000, legend = c("Revenue (2014-2023)", 
                              "Regression Forecast: Pre-Covid Period (2014-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_quad.trend.pred$fitted, tesla_revenue.ts),3)


#---------------- MODEL 3C: REGRESSION MODEL WITH SEASONALITY ----------------#

tesla_revenue.season <- tslm(tesla_revenue.ts ~ season)
summary(tesla_revenue.season)

# FORECAST FOR POST-COVID PERIOD
tesla_revenue.season.pred <- forecast(tesla_revenue.season, h = 16, level = 0)
tesla_revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(tesla_revenue.season.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 3C: REGRESSION MODEL WITH SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_revenue.season.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,23000, legend = c("Revenue (2014-2023)", 
                              "Regression Forecast: Pre-Covid Period (2014-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_revenue.season.pred$fitted, tesla_revenue.ts),3)


#-------- MODEL 3D: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

tesla_lin.season <- tslm(tesla_revenue.ts ~ trend + season)
summary(tesla_lin.season)

# FORECAST FOR POST-COVID PERIOD
tesla_lin.season.pred <- forecast(tesla_lin.season, h = 16, level = 0)
tesla_lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
plot(tesla_lin.season.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 3D: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_lin.season.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,23000, legend = c("Revenue (2014-2023)", 
                              "Regression Forecast: Pre-Covid Period (2014-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_lin.season.pred$fitted, tesla_revenue.ts),3)


#------- MODEL 3E: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

tesla_quad.season <- tslm(tesla_revenue.ts ~ trend + I(trend^2) + season)
summary(tesla_quad.season)

# FORECAST FOR POST-COVID PERIOD
tesla_quad.season.pred <- forecast(tesla_quad.season, h = 16, level = 0)
tesla_quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(tesla_quad.season.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 3E: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 3)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_quad.season.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,23000, legend = c("Revenue (2014-2023)", 
                              "Regression Forecast: Pre-Covid Period (2014-2019)",
                              "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_quad.season.pred$fitted, tesla_revenue.ts),3)

  
#-------------------- MODEL 4: TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 4A: REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
tesla_tot.trend.seas <- tslm(tesla_revenue.ts ~ trend  + season)
summary(tesla_tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
tesla_tot.trend.seas.pred <- forecast(tesla_tot.trend.seas, h = 16, level = 0)
tesla_tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
tesla_tot.trend.seas.res <- tesla_tot.trend.seas$residuals
tesla_tot.trend.seas.res

# LEVEL 2: TRAILING MA MODEL TO FORECAST RESIDUALS
tesla_tot.ma.trail.res_2 <- rollmean(tesla_tot.trend.seas.res, k = 2, align = "right")
tesla_tot.ma.trail.res_2
tesla_tot.ma.trail.res_3 <- rollmean(tesla_tot.trend.seas.res, k = 3, align = "right")
tesla_tot.ma.trail.res_3
tesla_tot.ma.trail.res_4 <- rollmean(tesla_tot.trend.seas.res, k = 4, align = "right")
tesla_tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
tesla_tot.ma.trail.res_2.pred <- forecast(tesla_tot.ma.trail.res_2, h = 16, level = 0)
tesla_tot.ma.trail.res_2.pred
tesla_tot.ma.trail.res_3.pred <- forecast(tesla_tot.ma.trail.res_3, h = 16, level = 0)
tesla_tot.ma.trail.res_3.pred
tesla_tot.ma.trail.res_4.pred <- forecast(tesla_tot.ma.trail.res_4, h = 16, level = 0)
tesla_tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS 
tesla_tot.fst.2level_2 <- tesla_tot.trend.seas.pred$mean + tesla_tot.ma.trail.res_2.pred$mean
tesla_tot.fst.2level_2
tesla_tot.fst.2level_3 <- tesla_tot.trend.seas.pred$mean + tesla_tot.ma.trail.res_3.pred$mean
tesla_tot.fst.2level_3
tesla_tot.fst.2level_4 <- tesla_tot.trend.seas.pred$mean + tesla_tot.ma.trail.res_4.pred$mean
tesla_tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
tesla_future_2.df <- round(data.frame(tesla_tot.trend.seas.pred$mean, tesla_tot.ma.trail.res_2.pred$mean, 
                                tesla_tot.fst.2level_2), 3)
names(tesla_future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_future_2.df

tesla_future_3.df <- round(data.frame(tesla_tot.trend.seas.pred$mean, tesla_tot.ma.trail.res_3.pred$mean, 
                                tesla_tot.fst.2level_3), 3)
names(tesla_future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_future_3.df

tesla_future_4.df <- round(data.frame(tesla_tot.trend.seas.pred$mean, tesla_tot.ma.trail.res_4.pred$mean, 
                                tesla_tot.fst.2level_4), 3)
names(tesla_future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(20, 27000), 
     bty = "l", xlim = c(2014, 2024), lwd =2, xaxt = "n",
     main = "MODEL 4A: LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2014, 2024,1), labels = format(seq(2014, 2024, 1)))
lines(tesla_tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(tesla_tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2009,115000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(tesla_tot.trend.seas.res, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(-2000, 3000), 
     bty = "l", xaxt = "n", xlim = c(2014, 2024), lwd =2, col = "brown", 
     main = "MODEL 4A: LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(tesla_tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(tesla_tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(tesla_tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(tesla_tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(tesla_tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(tesla_tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2014, 2900, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-2000,3000)) # FOR VALIDATION DATASET
text(2017,3000, "PRE-COVID", col = "blue")
text(2022.2, 3000, "COVID & POST-COVID", col ="green")
arrows(2014.1,2800,2019.9, 2800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2800,2023.9, 2800,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (LINEAR TREND AND SEASONALITY, AND MA FOR RESIDUALS, k=2) 
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), lwd =2, xaxt = "n",
     main = "MODEL 4A: TWO-LEVEL FORECAST WITH LINEAR REGRESSION & TRAILING MA RESIDUALS") 
axis(1, at = seq(2014, 2024,1), labels = format(seq(2014, 2024, 1)))
lines(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(tesla_tot.fst.2level_2, col = "green", lwd = 3)
legend(2014,25000, legend = c("Tesla Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2014-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-2000,27000)) # FOR PRE-COVID DATA
text(2017,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_2, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_3, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_4, tesla_revenue.ts), 3)

#--MODEL 4B: REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
tesla_tot.quad.seas <- tslm(tesla_revenue.ts ~ trend + I(trend^2) + season)
summary(tesla_tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR POST-COVID PERIOD
tesla_tot.quad.seas.pred <- forecast(tesla_tot.quad.seas, h = 16, level = 0)
tesla_tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
tesla_tot.quad.seas.res <- tesla_tot.quad.seas$residuals
tesla_tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
tesla_quad.ma.trail.res_2 <- rollmean(tesla_tot.quad.seas.res, k = 2, align = "right")
tesla_quad.ma.trail.res_2
tesla_quad.ma.trail.res_3 <- rollmean(tesla_tot.quad.seas.res, k = 3, align = "right")
tesla_quad.ma.trail.res_3
tesla_quad.ma.trail.res_4 <- rollmean(tesla_tot.quad.seas.res, k = 4, align = "right")
tesla_quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR POST-COVID PERIOD
tesla_quad.ma.trail.res_2.pred <- forecast(tesla_quad.ma.trail.res_2, h = 16, level = 0)
tesla_quad.ma.trail.res_2.pred$mean
tesla_quad.ma.trail.res_3.pred <- forecast(tesla_quad.ma.trail.res_3, h = 16, level = 0)
tesla_quad.ma.trail.res_3.pred$mean
tesla_quad.ma.trail.res_4.pred <- forecast(tesla_quad.ma.trail.res_4, h = 16, level = 0)
tesla_quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR POST-COVID PERIODS
tesla_quad.fst.2level_2 <- tesla_tot.quad.seas.pred$mean + tesla_quad.ma.trail.res_2.pred$mean
tesla_quad.fst.2level_2
tesla_quad.fst.2level_3 <- tesla_tot.quad.seas.pred$mean + tesla_quad.ma.trail.res_3.pred$mean
tesla_quad.fst.2level_3
tesla_quad.fst.2level_4 <- tesla_tot.quad.seas.pred$mean + tesla_quad.ma.trail.res_4.pred$mean
tesla_quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIODS
tesla_futureq_2.df <- round(data.frame(tesla_tot.quad.seas.pred$mean, tesla_quad.ma.trail.res_2.pred$mean, 
                                 tesla_quad.fst.2level_2), 3)
names(tesla_futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_futureq_2.df

tesla_futureq_3.df <- round(data.frame(tesla_tot.quad.seas.pred$mean, tesla_quad.ma.trail.res_3.pred$mean, 
                                 tesla_quad.fst.2level_3), 3)
names(tesla_futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_futureq_3.df

tesla_futureq_4.df <- round(data.frame(tesla_tot.quad.seas.pred$mean, tesla_quad.ma.trail.res_4.pred$mean, 
                                 tesla_quad.fst.2level_4), 3)
names(tesla_futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
tesla_futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(20, 27000), 
     bty = "l", xlim = c(2014, 2024), lwd = 2, xaxt = "n",
     main = "MODEL 4B: LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2014, 2024,1), labels = format(seq(2014, 2024, 1)))
lines(tesla_tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(tesla_tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2014,25000, legend = c("Revenue (2010-2023)", 
                               "Regression Forecast: Pre-Covid Period (2010-2019)",
                               "Regression Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(0,27000)) 
text(2018.5,27000, "PRE-COVID", col = "blue")
text(2022.2, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2024.1, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(tesla_tot.quad.seas.res, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(-1000, 2000), 
     bty = "l", xaxt = "n", xlim = c(2014, 2024), lwd =2, col = "brown", 
     main = "MODEL 4B: LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(tesla_quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(tesla_quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(tesla_quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(tesla_quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(tesla_quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2014, 1900, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE PREDICTION FOR TRAINING, VALIDATION, AND COVID & POST-COVID INTERVALS
lines(c(2020,2020), c(-5000,2000)) 
text(2017,2000, "PRE-COVID", col = "blue")
text(2022.2,2000, "COVID & POST-COVID", col ="green")
arrows(2014.1,1900,2019.9, 1900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,1900,2024.1, 1900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT FOR BEST TWO-LEVEL FORECAST (QUADRATIC TREND AND SEASONALITY, AND TRAILING MA FOR RESIDUALS, k=2) 
plot(tesla_allrevenue.ts, 
    xlab = "Time", ylab = "Tesla's Revenue (in Million $)", 
    ylim = c(20, 27000), bty = "l", xlim = c(2014, 2024), lwd =2, xaxt = "n",
    main = "MODEL 4B: TWO-LEVEL FORECAST WITH QUADRATIC REGRESSION & TRAILING MA RESIDUALS") 
axis(1, at = seq(2014, 2024,1), labels = format(seq(2014, 2024, 1)))
lines(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_2, 
      lwd=3, col = "yellow")
lines(tesla_quad.fst.2level_2, col = "green", lwd = 3)
legend(2014,23000, legend = c("Tesla Revenue (2010-2023)", 
                              "Two-level Forecast: Pre-Covid Period (2010-2019)",
                              "Two-level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_2, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_3, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_4, tesla_revenue.ts), 3)


#------------------ MODEL 5: AUTOMATED HOLT-WINTER'S MODEL ------------------#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
tesla_HW.ZZZ <- ets(tesla_revenue.ts, model = "MAN")
tesla_HW.ZZZ 
# MODEL : (M, A, N); alpha = 0.7582, beta = 0.0616

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
tesla_HW.ZZZ.pred <- forecast(tesla_HW.ZZZ, h = 16 , level = 0)
tesla_HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(tesla_HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 5: HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 3) 
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_HW.ZZZ.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_allrevenue.ts, lwd = 2)
legend(2014,25000, 
       legend = c("Tesla Revenue (2014-2023)", 
       "Holt-Winter's Forecast: Pre-Covid Period (2014-2019)",
       "Holt-Winter's Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_HW.ZZZ.pred$fitted, tesla_revenue.ts), 3)



#-------------- MODEL 6: AUTOCORRELATION & AUTOREGRESSIVE MODEL ---------------#

#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
tesla_HW.ZZZ <- ets(tesla_revenue.ts, model = "MAN")
tesla_HW.ZZZ 
# MODEL : (M, A, N); alpha = 0.7582, beta = 0.0616

# AUTOMATED HW'S MODEL FORECAST FOR POST-COVID PERIOD
tesla_HW.ZZZ.pred <- forecast(tesla_HW.ZZZ, h = 16 , level = 0)
tesla_HW.ZZZ.pred

# LEVEL 2: AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
tesla_residual.ar1 <- Arima(tesla_HW.ZZZ$residuals, order = c(1,0,0))
tesla_residual.ar1.pred <- forecast(tesla_residual.ar1, h = 16, level = 0)
summary(tesla_residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(tesla_residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR POST-COVID PERIOD
tesla_HW.ZZZ.ar1.pred <- tesla_HW.ZZZ.pred$mean + tesla_residual.ar1.pred$mean
tesla_HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR POST-COVID PERIOD
tesla_table.df <- round(data.frame(tesla_HW.ZZZ.pred$mean, 
                             tesla_residual.ar1.pred$mean, tesla_HW.ZZZ.ar1.pred),3)
names(tesla_table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
tesla_table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", ylim = c(0, 27000), 
     bty = "l", xlim = c(2014, 2024), xaxt = "n",
     main = "MODEL 6: TWO-LEVEL MODEL (HW MODEL + AR(1) MODEL FOR RESIDUALS)", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_HW.ZZZ$fitted + tesla_residual.ar1$fitted, 
      col = "yellow", lwd = 3)
lines(tesla_HW.ZZZ.ar1.pred, col = "green", lwd = 3)
legend(2014,25000, legend = c("Revenue (2014-2023)", 
                              "Two-Level Forecast: Pre-Covid Period (2014-2019)", 
                              "Two-Level Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) 
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_HW.ZZZ$fitted + tesla_residual.ar1$fitted, tesla_revenue.ts),3) 


#- MODEL 7: AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#---------------------------- FOR POST-COVID PERIOD ---------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
tesla_revenue.auto.arima <- arima(tesla_revenue.ts, 
                                  order = c(1,1,1), 
                                  seasonal = c(1,1,1), 
                                  method = "ML")
summary(tesla_revenue.auto.arima)


# FORECAST FOR POST-COVID PERIOD
tesla_revenue.auto.arima.pred <- forecast(tesla_revenue.auto.arima, h = 16, level = 0)
tesla_revenue.auto.arima.pred$mean
tesla_revenue.auto.arima.pred$fitted

# PLOT THE PREDICTIONS FOR ARIMA MODEL.
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", 
     ylim = c(0, 27000), bty = "l", xlim = c(2014, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 7: ARIMA MODEL")  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla_revenue.auto.arima.pred$fitted, col = "yellow", lwd = 3)
lines(tesla_revenue.auto.arima.pred$mean, col = "green", lwd = 3)
legend(2014,25000, legend = c("Revenue (2014-2023)", 
                              "ARIMA Forecast: Pre-Covid Period (2014-2019)", 
                              "ARIMA Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


# FORECAST ACCURACY FOR POST-COVID PERIOD
round(accuracy(tesla_revenue.auto.arima.pred$fitted, tesla_revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON tesla_revenue.ts (2010-2019)

#---------------------------- MODEL 1: NAIVE MODEL ---------------------------#
round(accuracy((naive(tesla_revenue.ts))$fitted, tesla_revenue.ts), 3)

#------------------------ MODEL 2: SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(tesla_revenue.ts))$fitted, tesla_revenue.ts), 3)

#--------------- MODEL 3A: REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(tesla_lin.trend.pred$fitted, tesla_revenue.ts),3)

#-------------- MODEL 3B: REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(tesla_quad.trend.pred$fitted, tesla_revenue.ts),3)

#---------------- MODEL 3C: REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(tesla_revenue.season.pred$fitted, tesla_revenue.ts),3)

#-------- MODEL 3D: REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(tesla_lin.season.pred$fitted, tesla_revenue.ts),3)

#------- MODEL 3E: REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(tesla_quad.season.pred$fitted, tesla_revenue.ts),3)

#--- MODEL 4A: REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_2, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_3, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.trend.seas.pred$fitted + tesla_tot.ma.trail.res_4, tesla_revenue.ts), 3)

#--MODEL 4B: REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_2, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_3, tesla_revenue.ts), 3)
round(accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_4, tesla_revenue.ts), 3)

#------------------ MODEL 5: AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(tesla_HW.ZZZ.pred$fitted, tesla_revenue.ts), 3)


#---------------- MODEL 6: AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(tesla_HW.ZZZ$fitted + tesla_residual.ar1$fitted, tesla_revenue.ts),3) 

#- MODEL 7: AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(tesla_revenue.auto.arima.pred$fitted, tesla_revenue.ts), 3)


#--------------------------ENSEMBLE MODEL DEVELOPMENT--------------------------#

# Calculate the accuracy and store in accuracy_results for each model
tesla_accuracy_model_1 <- accuracy(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_2, tesla_revenue.ts)
tesla_accuracy_model_2 <- accuracy(tesla_HW.ZZZ.pred$fitted, tesla_revenue.ts)
tesla_accuracy_model_3 <- accuracy(tesla_revenue.auto.arima.pred$fitted, tesla_revenue.ts)

# Extract RMSE and MAPE for each model
tesla_rmse_model_1 <- tesla_accuracy_model_1[1, "RMSE"]
tesla_mape_model_1 <- tesla_accuracy_model_1[1, "MAPE"]

tesla_rmse_model_2 <- tesla_accuracy_model_2[1, "RMSE"]
tesla_mape_model_2 <- tesla_accuracy_model_2[1, "MAPE"]

tesla_rmse_model_3 <- tesla_accuracy_model_3[1, "RMSE"]
tesla_mape_model_3 <- tesla_accuracy_model_3[1, "MAPE"]

# Calculate the inverse RMSE and MAPE for each model
tesla_inv_rmse_model_1 <- 1 / tesla_rmse_model_1
tesla_inv_mape_model_1 <- 1 / tesla_mape_model_1

tesla_inv_rmse_model_2 <- 1 / tesla_rmse_model_2
tesla_inv_mape_model_2 <- 1 / tesla_mape_model_2

tesla_inv_rmse_model_3 <- 1 / tesla_rmse_model_3
tesla_inv_mape_model_3 <- 1 / tesla_mape_model_3

# Calculate the total inverse RMSE and MAPE to normalize weights
tesla_total_inv_rmse <- tesla_inv_rmse_model_1 + tesla_inv_rmse_model_2 + tesla_inv_rmse_model_3
tesla_total_inv_mape <- tesla_inv_mape_model_1 + tesla_inv_mape_model_2 + tesla_inv_mape_model_3

# Calculate the inverse RMSE and MAPE weights
tesla_inv_rmse_weight_1 <- tesla_inv_rmse_model_1 / tesla_total_inv_rmse
tesla_inv_rmse_weight_2 <- tesla_inv_rmse_model_2 / tesla_total_inv_rmse
tesla_inv_rmse_weight_3 <- tesla_inv_rmse_model_3 / tesla_total_inv_rmse

tesla_inv_mape_weight_1 <- tesla_inv_mape_model_1 / tesla_total_inv_mape
tesla_inv_mape_weight_2 <- tesla_inv_mape_model_2 / tesla_total_inv_mape
tesla_inv_mape_weight_3 <- tesla_inv_mape_model_3 / tesla_total_inv_mape

# Calculate the total weight for each model
tesla_total_weight_1 <- (tesla_inv_rmse_weight_1 + tesla_inv_mape_weight_1) / 2
tesla_total_weight_2 <- (tesla_inv_rmse_weight_2 + tesla_inv_mape_weight_2) / 2
tesla_total_weight_3 <- (tesla_inv_rmse_weight_3 + tesla_inv_mape_weight_3) / 2

# Print the results
cat("Model: Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2\n",
    "RMSE:", tesla_rmse_model_1, "MAPE:", tesla_mape_model_1, 
    "Inverse RMSE:", tesla_inv_rmse_model_1, "Inverse MAPE:", tesla_inv_mape_model_1, 
    "Inv. RMSE Weight:", tesla_inv_rmse_weight_1, "Inv. MAPE Weight:", tesla_inv_mape_weight_1, 
    "Total Weight:", tesla_total_weight_1, "\n\n")

cat("Model: Automated Holt-Winter's Model\n",
    "RMSE:", tesla_rmse_model_2, "MAPE:", tesla_mape_model_2, 
    "Inverse RMSE:", tesla_inv_rmse_model_2, "Inverse MAPE:", tesla_inv_mape_model_2, 
    "Inv. RMSE Weight:", tesla_inv_rmse_weight_2, "Inv. MAPE Weight:", tesla_inv_mape_weight_2, 
    "Total Weight:", tesla_total_weight_2, "\n\n")

cat("Model: Automated ARIMA Model\n",
    "RMSE:", tesla_rmse_model_3, "MAPE:", tesla_mape_model_3, 
    "Inverse RMSE:", tesla_inv_rmse_model_3, "Inverse MAPE:", tesla_inv_mape_model_3, 
    "Inv. RMSE Weight:", tesla_inv_rmse_weight_3, "Inv. MAPE Weight:", tesla_inv_mape_weight_3, 
    "Total Weight:", tesla_total_weight_3, "\n\n")

# Calculate the ensemble model weights
tesla_total_inv_rmse_all <- tesla_inv_rmse_model_1 + tesla_inv_rmse_model_2 + tesla_inv_rmse_model_3
tesla_total_inv_mape_all <- tesla_inv_mape_model_1 + tesla_inv_mape_model_2 + tesla_inv_mape_model_3

tesla_ensemble_inv_rmse_weight <- tesla_total_inv_rmse_all / tesla_total_inv_rmse_all
tesla_ensemble_inv_mape_weight <- tesla_total_inv_mape_all / tesla_total_inv_mape_all

# Create a dataframe-like structure
tesla_results <- data.frame(
  Model = c(
    "Regression Model with Quadratic Trend & Seasonality + Trailing MA with k = 2",
    "Automated Holt-Winter's Model",
    "Automated ARIMA Model",
    "ENSEMBLE MODEL"
  ),
  RMSE = c(tesla_rmse_model_1, tesla_rmse_model_2, tesla_rmse_model_3, ""),
  MAPE = c(tesla_mape_model_1, tesla_mape_model_2, tesla_mape_model_3, ""),
  `Inverse RMSE` = c(tesla_inv_rmse_model_1, tesla_inv_rmse_model_2, tesla_inv_rmse_model_3, tesla_total_inv_rmse_all),
  `Inverse MAPE` = c(tesla_inv_mape_model_1, tesla_inv_mape_model_2, tesla_inv_mape_model_3, tesla_total_inv_mape_all),
  `RMSE Weight` = c(tesla_inv_rmse_weight_1, tesla_inv_rmse_weight_2, tesla_inv_rmse_weight_3, tesla_ensemble_inv_rmse_weight),
  `MAPE Weight` = c(tesla_inv_mape_weight_1, tesla_inv_mape_weight_2, tesla_inv_mape_weight_3, tesla_ensemble_inv_mape_weight),
  `Total Weight` = c(tesla_total_weight_1, tesla_total_weight_2, tesla_total_weight_3, 1)
)


# Print the dataframe.
print(tesla_results, right = F)


#---------------USE ENSEMBLE MODEL IN PRE-COVID PERIOD (2010-2019)-------------#

# Create ensemble forecast for pre-COVID period.
tesla.ensemble.pre_covid <-( 
  (tesla_total_weight_1*(tesla_tot.quad.seas.pred$fitted + tesla_quad.ma.trail.res_2))
  + (tesla_total_weight_2*tesla_HW.ZZZ.pred$fitted)
  + (tesla_total_weight_3*tesla_revenue.auto.arima.pred$fitted)
)
# Display ensemble forecast for pre-COVID period.     
tesla.ensemble.pre_covid

# Check the accuracy of the ensemble forecast for the pre-COVID period.
round(accuracy(tesla.ensemble.pre_covid, tesla_revenue.ts), 3)

#---------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)---------#

# Create ensemble forecast for COVID & post-COVID periods.
tesla.ensemble.covid <-( 
  (tesla_total_weight_1*tesla_quad.fst.2level_2)
  + (tesla_total_weight_2*tesla_HW.ZZZ.pred$mean)
  + (tesla_total_weight_3*tesla_revenue.auto.arima.pred$mean)
)

# Display ensemble forecast for COVID and post-COVID periods.     
tesla.ensemble.covid 


# PLOT THE PREDICTIONS FOR ENSEMBLE MODEL: PRE-COVID, COVID & POST-COVID.
plot(tesla_allrevenue.ts, 
     xlab = "Time", ylab = "Tesla's Revenue (in Million $)", 
     ylim = c(0, 27000), bty = "l", xlim = c(2014, 2024), 
     xaxt = "n", lwd = 2,
     main = "MODEL 8: ENSEMBLE MODEL")  
axis(1, at = seq(2014, 2024, 1), labels = format(seq(2014, 2024, 1)))
lines(tesla.ensemble.pre_covid, col = "yellow", lwd = 3)
lines(tesla.ensemble.covid, col = "green", lwd = 3)
legend(2014,25000, legend = c("Revenue (2014-2023)", 
                              "Ensemble Forecast: Pre-Covid Period (2014-2019)", 
                              "Ensemble Forecast: Covid & Post-Covid Periods (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART ON PRE-COVID, COVID AND POST-COVID PERIODS.
lines(c(2020,2020), c(-1000,27000)) # FOR PRE-COVID DATA
text(2017.1,27000, "PRE-COVID", col = "blue")
text(2022, 27000, "COVID & POST-COVID", col ="green")
arrows(2014.1,26000,2019.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,26000,2023.9, 26000,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")


#--------------USE ENSEMBLE MODEL IN COVID & POST-COVID PERIODS (2020-2023)--------------#

# Develop data frame to show revenue changes during COVID &
# post-COVID periods. For that, identify the difference between
# actual revenue and ensemble forecast during 2020-2023. 

tesla.revenue.change <- tesla_future.ts - tesla.ensemble.covid

tesla_revenue_covid.df <- round(data.frame(
  tesla_future.ts, 
  tesla.ensemble.covid, 
  tesla.revenue.change), 3)
names(tesla_revenue_covid.df) <- c(
  "Actual_Revenue", 
  "Ensemble_Fst", 
  "Difference")
tesla_revenue_covid.df

library("writexl")
write_xlsx(tesla_revenue_covid.df, 
           "C:/Users/STSC/Documents/Full Time/Personal Projects/Time Series Forecast - Revenue - MAGNIFICENT 7/PRE-FINAL/M7-R Files/Sum_tesla_20_23_actual_fst.xlsx")


