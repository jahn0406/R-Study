# Time Series Analysis Practice 
getwd()

install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")

library(ggplot2)
library(forecast)
library(tseries)

bike <- read.csv("Time Series Practice Dataset/day.csv")

# STEP 1 : Examining your data 

# plotting a time series model 
bike$date <- as.Date(bike$dteday) 
ggplot2::ggplot(bike) +geom_line(aes(date , cnt)) + scale_x_date('mnth') + ylab('bike usage') + xlab('month')

# cleaning out outliers from cnt to reduce volality in our plot 
count_ts <- ts(bike[,'cnt'])
bike$clean.cnt <- tsclean(count_ts)

ggplot2::ggplot(bike, aes(x = date, y = cnt)) + geom_line() + scale_x_date('mnth') + ylab('bike usage') +xlab('month') # even after removing the outliers, data is still fairly volatile 

# applying simple moving average(MA)
# Moving Average(MA) : the estimate of the trend-cycle at time t is obtained by averaging values of the time series within k periods of t 
# order 7 is equivalent of n = 3 since m = 2n + 1 
# a larger order means a smoother curve

bike$cnt.ma <- ma(bike$clean.cnt, order = 7)
bike$cnt.ma30 <- ma(bike$clean.cnt, order = 30)
na.omit(bike$cnt.ma30)

ggplot2::ggplot() +
              geom_line(data = bike , aes(x = date, y = clean.cnt, colour = "total")) + 
              geom_line(data = bike, aes(x = date, y = cnt.ma, colour = "weekly")) +
              geom_line(data = bike, aes(x = date, y = cnt.ma30, colour = "monthly")) +
              ylab("bike usage")

# STEP 2 : Decomposing your data - seasonality, trend, cycle 
# Multiplicative vs Additive Model - additive model is more suitable when the seasonal or trend component is not proportional to the level of the series  
# by default, stl assumes an additive model, so if you intend to incorporate the multiplicative model, allow.multiplicative.trend = TRUE 
# Decompositions some way between additive and multiplicative can be obtained using a Box-Cox transformation of the data with 0<ル<10<ル<1. A value of ル=0ル=0 corresponds to the multiplicative decomposition while ル=1ル=1 is equivalent to an additive decomposition.

?ts # create time-series objects 
?stl # decompose a time series into seasonal, trend, and irregular components
?seasadj #returns seasonally adjusted data by removing the seasonal component
count.ma <- ts(na.omit(bike$cnt.ma), frequency = 30) 
decomp <- stl(count.ma, s.window = "periodic", t.window = 5, robust = TRUE) 
deseasonal <- forecast::seasadj(decomp, ylab("bike usage"), xlab("time")) # Only contain the remainder and trend-cycle component ; used when seasonality is not our primary concern 
plot(decomp)
lines(decomp$time.series[,2], col = "red" , ylab = "trend")

# If decomposing a multiplicative model, decompose(timeseriesdata , type="multiplicative")

# STEP 3: Confirming Stationarity - need the data to be stationary to apply ARIMA model

adf.test(count.ma, alternative = "stationary") #since p-value is greater than alpha (.05), need to transform your data 
Acf(count.ma, main = "")
Pacf(count.ma, main = "")

# differencing is a commonly used method 
# ACF 
# PACF 

count.ma.chg <- diff(deseasonal, differences = 1)
plot(count.ma.chg)
adf.test(count.ma.chg, alternative = "stationary") #since the p-value is less than .05, reject the null hypothesis that count.ma.chg is not stationary. 

par(mfrow = c(1,1))
acf(count.ma.chg, main = "ACf for Differenced Series")
pacf(count.ma.chg, main = "PACF for Differenced Series")

# STEP 4 : ARIMA model 
auto.arima(count.ma.chg)
fit <- auto.arima(count.ma.chg, seasonal = TRUE)
tsdisplay(residuals(fit), lag.max = 100)
  
seas_fcast <- forecast(auto.arima(count.ma.chg), h=10)sw65
plot(seas_fcast, main = "forecasting bike usage")

# How to replace time with another variable in x-axis 

?forecast 

