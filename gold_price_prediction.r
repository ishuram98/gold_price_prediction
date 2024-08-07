library(dplyr)
library(tseries)
library(TSA)

gold_data = read.csv("gold_price_monthly_dataset.csv")
summary(gold_data[1:96,])
head(gold_data)
dim(gold_data)[1]

gold_price = gold_data$Price

# Convert "Date" column to Date object
date1 = gold_data$Date
class(date1)
date1 = as.Date(gold_data$Date, format = "%Y-%m")
class(date1)
gold_data$Date = date1
class(gold_data$Date)

length(gold_data$Price)

#feeding data from 2014-2021
gold_data_ts = ts(gold_data[1:96,2], start = c(2014,1),frequency = 12)
tail(gold_data_ts)
gold_data_ts

#2022 data for prediction ( 8 months of 2022)
x.new = ts(gold_data[97:dim(gold_data)[1],2], start = c(2022, 1), frequency = 12)
x.new

trend_seasonal_split = decompose(gold_data_ts, "multiplicative")

plot(trend_seasonal_split)

plot(trend_seasonal_split$trend)

plot(trend_seasonal_split$seasonal)

boxplot(gold_data_ts~cycle(gold_data_ts), names=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July",
                                                  "Aug", "Sep", "Oct", "Nov", "Dec"))


# Initial 
plot(gold_data_ts, xlim=c(2014,2022),main = "Gold Price Over Time", xlab = "Year", ylab = "Gold Price")
gold_price_ini = gold_data$Price
par(mfrow = c(1, 2))
acf(gold_price_ini)
pacf(gold_price_ini)
par(mfrow = c(1, 1))
adf.test(gold_price_ini)

# log transformation
logx = log(gold_data_ts)
plot(logx, type = 'l', main = "log-transformed Gold Price Data", xlab = 'Year', ylab = "Log(Gold Price)")
par(mfrow = c(1, 2))
acf(logx,lag.max=60)
pacf(logx,lag.max=60)
par(mfrow = c(1, 1))

adf.test(logx)
Box.test(logx,lag=12,type='Ljung')

# differenced log transformation
dlogx=diff(logx)

plot(dlogx, type = 'l', main = "differenlog-transformed Gold Price", xlab = 'Year', ylab = "Diff(Log(Gold Price))")
#dev.off()

par(mfrow = c(1, 2))
acf(dlogx,lag.max=60)
pacf(dlogx,lag.max=60)
par(mfrow = c(1, 1))

library(tseries)

adf.test(dlogx)
Box.test(dlogx,lag=12,type='Ljung')
eacf(dlogx)






ddlogx = diff(dlogx, 12)
plot(ddlogx)
par(mfrow=c(1,2))
acf(ddlogx,lag.max=60)  ##SMA1     
pacf(ddlogx,lag.max=60) ##SAR1
par(mfrow=c(1,1))
Box.test(ddlogx,lag=12,type='Ljung')

library(lmtest)

# SMA(1)
sma1 = arima(ddlogx, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=12))
sma1
coeftest(sma1)

sma1_intercept_dropped = arima(logx, order=c(0,1,0), seasonal=list(order=c(0,1,1), period=12))
sma1_intercept_dropped
coeftest(sma1_intercept_dropped) #aic = -267.88

plot(sma1_intercept_dropped$residuals) 
par(mfrow=c(1,2))
acf(sma1_intercept_dropped$residuals,lag.max=60) 
pacf(sma1_intercept_dropped$residuals,lag.max=60) 
par(mfrow=c(1,1))
Box.test(sma1_intercept_dropped$residuals,lag=12,type='Ljung')
eacf(sma1_intercept_dropped$residuals) 
adf.test(sma1_intercept_dropped$residuals)

#SAR(1)
sar1 = arima(ddlogx, order=c(0,0,0), seasonal=list(order=c(1,0,0), period=12))
sar1
coeftest(sar1)

#drop intercept
sar1_intercept_dropped = arima(logx, order = c(0,1,0) ,seasonal = list(order=c(1,1,0), period=12))
sar1_intercept_dropped
coeftest(sar1_intercept_dropped) # aic = -269.74

plot(sar1_intercept_dropped$residuals)
par(mfrow=c(1,2))
acf(sar1_intercept_dropped$residuals,lag.max=60) 
pacf(sar1_intercept_dropped$residuals,lag.max=60) 
par(mfrow=c(1,1))
Box.test(sar1_intercept_dropped$residuals,lag=12,type='Ljung')
eacf(sar1_intercept_dropped$residuals) 
adf.test(sar1_intercept_dropped$residuals)


#SAR(1) has lower AIC value

#ARMA(1,1) from EACF plot for SAR(1)
arma11.sar1 = arima(logx, order = c(1,1,1) ,seasonal = list(order=c(1,1,0), period=12))
arma11.sar1
coeftest(arma11.sar1) # aic = -265.82
plot(arma11.sar1$residuals)
par(mfrow=c(1,2))
acf(arma11.sar1$residuals,lag.max=60) 
pacf(arma11.sar1$residuals,lag.max=60) 
par(mfrow=c(1,1))
Box.test(arma11.sar1$residuals,type='Ljung',lag=12) 

adf.test(arma11.sar1$residuals) 

#ARMA(1,1) - Stationary check # The roots are outside the unit cycle and greater than 1.
polyroot(c(1, -arma11.sar1$coef[1]))
abs(polyroot(c(1, -arma11.sar1$coef[1])))

#ARMA(1,1) -Model Redundancy #There are no matching roots. So, the model is not redundant.
polyroot(c(1, -arma11.sar1$coef[1]))
polyroot(c(1, arma11.sar1$coef[2]))


#MA(2) from ACF plot from dropped intercept SAR(1) model ( and EACF plot also)
ma2.sar1 = arima(logx, order = c(0,1,2) ,seasonal = list(order=c(1,1,0), period=12))
ma2.sar1
coeftest(ma2.sar1) #aic = -269.72


ma2.sar1.dropped = arima(logx, order = c(0,1,2) ,seasonal = list(order=c(1,1,0), period=12),fixed=c(0,NA,NA))
ma2.sar1.dropped
coeftest(ma2.sar1.dropped) #aic = -271.62
ma2.sar1.dropped$aic
plot(ma2.sar1.dropped$residuals)
par(mfrow=c(1,2))
acf(ma2.sar1.dropped$residuals,lag.max=60) 
pacf(ma2.sar1.dropped$residuals,lag.max=60) 
par(mfrow=c(1,1))
Box.test(ma2.sar1.dropped$residuals,type='Ljung',lag=12)
eacf(ma2.sar1.dropped$residuals)

adf.test(ma2.sar1.dropped$residuals) 


source("rolling.forecast.R")
rolling.forecast(logx, 5, length(gold_data_ts)-50, c(1,1,1), seasonal = list(order=c(1,1,0), period=12))
rolling.forecast(logx, 5, length(gold_data_ts)-50, c(0,1,2), seasonal = list(order=c(1,1,0), period=12),fixed=c(0,NA,NA))


error1 = rolling.forecast(logx, 5, length(gold_data_ts)-50, c(1,1,1), seasonal = list(order=c(1,1,0), period=12))
error2 = rolling.forecast(logx, 5, length(gold_data_ts)-50, c(0,1,2), seasonal = list(order=c(1,1,0), period=12),fixed=c(0,NA,NA))

error= c(error1, error2)
par(mfrow=c(1,1))

plot(error1, type = 'l', ylim = c(min(error), max(error)), main = 'Rolling Forecasting Errors for Different Models', xlab = 'Forecast horizon', ylab = 'Error')
lines(error2, col = 2)
legend.text = c("ARIMA(1,1,1)x(1,1,0)","ARIMA(0,1,2)x(1,1,0)")
legend("bottomright", legend.text, lty = rep(1, 6), col = 1:6)



# fitting ma2.sar1.dropped model as it is kind of stationary(p value :0.05287) 
#and white noise and has lower aic value, low rolling forecast error
fit = ts(exp(fitted.values(ma2.sar1.dropped)), start = 2014, frequency = 12)


par(mfrow = c(1,1))
plot(gold_data_ts, xlim = c(2014, 2022),ylim=c(min(gold_data_ts), max(fit)), xlab = 'time', ylab = 'price', main = 'Gold Price')
points(gold_data_ts, pch=1)
points(fit, col = 'red', pch = 2)
lines(fit, col = 'red', pch = 2)
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2)





pp = predict(ma2.sar1.dropped, 8) # predicting 8 months of 2022 so 8 data points
pred = ts(exp(pp$pred), start = 2022, frequency = 12)
pred
pred.upp = ts(exp(pp$pred+2*pp$se), start = 2022, frequency = 12)
pred.low = ts(exp(pp$pred-2*pp$se), start = 2022, frequency = 12)

plot(gold_data_ts, type = 'o', xlim = c(2014, 2023),ylim=c(20000,80000), xlab = 'time', ylab = 'price', main = 'Gold price')
lines(pred, col = 'red', pch = 2)
points(pred, col = 'red', pch = 2)
lines(pred.low, col = 'red', lty = 2)
lines(pred.upp, col = 'red', lty = 2)
lines(x.new, type = 'o') # Actual last 8 values from data
legend.text=c("Actual values", "Prediction")
legend("topleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2)




