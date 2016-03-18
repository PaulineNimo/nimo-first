#Forecasting daily revenue
#Setting the working directory
setwd("C:/Users/Nimo/Desktop/Job/Forecasting")

#Importing the data into R
data<-read.csv("daily_revenue.csv",header=TRUE)

#Selecting the column with the time series
data2<-data$Revenue

#Converting the data into a time series
data.series<-ts(data2,frequency=365)
data.series
plot.ts(data.series)

#Smoothing the time series
#Using Moving Average
library("TTR")
decomposed.series<-SMA(data.series,n=60)
plot.ts(decomposed.series)

#Estimating the components
data.components<-decompose(data.series,type="multiplicative")
plot(data.components)

#Using Simple Exponential Smoothing
series.forecast<-HoltWinters(data.series,beta=FALSE,gamma=FALSE)
series.forecast
#Alpha is 0.02311467 which is close to zero and hence forecasts are based on both recent and not not so recent observations though more weight is placed on those recent.
plot(series.forecast)
#The red line are the forecasts which is smoother than the original data.

#Forecasting 10 steps ahead
library("forecast")
series.forecast2<-forecast.HoltWinters(series.forecast,h=10)
series.forecast2
#This gives the 80% and the 95% prediction interval.
plot.forecast(series.forecast2)
#The forecasts are the thin blue line, the 80% prediction interval is the next shade of blue and the 95% prediction interval is the lighest shade of blue.

#Plotting the ACF
acf(series.forecast2$residuals,lag.max=20)
#There is significant evidence for non-zero correlations since the autocorrelations are beyond the significance bounds. 
#Using the Ljung-Box test
Box.test(series.forecast2$residuals,lag=20,type="Ljung-Box")
#The p value is close to zero hence there is sufficient evidence of non-zero autocorrelations. 

#checking whether the errors have constant variance
plot.ts(series.forecast2$residuals)
#The plot shows that the residuals have roughly the same variance over time.

#This shows that the simple exponential smoothing does not provide an adequate predictive model.

#Holt's Exponential Smoothing
data.forecast2<-HoltWinters(data.series,gamma=FALSE)
data.forecast2
#The value of the  alpha is 0.02626502 and beta=0.04206414 which is close to zero meaning that the forecasts are based on both recent and not so recent data but more weight is placed on recent observations.
#Plotting the time series against the forecasts
plot(data.forecast2)
#the forecasts are the red line which is much smoother. 
library("forecast")

series.forecast2<-forecast.HoltWinters(data.forecast2,h=10)
series.forecast2

#The output gives the prediction, 80% and the 95% prediction intervals.

#Generating the acf
acf(series.forecast2$residuals,lag.max=20)
#Using the Ljung Box test
Box.test(series.forecast2$residuals,lag=20,type="Ljung-Box")
#The p value is close to zero and hence there is evidence of non-zero autocorrelations.

plot(series.forecast2)

#The forecasts are the thin blue line, the 80% prediction interval is the next shade of blue and the 95% prediction interval is the lighest shade of blue.

plot.ts(series.forecast2$residuals)
#The variance of the residuals appears to be constant at around zero.

