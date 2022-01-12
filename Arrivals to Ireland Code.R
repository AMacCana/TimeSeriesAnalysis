#------------------------------------------------------

#reading in data and labelling columns
sa <- read.csv("C:\\Users\\andre\\Documents\\UCC\\2020-2021\\Semester 2\\ST4064 - Time Series\\Assignment\\Potential Answers\\Arrivals to Ireland Analysis\\117305871.csv", header=TRUE)
sa

#coverting data to time series 
ts_sa = ts(sa[2], start=c(2010,1), frequency=12)

#splitting the data for modelling and testing 
ts_modelsa = window(ts_sa, start=c(2010,1), end=c(2018,4), frequency=12)
ts_testsa = window(ts_sa, start=c(2018,5), frequency=12)

#------------------------------------------------------
#LOOKING AT THE DATA

plot(ts_modelsa, type = "o", xlab="Time (Monthly Data)", ylab="Passengers (Thousands)", 
     main="Arrivals to Ireland by Sea and Air")
plot(decompose(ts_modelsa))

layout(mat=matrix(c(1,1,2,3),byrow=TRUE,ncol=2))
plot(ts_modelsa, type = "o", xlab="Time (Monthly)", ylab="Passengers (Thousands)", 
     main="Arrivals to Ireland by Sea and Air")
acf(ts_modelsa, lag.max = 60, main="ACF plot for Arrivals to Ireland")
pacf(ts_modelsa, lag.max = 60, main="PACF plot for Arrivals to Ireland")

par(mfrow=c(1,1))
cpgram(ts_modelsa)

#checking stationarity of the raw data
#install.packages("seastests")
library(tseries)
adf.test(ts_modelsa) #stationary

#visualising the seasonal components

par(mfrow=c(1,1))
saseasonal = matrix(c(ts_modelsa), nrow = 12, byrow = FALSE) 
matplot(saseasonal, type = "l")

saseasonal = c(ts_modelsa)
samonths = c(cycle(ts_modelsa))
boxplot(saseasonal ~ samonths, xlab="Month", ylab="Passengers (Thousands)",
        main="Boxplot for number of passengers that arrived to Irealnd in each month from 2010 to 2018", xaxt="n")
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12), label=c("Jan", "Feb", "Mar", "Apr",
                                                  "May", "Jun", "Jul", "Aug", 
                                                  "Sep", "Oct", "Nov", "Dec"))

#-----------------------------------------------------
#STATIONARITY

#differenced seasonally at lag 12 
y12 = diff(ts_modelsa, lag = 12)
adf.test(y12) #not stationary

layout(mat=matrix(c(1,1,2,3),byrow=TRUE,ncol=2))
plot(y12, main = "Arrivals to Ireland Data Adjusted Seasonally", 
     xlab = "Time (Monthly)", ylab = "Passengers (Thousands)") 
acf(y12, lag.max = 72, main="ACF plot for Seasonally Adjusted Arrivals to Ireland")
pacf(y12, lag.max = 72, main="PACF plot for Seasonally Adjusted Arrivals to Ireland")
par(mfrow=c(1,1))
cpgram(y12)

#differenced once and then differenced seasonally at lag 12
y112 = diff(ts_modelsa)
y112 = diff(y112, lag = 12)
adf.test(y112) #stationary

layout(mat=matrix(c(1,1,2,3),byrow=TRUE,ncol=2))
plot(y112, main = "Arrivals to Ireland Data Adjusted Seasonally and for 1-Month Prior",
     xlab = "Time (Monthly)", ylab = "Passengers (Thousands)")
acf(y112, lag.max = 72,  main="ACF plot for Data Ajusted Seasonally and for 1-Month Prior")
pacf(y112, lag.max = 72, main="PACF plot for Data Ajusted Seasonally and for 1-Month Prior")
par(mfrow=c(1,1))
cpgram(y112)
#Nonseasonally: 
  #acf cuts off after 1
  #pacf tails off
  #try (0,1,1)
#Seasonally:
  #acf cuts off after lag 4
  #pacf is within bounds at all lags and appears to tail off as lags increase
  #try (0,1,4)[12]

#----------------------------------------------------
#MODEL FITTING: models are fitted and diagnostics and aics are assessed

#starting model based on acf and pacf
fit1 =  arima(ts_modelsa, order=c(0, 1, 1), list(order=c(0, 1 ,4), period=12))
fit1

tsdiag(fit1)

resids1 = residuals(fit1)

par(mfrow=c(2,2))
acf(resids1,lag.max=48)	
pacf(resids1,lag.max=48)
qqnorm(scale(resids1))
abline(a=0, b=1)
cpgram(resids1)

#assessing other potential models

fit2 =  arima(ts_modelsa, order=c(0, 1, 1), list(order=c(0, 1 ,3), period=12))
fit2

tsdiag(fit2)

resids2 = residuals(fit2)

par(mfrow=c(2,2))
acf(resids2, main="ACF of residuals from fitted model", lag.max = 48)
pacf(resids2, main="PACF of residuals from fitted model", lag.max = 48)
qqnorm(scale(resids2), main="Normal Q-Q Plot for scaled residuals from fitted model")
abline(a=0, b=1)
cpgram(resids2, main="Cumulative periodogram of residuals from fitted model")

fit3 =  arima(ts_modelsa, order=c(0, 1, 1), list(order=c(0, 1 ,2), period=12))
fit3

tsdiag(fit3)

resids3 = residuals(fit3)

par(mfrow=c(2,2))
acf(resids3,lag.max=48)	
pacf(resids3,lag.max=48)
qqnorm(scale(resids3))
abline(a=0, b=1)
cpgram(resids3)

fit4 =  arima(ts_modelsa, order=c(0, 1, 1), list(order=c(0, 1 ,1), period=12))
fit4

tsdiag(fit4)

resids4 = residuals(fit4)

par(mfrow=c(2,2))
acf(resids4,lag.max=48)	
pacf(resids4,lag.max=48)
qqnorm(scale(resids4))
abline(a=0, b=1)
cpgram(resids4)

#----------------------------------------------------
#FORECASTING
#S-ARIMA(0,1,1)x(0,1,3)[12] appears to have the best 
#combination of low AIC and good diagnostics so this 
#is the chosen model for forecasting

fit2pred = predict(fit2, n.ahead = 33)

par(mfrow=c(1,1))
plot(ts_modelsa, xlab="Time (Monthly)", ylab="Passengers (Thousands)", 
     main="Arrivals to Ireland by Sea and Air", xlim=c(2010,2022), ylim=c(0,2600))

lines(fit2pred$pred,col="red")
lines(fit2pred$pred+2*fit2pred$se,col="grey")
lines(fit2pred$pred-2*fit2pred$se,col="grey")

lines(ts_testsa, col="blue")

legend("topleft", legend=c("Modelled Data","Predicted Values","Upper and Lower 95% Confidence Limit for Predicted Values","Observed Values over test period"), 
       lty=c(1,1,1,1), col=c('black','red','grey','blue'), lwd=3,bty='n')

#zoomed in predictions
par(mfrow=c(1,1))
plot(fit2pred$pred, col="red", xlab="Time (Monthly)", ylab="Passengers (Thousands)", 
     main="Zoomed in plot of Predicted and Observed values over model test period", xlim=c(2018,2021), ylim=c(0,2600), xaxt="n")
axis(1, at=c(2018,2019,2020,2021), label=c(2018,2019,2020,2021))


lines(fit2pred$pred,col="red")
lines(fit2pred$pred+2*fit2pred$se,col="grey")
lines(fit2pred$pred-2*fit2pred$se,col="grey")

lines(ts_testsa, col="blue")

legend("topleft", legend=c("Predicted Values","Upper and Lower 95% Confidence Limit for Predicted Values","Observed Values over test period"), 
       lty=c(1,1,1,1), col=c('red','grey','blue'), lwd=3,bty='n')

