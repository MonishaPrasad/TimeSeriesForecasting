library(forecast)
library(ggplot2)
library(tseries)

pacman::p_load(tidyverse, lubridate,zoo,forecast, fUnitRoots)
data = read.csv("ActualRatings_weeklyGRP_data.csv", header=T)

sapply(data, class)
str(data)
head(data, 3)

# plot(data$week, data$GRP)
# lines(data$week, data$GRP)
plot(data$t, data$GRP)
lines(data$t, data$GRP)

trainingData = data[1:72,]
testData = data[73:92,]

rating_ts = ts(data$GRP, frequency=52, start=c(2007, 25))
rating_ts

training_ts = ts(trainingData$GRP, frequency=52, start=c(2007, 25))
training_ts

test_ts = ts(testData$GRP, frequency=52, start=c(2008, 45))
test_ts

#Linear Regression
model = lm(GRP ~ t, data=trainingData)
print(model)
summary(model)
accuracy(model)

trainingData$fitted = model$fitted.values
head(trainingData)

ggplot(trainingData, aes(t)) +
  geom_line(aes(y = GRP, colour = "Actual")) +
  geom_line(aes(y = fitted, colour = "Fitted"))

testData$predictions = predict(model,testData)
head(testData)
accuracy(testData$predictions, testData$GRP)

ggplot(testData, aes(t)) +
  geom_line(aes(y = GRP, colour = "Actual")) +
  geom_line(aes(y = predictions, colour = "Predicted"))

#Time series regression

tslm_fit <- tslm(training_ts ~ trend + season)
summary(tslm_fit)
accuracy(tslm_fit)
plot(training_ts, col = 2, ylab = 'GRP')
lines(tslm_fit$fitted.values, col = 3)
legend(2008.4, 300, legend=c("Actual", "Fitted"), col=c(2,3), lty=1:1)

tslm_predictions <- forecast(tslm_fit, h=20)
plot(tslm_predictions, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
accuracy(tslm_predictions, test_ts)

#Decomposition

# decompose_df <- tslm(training_ts ~ trend + fourier(training_ts, 2))
# summary(decompose_df)
# trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(training_ts)
# components <- cbind(
#   data = training_ts,
#   trend = trend,
#   season = rating_ts - trend - residuals(decompose_df),
#   remainder = residuals(decompose_df)
# )
# autoplot(components, facet=TRUE)

# adjust_df <- training_ts - components[,'season']
# autoplot(training_ts, series="Data") +
#   autolayer(adjust_df, series="Seasonally adjusted")

# visualize the various components of TS
# rating_decom = decompose(rating_ts)
# plot(rating_decom, lwd=4, col="blue")

#Holt's Linear Model
holt1 <- holt(training_ts, h=20, damped=FALSE, level=c(80,95), alpha=NULL, beta=NULL)

plot(training_ts, col = 2, ylab = 'GRP')
lines(holt1$fitted, col = 3)
legend(2008.4, 300, legend=c("Actual", "Fitted"), col=c(2,3), lty=1:1)

plot(holt1, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
autoplot(holt1)
holt1$model
accuracy(holt1, test_ts)
# identify optimal parameter
beta <- seq(0.0001, 0.5, by = 0.001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(training_ts,h=20, damped=FALSE, level=c(80,95), alpha=NULL, beta = beta[i])
  RMSE[i] <- accuracy(fit, test_ts)[2,2]
}
# convert to a data frame and idenitify min beta value
beta_fit <- data_frame(beta, RMSE)
plot(beta_fit$beta,beta_fit$RMSE)
min_RMSE = min(RMSE)
for (i in seq(1,500)) {
  if(beta_fit[i,]$RMSE == min_RMSE)
    optimal_beta <- beta_fit[i,]$beta
}
print(min_RMSE)
print(optimal_beta)
# second model with optimal beta
holt2 <- holt(training_ts,h=20, damped=FALSE, level=c(80,95), alpha=NULL, beta = optimal_beta)
accuracy(holt2, test_ts)
holt2$model

plot(training_ts, col = 2, ylab = 'GRP')
lines(holt2$fitted, col = 3)
legend(2008.4, 300, legend=c("Actual", "Fitted"), col=c(2,3), lty=1:1)

plot(holt2, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
autoplot(holt2)

#ARIMA
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

#Null hypothesis :  data is non stationary and there is a unit root at some level of confidence.
#Augmented Dickey-Fuller (ADF) statistic, used in the test, is a negative number. The more negative it is, the stronger the rejection of the null hypothesis.
adf.test(training_ts, alternative = "stationary")
#less negative number, so null hypothesis is not rejected. Data is non stationary.

Acf(training_ts, main='')
Pacf(training_ts, main='')

d1 = diff(training_ts, differences = 1)
plot(d1)
adf.test(d1, alternative = "stationary")
#still it is less ngative number

Acf(d1, main='ACF for Differenced Series') #q 0
Pacf(d1, main='PACF for Differenced Series') # p 2

# d2 = diff(training_ts, differences = 2)
# plot(d2)
# adf.test(d2, alternative = "stationary")
#significantly larger negative number

# Acf(d2, main='ACF for Differenced Series') #q 1
# Pacf(d2, main='PACF for Differenced Series') # p 1,2,3 (but it is mostly enough to choose values 0,1 or 2)

auto_arima_fit<-auto.arima(training_ts, seasonal=FALSE)
auto_arima_fit
accuracy(auto_arima_fit)
tsdisplay(residuals(auto_arima_fit), lag.max=45, main='(1,1,1) Model Residuals')
plot(training_ts, col = 2, ylab = 'GRP')
lines(auto_arima_fit$fitted, col = 3)
legend(2008.4, 300, legend=c("Actual", "Fitted"), col=c(2,3), lty=1:1)
auto_arima_fcast <- forecast(auto_arima_fit, h=20)
plot(auto_arima_fcast, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
summary(auto_arima_fcast)
accuracy(auto_arima_fcast, test_ts)

arima_fit1 = arima(training_ts, order=c(2,1,1))
arima_fit1
accuracy(arima_fit1)
tsdisplay(residuals(arima_fit1), lag.max=45, main='(2,1,1) Model Residuals')
arima_fcast1 <- forecast(arima_fit1, h=20)
plot(arima_fcast1, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
summary(arima_fcast1)
accuracy(arima_fcast1, test_ts)
bic=AIC(arima_fit1, k = log(length(training_ts)))
bic

arima_fit2 = arima(training_ts, order=c(2,1,0))
arima_fit2
accuracy(arima_fit2)
tsdisplay(residuals(arima_fit2), lag.max=45, main='(2,1,0) Model Residuals')
arima_fcast2 <- forecast(arima_fit2, h=20)
plot(arima_fcast2, xlab = 'Time', ylab = 'GRP')
lines(rating_ts)
summary(arima_fcast2)
accuracy(arima_fcast2, test_ts)
bic=AIC(arima_fit2, k = log(length(training_ts)))
bic

# arima_fit3 = arima(training_ts, order=c(0,1,2))
# arima_fit3
# accuracy(arima_fit3)
# tsdisplay(residuals(arima_fit3), lag.max=45, main='(0,1,2) Model Residuals')
# arima_fcast3 <- forecast(arima_fit3, h=20)
# plot(arima_fcast3)
# lines(rating_ts)
# summary(arima_fcast3)
# accuracy(arima_fcast3, test_ts)
# bic=AIC(arima_fit3, k = log(length(training_ts)))
# bic

# arima_fit4 = arima(training_ts, order=c(1,2,1))
# arima_fit4
# accuracy(arima_fit4)
# tsdisplay(residuals(arima_fit4), lag.max=45, main='(1,2,1) Model Residuals')
# arima_fcast4 <- forecast(arima_fit4, h=20)
# plot(arima_fcast4)
# lines(rating_ts)
# summary(arima_fcast4)
# accuracy(arima_fcast4, test_ts)
# bic=AIC(arima_fit4, k = log(length(training_ts)))
# bic

# arima_fit5 = arima(training_ts, order=c(2,2,1))
# arima_fit5
# accuracy(arima_fit5)
# tsdisplay(residuals(arima_fit5), lag.max=45, main='(2,2,1) Model Residuals')
# arima_fcast5 <- forecast(arima_fit5, h=20)
# plot(arima_fcast5)
# lines(rating_ts)
# summary(arima_fcast5)
# accuracy(arima_fcast5, test_ts)
# bic=AIC(arima_fit5, k = log(length(training_ts)))
# bic