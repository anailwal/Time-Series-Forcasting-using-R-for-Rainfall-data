# Load the required libraries
library(fpp2)

# Load the time series data
data <- read.csv("D:/term5/time_series/Rainfall_data1.csv")

ts_data <- ts(data$Precipitation, start = c(2000, 1), frequency = 12)
class(ts_data)
autoplot(ts_data) +
  ggtitle("Rainfall Data from 2000 to 2020 for Mumbai, India") +
  xlab("Year") +
  ylab("Precipitation")

# Determine the order of differencing d required to make the time series stationary
d <- ndiffs(ts_data)
d

library(tseries)
# Perform the KPSS test
kpss.test(ts_data)


#Create a time series plot, seasonal plot, seasonal subseries plot and lag plot
## Seasonal plot
ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Millimetres") +
  ggtitle("Seasonal plot: Rainfall")

##Seasonal subseries plot
ggsubseriesplot(ts_data) +
  ylab("Millimetres") +
  ggtitle("Seasonal subseries plot: ")

tsdisplay(ts_data,lag.max = 48) #ACF & PACF plot

#Lag plot
df<- window(ts_data, start=2000)
gglagplot(df)

#ACF plot
ggAcf(df, lag=48)


#Classical Decomposition plot

ts_data %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition
    of Rainfall data")


##########################################
#Simple exponential smoothing
# Estimate parameters

fc <- ses(ts_data, h=10)
# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Rainfall (mm))") + xlab("Year")

checkresiduals(fc)


#Trend Method (Holt's)
fc1 <- holt(ts_data, h=10)
fc1

fc2 <- holt(ts_data, damped=TRUE, phi = 0.9, h=10)
autoplot(ts_data) +
  autolayer(fc1, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Rainfall (mm)") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc2)
checkresiduals(fc1)

# Holt-Winters’ seasonal method

fit_hw <- hw(ts_data,seasonal="additive")

autoplot(ts_data) +
  autolayer(fit_hw, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Rainfall(mm)") +
  ggtitle("Rainfall data for mumbai") +
  guides(colour=guide_legend(title="Forecast"))
summary(fit_hw)
checkresiduals(fit_hw)

#####State space models ETS()

# Split the data into training and testing sets
train_data <- window(ts_data, end = c(2019, 12))

test_data <- window(ts_data, start = c(2020, 1))

# Perform exponential smoothing on the training data
fit_ets <- ets(train_data, model = "ANA", alpha = NULL, beta =NULL, gamma = NULL)

# Make forecasts on the testing data
forecasts <- forecast(fit_ets, h = length(test_data))

summary(forecasts)
checkresiduals(fit_ets)

# Plot the actual and forecasted values
plot(forecasts, main = "Exponential Smoothing Forecast for Rainfall", ylab = "Rainfall (mm)")
lines(test_data, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)

       
       
############################
#############################

diff_data <- diff(ts_data, differences = 12)

# Plot the differenced data
plot(diff_data, main = "Differenced Rainfall Data")

#Decompose
diff_data %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition
    of Rainfall data")

########################
###############################################

# Fit a SARIMA model to the training data
fit_s <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit_s)
# Make forecasts on the testing data
forecasts <- forecast(fit, h = length(test_data))

# Plot the actual and forecasted values
plot(forecasts, main = "SARIMA Forecast for Rainfall", ylab = "Rainfall (mm)")
lines(test_data, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)

checkresiduals(fit_s)

################################################
## Manually checking the values of (p,d,q) & seasonal order(P,D,Q)

# Fit an ARIMA model with specified values of p, d, q, and seasonal order P, D, Q
arima_model1 <- arima(ts_data, order = c(1,0,1), seasonal = list(order = c(1,1,1), period = 12))
arima_model1
accuracy(arima_model1)
##second
arima_model2 <- arima(ts_data, order = c(1,0,0), seasonal = list(order = c(1,1,2), period = 12))
arima_model2
accuracy(arima_model2)
#third
arima_model3 <- arima(ts_data, order = c(1,0,1), seasonal = list(order = c(1,1,2), period = 12))
arima_model3
accuracy(arima_model3)
#fourth
arima_model4 <- arima(ts_data, order = c(2,0,0), seasonal = list(order = c(1,1,1), period = 12))
arima_model4
accuracy(arima_model4)
#fifth
arima_model5 <- arima(ts_data, order = c(2,0,0), seasonal = list(order = c(1,1,2), period = 12))
arima_model5
accuracy(arima_model5)
#sixth
arima_model6 <- arima(ts_data, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 12))
arima_model6
accuracy(arima_model6)
summary(arima_model6)

###################################
#####################################################

# Fit a linear regression model with a seasonal component
library(lmtest)
lm_model <- tslm(ts_data ~ trend + season)

lm_model
# Plot the fitted model
plot(lm_model)

# Forecast the rainfall data for the next 12 months
library(forecast)
forecast <- forecast(lm_model, h = 12)

autoplot(forecast) +
  ggtitle("Forecasts of Rainfall using regression") +
  xlab("Year") + ylab("rainfall(mm)")
accuracy(forecast)

checkresiduals(lm_model)
summary(lm_model)

forecast

########################################











