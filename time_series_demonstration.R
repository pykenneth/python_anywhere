install.packages('forecast')
install.packages('tseries')
# Import the necessary libraries.
library(tidyverse)
library(forecast)
library(tseries)
# Import the data set
FRED <-read.csv('FRED.csv')
# Convert the first column into date time format
FRED$DATE <- as.Date(FRED$DATE, format = "%d/%m/%Y")
colnames(FRED) <- c("Date","Index")

# Covert the data frame into time-series data
FRED_ts <- ts(FRED$Index,
              start = c(1972,1),
              frequency = 12)
plot(FRED_ts)

# View a window of plot from the dataset
plot(window(FRED_ts,1972,1974))
boxplot(FRED_ts~cycle(FRED_ts),
        xlab = "Month",
        ylab = "Index",
        main = "confectionary product by month")

# Decompose the time series index
FRED_components <- decompose(FRED_ts)

# Plot the components
plot(FRED_components)

# View specific components - "Seasonal"
plot(window(FRED_components$seasonal, c(1972,1),c(1974,12)))

# Testing for stationarity and autocorrelation
# First, we'll run an augmented Dickey-Fuller (ADF) test
# to check whether our time series is stationary and then
# an ACF plot, where each bar represents the size 
# and the direction of correlation. 
# (Hint: If the time series is stationary, 
# we'll use the ARMA model; however, 
# if we have a non-stationary time series, we'll use the ARIMA model.)

# H0: The time series is not stationary.
# H1: The time series is stationary.
# Test stationarity with augmented ADF test.
adf.test(FRED_ts)
# Our p-value is 0.01 and, therefore, less than 5% (0.05). 
# We can reject the H0 
# and conclude that our time series data is stationary. 

# Review random time series variables.
FRED_components$random

# Plot values, and remove NA values while doing so.
autoplot(acf(na.remove(FRED_components$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic()

# Plot random variables to check the distribution.
hist(FRED_components$random)

# Fit the model to our time series. 
arima_FRED_ts <- auto.arima(FRED_ts)
summary(arima_FRED_ts)

# Make a forecast for the next three months.
forecast3_FRED_ts <- forecast(arima_FRED_ts, 3)

# Plot the forecast on a graph.
autoplot(forecast3_FRED_ts) + theme_classic()

# Print the values in the data frame.
forecast3_FRED_ts

# Prediction for 5 years:
# Extend the prediction, set data source and time span, and assign a new object.
forecast2_FRED_ts_60 <- forecast(arima_FRED_ts, 60)
plot(forecast2_FRED_ts_60)

# Prediction for 10 years:
# Extend the prediction, set data source and time span, and assign a new object.
forecast2_FRED_ts_120 <- forecast(arima_FRED_ts, 120)
plot(forecast2_FRED_ts_120)

# Prediction for 20 years:
# Extend the prediction, set data source and time span, and assign a new object.
forecast2_FRED_ts_240 <- forecast(arima_FRED_ts, 240)
plot(forecast2_FRED_ts_240)

# Testing for accuracy
# Split the data into two parts: train and test
# We test against the trained data by subseting past 12 months
FRED_train_ts <-window(FRED_ts, 
                       start=c(1972,1),
                       end=c(2020,12))
FRED_test_ts <- window(FRED_ts,
                       start=c(2021,1),
                       end=c(2021,12))
# View new data frames.
head(FRED_train_ts)
head(FRED_test_ts)

forecast_FRED_train_ts<-forecast(auto.arima(FRED_train_ts),12)
autoplot(forecast_FRED_train_ts)+ theme_classic()

# Add the autolayer(), and specify the data set and series name.
autoplot(forecast_FRED_train_ts) +
  autolayer(FRED_train_ts, series='Train') +
  autolayer(FRED_test_ts, series='Test') +
  theme_classic()

# Check the accuracy of the test and forecasted values
accuracy(forecast_FRED_train_ts, FRED_test_ts)
