#Q1 
#Analyze given time series data according to simple exponential series by considering alpha α=0.3
#Use holts exponential smoothing method with α=0.3 and β=0.2 smooth the given data.
#Conclude your results by comparing both the plots with original series.
#t	observation 
#1996	150.3
#1997	150.9
#1998	151.4
#1999	151.9
#2000	152.5
#2001	152.9


#QUESTION 1
# Load necessary libraries
install.packages("forecast")
library(forecast)
library(readxl)  # Load the library to read Excel files

# Load the data from Excel
dataa <- read_excel("C:/Users/Yashraj/Downloads/Book1.xlsx")
# Prepare the time series data
years <- dataa$t
obs<- dataa$observation
ts_data <- ts(obs, start = min(years), frequency = 1)

# Simple Exponential Smoothing
alpha <- 0.3
ses_model <- ses(ts_data, alpha = alpha)
ses_model

# Holt’s Exponential Smoothing
beta <- 0.2
holt_model <- holt(ts_data, alpha = alpha, beta = beta)
holt_model

# Plot the original data and both smoothed models on the same graph
plot(ts_data, type = "o", col = "blue", xlab = "Year", ylab = "Observation", main = "Time Series with Smoothing")
lines(ses_model$fitted, col = "red", lty = 1)  # SES smoothed values
lines(holt_model$fitted, col = "green", lty = 2)  # Holt's smoothed values

# Add a legend
legend("topleft", legend = c("Original Data", "SES Smoothed", "Holt's Smoothed"),
       col = c("blue", "red", "green"), lty = 1:2, pch = 1)

#SES is useful for time series data where the observations fluctuate around a constant mean (i.e., no clear trend). The forecast remains flat, with predictions close to the recent historical values.
#Holt’s method is more suitable for time series data that exhibit a trend, as it captures both the level and the trend components. It is better for making forecasts where you expect a continuation of the trend.
#In this case, since there is a visible increasing trend in the data, Holt’s Exponential Smoothing provides a more accurate model for long-term forecasts compared to SES, which ignores the trend and offers constant predictions.

