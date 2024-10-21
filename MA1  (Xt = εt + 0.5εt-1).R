#ii.	Q2 
#Consider an MA(1) Process:
  #Xt = εt + 0.5εt-1
#where εt is white noise with mean 0 and variance 1.
#1.	

# Load necessary libraries
library(forecast)
library(tseries)

# 1 (a) Simulate 100 observations from MA(1) process
set.seed(42)
n <- 100
theta <- 0.5
ma1_process <- arima.sim(model = list(ma = theta), n = n)

# 1 (b) Plot the time series
plot.ts(ma1_process, main = "Simulated MA(1) Process", ylab = "X_t", col = "blue")

# 1 (c) Estimate MA(1) parameter using arima function
ma1_fit <- arima(ma1_process, order = c(0, 0, 1))
theta_estimated <- ma1_fit$coef[1]
theta_estimated  # Print estimated theta

# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ma1_process, main = "ACF of MA(1) Process")
pacf(ma1_process, main = "PACF of MA(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid
#Both plots indicate that the process is likely an MA(1) since the ACF cuts off quickly after lag 1, and the PACF shows a quick decay after lag 1.
# 3 (i) Fit MA(1) or MA(2) model to the data
ma_fit <- arima(ma1_process, order = c(0, 0, 1))  # MA(1) model
ma_fit_2 <- arima(ma1_process, order = c(0, 0, 2))  # MA(2) model

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ma_fit, h = 10)
plot(forecasted_values, main = "Forecast of MA(1) Process", col = "red")

