
#Q1
#AR(1) Process
#Consider an AR(1) process defined by the equation:
  #Xt=0.5Xt−1+ϵt

# Load necessary libraries
library(forecast)
library(tseries)
#Q-1))
# 1 (a) Simulate 100 observations from AR(1) process
set.seed(42)
n <- 100
phi <- 0.5
ar1_process <- arima.sim(model = list(ar = phi), n = n)



# 1 (b) Plot the time series
plot.ts(ar1_process, main = "Simulated AR(1) Process", ylab = "X_t", col = "blue")


# 1 (c) Estimate AR(1) parameter using arima function
ar1_fit <- arima(ar1_process, order = c(1, 0, 0))
phi_estimated <- ar1_fit$coef[1]
phi_estimated  # Print estimated phi

#ϕ=0.5192709 suggests that the AR(1) process has a moderate level of autocorrelation and the current observation is moderately dependent on the immediate previous one.

# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ar1_process, main = "ACF of AR(1) Process")
pacf(ar1_process, main = "PACF of AR(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid
#The ACF shows a slow decay, and the PACF cuts off after lag 1. These are typical characteristics of an AR(1) process.

# 3 (i) Fit AR(1) or AR(2) model to the data
ar_fit <- arima(ar1_process, order = c(1, 0, 0))  # AR(1)
ar_fit_2 <- arima(ar1_process, order = c(2, 0, 0))  # AR(2)

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ar_fit, h = 10)
plot(forecasted_values, main = "Forecast of AR(1) Process", col = "red")

# Compare AIC of AR(1) and AR(2) models
aic_ar1 <- AIC(ar_fit)
aic_ar2 <- AIC(ar_fit_2)
cat("AIC for AR(1):", aic_ar1, "\nAIC for AR(2):", aic_ar2, "\n")
#The AR(1) model is better suited for your data based on the AIC values. Therefore, the single-lag AR(1) model is sufficient and preferable over the AR(2) model.







