# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Set up the Data
data <- data.frame(
  Quarter = 1:12,
  Sales = c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
)

# Convert the sales data to a time series object
ts_sales <- ts(data$Sales, start = 1, frequency = 1)

# Step 2: Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up the plotting area
acf(ts_sales, main = "ACF of Sales Data")
pacf(ts_sales, main = "PACF of Sales Data")

# Step 3: Fit an ARMA(1,1) Model
model_arma <- arima(ts_sales, order = c(1, 0, 1))
summary(model_arma)

# Step 4: Check the Residual Diagnostics
checkresiduals(model_arma)

# Step 5: Forecast the Next 12 Values
forecasted_values <- forecast(model_arma, h = 12)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Sales Values", 
     xlab = "Quarter", ylab = "Sales", ylim = c(480, 800))
lines(ts_sales, col = "blue", lwd = 2)  # Original values
lines(fitted(model_arma), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")

