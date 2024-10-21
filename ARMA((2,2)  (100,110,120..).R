# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Set up the Data
data <- data.frame(
  Time = 1:15,
  Values = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240)
)

# Convert the data to a time series object
ts_data <- ts(data$Values)

# Step 2: Plot the ACF and PACF of the Original Time Series
par(mfrow = c(1, 2))  # Set the layout to show two plots side by side
acf(ts_data, main = "ACF of Original Time Series")
pacf(ts_data, main = "PACF of Original Time Series")

# Step 3: Use auto.arima() for automatic differencing and model selection
model_arima <- auto.arima(ts_data)

# Display the summary of the fitted ARIMA model
print(summary(model_arima))
#Model Fit: The ARIMA(0,1,0) with drift seems to fit the training data exceptionally well, indicated by the low error metrics and the coefficients. However, the negative AIC and BIC values suggest that the model might be too simplistic, potentially overfitting the data.
#Drift Term: The positive drift of 10 indicates a consistent upward trend in the series.
#Error Metrics: Overall, the training set error metrics indicate a good fit, with low bias and small errors in the forecasts.

# Step 4: Check the Residual Diagnostics of the Fitted Model
checkresiduals(model_arima)

# Step 5: Forecast the Next 10 Values
forecasted_values <- forecast(model_arima, h = 10)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
# Plot forecast
plot(forecasted_values, main = "Original, Fitted, and Forecasted Values", 
     xlab = "Time", ylab = "Values", ylim = c(100, 300))

# Add original time series to the plot
lines(ts(data$Values), col = "blue", lwd = 2)

# Add fitted values to the plot
lines(fitted(model_arima), col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")




