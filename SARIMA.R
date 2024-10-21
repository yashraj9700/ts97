
#Question 1: Data Preparation
#You are given a dataset containing monthly sales data for a retail store over the past five years. The data shows a clear seasonal pattern.
#a) Plot the time series and describe its key features (trend, seasonality, and noise).
#b) Before applying SARIMA, how would you transform the data to achieve stationarity?
  
library(forecast)

# Create the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Convert to time series object
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# Plot the sales data
plot(sales_ts, main = "Monthly Sales Data", xlab = "Time", ylab = "Sales", col = "blue", lwd = 2)

# Observations: Check for seasonality and trend by inspecting the plot

#B]
# Differencing to remove trend
diff_sales_ts <- diff(sales_ts, differences = 1)
plot(diff_sales_ts, main = "Differenced Sales Data", xlab = "Time", ylab = "Differenced Sales")

#Answer 1:
#a) After plotting the time series, we may observe a long-term increasing trend with a recurring seasonal pattern. The series might also show random fluctuations.
#b) To achieve stationarity, the data may require differencing (to remove the trend) and possibly seasonal differencing (to remove the seasonal component). Log transformation can also stabilize the variance.



#Question 2: Model Identification
#Given the same dataset:
  #a) Explain how you would identify the order of differencing required for the series (d in ARIMA).
#b) How would you determine the seasonal differencing term (D) for the SARIMA model?
 # c) Identify the potential AR and MA orders (p, q, P, Q) using the ACF and PACF plots.
#2) MODEL IDETIFICATION
#A)
# Perform Augmented Dickey-Fuller test for stationarity
library(tseries)
adf_test <- adf.test(sales_ts)
print(adf_test)
#The negative Dickey-Fuller statistic indicates that the time series does not have a unit root, reinforcing the conclusion of stationarity.
# If p-value > 0.05, differencing is needed (non-stationary).

#B)
# Seasonal differencing can be checked by examining the ACF plot
Acf(sales_ts)
# Look for seasonal lags in the ACF plot (e.g., significant peaks at 12 months).

#c)
# ACF and PACF plots
Acf(diff_sales_ts, main="ACF of Differenced Sales Data")
Pacf(diff_sales_ts, main="PACF of Differenced Sales Data")
#Answer 2:
#a) The order of differencing (d) can be determined by checking the stationarity of the series using methods like the Augmented Dickey-Fuller (ADF) test. If the p-value is above a threshold (e.g., 0.05), differencing is needed.
#b) The seasonal differencing term (D) is usually determined by checking for seasonality at the seasonal lag (e.g., 12 months for yearly seasonality). This can be confirmed through ACF plots showing peaks at seasonal lags.
#c) The ACF and PACF plots are used to identify the AR (p) and MA (q) terms, and the seasonal AR (P) and MA (Q) terms by looking at significant lags.


#Question 3: SARIMA Model Fitting
#After identifying the appropriate (p, d, q) and (P, D, Q, m) orders for the seasonal data:
#a) Fit a SARIMA model to the data.
#b) Interpret the model output and residual diagnostics.
#c) If the residuals show autocorrelation, what steps would you take to improve the model?
# Load necessary libraries
library(forecast)
library(tseries)

# Assuming 'sales_ts' is your time series data
sarima_model <- auto.arima(sales_ts, seasonal = TRUE)
summary(sarima_model)
# Check the summary of the fitted model
summary(sarima_model)

# Plot residuals to check for white noise
checkresiduals(sarima_model)

# Additional diagnostics
# ACF plot of residuals
Acf(residuals(sarima_model), main='ACF of Residuals')

# Histogram of residuals
hist(residuals(sarima_model), main='Histogram of Residuals', xlab='Residuals')

#Answer 3:
#a) Use SARIMA(p, d, q)(P, D, Q, s) to fit the model using statistical software like  R.
#b) Interpret the coefficients and check residual diagnostics, including ACF plots and tests for white noise (Ljung-Box test).
#c) If autocorrelation remains, consider refining the AR and MA terms, adding additional seasonal terms, or applying further differencing.

#Question 4: Forecasting with SARIMA
#Using the SARIMA model from the previous question:
#a) Forecast the sales for the next 12 months.
#b) Plot the forecasted values along with the original time series data. How well does the model capture the seasonality?
# Load necessary libraries
library(forecast)

# Forecast for the next 12 months
forecast_sales <- forecast(sarima_model, h = 12)

# Plot the forecasted values
plot(forecast_sales, main = "Sales Forecast for Next 12 Months", xlab = "Time", ylab = "Sales")
# Plot the forecasted values with original time series data
plot(sales_ts, main = "Sales Forecast with Actual Data", xlab = "Time", ylab = "Sales", xlim = c(start(sales_ts)[1], end(sales_ts)[1] + 1))
lines(forecast_sales$fitted, col = "blue") # Fit
lines(forecast_sales$mean, col = "red") # Forecasted values

# Add legend
legend("topright", legend = c("Actual Sales", "Fitted Values", "Forecasted Sales"), 
       col = c("black", "blue", "red"), lty = 1)

# Optional: Highlight the forecast confidence intervals
plot(forecast_sales, main = "Sales Forecast for Next 12 Months with CI", xlab = "Time", ylab = "Sales")

#Answer 4:
#a) Use the fitted SARIMA model to generate 12-month forecasts using the predict or forecast function.
#b) Plot the original and forecasted values. The SARIMA model should effectively capture the seasonality and trend, but model performance can be evaluated using metrics like RMSE or MAPE.

#Question 5: Model Comparison
#You decide to compare the SARIMA model with a simple ARIMA model:
#a) Fit an ARIMA model without seasonality.
#b) Compare the performance of the SARIMA and ARIMA models using appropriate evaluation metrics (AIC, BIC, RMSE).
#c) Based on your findings, which model would you recommend and why?
# Fit a simple ARIMA model without seasonal terms
arima_model <- auto.arima(sales_ts, seasonal = FALSE)

# Compare SARIMA and ARIMA using AIC, BIC, and RMSE
sarima_aic <- AIC(sarima_model)
arima_aic <- AIC(arima_model)
sarima_bic <- BIC(sarima_model)
arima_bic <- BIC(arima_model)
sarima_metrics <- accuracy(sarima_model)
arima_metrics <- accuracy(arima_model)

cat("SARIMA AIC:", sarima_aic, "ARIMA AIC:", arima_aic, "\n")
cat("SARIMA BIC:", sarima_bic, "ARIMA BIC:", arima_bic, "\n")
sarima_metrics
arima_metrics

#Answer 5:
#a) Fit an ARIMA model without seasonal terms using the same dataset.
#b) Compare the models using metrics like AIC (Akaike Information Criterion), BIC (Bayesian Information Criterion), and RMSE (Root Mean Squared Error). SARIMA typically performs better with seasonal data.
#c) SARIMA is usually recommended for datasets with strong seasonal components, as it captures the seasonality more effectively than ARIMA.
# Load necessary libraries



#Question 6: Holiday Adjustment in SARIMA
#Suppose the retail store experiences a surge in sales during certain holiday months, which is not captured by the original SARIMA model.
#a) How would you modify the SARIMA model to account for the holiday effect?
#b) Refit the modified SARIMA model and compare the results with the previous model.

#a)
# Suppose you have a holiday variable (1 for holiday, 0 for non-holiday)
# Add holiday variable as external regressor
holiday_effect <- c(rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1)

# Fit SARIMA with external regressors
sarima_with_holiday <- auto.arima(sales_ts, xreg = holiday_effect, seasonal = TRUE)
summary(sarima_with_holiday)


#b)
# Compare the SARIMA with and without holiday adjustments using AIC, BIC, and RMSE
sarima_holiday_aic <- AIC(sarima_with_holiday)
sarima_holiday_bic <- BIC(sarima_with_holiday)
sarima_holiday_rmse <- sqrt(mean(residuals(sarima_with_holiday)^2))

print(paste("SARIMA with holiday AIC:", sarima_holiday_aic, "BIC:", sarima_holiday_bic, "RMSE:", sarima_holiday_rmse))
#Based on the AIC, BIC, and error metrics, it seems that including holiday adjustments in the SARIMA model did not enhance the model's predictive capabilities and may have actually degraded it.
#Answer 6:
#a) Incorporate holiday effects as external regressors (exogenous variables) in the SARIMA model.
#b) After refitting the model with the holiday adjustment, check whether the model's performance improves by comparing metrics like AIC, BIC, or RMSE.

