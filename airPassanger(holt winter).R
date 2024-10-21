#QUESTION 3
# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the AirPassengers data
data("C:/Users/Yashraj/Downloads/airpassangers.csv")

# Plot the original data
plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Year")

# Apply Holt-Winters Multiplicative Model
holt_winters_model <- HoltWinters(AirPassengers, gamma = TRUE, seasonal = "multiplicative")

# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)
forecasted_values

# Plot the forecasts
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of Passengers", xlab = "Year")


#Code2
# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the built-in AirPassengers dataset
data(AirPassengers)

# Plot the original data
plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Year")

# Apply Holt-Winters Multiplicative Model
holt_winters_model <- HoltWinters(AirPassengers, seasonal = "multiplicative")

# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)

# Display forecasted values
print(forecasted_values)

# Plot the forecast along with the original data
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of Passengers", xlab = "Year")


