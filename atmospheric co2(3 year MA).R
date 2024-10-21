Q.2 Given table denotes Atmospheric CO2 Concentration at Mauna Lao from year 1991-2003.
a) Make time series plot of the given data.
b) Forecast 2004 value by 3 yearly moving average smoothing method.
Year 	Average CO2 Concentration
1991	355.62
1992	356.36
1993	357.1
1994	358.86


#QUESTION 2
# Load necessary libraries
install.packages("ggplot2")
library(ggplot2)  # For enhanced plotting

# Prepare the data
years <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 368.3, 369.47, 371.03, 373.61, 357.61)
ts_co2 <- ts(co2_concentration, start = min(years), frequency = 1)

# a) Time Series Plot
plot(ts_co2, type = "o", col = "blue", xlab = "Year", ylab = "Average CO2 Concentration", main = "CO2 Concentration Over Time")

# Calculate the 3-year moving average manually
moving_avg_manual <- function(data, window_size) {
  n <- length(data)
  ma <- rep(NA, n)  # Initialize with NA
  for (i in seq(window_size, n)) {
    ma[i] <- mean(data[(i-window_size+1):i])
  }
  return(ma)
}

# Calculate the 3-year moving average
window_size <- 3
ma_co2 <- moving_avg_manual(co2_concentration, window_size)

# Forecast for 2004 using the last moving average value
forecast_2004 <- tail(ma_co2, 1)

# Display the forecast
cat("Forecasted CO2 Concentration for 2004 using 3-Year Moving Average: ", forecast_2004, "\n")

