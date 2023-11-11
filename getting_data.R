library(httr)
library(jsonlite)
library(corrr)
library(plotly)
#install.packages("tseries")
library(tseries)
#install.packages("httr2")
#library(httr2)
#install.packages("pracma")
library(pracma)
# Load the required library for setnames function
library(data.table)

#Column 1 (Timestamp): Unix timestamp or time in milliseconds (Time at the start of the interval).
#Column 2 (Open): Opening price at the beginning of the interval.
#Column 3 (High): Highest price during the interval.
#Column 4 (Low): Lowest price during the interval.
#Column 5 (Close): Closing price at the end of the interval.
#Column 6 (Volume): Volume of the traded asset during the interval.
#Column 7 (Close Time): The time at the end of the interval.
#Column 8 (Quote Asset Volume): The volume of the asset being traded against during the interval.
#Column 9 (Number of Trades): The number of trades that occurred during the interval.
#Column 10 (Taker Buy Base Asset Volume): Taker buy base asset volume during the interval.
#Column 11 (Taker Buy Quote Asset Volume): Taker buy quote asset volume during the interval.
#Column 12 (Ignore): This column might represent some auxiliary or ignored data.
#You can interpret this data as OHLCV (Open, High, Low, Close, Volume) candlestick data that's commonly used in financial analysis, particularly in the context of cryptocurrency trading.




# Specify the API endpoint and parameters
endpoint <- "https://api.binance.com/api/v3/klines"
start_time <- as.numeric(as.POSIXct("2023-01-01"))  * 1000
#end_time <- as.numeric(as.POSIXct("2023-10-01")) 
params <- list(symbol = "BTCUSDT", interval = "1d",  limit=1000)# startTime = start_time , endTime = end_time),
# Make a GET request to the Binance API
response <- GET(url = endpoint, query = params)

# Parse JSON response
if (status_code(response) == 200) {
  data <- content(response, "text")
  parsed_data <- fromJSON(data)
  # The parsed_data will contain OHLCV data and other information
  
  # Convert the matrix into a dataframe
  df <- as.data.frame(parsed_data)
  
  # Assign appropriate column names
  colnames(df) <- tolower(c("Timestamp", "Open", "High", "Low", "Close", "Volume", "Close_Time", "Quote_Asset_Volume", "Number_of_Trades", "Taker_Buy_Base_Asset_Volume", "Taker_Buy_Quote_Asset_Volume", "Ignore"))
  
  # Convert timestamp to a readable date-time format
  df$timestamp <- as.POSIXct(as.numeric(df$timestamp) / 1000, origin = "1970-01-01")
  data <- df[, c("timestamp", "open", "high", "low", "close", "volume")]
  
  
} else {
  print("Error fetching data")
}



variables_numericas <- data[, c("open", "high", "low", "close", "volume")]

variables_numericas <- sapply(variables_numericas, as.numeric)

variables_numericas%>% 
  correlate() %>% 
  rplot()








# Create a candlestick chart
candlestick <- plot_ly(df, type = "candlestick", x = ~timestamp, open = ~open, high = ~high, low = ~low, close = ~close)

# Customize the chart layout 
candlestick <- candlestick %>% layout(title = "Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Show the chart
candlestick

#Perform a Dickey-Fuller test to evaluate stationary
df$timestamp <- as.POSIXct(df$timestamp)
close_prices <- data$close
close_time_series <- ts(close_prices)
result <- adf.test(close_time_series)
print(result)

# p-value is greater that 0.05 hence we fail to reject the null hyphotesis. The time series is non-stationary



data$close <- as.numeric(data$close)
# Calculate the first differences of the 'Close' column
#diff_close <- diff(data$close)
diff_close <- c(NA, diff(data$close))
# Remove any NA values resulting from differencing
diff_close_no_na <- diff_close[!is.na(diff_close)]
#
result <- adf.test(diff_close_no_na)
print(result)
# we got a p-value of 0.01 so we can reject the null hyphotesis and conclude that the time series is now stationary
# Therefore, it is necessary to model the first difference of the series:
data$diff_close <- diff_close


#### Decomposition
#The stl() function performs decomposition of the time series data into seasonal, trend, and remainder components using a seasonal-trend decomposition procedure based on Loess.
#The s.window = "periodic" argument specifies that a periodic window should be used for the seasonal component.
# Convert 'close' column to numeric
data$close <- as.numeric(as.character(data$close))

data$close <- as.ts(df$close)

# Plot the time series
plot(data$close)



seasonal_decompose <- function(data_column, frequency = 12) {
  # Convert the column to a time series
  ts_data <- ts(data_column, frequency = frequency)
  
  # Apply seasonal decomposition
  decomposed_data <- stl(ts_data, s.window = "periodic")
  
  # Access the decomposed components
  seasonal <- decomposed_data$time.series[, "seasonal"]
  trend <- decomposed_data$time.series[, "trend"]
  remainder <- decomposed_data$time.series[, "remainder"]
  
  # Set names for the components for plotting
  #setnames(decomposed_data$time.series, c("seasonal", "trend", "remainder"), 
  #         c("Componente Estacional", "Componente de Tendencia", "Componente de Residuo"))
  
  # Establecer un diseño de gráficos en 3 filas y 1 columna
  par(mfrow = c(4, 1))
  
  # Plot the original serie
  plot(data_column)
  # Plot the decomposed components
  plot(decomposed_data$time.series[, "seasonal"], main = "Componente Estacional")
  plot(decomposed_data$time.series[, "trend"], main = "Componente de Tendencia")
  plot(decomposed_data$time.series[, "remainder"], main = "Componente de Residuo")
}

#call the function for the complete close
seasonal_decompose(data$close, frequency = 7)  

# call the function for the last month
last_month_close<- tail(data$close, 30)

seasonal_decompose(last_month_close, frequency = 7) 

#There is a weekly seasonality, but its contribution is very small, so it can be neglected.




