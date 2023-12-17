#install.packages("reticulate")
#install.packages("tseries")
#install.packages("httr2")
#library(httr2)
#install.packages("pracma")
#install.packages('prophet')

library(httr)
library(jsonlite)
library(corrr)
library(plotly)
library(reticulate)
py_config()
library(tseries)
library(pracma)
library(data.table)
library(prophet)

pdf("output_plot.pdf")

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








# Crea un candlestick chart
candlestick <- plot_ly(df, type = "candlestick", x = ~timestamp, open = ~open, high = ~high, low = ~low, close = ~close)

# Customiza el layout 
candlestick <- candlestick %>% layout(title = "Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Muestra el chart
candlestick

# Corremos el test Dickey-Fuller para evaluar stationary
df$timestamp <- as.POSIXct(df$timestamp)
close_prices <- data$close
close_time_series <- ts(close_prices)
result <- adf.test(close_time_series)
print(result)

# p-value is greater that 0.05 hence we fail to reject the null hyphotesis. The time series is non-stationary
# El p-valor es mayor que 0.05 por lo tanto falla en rechazar la hipotesis nula. La serie de tiempo es no estacionaria


data$close <- as.numeric(data$close)
# Calculate the first differences of the 'Close' column
# Calculamos la primera diferencia de la serie

diff_close <- c(NA, diff(data$close))

# Removemos NAs
diff_close_no_na <- diff_close[!is.na(diff_close)]
# Aplicamos nuevamente el test de Dickey-Fuller
result <- adf.test(diff_close_no_na)
print(result)

# we got a p-value of 0.01 so we can reject the null hyphotesis and conclude that the time series is now stationary
# Therefore, it is necessary to model the first difference of the series:
# Obtenemos un p-valor de 0.01 por lo que rechazamos la hipotesis nula y concluimos que la serie de tiempo es ahora estacionaria
# Por lo tanto, es necesario modelar la primera diferencia de la serie
data$diff_close <- diff_close


#### Decomposicion 
#La función stl() realiza la descomposición de los datos de series temporales en componentes estacionales, de tendencia y residuales utilizando un procedimiento de descomposición estacional basado en Loess.

#El argumento s.window = "periodic" especifica que se debe usar una ventana periódica para la componente estacional.

#Convertimos la columna 'close' a numérica.
data$close <- as.numeric(as.character(data$close))

data$close <- as.ts(df$close)

# graficamos la serie
plot(data$close)



seasonal_decompose <- function(data_column, frequency = 12) {
  # Convertir la columna a una serie temporal.
  ts_data <- ts(data_column, frequency = frequency)
  
  # Aplicar descomposición estacional.
  decomposed_data <- stl(ts_data, s.window = "periodic")
  
  # Acceder a los componentes descompuestos.
  seasonal <- decomposed_data$time.series[, "seasonal"]
  trend <- decomposed_data$time.series[, "trend"]
  remainder <- decomposed_data$time.series[, "remainder"]
  
  # Set names for the components for plotting
  #setnames(decomposed_data$time.series, c("seasonal", "trend", "remainder"), 
  #         c("Componente Estacional", "Componente de Tendencia", "Componente de Residuo"))
  
  # Establecer un diseño de gráficos en 3 filas y 1 columna
  par(mfrow = c(4, 1))
  
  # Graficar la serie original.
  plot(data_column)
  
  # Graficar los componentes
  plot(decomposed_data$time.series[, "seasonal"], main = "Componente Estacional")
  plot(decomposed_data$time.series[, "trend"], main = "Componente de Tendencia")
  plot(decomposed_data$time.series[, "remainder"], main = "Componente de Residuo")
}

#Llamamos a la funcion para toda la serie
seasonal_decompose(data$close, frequency = 7)  

# Llamamos a la funcion para el ultimo mes
last_month_close<- tail(data$close, 30)

seasonal_decompose(last_month_close, frequency = 7) 

#Hay una estacionalidad semanal, pero su contribución es muy pequeña, por lo que se puede ignorar.


# Separamos en train, validation y test

get_train_valid_test_ts <- function(df, forecasting_days, target = "close") {
  # Obtener los conjuntos de datos de entrenamiento, validación y prueba con el target para la serie temporal
  
  # Preparacion de los datos
  df <- na.omit(df)
  df <- df[, c("timestamp", "close")]
  names(df) <- c("ds", "y")
  
  N <- nrow(df)
  train <- df[1:(N - 2 * forecasting_days - 1), ]
  valid <- df[(N - 2 * forecasting_days):(N - forecasting_days - 1), ]
  test <- df[(N - forecasting_days):N, ]
  
  # Train+valid - para entrenamiento del modelo final
  train_valid <- rbind(train, valid)
  
  cat(sprintf("El dataset original tiene %d registros y %d features\n", nrow(df), ncol(df)))
  cat(sprintf("El dataset de entrenamiento cuenta con %d registros\n", nrow(train)))
  cat(sprintf("El dataset de validacion cuenta con %d registros\n", nrow(valid)))
  cat(sprintf("El dataset de pruebas cuenta con %d registros\n", nrow(test)))
  
  return(list(train = train, valid = valid, test = test, train_valid = train_valid))
}

forecasting_days <- 10
data <- get_train_valid_test_ts(data, forecasting_days)

# Accedemos a los datasets resultantes
train <- data$train
valid <- data$valid
test <- data$test
train_valid <- data$train_valid




# PROPHET
# Results of all models
result <- data.frame(
  name_model = character(),
  type_data = character(),
  r2_score = numeric(),
  rmse = numeric(),
  mape = numeric(),
  params = I(list()),
  ypred = I(list())
)

# FUNCIONES REQUERIDAS
# Funcion para calcular metricas
calc_metrics <- function(type_score, list_true, list_pred) {
  # Calculation score with type=type_score for list_true and list_pred 
  if (type_score == 'rmse') {
    score <- sqrt(mean((list_true - list_pred)^2))
  } else if (type_score == 'mape') {
    score <- mean(abs((list_true - list_pred) / list_true)) * 100
  }
  return(score)
}

# Funcion que agrega las metricas al dataset resultante
result_add_metrics <- function(result, n, y_true, y_pred) {
  # Cálculo y adición de métricas al dataset result[n, :].

  #result[n, 'r2_score'] <- calc_metrics('r2_score', y_true, y_pred)
  result[n, 'rmse'] <- calc_metrics('rmse', y_true, y_pred)      # in coins
  result[n, 'mape'] <- calc_metrics('mape', y_true, y_pred)  # in %

  return(result)
}


cryptocurrency <- 'BTCUSDT'
name_model <- 'Prophet'
prophet_modeling <- function(result, 
                             cryptocurrency, 
                             train, 
                             test,
                             period_days,
                             fourier_order_seasonality,
                             forecasting_period,
                             name_model,
                             type_data) {
  # Realiza el entrenamiento del modelo FB Prophet para el dataset de entrenamiento dado y seasonality_mode
  # Realiza pronósticos con un periodo por este modelo, visualización y estimación de errores
  # df - dataset con datos reales en el período de pronóstico
  # Guarda los resultados en el dataset result
  
  # Construir un modelo Prophet con parámetros y estructura
  model <- prophet(
    daily.seasonality = FALSE,
    weekly.seasonality = FALSE,
    yearly.seasonality = FALSE,
    changepoint.range = 1,
    changepoint.prior.scale = 0.5,
    seasonality.mode = 'multiplicative'
  )
  
  model <- add_seasonality(
    model,
    name = 'seasonality',
    period = period_days,
    fourier.order = fourier_order_seasonality,
    mode = 'multiplicative',
    prior.scale = 0.5
  )
  
  # Entrenamiento
  model <- fit.prophet(model, train)
  
  # Hacer una prediccion
  future <- make_future_dataframe(model, periods = forecasting_period)
  forecast <- predict(model, future)
  print(forecast)

  
  # Dibuja el gráfico de los valores con datos de pronóstico
  plot_result <- plot(model, forecast, xlabel = 'Date', ylabel = paste0(name_model, " for ", cryptocurrency))
  print(plot_result)
  
  # Pronosticar datos con el modelo
  forecast_ds <- forecast$ds[(nrow(forecast) - forecasting_period + 1):nrow(forecast)]
  ypred <- forecast$yhat[(nrow(forecast) - forecasting_period + 1):nrow(forecast)]
  

  # Guarda los resultados
  n <- nrow(result)
  new_row <- data.frame(
    name_model = paste0("Prophet_", name_model),
    type_data = type_data,
    params = c(period_days, fourier_order_seasonality),
    ypred = ypred,
    ytrue = test$y
  )

  # agrega new_row al dataset
  result <- rbind(result, new_row)
  print(result)
  return (result)
}



is_Prophet <- TRUE
# Convertimos 'test$y' a numerico
test$y <- as.numeric(test$y)
test <- test[1:(nrow(test) - 1), , drop = FALSE]
valid$y <- as.numeric(valid$y)

if (is_Prophet) {
  for (period_days in c(4, 5, 7, 14, 30)) {
    for (fourier_order_seasonality in c(3, 12)) {
      result <- prophet_modeling(result,
                                 cryptocurrency,
                                 train,
                                 valid,
                                 period_days,
                                 fourier_order_seasonality,
                                 forecasting_days,
                                 paste0(period_days, "_days_", fourier_order_seasonality, "_order"),
                                 'valid')
    }
  }
}

# Crea un dataframe vacio para almacenar los resultados
unique_models <- unique(result$name_model)
result_df <- data.frame(matrix(ncol = 4, nrow = length(unique_models)))
colnames(result_df) <- c("name_model","data", "rmse", "mape") #"r2_score",

# Recorre cada modelo único
for (i in seq_along(unique_models)) {
  model <- unique_models[i]
  subset_data <- result[result$name_model == model, ]
  
  # Calcula las metricas
  y_true <- subset_data$ytrue
  y_pred <- subset_data$ypred
  data <- 'valid'
  metrics <- c(#calc_metrics('r2_score', y_true, y_pred),
               calc_metrics('rmse', y_true, y_pred),
               calc_metrics('mape', y_true, y_pred))
  
  # Agrega las metricas al resultado
  result_df[i, ] <- c(model, data, metrics)
}

# Muestra el dataframe resultante
print(result_df)


# Testeando el modelo con los hiperparametros optimos

test_result <- data.frame(
  name_model = character(),
  type_data = character(),
  rmse = numeric(),
  mape = numeric(),
  params = I(list()),
  ypred = I(list())
)

test_result <- prophet_modeling(test_result,
                           cryptocurrency,
                           train_valid,
                           test,
                           14,
                           12,
                           forecasting_days,
                           paste0(period_days, "_days_", fourier_order_seasonality, "_order"),
                           'test')

model <- unique(test_result$name_model)
test_result_df <- data.frame(matrix(ncol = 4, nrow = 1))
colnames(test_result_df) <- c("name_model", "data", "rmse", "mape")
subset_data <- test_result[test_result$name_model == model, ]

# Calculamos las metricas
y_true <- subset_data$ytrue
y_pred <- subset_data$ypred
data <- 'test'
metrics <- c(#calc_metrics('r2_score', y_true, y_pred),
              calc_metrics('rmse', y_true, y_pred),
              calc_metrics('mape', y_true, y_pred))

# Agregamos las metricas al dataframe
test_result_df[1, ] <- c(model, data, metrics)
print(test_result_df)



# Y vs Yhat
# Agregamos la columna ypred al df test
test <- cbind(test, ypred = test_result$ypred)

# Generamos el grafico
plot <- plot_ly(test, x = ~ds)
plot <- add_trace(plot, y = ~y, type = 'scatter', mode = 'markers', name = 'y', marker = list(color = 'green'))
plot <- add_trace(plot, y = ~ypred, type = 'scatter', mode = 'markers', name = 'ypred', marker = list(color = 'red'))
# Cambiamos el layout
plot <- layout(plot, title = "Actual vs Predicted", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
plot

dev.off()
