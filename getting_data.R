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
library(ggplot2)
library(forecast)# Contiene el modelo ARIMA
#install.packages("astsa")
library(astsa)

#pdf("output_plot.pdf")

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




# Especificamos el API endpoint y los parametros
endpoint <- "https://api.binance.com/api/v3/klines"
params <- list(symbol = "BTCUSDT", interval = "1d",  limit=1000)

# Hacer un GET request a la API de Binance
response <- GET(url = endpoint, query = params)

# Parseamos la respuesta JSON 
if (status_code(response) == 200) {
  data <- content(response, "text")
  
  # parsed_data contiene OHLCV y otra informacion
  parsed_data <- fromJSON(data)
  
  # Convertimos a dataframe
  df <- as.data.frame(parsed_data)
  
  # Asignamos el nombre apropiado a las columnas
  colnames(df) <- tolower(c("Timestamp", "Open", "High", "Low", "Close", "Volume", "Close_Time", "Quote_Asset_Volume", "Number_of_Trades", "Taker_Buy_Base_Asset_Volume", "Taker_Buy_Quote_Asset_Volume", "Ignore"))
  
  # Convertimos timestamp a un formato datetime legible
  df$timestamp <- as.POSIXct(as.numeric(df$timestamp) / 1000, origin = "1970-01-01")
  data <- df[, c("timestamp", "open", "high", "low", "close", "volume")]
  
} else {
  print("Error al extraer los datos")
}


# Candlestick
# Crea un candlestick chart
candlestick <- plot_ly(df, type = "candlestick", x = ~timestamp, open = ~open, high = ~high, low = ~low, close = ~close)

# Customiza el layout 
candlestick <- candlestick %>% layout(title = "Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Muestra el chart
candlestick


# Corremos el test Dickey-Fuller para evaluar estacionaridad
df$timestamp <- as.POSIXct(df$timestamp)
close_prices <- data$close
close_time_series <- ts(close_prices)
result <- adf.test(close_time_series)
print(result)

# El p-valor es mayor que 0.05 por lo tanto falla en rechazar la hipotesis nula. La serie de tiempo es no estacionaria


data$close <- as.numeric(data$close)

# Calculamos la primera diferencia de la serie

diff_close <- c(NA, diff(data$close))

# Removemos NAs
diff_close_no_na <- diff_close[!is.na(diff_close)]

# Aplicamos nuevamente el test de Dickey-Fuller
result <- adf.test(diff_close_no_na)
print(result)

# Obtenemos un p-valor de 0.01 por lo que rechazamos la hipotesis nula y concluimos que la serie de tiempo es ahora estacionaria
# Por lo tanto, es necesario modelar la primera diferencia de la serie
data$diff_close <- diff_close


#### Decomposicion 
#La función stl() realiza la descomposición de los datos de series temporales en componentes estacionales,
#de tendencia y residuales utilizando un procedimiento de descomposición estacional basado en Loess.

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






#******************************* ARIMA ******************************* 

#ARIMA es un modelo que utiliza la información de los valores pasados de la serie de tiempo para predecir los valores futuros.
#Cualquier serie de tiempo "no estacional" que muestre patrones y no sea un ruido blanco aleatorio se puede modelar con modelos ARIMA.
#Su nombre es un abreviatura de “AutoRegressive Integrated Moving Average” en inglés, donde "AR" (siglas para Autoregressive)
#, MA (siglas para Moving Average) e I (siglas para Integrated).

#El modelo usa 3 parámetros p, q y d(no necesariamente deben estar todos presentes)
# p es el orden del término AR, autoregresivo
# d es el número de diferenciaciones necesarias para que la serie de tiempo se vuelva estacionaria
# q es el orden del término MA, media móvil

# AR(p) Autoregresión – un modelo de regresión que utiliza la relación dependiente entre una observación actual y las observaciones durante un período anterior.
#Un componente autorregresivo (AR(p)) se refiere al uso de los valores pasados en la ecuación de regresión de la serie de tiempo.

# I(d) Integración – utiliza la diferenciación de observaciones.
#La diferenciación implica la resta de los valores actuales de una serie con sus valores anteriores un número determinado de veces.

# MA(q) Media Móvil– Un componente de media móvil representa el error del modelo como una combinación de términos de errores anteriores. 
#El orden q representa el número de términos que se incluirán en el modelo.



#1. Cómo calcular el orden de diferenciación (d) 

#El objetivo de la diferenciación es lograr que la serie de tiempo sea estacionaria. 
#Debemos tener cuidado de no diferenciar demasiado la serie porque poría afectar el modelo
#El orden correcto de diferenciación es la diferenciación mínima requerida para obtener una serie estacionaria.
#Si la serie de tiempo es estacionaria, d es igual a cero y no requiere que se realice diferenciacion.
#Una serie es estacionaria cuando su media, varianza y autocovarianza son invariantes en el tiempo
#Si la serie no se vuelve estacionaria con 1 o 2 diferencias, se recomienda usar diferencias con logaritmo.



# Convertir a formato numérico y transformamos en tipo serie de tiempo 
test$y <- as.numeric(test$y)
valid$y <- as.numeric(valid$y)
test$y <- as.numeric(test$y)
train_valid$y <- as.numeric(train_valid$y)

ts_train <-ts(train$y)
ts_valid <-ts(valid$y)
ts_test <- ts(test$y)
ts_train_valid <- ts(train_valid)

#en el grafico observamos que podria no ser una serie estacionaria
#par(mar = c(1, 1, 1, 1))
plot(ts_train)

#realizamos el test Augmented Dickey Fuller (Prueba ADF) para ver si la serie de tiempo es estacionaria
#La hipótesis nula (Ho) de la prueba ADF es que la serie temporal no es estacionaria
result <- adf.test(ts_train)
print(result)
#el p-valor es 0.6539 es mayor a 0.05, en consecuencia no rechazamos la Ho (la serie NO es estacionaria)


#calculamos la primera diferencia

train$y <- as.numeric(train$y)
train_time_series_d1 <-c(NA, diff(train$y))
train_time_series_d1_na <- diff_close[!is.na(train_time_series_d1)]

#verificamos con el test Augmented Dickey Fuller (Prueba ADF)
result <- adf.test(train_time_series_d1_na)
print(result)
#el p-valor es 0.01 es menor a 0.05, con una diferenciación logramos que la serie de tiempo sea estacionaria

#En conclusión, el valor para el parámetro de orden de diferencia(d) del modelo ARIMA es igual a 1

#2. Cómo calcular el orden del término p

#Para identificar si el modelo necesita algún término AR, analizamos el gráfico de autocorrelación parcial (PACF).

#La función de autocorrelación parcial mide la correlación entre dos variables separadas por k períodos cuando no se considera la depedencia creada por los retardos intermedios entre ambas variables

#La autocorrelación parcial del lag (k) de una serie es el coeficiente de ese lag en la ecuación de autorregresión de Y.

#Yt=α0+α1Yt−1+α2Yt−2+α3Yt−3

# si Y_t es la serie actual e Y_t-1 es el lag 1 de Y, entonces la autocorrelación parcial del lag 3 (Y_t-3) es el coeficiente
#α3 de Y_t-3 en la ecuación anterior.

#Para calcular tenemos en cuenta los valores significativos
pacf(train_time_series_d1_na, main='Función de autocorrelación parcial (PACF) -Serie de Tiempo')

#el valor de (p) es 9, es un valor grande pero lo tomaremos como posible candidato.
#Otro canditato sera el valor 12

#3 Cómo calcular el orden del término MA(q) 

#Para encontrar el número de términos de AR, utilizamos el gráfico ACF.
#El gráfico ACF representa a la función de autocorrelación que mide la correlación entre dos variables separadas por k períodos


acf(train_time_series_d1_na,frequency =1, main='Función de autocorrelación (ACF) -Serie de Tiempo')

#el valor de (q) es 0, otros potenciales candidatos podrían ser 9 y 12, aunque son valores grandes


#4. Luego de identificar los parámetros podemos ajustar el modelo ARIMA

#usamos arima del paquete y libreria forecast , donde order es p,d,q
modelo1 = arima(as.numeric(ts_train),order = c(9,1,0))
summary(modelo1) #para ver las MAE, RMSE

tsdisplay(residuals(modelo1))

#para ver si el modelo esta bien ajustado tiene que haber ruido blanco, los errores deben tener media igual a cero, no estar correlacionados
#y la varianza debe ser constante.

Box.test(modelo1$residuals, lag = 1, type = "Ljung-Box") 
#si p-valor es 0.979, es mayor a 0.05 los residuos son independientes

shapiro.test(modelo1$residuals)
#el p-valor es menor a 0.05 los residuos siguen una distribucion normal
#CONCLUSION EL MODELO ESTA BIEN AJUSTADO


#modelo 2
modelo2 = arima(as.numeric(ts_train),order = c(12,1,0))
summary(modelo2) #para ver las MAE, RMSE

#para ver si el modelo esta bien ajustado tiene que haber ruido blanco, los errores deben tener media igual a cero, no estar correlacionados
#y la varianza debe ser constante.

Box.test(modelo2$residuals, lag = 1, type = "Ljung-Box") 
#si p-valor es 0.8957, es mayor a 0.05 los residuos son independientes

shapiro.test(modelo2$residuals)
#el p-valor es menor a 0.05 los residuos siguen una distribucion normal
#CONCLUSION EL MODELO ESTA BIEN AJUSTADO


# 5. Construcción del modelo ARIMA automaticamente
#Utilizamos la opción auto.arima para luego compararlo con el modelo que hicimos identificando a mano los parámetros

modelo3=auto.arima(as.numeric(ts_train))
summary(modelo3)
#los parámetros elegidos automaticamente son p=0, d=1 y q =0
tsdisplay(residuals(modelo3))

#validacion de los residuos
Box.test(modelo3$residuals, lag = 1, type = "Ljung-Box")
#los residuos son independientes, el p-valor es mayor a 0.05

shapiro.test(modelo3$residuals) 
#los residuos siguen una distribucion normal

#El menor RMSE lo obtuvo el modelo con p=12, d=1, q=0, valor 1216.05 
#El menor MAPE lo obtuvo el modelo con p=9, d=1, q=0, valor 2.219797 




#*******************************  Forecast ******************************* 
pronostico_m1 <- forecast(modelo1, h = length(ts_valid))
pronostico_m2 <- forecast(modelo2, h = length(ts_valid))
pronostico_m3 <- forecast(modelo3, h = length(ts_valid))
pronostico_m1 <- ts(pronostico_m1$mean, start = start(ts_valid), frequency = frequency(ts_valid))
pronostico_m2 <- ts(pronostico_m2$mean, start = start(ts_valid), frequency = frequency(ts_valid))
pronostico_m3 <- ts(pronostico_m3$mean, start = start(ts_valid), frequency = frequency(ts_valid))


# Metricas Modelo 1
rmse <- sqrt(mean((ts_valid - pronostico_m1)^2))
mape <- mean(abs((ts_valid - pronostico_m1) / ts_valid)) * 100
cat("RMSE:", rmse, "\n") # en cuanto difieren en promedio las predicciones del valor real
cat("MAPE:", mape, "%\n") # mide el error porcentual promedio

# Metricas Modelo 2
rmse <- sqrt(mean((ts_valid - pronostico_m2)^2))
mape <- mean(abs((ts_valid - pronostico_m2) / ts_valid)) * 100
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#  Metricas Modelo 3
rmse <- sqrt(mean((ts_valid - pronostico_m3)^2))
mape <- mean(abs((ts_valid - pronostico_m3) / ts_valid)) * 100
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

# El modelo con mejores resultados es el modelo 2

# Vamos a reentrenar el modelo 2 pero con utilizando train_valid y evaluar en test
modelo2 = arima(as.numeric(ts_train_valid),order = c(12,1,0))
pronostico_m2 <- forecast(modelo2, h = length(ts_test))
pronostico_m2 <- ts(pronostico_m2$mean, start = start(ts_test), frequency = frequency(ts_test))


# Calcular RMSE
rmse <- sqrt(mean((ts_test - pronostico_m1)^2))
# Calcular MAPE
mape <- mean(abs((ts_test - pronostico_m1) / ts_valid)) * 100
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
#******************************* ******************************* 




































#*******************************  PROPHET ******************************* 
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
                             period_days, # número de días que el modelo debería considerar al ajustar las estacionalidades
                             fourier_order_seasonality, # el orden de las funciones de Fourier 
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
  for (period_days in c(4, 5, 7, 14)) { # número de días que el modelo debería considerar al ajustar las estacionalidades
    for (fourier_order_seasonality in c(3, 12)) { # orden de la serie de fourier
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
                           14, # número de días que el modelo debería considerar al ajustar las estacionalidades
                           12, # orden de la serie de fourier
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



#******************************* Y vs Yhat ******************************* 
# Agregamos la columna ypred al df test
test <- cbind(test, ypred = test_result$ypred)

# Generamos el grafico
plot <- plot_ly(test, x = ~ds)
plot <- add_trace(plot, y = ~y, type = 'scatter', mode = 'markers', name = 'y', marker = list(color = 'green'))
plot <- add_trace(plot, y = ~ypred, type = 'scatter', mode = 'markers', name = 'ypred', marker = list(color = 'red'))
# Cambiamos el layout
plot <- layout(plot, title = "Actual vs Predicted", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
plot

#dev.off()
