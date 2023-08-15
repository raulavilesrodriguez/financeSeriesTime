library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(rsconnect)
library(DT)
library(devtools)
library(lattice)
library(latticeExtra)
library(TSA)
library(tseries)
library(pander)
library(zoo)
library(xts)
library(caret)
library(forecast) # forecasting time series ARIMA 
library(prophet) # forecasting time series
library(highcharter) #Interactive Plot

#------WRANGLING-------
# import the data
lines_sma <- read_excel(
  '1.1.1-Lineas-activas-por-servicio_y_Densidad_Abr-2023.xlsx', 
  sheet = "Líneas por servicio")
lines_sma
lines_sma <- lines_sma[-(1:7),]
lines_sma <- lines_sma[-(176:202),-c(18,19)]
lines_sma[1,(3:5)] <- 'CONECEL S.A.'
lines_sma[1,(8:10)] <- 'OTECEL S.A.'
lines_sma[1,(13:15)] <- 'CNT EP'
lines_sma[2, which(is.na(lines_sma[2, ]))] = ''
new_names <- sapply(lines_sma[1:2, ], paste, collapse=" ")
colnames(lines_sma) <- new_names
lines_sma <- lines_sma[-(1:2),]
lines_sma[is.na(lines_sma)] <- '0'
# transform of character to numeric
lines_sma[,-1] <- lapply(lines_sma[,-1], as.numeric)

# to transform to Date format the first column with regex and lubricate
lines_sma$`MES/AÑO ` <- str_replace(lines_sma$`MES/AÑO `, "^\\d+", paste('Dec', lines_sma$`MES/AÑO `))
lines_sma$`MES/AÑO ` <- str_replace(lines_sma$`MES/AÑO `, "Ene", "Jan")
lines_sma$`MES/AÑO ` <- str_replace(lines_sma$`MES/AÑO `, "Abr", "Apr")
lines_sma$`MES/AÑO ` <- str_replace(lines_sma$`MES/AÑO `, "Ago", "Aug")
lines_sma$`MES/AÑO ` <- my(lines_sma$`MES/AÑO `) # to character to date
sapply(lines_sma, class)

# transform tibble to ts
lines.total <- tibble(date = lines_sma$`MES/AÑO `, lines = lines_sma$`TOTAL NACIONAL DE LÍNEAS ACTIVAS `)
lines.total <- xts(lines.total$lines, lines.total$date)
colnames(lines.total) <- c('total')

#-----FORECASTING----
# the set seed
set.seed(1234)

months <- c("J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J")

xyplot(lines.total, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.text(x, y, labels = months)
})

# transform to ts (time serie)
lines.total.ts <- ts(lines.total["2016-01-01/2023-04-01"])
n <- 12 #future predictions
startPrediction <- length(lines.total.ts) + 1
endPrediction <- startPrediction + n - 1

historic <- data.frame(lines.total["2008-12-01/2015-12-01"])

# MODEL LINEAR REGRESSION
lines.total_fit1 <- lm(lines.total.ts ~ time(lines.total.ts))
summary(lines.total_fit1)

lines.total_rst <- rstudent(lines.total_fit1)
xyplot(lines.total_rst ~ time(lines.total_rst), type = "l",
       xlab = "Time", ylab = "Studentized residuals")

forecast_data1 <- sapply(c(startPrediction:endPrediction), function(x){
  lines.total_fit1$coefficients[1] + lines.total_fit1$coefficients[2]*x
})
forecast_data1

valuesLinear <- rbind(data.frame(linear = historic[['total']]),
                         data.frame(linear = c(lines.total_fit1$fitted.values)),
                         data.frame(linear = c(forecast_data1))
)
rownames(valuesLinear) <- NULL   #reset index numbers

# MODEL CUADRATIC REGRESSION
lines.total_fit2 <- lm(lines.total.ts ~ time(lines.total.ts) + I(time(lines.total.ts)^2))
summary(lines.total_fit2)

forecast_data2 <- sapply(c(startPrediction:endPrediction), function(x){
  lines.total_fit2$coefficients[1] + lines.total_fit2$coefficients[2]*x + lines.total_fit2$coefficients[3]*x^2
})
forecast_data2

valuesCuadratic <- rbind(data.frame(cuadratic = historic[['total']]),
                               data.frame(cuadratic = c(lines.total_fit2$fitted.values)),
                               data.frame(cuadratic = c(forecast_data2))
                              )
rownames(valuesCuadratic) <- NULL   #reset index numbers

# Model ARIMA MANUAL
arima_model1 <- arima(lines.total.ts, order = c(3, 1, 1))
predicted_values <- predict(arima_model1, n.ahead = n)
predicted_values$pred
valuesArima1 <- rbind(data.frame(arima.manual = historic[['total']]),
                      data.frame(arima.manual=c(lines.total.ts - arima_model1$residuals)), 
                      data.frame(arima.manual=c(predicted_values$pred)))

# Model ARIMA AUTO
arima_model2 <- auto.arima(lines.total.ts, seasonal = FALSE)
summary(arima_model2)
predicted_values2 <- forecast(arima_model2, h = n)
predicted_values2
valuesArima2 <- rbind(data.frame(arima.auto = historic[['total']]),
                      data.frame(arima.auto=c(arima_model2$fitted)), 
                      data.frame(arima.auto=c(predicted_values2$mean)))

# Model Prophet
df_p <- tibble(date = lines_sma$`MES/AÑO `, lines = lines_sma$`TOTAL NACIONAL DE LÍNEAS ACTIVAS `)
df_p <- df_p |> filter(date >= '2016-01-01')
colnames(df_p) <- c('ds', 'y')
prophet.model <- prophet(df_p)


#-------Analisis Predictions-------
start <- as.Date("2008-12-01")
t <- seq(from = start, length = nrow(lines.total) + n, by = "month")
predictions <- data.frame(date = t, 
                     linear = valuesLinear$linear,
                     cuadratic = valuesCuadratic$cuadratic,
                     arima.manual = valuesArima1$arima.manual,
                     arima.auto = valuesArima2$arima.auto
                     )

dfReal <- fortify.zoo(lines.total) #convert xts to dataframe
colnames(dfReal)[1] <- "date"
predictions <- predictions |> left_join(dfReal, by='date')
predictions['total'][is.na(predictions['total'])] <- 0
colnames(predictions)[6] <- "real"



#____________Compare the Algorithms__________________
# MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE <- function(y_actual, y_predict){
  mean(abs((y_actual - y_predict) / y_actual))*100
}

# R SQUARED error metric -- Coefficient of Determination
RSQUARED <- function(y_actual, y_predict){
  cor(y_actual,y_predict)^2
}

# R SQUARED Adjusted
ADJ_RSQUARED <- function(y_actual, y_predict){
  r2 <- cor(y_actual,y_predict)^2
  observations <- length(y_actual) # number of observations
  k <- 1 # number of predictors
  adj_r2 <- 1- ((1-r2) * (observations - 1) / (observations - k - 1)) 
  adj_r2
}

test <- data.frame(y = c(lines.total.ts))
# Linear Model
y_hat_linear <- data.frame((y = lines.total_fit1['fitted.values']))
names(y_hat_linear)[1] <- 'y'

linear_MAPE <- MAPE(test$y, y_hat_linear$y)
linear_MAPE
linear_r2 <- RSQUARED(test$y, y_hat_linear$y)
linear_r2
linear_adj_r2 <- ADJ_RSQUARED(test$y, y_hat_linear$y)
linear_adj_r2
linearAccuracy <- 100 - linear_MAPE
linearAccuracy

# Cuadratic Model
y_hat_cuadratic <- data.frame((y = lines.total_fit2['fitted.values']))
names(y_hat_cuadratic)[1] <- 'y'

cuadratic_MAPE <- MAPE(test$y, y_hat_cuadratic$y)
cuadratic_MAPE
cuadratic_r2 <- RSQUARED(test$y, y_hat_cuadratic$y)
cuadratic_r2
cuadratic_adj_r2 <- ADJ_RSQUARED(test$y, y_hat_cuadratic$y)
cuadratic_adj_r2
linearAccuracy <- 100 - cuadratic_MAPE
linearAccuracy

# ARIMA MANUAL Model
y_hat_arima_manual <- data.frame(arima.manual=c(lines.total.ts - arima_model1$residuals))
names(y_hat_arima_manual) <- 'y'

arimaManual_MAPE <- MAPE(test$y, y_hat_arima_manual$y)
arimaManual_MAPE
arimaManual_r2 <- RSQUARED(test$y, y_hat_arima_manual$y)
arimaManual_r2
arimaManual_adj_r2 <- ADJ_RSQUARED(test$y, y_hat_arima_manual$y)
arimaManual_adj_r2
arimaManualAccuracy <- 100 - arimaManual_MAPE
arimaManualAccuracy

# Arima AUTO Model
y_hat_arima_auto <- data.frame(y=c(arima_model2$fitted))

arimaAuto_MAPE <- MAPE(test$y, y_hat_arima_auto$y)
arimaAuto_MAPE
arimaAuto_r2 <- RSQUARED(test$y, y_hat_arima_auto$y)
arimaAuto_r2
arimaAuto_adj_r2 <- ADJ_RSQUARED(test$y, y_hat_arima_auto$y)
arimaAuto_adj_r2
arimaAutoAccuracy <- 100 - arimaAuto_MAPE
arimaAutoAccuracy


#Interactive Plot
highchart()|> 
  hc_add_series(predictions$linear) |>
  hc_title(text="<b>Mobile Service SMA</b>")
hchart(predictions, "line", hcaes(x = date, y = linear))
