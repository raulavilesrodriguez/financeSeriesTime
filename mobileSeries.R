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
library(pander)
library(zoo)
library(xts)

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


# the set seed
set.seed(1234)

months <- c("J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J")

xyplot(lines.total, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.text(x, y, labels = months)
})


