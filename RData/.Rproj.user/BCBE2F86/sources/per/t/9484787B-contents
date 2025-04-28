library(readxl)
library(dplyr)
library(timeSeries)
library(forecast)

datos <- read_excel("Files/ventas.xlsx",sheet =1 , col_names = T)
View(datos)

s<- ts(datos$CIERE, start = c(1986,1),frequency = 12)

s.ts <- ts(s)
class(s.ts)

class(s.ts)
head(s.ts)
plot(s.ts)

s.ts.a <- ts(s, start = 2001)
s.ts.a
plot(s.ts.a)
s.ts.m <- ts(s, start = c(2001,1), frequency = 12)
s.ts.m
plot(s.ts.m)

s.ts.q <- ts(s, start = 2001, frequency = 4)
s.ts.q
plot(s.ts.q)

start(s.ts.m)
end(s.ts.m)
frequency(s.ts.m)

start(s.ts.q)
end(s.ts.q)
frequency(s.ts.q)

#--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


datos <- read_excel("Files/ventas.xlsx",sheet =1 , col_names = T)
View(datos)
datos
datos.ts<- ts(datos$CIERE, start = c(1986,1),frequency = 12)
plot(datos.ts,type="o")
datos.l <- log(datos.ts)

datos.stl<- stl(datos.ts, s.window = "period")
plot(datos.stl)

datos.hw <- HoltWinters(datos.l)

plot(datos.hw, col = "blue",  col.predicted = "red", type = "l", lwd = 3)

datos.fore <- forecast(datos.hw, h=12)
plot(datos.fore, type = "l", lwd = 3)

plot(datos.fore, type = "l", lwd = 3, xlim = c(1986, 2025))
title(main = "Predicción con Holt-Winters", xlab = "Año", ylab = "Valor")

predicciones<-as.data.frame(datos.fore)
View(predicciones)

#xxxxxxxxxxxxxxxxxxxxxx  validamos el modelo con datos reales xxxxxxxxxxx

end(datos.ts)

# Ajusta el modelo Holt-Winters hasta 2018 para ver como funcna con los otros datos
hw.model <- HoltWinters(window(datos.ts, end = c(2016, 12)))

# Predice los valores para los dos años 
hw.forecast <- forecast(hw.model, h = 48)

# Lee los datos reales de los últimos 2 años para esta prueba
real_data_last_years <- window(datos.ts, start = c(2017, 1), end = c(2020, 12))

# Grafica las predicciones y los datos reales
plot(real_data_last_years, col = "blue", type = "o", lwd = 3, main = "Holt-Winters Forecast vs datos reales")
lines(hw.forecast$mean, col = "red",lwd = 3)

# Calcula el error de predicción 
errors <- real_data_last_years - hw.forecast$mean
mean_absolute_error <- mean(abs(errors))
mean_squared_error <- mean(errors^2)



