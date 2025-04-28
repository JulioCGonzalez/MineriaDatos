#Ejemplo practivo 1 - Arina y Gasolina
prices <- read.csv("files/prices.csv")
head(prices)
View(prices)
# los datos son desde 1980 y son mensuales
prices.ts <- ts(prices, start = c(1980,1), 
                frequency = 12)
prices.ts
plot(prices.ts,type="o" ,col = 1:2)
#poner todo en un solo panel plot.type = "single" y los colones
plot(prices.ts, plot.type = "single", col = 1:2, type="o")
# agregar las leyendas para saber cual es cual 
legend("topleft", colnames(prices.ts), col = 1:2, lty = 1)

#Descomposición de una serie temporal 
#stl
#La descomposición de una serie temporal ayuda a identificar patrones y tendencias en la serie temporal y a predecir su comportamiento futuro.
# Aplicamos log para eliminar las variaciones irreglulares del la serie
flour.l <- log(prices.ts[,1])
#stl = Seasonal Decomposition of Time Series by Loess
flour.stl<- stl(flour.l, s.window = "period")
plot(flour.stl)
flour.stl

gas.l <- log(prices.ts[,2])
gas.stl <- stl(gas.l, s.window = "period")
plot(gas.stl)

#install.packages("TSstudio")
#library(TSstudio)
#ts_seasonal(flour.l,type = "normal")
#ts_seasonal(flour.l,type = "box")
#ts_seasonal(flour.l,type = "all")
#ts_decompose(flour.l,type = "both")

#decompose
#Classical Seasonal Decomposition by Moving Averages

flour.dec <- decompose(flour.l)
plot(flour.dec)
ts_decompose(flour.l,type = "both")
gas.dec <- decompose(gas.l)
plot(gas.dec)

gas.season.adjusted <- prices.ts[,2] - (gas.dec$seasonal)
plot(gas.season.adjusted)

# periodo de muestreo
n <- 6
# sides =2 es bilateral sides=1 es unilateral filtra los 6 anteriores 
# 2 es 7  hace promedios 3 atras y 3 adelantes para localizar mejor
# las tendencias
# los pesos se hace con  rep(1/n, n)
#prices.ts[,2] es la gasolina prices.ts[,1] es la arina 
gas.f.1 <- filter(prices.ts[,2], filter = rep(1/n, n), sides = 2)
gas.f.2 <- filter(prices.ts[,2], filter = rep(1/n,n), sides = 1)

plot(prices.ts[,2],lwd = 3) # esta es la gasolina 

lines(gas.f.1, col = "blue", lwd = 3)
lines(gas.f.2, col = "red", lwd = 3)

library(forecast)
# Ajustar el modelo de Holt-Winters

head(prices.ts)
hw.model <- hw(prices.ts[, 2])
end(prices.ts)
hw.forecast <- forecast(hw.model, h = 12)


# Graficar la serie de tiempo, los datos ajustados y los pronósticos
plot(hw.model, main = "Holt-Winters Forecast for Gasoline Prices")
lines(hw.forecast$mean, col = "red")
#ver las predicciones
hw.forecast
# Accede a los pronósticos
pronosticos <- hw.forecast$mean



################################################################################################################

# para hacer un suavizado las curvas eliminar el potencial ruido
inf <- read.csv("files/INFY-monthly.csv")
tail(inf)

inf.ts <- ts(inf$Adj.Close, start=c(1999,3), frequency = 12)
inf.ts
plot(inf.ts)
inf.hw <- HoltWinters(inf.ts)

plot(inf.hw, col = "blue", col.predicted = "red",type="l")

#suma de los errores al cuadrados entre los reales y los ajustados
inf.hw$SSE
inf.hw$alpha
inf.hw$beta
inf.hw$gamma
# ver los valores ajustados las previsiones
head(inf.hw$fitted)

install.packages("forecast")
library(forecast)

# predecir mas alla de los datos 12 significa a 1 año
infy.fore <- forecast(inf.hw, h=12)
plot(infy.fore)

# revisamos si estamos por debajo u por encima de los datos reales
# de los intervalos de confianza
# prediccion para los proximos 24 meses
# para que la preduccion sea buena debo estar dentro del 80/95 del lower
# o del 80/95 del apper
infy.fore$lower
infy.fore$upper


#modelo autorregresivo integrado de media móvil ARIMA
inf <- read.csv("files/INFY-monthly.csv")
inf.ts <- ts(inf$Adj.Close, start = c(1999,3), frequency = 12)
inf.arima <- auto.arima(inf.ts)
#AIC AICc BIC para validar el modelos
#AICc Akaique corregido
# Creiterio de informacion Akaike y bayesiano
summary(inf.arima)

ts_plot(inf.ts)

inf.fore <- forecast(inf.arima, h = 12)
inf.fore$lower
inf.fore$upper
# arima se usa a corto plazo a medio o largo se habren mucho los 
# intervalos de confianza
plot(inf.fore, col = "red",fcol = "blue",type = "o")







