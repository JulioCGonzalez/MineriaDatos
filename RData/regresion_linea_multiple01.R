#librerias
install.packages("GGally")
install.packages("caret")
install.packages(c("GGally", "corrplot", "PerformanceAnalytics", "psych", "dplyr", "caret"))

library(GGally)
library(corrplot)
library(PerformanceAnalytics)
require(psych)
library(dplyr)
library(caret)

list.files("Files/")

getwd()


datos<-read.csv("Files/rentadebicis.csv",fileEncoding = "latin1", header = T,sep = ",")
summary(datos)
plot(datos$rentas_totales)
plot(datos$rentas_totales, type = "b")
hist(datos$rentas_totales)


plot(datos$rentas_totales, type = "b", pch = 19, 
     col = "green", xlab = "x", ylab = "y")

View(table( cor(datos)))

corrplot(cor (datos),method = "circle")

names(datos)
View(datos)
variables<-data.frame(datos$temperatura,
                      datos$humedad,datos$sensacion_termica)
chart.Correlation(variables, histogram = TRUE, method = "pearson")


plot (datos$temperatura,datos$rentas_totales, 
      main = " influencia de la temperaturasobre las ventas",
      xlab = "temperatura",ylab = "cantidad de rentas")

c1 <- rainbow(10)
c2 <- rainbow(10, alpha=0.2)
c3 <- rainbow(10, v=0.7)


par(mfrow=c(3,5))

for (i in 1:14) {
  boxplot(datos[,i],main=names(datos)[i],col=c2, medcol=c3, whiskcol=c1, 
          staplecol=c3, boxcol=c3, outcol=c3, pch=23, cex=1)
}

# PARTICIONAMIENTO  DE LOS DATOS
t.id<-createDataPartition(datos$rentas_totales,p=0.7,list = F)
View(t.id)

# crear el modelo de regresion multiple
regresion <- lm(data = datos[t.id,], rentas_totales ~ 
                  hora+dia+mes+año+estacion+dia_de_la_semana+asueto+
                  temperatura+sensacion_termica+humedad+velocidad_del_viento)

regresion <- lm(data = datos[t.id,-c(12,13)], rentas_totales ~. )

step(regresion)

#Intervalos de confianza para los coeficientes del modelo
confint(regresion)

#evaluar el modelo para ver cuales son las mejores variables 
#para recalibrar el modelo
summary(regresion)

step(regresion, direction = "both", trace = 1)

# se selecionar las variables y se reprocesa el modelo
regresion <- lm(data = datos[t.id,], rentas_totales ~hora+mes+año)

# el modelo no es tan exacto con los residios se pueden distribuir los residuos
library(dplyr)
boxplot (regresion$residuals)

#Prediccion con los datos de testeo 
#
prediccion<-predict(regresion,datos[-t.id,-c(12,13)])

# Agregar predicciones al conjunto de datos de prueba
datos_predichos <- datos[-t.id,-c(12,13)]  # Copiar el conjunto de datos de prueba
datos_predichos$prediccion <- predict(regresion, newdata = datos[-t.id,-c(12,13)])  # Agregar la columna de predicciones

# Agregar predicciones redondeadas al conjunto de datos de prueba
datos_predichos$prediccion_redondeada <- round(datos_predichos$prediccion)

# Visualizar el conjunto de datos con las predicciones
head(datos_predichos)
View(datos_predichos)

# contruir el modelo de preduccion con datos nuevos

datos_nuevos<-data.frame(hora=1,mes=2,año=2024)
predict(regresion,datos_nuevos)


#El error cuadrático medio (MSE por sus siglas en inglés, Mean Squared Error) 
#es una medida que nos ayuda a entender cuán cerca están las predicciones de un 
#modelo estadístico de los valores reales. Para calcularlo, tomamos la diferencia entre 
#cada valor predicho y su valor real, lo elevamos al cuadrado para que todos los valores
#sean positivos y para dar más peso a los errores grandes, luego promediamos estos valores

#Error Cuadrático Medio (MSE): Mide la media de los errores cuadrados entre las predicciones y 
#los valores reales. Cuanto más bajo sea el MSE, mejor será el rendimiento del modelo.


mse <- mean((regresion$residuals)^2)

sqrt(mean((regresion$fitted.values-datos[t.id,-c(12,13)]$rentas_totales)^2))

# Calcular el Error Cuadrático Medio (ECM)
mse <- mean((datos_predichos$rentas_totales - datos_predichos$prediccion_redondeada)^2)

# Imprimir el Error Cuadrático Medio (ECM)
cat("Error Cuadrático Medio (MSE):", mse, "\n")

