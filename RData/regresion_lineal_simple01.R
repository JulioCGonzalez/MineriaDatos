library(car)
library(ggplot2)
library(xlsx)
library(dplyr)


install.packages("boot")
install.packages("QuantPsyc")


datos<-read.xlsx("files/sales.xlsx",sheetIndex = 1)

names(datos)<-c("publicidad","cantidad_ventas")
View(datos)
names(datos)
attach(datos)

glimpse(datos)

# revisar valores atipicos
boxplot(datos$publicidad)
boxplot(datos$cantidad_ventas)
# ver si hay nulos en el conjunto de datos
which( is.na(datos))
# ver  cuales son los valores atipicos
datos %>% filter(publicidad<=2000) %>% 
  summarise(mediana=median(publicidad))
# ver la concentracion de los datos 
hist(datos$publicidad,freq = F, col = "green")
lines(density(datos$publicidad),col="red",lty = 2, lwd = 3)

datos$publicidadlog<-log(datos$publicidad)
hist(datos$publicidadlog,freq = F, col = "green")
lines(density(datos$publicidadlog),col="red",lty = 2, lwd = 3)

# tratar los valores atipicos

outlayers <- function(data, inferior, superior) {
  data[data < inferior] <- mean(data)
  data[data > superior] <- median(data)
  data
}

# agregarmos la nueva columna con los datos tratados
cdatos<-datos %>% mutate(cpublicidad= outlayers( datos$publicidad,10,1900))
boxplot(cdatos$cpublicidad)

modelo=lm(data = datos,cantidad_ventas ~ publicidad)

summary(modelo)
# p-value tiene que ser mayor que 0.05  para ser valido

# Multiple R-squared:  0.3346,
# lo anterior nos da que tanto explica las ventas la publicidad 
# lo ideal es que Multiple R-squared  este mas cercano a 1 pero el 0.2784 de es muy bajo.

# entonces explica que :

#Ventas = intercepto + Beta1 + error
#ventas = 1.34 + 9.61 * publicidad

#quiere decir que por cada ($1.34) las ventas se incrementa en 9.61

#la correlacion de pearson 
#
cor.test(cdatos$cpublicidad,cdatos$cantidad_ventas)
#  A medida que aumenta la publicidad aunmentan las ventas
sqrt(0.2784)


plot(cdatos$cpublicidad,cdatos$cantidad_ventas,xlab = "Publicidad", 
     ylab = "Ventas",main = "Prueba de regresion lineal")

abline(modelo,col="blue")

# Graficamos mas  bonito 
ggplot(cdatos,aes(cpublicidad,cantidad_ventas))+
  geom_point(colour="red")+
  geom_smooth(method = "lm",colour="blue")

#datos_nuevos<-data.frame(publicidad=5000,level=0.95,interval="prediction")
#
predict(modelo,data.frame(cpublicidad=1000))
boxplot(cdatos$cpublicidad)

resultado<-data.frame(cdatos$cpublicidad,
                      cdatos$cantidad_ventas,
                      modelo$fitted.values,
                      round(cdatos$cantidad_ventas-modelo$fitted.values)
)

names(resultado)<-c("publicidad","cantidad_ventas","Predicho" ,"diferencia")

View(resultado)



# R-squared y R-squared ajustado
cat("R-squared:", summary(modelo)$r.squared, "\n")
cat("Adjusted R-squared:", summary(modelo)$adj.r.squared, "\n")

# Error Cuadrático Medio (MSE)
mse <- mean((modelo$residuals)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Gráficos de Residuos
par(mfrow=c(2,2))
plot(modelo)
