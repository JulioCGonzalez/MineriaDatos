
install.packages(c("dply","ggplot"),dependencies = T)

install.packages("ggplot2")
install.packages("stringr")
install.packages("dplyr")
install.packages("heatmaply")
library(hexbin)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(visdat) 
library(naniar)
library(ggplot2)
library(stringr)
library(visdat)
library(heatmaply)



getwd()
setwd()





datos<-12
rm(datos)
datos<-read.csv(file = "Files/Pasajeros.csv",header = T,sep = ",",encoding = "UTF-8")
View(datos)
x=1
class(datos)
head(datos)
class(datos)
names(datos)
glimpse(datos)
str(datos)
dim(datos)
colnames(datos)
rownames(datos)
colnames(datos)<-c("aeropuerto","mes","anio","nacionales","extranjeros","transito","exentos","total")
# cambiar el nombre a una sola columna
names(datos)[1]<-"aeropuerto"

datos$nacionales

datos$aeropuerto<-str_trim(str_replace(datos$aeropuerto,"Aeropuerto Internacional",""))
unique(datos$aeropuerto)

datos[1,c(1,3,5)]
datos[,]
datos[c(2,4,5),3:7]

datos %>% select("mes","anio")
prueba<-datos %>% select("mes","anio")
datos$anio
datos$mes<- as.factor(datos$mes)
datos$fecha<-as.Date(datos$fecha)

View(datos)

# acceder a columnas y filas de un dataframe

datos[,]
datos[,1]
datos[c(1:3),c(1,2)]
datos[,c(1:3)]

# conversiones de datos
datos$anio<-as.factor(datos$anio)

#crear o modificar columnas de un dataframe
datos<-datos %>%  mutate(sdnacionales=sqrt(nacionales) )
View(datos)
datos$sdnacionales<-NULL

class(datos)
datos[,c(1,2,3,4)]
# conversion  de tipos  de datos en columnas
datos %>% select(aeropuerto,nacionales)
datos_aeropuerto<-datos %>% select(aeropuerto,nacionales)


is.na(datos)        # Vector lógico con T==NA
any(is.na(datos))   # TRUE = hay al menos un valor NA
anyNA(datos)        # Alternativa a lo anterior
which(is.na(datos)) # Indica las coordenadas donde están los NA
mean(is.na(datos))  # Porcentaje de valores NA
sum(is.na(datos))   # Cantidad de valores perdidos en el vector
table(datos$nacionales, useNA = "always")

sum(is.na(datos)) / length(datos) #proporción de valores faltantes

apply(is.na(datos), 2, mean)   # Porcentaje de NA por columna
apply(is.na(datos), 2, sum)    # Cantidad de NA por columna
apply(is.na(datos), 2, which)  # Posición de NA por columna

sapply(datos, function(x) sum(is.na(x)))

which(is.na(datos[ , 5]))

heatmaply::heatmaply_na(datos) # ver los na de forma grafica


names(datos)
vis_miss(datos ,sort_miss = TRUE, cluster = TRUE)
datos %>% gg_miss_upset()

#Eliminar filas con nulos en una columna concreta
datos <- datos[!is.na(datos$id),]
datos <- datos[!is.na(datos$texto),]
head(datos, n= 2)

#Eliminar todas las filas que contengan algun valor nulo

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}


mifuncion<-function(valor1,valor2) {
  resultado = valor1+valor2
  return( resultado)
}

valores <-mifuncion(2,4)


# Generar algunas graficas

plot(datos$nacionales)


ggplot(datos, aes(anio,nacionales)) +
  #geom_hex() +
  geom_jitter(width =.3, alpha = 0.5) +
  labs(title="Diagrama de dispersión",
       subtitle= "ScatterPlot",
       caption="Fuente: CPS1985 (paquete AER)",
       x="Años",
       y="Cantidad de nacionales") +
  scale_fill_continuous("Pasajeros nacionales") +
  theme_classic()


hist(datos$nacionales, main = "Histograma de las ventas",
     xlab = "pasajeros", ylab = "Frecuencia",
     col = "purple")


boxplot(datos$nacionales,boxwex=0.8)
boxplot(datos$anio,boxwex=0.8)
unique(datos$aeropuerto)

library(dplyr)
nacionales_ae1<-datos %>% filter(aeropuerto=="Limón" )

boxplot(nacionales ~ anio, 
        col = rainbow(ncol(nacionales_ae1)),
        data = nacionales_ae1,
        boxwex=0.8,
        main = "Nacionales por mes",
        scale_y_continuous(labels=scales::comma)
)
stripchart(data=datos,nacionales ~ anio,vertical = TRUE, 
           method = "jitter",pch=19,add = TRUE,
           col = 1:12
)

unique(datos$aeropuerto)

ggplot(datos, aes(x =aeropuerto , y = nacionales, fill = aeropuerto) ) +
  geom_boxplot(alpha = 0.3, outlier.colour = "blue") +
  labs(x = "Nacionales", y = "Pasajeros") +
  guides(fill = FALSE) +
  coord_flip() +              
  geom_point(stat =  "summary", fun.y = mean, shape = 16, size = 4, color = "red") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.2) 


atipicos<-function(df,valorx,valory){
  grafico <- ggplot(df, aes(x =valorx , y = valory, fill = valorx) ) +
    geom_boxplot(alpha = 0.3, outlier.colour = "blue") +
    labs(x = "Nacionales", y = "Pasajeros") +
    guides(fill = FALSE) +
    coord_flip() +              
    geom_point(stat =  "summary", fun.y = mean, shape = 16, size = 4, color = "red") +
    geom_point(position = position_jitter(width = 0.1), alpha = 0.2) 
  
  return(grafico)
} 

conjunto <-datos %>% filter(datos$aeropuerto=="Daniel Oduber Quirós")
unique(datos$aeropuerto)
atipicos(conjunto,conjunto$aeropuerto,conjunto$extranjeros)

valores<-datos$nacionales
valores<-na.omit(valores)



hist(valores, prob = TRUE,
     main = "Histograma con curva normal", ylab = "Densidad", col="ivory")
options(scipen = 99999)
x <- seq(min(valores), max(valores), length = length(valores))
f <- dnorm(x, mean = mean(valores), sd = sd(valores))
lines(x, f, col = "red", lwd = 2)
lines(density(valores), lwd = 2, col = 'blue')
legend("topright", c("Histograma", "Densidad", "Normal"), box.lty = 0,
       lty = 1, col = c("black", "blue", "red"), lwd = c(1, 2, 2))
par(new = TRUE)
boxplot(valores, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



grupos<- datos %>% group_by(aeropuerto) %>% 
  summarise(totales=sum(total))

grupos
unique(datos$aeropuerto)

ggplot(grupos,aes(x=aeropuerto,y=totales,fill=aeropuerto))+
  geom_bar(stat="identity")+
  geom_text(aes(label = format(totales, big.mark= ".", trim = TRUE)), 
            position = "dodge",vjust = 0,fontface = "bold",size=5,color="blue"
  )+
  ggtitle("Grafico de pasajeros por aeropuerto")+
  xlab("Aeropuertos") +
  ylab("Totales") +
  scale_y_continuous(labels=scales::comma)+
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")+
  coord_flip()+
  theme(legend.position = "none")  


## El histograma 
hist(datos$nacionales) 
## El boxplot
boxplot(datos$nacionales)


grupo01<-datos %>% filter(aeropuerto=="Juan Santamaría")

ggplot(grupo01, aes(aeropuerto, nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")

frecuencia<-table(datos$nacionales,datos$mes)


#########################################

head(datos)
unique(datos$aeropuerto)

grupo02<-
  datos %>%  
  filter(aeropuerto == "Limón")

ggplot(grupo02, aes(grupo02$aeropuerto, grupo02$nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")


ggplot(grupo02, aes( x=mes, y=nacionales, fill=mes)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colores
               alpha = 0.9, outlier.colour = "red",size=1)+
  geom_point()+
  scale_y_continuous(name = "nacionales") +  # Etiqueta de la variable continua
  scale_x_discrete(name = "mes") +        # Etiqueta de los grupos
  stat_boxplot(geom = "errorbar", width = 0.5)+  # Bigotes
  geom_jitter(position=position_jitter(0.4),col="blue",size=2)+
  ggtitle("Boxplot por grupos en ggplot2") +       # Título del plot
  theme(axis.line = element_line(colour = "green", size = 0.5)) + # Personalización del tema
  scale_color_brewer(palette="RdBu")+ theme_minimal()+
  stat_summary(fun=mean,geom = "point",colour="darkred",size=3)+
  stat_summary(fun.data = mean_cl_boot,geom = "linerange",colour="red",size=3,alpha=0.2)


## grupo 3 

grupo03<-
  datos %>%  
  filter(aeropuerto == "Limón")

ggplot(grupo03, aes(aeropuerto, nacionales)) + 
  geom_boxplot(notch = TRUE,fill = "white", colour = "#3366FF")

limon<-grupo03$nacionales

head(limon)
dim(limon)


x2 <- seq(min(limon), max(limon), length = length(limon))
f <- dnorm(x2, mean(limon), sd(limon))


hist(limon,freq = FALSE,main = "Cambiar color", ylab = "Frecuencia", col = "lightblue")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
lines(density(limon), lwd = 2, col = 'red')
lines(density(limon), col = "blue", lwd = 2)
lines(x2, f, col = "red", lwd = 2) # Normal
par(new = TRUE)
boxplot(limon, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



calculo <-function(numero, valor){
  resultado <- numero * valor
  return(resultado) 
  
}

calculo(1052,520)

install.packages("ggstance")
library(ggstance)

nbreaks <- pretty(range(limon), n = nclass.Sturges(limon),
                  min.n = 1)
View(limon)
colnames(limon)<-c("provincia")

limondf<-as.data.frame(limon)
colnames(limondf)<-c("provincia")

head(datos)
limondf<-datos %>% select(aeropuerto,nacionales) %>% filter(aeropuerto=="Aeropuerto Internacional Daniel Oduber Quirós")

ggplot(limondf, aes(x = nacionales)) +
  geom_histogram( bins = 30,alpha=0.5 ,color = "blue", fill = "blue") +
  geom_boxploth(aes(y = 3), width = 2, color = "red") +
  theme_minimal() 

########## analizar la correlacion de las variables  ##########################

# La correlación es una medida estadística que expresa hasta qué punto dos
# variables están relacionadas linealmente

library(corrplot)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)
require(psych)
library(dplyr)
library(caret)

names(datos)

iris
variables<-data.frame(datos$nacionales,datos$extranjeros,datos$transito)
chart.Correlation(variables, histogram = TRUE, method = "pearson")

variables<-data.frame(datos$nacionales,datos$extranjeros,datos$transito)


ozono<-read.csv("Files/ozone.csv",header = T,sep = ";")
ozono<-na.omit(ozono)
variables<-data.frame(ozono$Humidity,ozono$Wind_speed,ozono$Visibility,ozono$pressure_height,ozono$Inversion_temperature    )

chart.Correlation(variables, histogram = TRUE, method = "pearson")
cor(iris[,c(1:4)])
corrplot.mixed(cor(ozono),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
library(mongolite)

mongo(
  collection = "test",
  db = "test",
  url = "mongodb://localhost",
  verbose = FALSE,
  options = ssl_options()
)
my_collection$insert(name_of_collection)
data <- mongo_db$find(query = '{}')

##########  analizar la importacia de las variables xxxxxxxxxxxxxxxxxxxxxxxxx

install.packages("lattice")
set.seed(4543)
data(mtcars)

library(randomForest)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
imp <- varImpPlot(mtcars.rf,type = 2) # let's save the varImp object

library(lattice)
dotplot(imp, scales=list(y =list(cex=1,
                                 at = c(1:10),
                                 col='red',
                                 rot =15,
                                 axs='i') ,
                         x =list(cex=1,col='blue')) )

# otra forma
library(data.table)
library(Boruta)
mtcars
boruta.model <- Boruta(mpg~., data = mtcars, doTrace = 2)
plot(boruta.model)
boxplot(mtcars$qsec)

# en otro juego de datos

ozono<-read.csv("Files/ozone.csv",header = T,sep = ";")
library(randomForest)
names(ozono)
ozono<-na.omit(ozono)
View(ozono)
ozono.rf <- randomForest(Humidity ~ ., data=ozono, ntree=1000, keep.forest=FALSE,
                         importance=TRUE)
imp <- varImpPlot(ozono.rf,type = 2) # let's save the varImp object
cor.plot(ozono)

library(lattice)
dotplot(imp, scales=list(y =list(cex=1,
                                 at = c(1:10),
                                 col='red',
                                 rot =15,
                                 axs='i') ,
                         x =list(cex=1,col='blue')) )


# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) 
rownames(imp) <- NULL  
imp$var_categ <- rep(1:2, 5) 

# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 
ggplot(imp, aes(x=reorder(varnames, IncNodePurity), weight=IncNodePurity, fill=as.factor(var_categ))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name")


ggplot(imp, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity, color=as.factor(var_categ))) + 
  geom_point(size=5) +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
  scale_color_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()

########################################



