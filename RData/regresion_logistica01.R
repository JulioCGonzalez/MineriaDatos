# 
datos<-read.csv("files/titanic.csv",header = T,sep = ",")
attach(datos)
# realizamos el EDA
View(datos)
datos<- na.omit(datos)
names(datos)
library(dplyr)
library(caret)
glimpse(datos)
head(datos,2)
datos_modelo<-datos %>%  select(Pclass,Sex,Age,Survived)
datos_modelo$Survived <- factor(datos_modelo$Survived, levels = c(0,1))

attach(datos_modelo)
names(datos_modelo)
head(datos_modelo)

c2<-"red"
c1<-"blue"
c3<-"green"

boxplot(Pclass,main="Clase del pasajero",
        col=c2, medcol=c3, whiskcol=c1, staplecol=c3, 
        boxcol=c3, outcol=c3, pch=23, cex=1)

boxplot(Age,main="Edad del pasajero", pch=23, cex=2,col="blue")

which(is.na(datos_modelo))
mean(is.na(datos_modelo)) 
sum(is.na(datos_modelo$Age))
dim(datos_modelo)
summary(datos_modelo)

#podemos eliminar columnas con valores nulos si lo decidimos
#datos_modelo<-na.omit(datos_modelo)

datos_modelo<-na.omit(datos_modelo)
which(is.na(datos_modelo))

casos_edad <-datos_modelo %>% mutate(grupo=ifelse(Age >54,1,2)) %>% 
  group_by(grupo) %>% 
  summarise(total=n())
casos_edad$grupo<-as.factor(casos_edad$grupo)

casos_edad <-casos_edad %>% mutate(porcentage = total / sum(total)*100)

#stat = "identity" es para graficar  una variable CHAR con una numerica
#position = "dodge"  es por si esta aplicado

ggplot(casos_edad,aes(x=grupo,y=porcentage))+
  geom_bar(stat = "identity",fill = c("dodgerblue4","firebrick4"))+
  ggtitle ("Grafico de grupo de edades")+
  theme (plot.title = element_text(size=rel(1), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) + #Separación entre líneas
  geom_text(aes(label=paste0(round(porcentage,2),"% - ",total)),size=6, color="blue",vjust=-0.3) +
  labs(x = "Grupos de Edad",y = "Porcentaje") + # Etiquetas o títulos de los ejes
  #theme(axis.title = element_text(face="italic", colour="brown", size=rel(1.5))) # Tamaño de los títulos de los ejes
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="orange", size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1))) 



ggplot(casos_edad,aes(x=grupo,y=total,fill=grupo)) + 
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=total),size=10, color="blue")



# hacemos la particion de los datos 
set.seed(5252)
View(datos_modelo)

#Tipo de Variables (Dependiente y Independientes) 

datos_modelo<-datos %>%  select(Pclass,Sex,Age,Survived)
datos_modelo$Survived <- factor(datos_modelo$Survived, levels = c(0,1))

train <- createDataPartition(datos_modelo$Survived, p=0.7, list = F)

glm_mod<-glm( formula = Survived ~ .,data = datos_modelo[train,] , 
              family = "binomial")

summary(glm_mod)

# generamos las probabilidades del modelos
probabilidad <-predict(glm_mod,type = "response")

#creamos la curva ROC
#el area bajo la curva AUC debe  ser mayor a 0.5 para decir que el modelo se puede usar
#ver el resultdo del modelo.. en % de efectividad
#el punto donde decide 1  o 0 se debe revisar para hacer la clasificacion

library(pROC)

curva <-roc(datos_modelo[train,"Survived"],
            probabilidad,auc = T,
            ci=T,
            levels = c(0, 1)
            #,   direction = "<"
)

print(curva)
##windows(height = 10,width = 20)

plot.roc(main="Curva ROC del modelo logistico",
         curva,legacy.axes = T,
         print.thres = "best",
         print.auc = T,
         of="thresholds",
         thresholds=T,
         grid = T,
         type = "shape",
         col="#1c61b6AA"
)


# realizarmos las predicciones sobre el conjunto de datos totales
valor_optimo<-0.463


prediccion<-predict(object= glm_mod,newdata =
                      datos_modelo[train,] ,type = 'response') 

prediccion<-ifelse(prediccion >valor_optimo,yes = 1,no = 0)

#Comparar los resultados
#
resultado <- datos_modelo[train,] %>% mutate(predicho=prediccion) %>% 
  select(predicho,everything()) %>% 
  mutate(comp=predicho==Survived) 

View(resultado)

# vemos la efectividad del modelo  basicamente extrayecto la media de los  resultados
round(mean(resultado$comp)*100) # 81%
# otra forma de ver la exactitud del modelos
install.packages("MLmetrics")
library(MLmetrics)
Accuracy(y_pred = prediccion,y_true = datos_modelo[train,"Survived"] )*100

# comparar los datos 
#Generamos la informacion para los datos de entrenamiento

datos_modelo[train,"prob_exito"] <- predict(glm_mod, newdata = datos_modelo[train,], type="response")
datos_modelo[train,"pred_optima"] <- ifelse(datos_modelo[train, "prob_exito"]>=valor_optimo, 1, 0)

table(datos_modelo[train,"Survived"], 
      datos_modelo[train,"pred_optima"], 
      dnn=c("Actual","Predicho"))

# otra forma de ver la matrix de confusion 
ConfusionMatrix(y_pred = prediccion,y_true = datos_modelo[train,"Survived"])
# el area bajo la curva
AUC(y_pred = prediccion,y_true = datos_modelo[train,"Survived"])
# el error
#MSE(y_pred = prediccion,y_true = Survived)

# PROBANDO EL MODELO CON TEST
# ejecutamos el modelo para el conjunto de datos de prueba


prediccion_test<-predict(object= glm_mod,
                         newdata = datos_modelo[-train,] ,
                         type = 'response') 

prediccion_test<-ifelse(prediccion_test >valor_optimo,yes = 1,no = 0)

Accuracy(y_pred = prediccion_test,y_true = 
           datos_modelo[-train,"Survived"] )

datos_modelo[-train,"prob_exito"]<- predict(glm_mod, newdata = datos_modelo[-train,], type="response")
datos_modelo[-train,"pred_optima"] <- ifelse(datos_modelo[-train,"prob_exito"]>=0.430, 1, 0)

# imprimimos la matriz de  confucion
table(datos_modelo[-train,"Survived"], 
      datos_modelo[-train,"pred_optima"], 
      dnn=c("Actual","Predicho"))

View(datos_modelo)










