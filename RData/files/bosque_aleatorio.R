library(rpart) 
library(ROCR)
library(caret)

library(randomForest)
library(randomForest)
library(dplyr)
library(PerformanceAnalytics)
library(caret)
library(rpart)
library(pROC)


## el RF es muy bueno quien le da pelea es la red neuronal

data("iris")
datos<-iris
View(datos)

tamano<-nrow(datos)
training<-round(tamano*0.7)
indices<-sample(1:tamano,size = training)

datos_train<-datos[indices,]
datos_test<-datos[-indices,]
chart.Correlation(datos_train[,-5])

help("randomForest")

modelo<-randomForest(Species~.,data=datos_train, 
                     ntree=500)

modelo
modelo$importance
varImpPlot(modelo)
plot(modelo)
getTree(modelo,1)
# hacemos las predicciones
predicciones<-predict(modelo,datos_test[,-5])
#  dos  formas
#  Errores o certezas del modelo
mc<-table(predicciones,datos_test$Species)
mc<-with(datos_test,table(predicciones,Species))
confusionMatrix(predicciones,datos_test$Species)


# para ver la efectividad del modelos
# sumamos los valores de la diagonal de la matriz entre el total de datos de la matriz
100 * sum(diag(mc)/sum(mc))

resultado <-datos_test %>% mutate(clasificacion= 
                                    predict(modelo, 
                                            datos_test[,-5])  )
View(resultado)
attach(resultado)
resultado %>% filter(Species!=clasificacion)
plot(modelo)



#--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

## ejemplo 2 
load(file = "files/churn.RData") 
names(churnTrain) 
head(churnTrain)
View(churnTrain)
train <- churnTrain[,-c(1,3)] 
test <- churnTest[,-c(1,3)] 
View(train)

modelo.RF <- randomForest(churn~.,  
                          data = train,  
                          ntree=1000,  
                          mtry=5,  
                          importance=T) 
modelo.RF 
varImpPlot(modelo.RF)
plot(modelo.RF)

names(modelo.RF) 

#Observamos los valores predichos para las primeras observaciones, 
#junto con las proporciones (o probabilidades) asociadas las clases yes y no. 

head(modelo.RF$predicted) 
head(modelo.RF$votes) 
modelo.RF$importance[,c(3,4)] 
varImpPlot(x = modelo.RF,sort = T, n.var = 10)


# utilizando GGPLOT
imp<-varImpPlot(x = modelo.RF,sort = T, n.var = 10)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

ggplot(imp, aes(x=reorder(varnames, MeanDecreaseAccuracy), 
                y=MeanDecreaseAccuracy)) + 
  geom_point(col="red",size=5) +
  labs(title = "Importancia de las variables")+
  ylab("MeanDecreaseAccuracy") +
  xlab("Variable") +
  coord_flip()+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "steelblue")  )+
  scale_color_discrete(name="Variable Group") 

# genramos las probabilidades

probs <- predict(modelo.RF, test, type = "prob")
head(probs)
#me quedo con los exitos y los datos de test 
pred <- prediction(probs[,1], test$churn)
perf <- performance(pred, "tpr", "fpr")


plot(perf,colorize=TRUE,type="l",xlab="False Positive Percentage", ylab="True Positive Percentage",
     lwd=5,col="steelblue")
abline(a=0,b=1)

#############################################

# Generar la curva ROC con pROC
curva <- roc(test$churn, probs[, 2], 
             auc = TRUE, ci = TRUE, 
             levels = c("no", "yes"))

print(curva)

plot.roc(curva, 
         main = "Curva ROC del modelo Random Forest", 
         legacy.axes = TRUE, 
         print.thres = "best", 
         print.auc = TRUE, 
         of = "thresholds", 
         thresholds = TRUE, 
         grid = TRUE, 
         type = "shape", 
         col = "#1c61b6AA")

############################################

AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values

cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

x<-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y<-perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]
points(x,y, pch=20, col="red")

cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte óptimo:",opt.cut)

# gregamos la prediccion al conjunto de datos originales 

probs <- predict(modelo.RF, test, type = "prob")
test$pred <- ifelse(probs[,1] > opt.cut, "yes", "no")
View(test)

#--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



##Ejerccios 3

library(randomForest)
library(caret)
bh <- read.csv("files/BostonHousing.csv")

set.seed(2021)
t.id <- createDataPartition(bh$MEDV, p = 0.7, list = F)

mod <- randomForest(x = bh[t.id, 1:13], y = bh[t.id, 14],
                    ntree = 1000, 
                    xtest = bh[-t.id, 1:13], ytest = bh[-t.id, 14],
                    importance = T, keep.forest = T)

mod

mod$importance


plot(bh[t.id,]$MEDV, predict(mod, newdata = bh[t.id,]),
     xlab = "Actual", ylab = "Predichos")
abline(0,1)


plot(bh[-t.id,]$MEDV, predict(mod, newdata = bh[-t.id,]),
     xlab = "Actual", ylab = "Predichos")
abline(0,1)

#mtry = m/3, donde m = # de predictores
#nodesize = 5
#maxnodes 