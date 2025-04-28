library(rpart) #Algoritmo para árbol de decisión
library(rpart.plot) #Plot de árbol de decisión
library("C50") #paquete que tiene el dataset de entrada y el algoritmo (rpart)
library(readr)
churn <- read_csv("RData/Files/churn.csv")
View(churn)
set.seed(1111)
churn<-read.csv("files/churn.csv",header = T,sep = ",")
View(churn)

#Usaremos solo algunas columnas (features)
names(churn[,c(2:8)]) # nombres en ingles
names(churn)[1]<-"cancelacion"
View(churn)

ind <- sample(2, nrow(churn), replace=TRUE, 
              prob=c(0.7, 0.3)) #train (60%) y test (40%)

trainData <- churn[ind==1, ] #train
testData <- churn[ind==2, ] #test


ArbolRpart <- rpart(cancelacion ~ ., method="class", 
                    data=trainData) 

#Formula: variable dependiente (Cancelacion) depende todas las otras


print(ArbolRpart)   

rpart.plot(ArbolRpart,extra=3)  # extra=4:probabilidad de observaciones por clase

printcp(ArbolRpart) 
plotcp(ArbolRpart)              # evolución del error a medida que se incrementan los nodos

ArbolRpart$cptable
# Podado del árbol
pArbolRpart <- prune(ArbolRpart, ArbolRpart$cptable[4, "CP"])
pArbolRpart<- prune(ArbolRpart, 
                    cp= ArbolRpart$cptable[which.min(
                      ArbolRpart$cptable[,"xerror"]),"CP"])
pArbolRpart<- prune(ArbolRpart, cp=0.016771)


printcp(pArbolRpart)
plotcp(pArbolRpart)
rpart.plot(pArbolRpart,extra=4)  # extra=4:probabilidad de observaciones por clase
# Validamos la capacidad de predicción del árbol con el fichero de validación
testPredRpart <- predict(pArbolRpart, 
                         newdata = testData, 
                         type = "class")
# Visualizamos una matriz de confusión
table(testPredRpart, testData$cancelacion)

sum(testPredRpart == testData$cancelacion) / 
  length(testData$cancelacion)*100


library(pROC)
# Calcular las probabilidades predichas para los datos de prueba
testProbRpart <- predict(pArbolRpart, 
                         newdata = testData, type = "prob")
# Generar la curva ROC y calcular el AUC
rocRpart <- roc(testData$cancelacion, testProbRpart[,2])
aucRpart <- auc(rocRpart)

# Graficar la curva ROC
plot(rocRpart, col="red", main=paste("Curva ROC - AUC =", round(aucRpart, 3)))
lines(x=c(0,1), y=c(0,1), col="blue")

predicciones <- predict(pArbolRpart, newdata = testData, type = "class")

# crear tabla de resultados
resultados <- cbind(predicciones, testData$cancelacion)
colnames(resultados) <- c("Predicciones", "Datos Reales")
View(resultados)


################  Otro EJEMPLO #########################


library(caret)
library(ROCR)
library(ggplot2)

banknote <- read.csv("files/banknote-authentication.csv")
set.seed(1112)

View(banknote)
training.ids <- createDataPartition(banknote$class, p = 0.7, list = F)

mod <- rpart(class ~ entropy+ curtosis +skew+ variance , 
             data = banknote[training.ids,],
             method = "class", 
             control = rpart.control(minsplit = 20, cp = 0.01))
mod

prp(mod, type = 1, extra = 4,roundint=FALSE, nn = TRUE,fallen.leaves = TRUE, 
    faclen = 10, varlen = 15)

mod$cptable
printcp(mod)

mod.pruned <- prune(mod, mod$cptable[6, "CP"])

prp(mod.pruned,roundint=FALSE, type = 2, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray")

pred.pruned <- predict(mod.pruned, banknote[-training.ids,], type="class")

table(banknote[-training.ids,]$class, pred.pruned, 
      dnn = c("Actual", "Predicho"))

pred.pruned2 <- predict(mod.pruned, banknote[-training.ids,], type = "prob")

head(pred.pruned)
head(pred.pruned2)

pred <- prediction(pred.pruned2[,2], banknote[-training.ids, "class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
