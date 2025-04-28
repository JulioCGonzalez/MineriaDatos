install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(dplyr)
data("airquality") 

View(airquality)
head(airquality) 
df.airquality <-na.omit(airquality) 

tree <- rpart(formula = Ozone~Solar.R+Wind+Temp, data = df.airquality)

tree <- rpart(formula = Ozone~., data = df.airquality)
rpart.plot(tree)
printcp(tree)

# Encontrar el punto óptimo de poda
min_error_index <- which.min(tree$cptable[, "xerror"])
cp_optimal <- tree$cptable[min_error_index, "CP"]

cat("Punto óptimo de poda (CP):", cp_optimal, "\n")

# Poda el árbol usando el punto óptimo
tree_prune <- prune(tree, cp = cp_optimal)
tree_prune
rpart.plot(tree_prune)
printcp(tree_prune)

testPredRpart <- predict(tree, newdata = df.airquality)

# Crear un dataframe con las predicciones y los valores reales
predictions <- testPredRpart
actual_values <- df.airquality$Ozone

# Calcular el error cuadrático medio
mse <- mean((predictions - actual_values)^2)

# Imprimir el error cuadrático medio
cat("Error Cuadrático Medio (MSE):", mse, "\n")

# Calcular el R cuadrado
ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Imprimir el R cuadrado
cat("R cuadrado (R^2):", r_squared, "\n")

rpart.plot(tree_prune)

#################################################################
#practica de Arbol de regresion prediccion numerica

#name: El nombre de la especie de mamífero.
#genus: El género al que pertenece la especie.
#vore: El tipo de alimentación del mamífero. Puede ser "carnívoro", "herbívoro", "insectívoro" (come insectos), o "omnívoro" (come tanto carne como plantas).
#order: El orden taxonómico al que pertenece la especie.
#conservation: El estado de conservación de la especie según la Unión Internacional para la Conservación de la Naturaleza (IUCN).
#sleep_total: El tiempo total de sueño, en horas.
#sleep_rem: El tiempo de sueño REM (movimiento rápido de los ojos), en horas.
#sleep_cycle: La duración promedio de un ciclo completo de sueño, en horas.
#awake: El tiempo despierto, en horas.
#brainwt: El peso del cerebro, en gramos.
#bodywt: El peso corporal, en kilogramos.

library(rpart)
library(rpart.plot)
library(ggplot2)

# Cargar el conjunto de datos msleep
data(msleep)
View(msleep)
# Filtrar las variables relevantes y eliminar filas con NA
data <- na.omit(msleep[, c("vore", "sleep_total", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt")])
# Convertir 'vore' a factor
data$vore <- as.factor(data$vore)
# Crear el modelo de árbol de clasificación para predecir la categoría de alimentación
tree <- rpart(vore ~ sleep_total + sleep_rem + sleep_cycle + awake + brainwt + bodywt, data = data, method = "class")
# Visualizar el árbol
rpart.plot(tree)

# este es un ejemplo de regresion
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

# Cargar el conjunto de datos msleep
data(msleep)
# Filtrar las variables relevantes y eliminar filas con NA
data <- na.omit(msleep[, c("sleep_total", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt")])
# Crear el modelo de árbol de regresión para predecir el tiempo total de sueño
tree <- rpart(sleep_total ~ sleep_rem + sleep_cycle + awake + brainwt + bodywt, data = data)
# Visualizar el árbol
rpart.plot(tree)

# Obtener la importancia de las variables

var_importance <- varImp(tree)
# Imprimir la importancia de las variables
cat("Importancia de las variables:\n")
print(var_importance)

# Crear un gráfico de barras
ggplot(var_importance, aes(x = rownames(var_importance), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Importancia de Variables",
       x = "Variables",
       y = "Importancia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


