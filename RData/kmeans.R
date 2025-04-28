#######Ejemplo 1####################################################
library(dplyr)

aseguradora <- paste(getwd(),"RData/Files/insurance.csv", sep = "")
View(insurance)
insurance <- read.csv("RData/Files/insurance.csv",  encoding="UTF-8", header=TRUE, sep=",", 
                      na.strings="NA", dec=".", strip.white=TRUE)


insurance.scale <- as.data.frame(scale(insurance[,5:9])) # escalar los datos
set.seed(80) # fijar semilla
insurance.km <- kmeans(insurance.scale, centers = 4) # Realizamos clustering

names(insurance.km) # contenido del objeto
head(insurance.km$cluster)
insurance.km$totss # inercia total
insurance.km$betweenss # inercia ínter grupos lo mas alta posible
insurance.km$withinss # inercia intra grupos 
insurance.km$tot.withinss # inercia intra grupos (total)  lo menor posible

sumbt<-kmeans(insurance.scale, centers = 1)$betweenss
# le agrego de 2  a 10 cluster para ver cuantos son adecuados
for(i in 2:10) sumbt[i] <- kmeans(insurance.scale, centers = i)$betweenss
plot(1:10, sumbt, type = "b", xlab = "número de clusters", 
     ylab = "suma de cuadrados ínter grupos")

# tambien se puede identificar aquel valor a partir del cual la reducción
#en la suma total de varianza intra-cluster deja de ser sustancial.
#install.packages("factoextra")
library(factoextra)
fviz_cluster(insurance.km, insurance.scale,main = "Grafico de Kmeans")

# con la inercia inter grupos se puede sacar la cantidad de clusters adecuados
#El cluster rojo representa la gente que es fiel y su experiendia
plot(insurance$ant_comp,insurance$ant_perm, col=insurance.km$cluster ,
     xlab = "Fidelidad a la compañía", ylab = "Experiencia" )

#por ejemplo para la mayor siniestralidad
aggregate(insurance[,5:9] ,by = list(insurance.km$cluster), mean)



#######Ejemplo 2####################################################

library(NbClust)
library(factoextra)
library(cluster)
library(gridExtra)

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
df <- na.omit(df)
# View the firt 3 rows of the data
head(df)

set.seed(123)
# Compute the gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 100) 
# Plot the result para el numero optimo de cluster
fviz_gap_stat(gap_stat)

fviz_nbclust(df, kmeans, method = "silhouette")

fviz_nbclust(df, kmeans, method = "wss")

fviz_nbclust(x = df, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(df, method = "euclidean"), nstart = 50)

?kmeans

res_km <- eclust(df, "kmeans", nstart = 25)

# Compute k-means
km_resultado <- kmeans(df, 4, nstart = 30)
head(km_resultado$cluster,30)

# imprimir el grafico
fviz_cluster(km_resultado, df,main = "Grafico de Kmeans")


# Mejora del grafico
fviz_cluster(object = km_resultado, data = df, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

aggregate(df [,1:4] ,by = list(km_resultado$cluster), mean)

# imprimir el grafico con otros centros K

k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Evaluacion del modelos 
km_resultado$totss
km_resultado$betweenss
km_resultado$tot.withinss
km_resultado$withinss

res.hc <- eclust(df, "hclust") # compute hclust
fviz_dend(res.hc, rect = TRUE) # dendrogam


