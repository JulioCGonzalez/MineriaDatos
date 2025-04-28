1. Creacion de conexiones
library(RODBC)

install.packages("corrplot")
2. Conección y crear las consultas
con.SqlServer <-odbcConnect("ODBC_SQL",uid = "sa",pwd = "Admin12345")
facturas<-query.employees.sqlserver<-sqlQuery(con.SqlServer,"select *  from invoices")
View(facturas)
3. Crear el diccionario de datos
Documento en word que describe las variables y su proposito
4. Explorar el conjunto de datos (summary, tipos de datos, dimensiones)
  library(dplyr)
  glimpse(facturas)
  str(facturas)
  dim(facturas)
  class(facturas)
  summary(facturas)
5. Conversion de tipos de datos
  facturas$OrderDate<-as.Date(facturas$OrderDate)
  facturas$RequiredDate<-as.Date(facturas$RequiredDate)
  facturas$ShippedDate<-as.Date(facturas$ShippedDate)
  facturas$Quantity<-as.numeric(facturas$Quantity)
6. Identificacion de valores nulos
facturas$Region<-NULL
facturas$ShipRegion<-NULL
facturas$PostalCode<-NULL
facturas$ShippedDate<-NULL

colSums(is.na(facturas))

7. Identificación de valores duplicados

sum(duplicated(facturas))
facturas[duplicated(facturas), ]

8. Identificacion de valores atipicos
colnames(facturas)
facturas<-facturas %>% mutate(total_sales=Quantity*UnitPrice)
View(facturas)

library(dplyr)

# Calcular valores atípicos para cada Country y Product Name
outliers <- facturas %>%
  group_by(Country, ProductName) %>%
  mutate(Q1 = quantile(ExtendedPrice, 0.25, na.rm = TRUE),
         Q3 = quantile(ExtendedPrice, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5 * IQR,
         upper_bound = Q3 + 1.5 * IQR,
         is_outlier = ExtendedPrice < lower_bound | ExtendedPrice > upper_bound) %>%
  filter(is_outlier) %>%
  select(Country, ProductName, ExtendedPrice, is_outlier)


outliers_mediana <- function(df, columna, groupby_col) {
  df[[groupby_col]] <- as.factor(df[[groupby_col]])
  resultados <- list()
  for (grupo in levels(df[[groupby_col]])) {
    datos_grupo <- df[df[[groupby_col]] == grupo, ]
    Q1 <- quantile(datos_grupo[[columna]], 0.25)
    Q3 <- quantile(datos_grupo[[columna]], 0.75)
    IQR <- Q3 - Q1
    limite_inferior <- Q1 - 1.5 * IQR
    limite_superior <- Q3 + 1.5 * IQR
    
    mediana_grupo <- median(datos_grupo[[columna]], na.rm = TRUE)
    
    datos_grupo[[columna]] <- ifelse(
      datos_grupo[[columna]] < limite_inferior | 
        datos_grupo[[columna]] > limite_superior,
      mediana_grupo,
      datos_grupo[[columna]]
    )
    
    resultados[[grupo]] <- datos_grupo
  }
  
  # Combinar todos los grupos en un solo data frame
  df_resultado <- do.call(rbind, resultados)
  
  return(df_resultado)
}

df_resultado <- outliers_mediana(facturas, 'total_sales', 'Country')
df_resultado

print(head(facturas))
print(head(df_resultado))


    library(ggplot2)
    ggplot(df_resultado, aes(x = Country, y = total_sales)) +
      geom_boxplot(aes(group = Country), fill = "lightblue", color = "black", alpha = 0.5) +  # Boxplot por país
      scale_color_manual(values = c("red", "blue")) + 
      labs(title = "Valores Atípicos Reemplazados por la Mediana",
           x = "País", y = "Total Ventas",
           color = "Es Outlier") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
    
    
# Mostrar los valores atípicos
print(outliers)



ggplot(facturas, aes(x = ProductName, y = ExtendedPrice, fill = Country)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Valores Atípicos de Total Sales por Country y Product Name")

----------------------------------------------------------
  eliminar_outliers <- function(df, columna, groupby_col) {
    df[[groupby_col]] <- as.factor(df[[groupby_col]])
    resultados <- list()
    
    for (grupo in levels(df[[groupby_col]])) {
      datos_grupo <- df[df[[groupby_col]] == grupo, ]
      
      # Cálculo de IQR
      Q1 <- quantile(datos_grupo[[columna]], 0.25, na.rm = TRUE)
      Q3 <- quantile(datos_grupo[[columna]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      limite_inferior <- Q1 - 1.5 * IQR
      limite_superior <- Q3 + 1.5 * IQR
      
      # Filtrar datos dentro de los límites
      datos_filtrados <- datos_grupo[datos_grupo[[columna]] >= limite_inferior & 
                                       datos_grupo[[columna]] <= limite_superior, ]
      
      resultados[[grupo]] <- datos_filtrados
    }
    
    # Combinar todos los grupos en un solo data frame limpio
    df_resultado <- do.call(rbind, resultados)
    
    return(df_resultado)
  }

df_limpio <- eliminar_outliers(facturas, 'total_sales', 'Country')

print(head(df_limpio))

ggplot(df_limpio, aes(x = Country, y = total_sales)) +
  geom_boxplot(aes(group = Country), fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Boxplot de Ventas Sin Valores Atípicos",
       x = "País", y = "Total Ventas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


9. Identicacion de correlaciones
numericas <- sapply(facturas,is.numeric)
df_numerico <- facturas[,numericas]

cor_matrix <- cor(df_numerico, use = "complete.obs")
print(cor_matrix)


numericas <- sapply(facturas, is.numeric)  
df_numerico <- facturas[, numericas]       
cor_matrix <- cor(df_numerico, use = "complete.obs") 

library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8)

10. Identificaciones de concentraciones de datos, Agrupaciones, barplot
    imputacion de datos
    library(dplyr)
    names(facturas)
    facturas %>% group_by(Country)%>%
      summarise(total=sum(total_sales),total_cliente=n())
    
    facturas %>% group_by(Country)%>%
      filter(Salesperson=="Steven Buchanan") %>%
      summarise(total=sum(total_sales),total_cliente=n())
    
11. Creacion de funcion para el manejo de NA´s
12. Creación de funciones para el manejo de atipicos
13. Guardar el conjunto de datos en CSV
write.csv(facturas, "facturas.csv", row.names = TRUE)



------------------------------------------------------------------------
  
install.packages("RODBC")
install.packages("xlsx")

library(xlsx)

#Lectura de datos de bases de datos relacionales
con<-odbcConnect("ODBC-ORACLE",uid = "pedidos",pwd = "Admin12345")

##Conexion  con Oracle

tbl_cliente <- sqlQuery(con, "select codigo_cliente as id_cliente, nombre_cliente, ciudad from cliente")
tbl_producto <- sqlQuery(con, "select codigo_producto as id_producto, nombre, gama from producto")
tbl_pedido <- sqlQuery(con, "select codigo_pedido id_pedido, codigo_cliente id_cliente,  estado from pedidos.pedido")
tbl_depedido <- sqlQuery(con, "select codigo_pedido as id_pedido, codigo_producto as id_producto, cantidad,precio_unidad from detalle_pedido")

library(dplyr)
ventas <- inner_join(tbl_cliente, tbl_pedido, by = "ID_CLIENTE") %>% 
  inner_join(tbl_depedido, by = "ID_PEDIDO") 
ventas <- inner_join(ventas, tbl_producto, by = "ID_PRODUCTO")
# Crear una nueva columna de ventas 
ventas$INGRESOS <- ventas$CANTIDAD * ventas$PRECIO_UNIDAD

datos <- ventas[,c("ESTADO", "GAMA", "CIUDAD", "INGRESOS")]

boxplot( ventas$INGRESOS ~ ESTADO + GAMA +  CIUDAD, data = datos, 
         main = "Diagrama de caja de ventas por estado, gama y ciudad", 
         xlab = "Combinaciones de estado, gama y ciudad", 
         ylab = "Ventas", outlier.colour = "red")

cliente <- sqlQuery(con,"Select codigo_cliente,nombre_cliente,ciudad from cliente")
print(cliente)



