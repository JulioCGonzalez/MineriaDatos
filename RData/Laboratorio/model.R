# model.R
library(RODBC)
library(dplyr)

# Conectar a la base de datos Northwind
con.SqlServer <- odbcConnect("ODBC_SQL", uid = "sa", pwd = "Admin12345")

# Extraer datos de las tablas Orders, OrderDetails y Products
orders <- sqlQuery(con.SqlServer, "SELECT OrderID, ShipCountry FROM Orders")
order_details <- sqlQuery(con.SqlServer, "SELECT OrderID, ProductID, UnitPrice, Quantity FROM [Order Details]")
products <- sqlQuery(con.SqlServer, "SELECT ProductID, ProductName FROM Products")

# Primero se combina order_details con orders mediante la columna OrderID
merged_data <- merge(order_details, orders, by = "OrderID")
# Luego se combina el resultado con products utilizando la columna ProductID
merged_data <- merge(merged_data, products, by = "ProductID")

# Calcular el TotalSales (Total de ventas) como UnitPrice * Quantity
merged_data$TotalSales <- merged_data$UnitPrice * merged_data$Quantity

# Guardar el dataframe final para su uso en la aplicación Shiny
sales_data <- merged_data
save(sales_data, file = "sales_data.RData")

# Cerrar la conexión a la base de datos
odbcClose(con.SqlServer)