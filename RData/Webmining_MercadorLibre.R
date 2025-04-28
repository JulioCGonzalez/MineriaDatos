install.packages("rvest")
library("rvest")
library("dplyr")
library("stringr")

rm(list = ls())
cat("\014")
options(encoding = "utf-8")


cars <- data.frame(name = character(), price = character(),atributes = character(),location = character(), stringsAsFactors = FALSE)
link = "https://autos.mercadolibre.co.cr/autos-camionetas/"
page = read_html(link)

name=page %>% html_nodes(".poly-component__title") %>% html_text() %>% trimws()
price=page %>% html_nodes(".poly-component__price") %>% html_text() %>% trimws()
year=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws()
year=substr(year, 1, 4)
kilometers=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws() %>% gsub("Km", "", .) 
kilometers=substr(kilometers, 5, nchar(kilometers))%>% trimws()
location=page %>% html_nodes(".poly-component__location") %>% html_text() %>% trimws()

cars = data.frame(name,price,year,kilometers,location, stringsAsFactors = FALSE)

cars <- cars %>% mutate(currency = ifelse(grepl("\\$", price), "dolares", "colones"))
cars$price <-gsub("[₡$,US]", "", cars$price)
cars$price<-as.double(cars$price)
View(cars)

View(cars)

-------------------------------------------------------------------
  

cars <- cars %>% separate(location, into = c("city", "province"), sep = " - ", fill = "right", extra = "merge")

View(cars)
colnames(cars)<-c("marca","precio","annio","kilometros","canto","provincia","moneda")
vehiculos_provincia<-cars %>% group_by(provincia) %>%
  summarise(cantidad=n())








-----------------------------------------------------------------------
  
  install.packages("rvest")
library("rvest")
library("dplyr")
library("stringr")
library("tidyr")

rm(list = ls())
cat("\014")
options(encoding = "utf-8")

# Definir el número de páginas que deseas scrapear
num_paginas <- 3  # Cambia este valor según necesites

# Crear un dataframe vacío para almacenar los datos
cars <- data.frame(name = character(), price = character(), year = character(),
                   kilometers = character(), location = character(), stringsAsFactors = FALSE)

# Loop para iterar sobre múltiples páginas
for (pagina in 1:num_paginas) {
  link <- paste0("https://autos.mercadolibre.co.cr/autos-camionetas/_Desde_", (pagina - 1) * 48 + 1)
  page <- read_html(link)
  
  name <- page %>% html_nodes(".poly-component__title") %>% html_text() %>% trimws()
  price <- page %>% html_nodes(".poly-component__price") %>% html_text() %>% trimws()
  year <- page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws()
  year <- substr(year, 1, 4)
  kilometers <- page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws() %>% gsub("Km", "", .) 
  kilometers <- substr(kilometers, 5, nchar(kilometers)) %>% trimws()
  location <- page %>% html_nodes(".poly-component__location") %>% html_text() %>% trimws()
  
  # Crear dataframe temporal
  cars_temp <- data.frame(name, price, year, kilometers, location, stringsAsFactors = FALSE)
  
  # Agregar datos al dataframe principal
  cars <- rbind(cars, cars_temp)
}

# Agregar columna de moneda
cars <- cars %>% mutate(currency = ifelse(grepl("\\$", price), "dolares", "colones"))

# Limpiar precio
cars$price <- gsub("[₡$,US]", "", cars$price)
cars$price <- as.double(cars$price)

# Separar la ubicación en ciudad y provincia
cars <- cars %>% separate(location, into = c("city", "province"), sep = " - ", fill = "right", extra = "merge")

# Renombrar columnas
colnames(cars) <- c("marca", "precio", "annio", "kilometros", "ciudad", "provincia", "moneda")

# Agrupar por provincia
vehiculos_provincia <- cars %>% group_by(provincia) %>%
  summarise(cantidad = n())

# Mostrar el dataframe
View(cars)
View(vehiculos_provincia)
