
# La legalidad de la descargar de datos.
#Sitios web que requiere que inicies sesión, ya que estopodría significar que ha aceptado unas Condiciones de Servicio que pueden prohibir las actividades de web scraping.
#Asegúrate de consultar las condiciones generales del sitio web para no incumplir el contrato.
#Aunque sean datos públicos, asegúrarse de que no estén protegidos por derechos de autor. Esto puede incluir artículos, vídeos y diseños.
#considerar la ética implicada. Aunque una actividad no sea ilegal, puede perjudicarle a usted o a otros o dañar su reputación.

# instalacion de las librerias 
install.packages("netstat")
library("RSelenium")
library("wdman")
library("netstat")

library("rvest")
library("dplyr")
library("stringr")
library("ggplot2")
library("tidyr")

selenium()
#inicializar 
selenium_object<-selenium(retcommand = T,check = F)

# configuracion con chrome
binman::list_versions("chromedriver")
remote_driver <-rsDriver(browser="chrome",
                         chromever="122.0.6261.94",
                         verbose=F,
                         port=free_port())

remote_driver$server$stop()

#Configuracion con Firefox

homepage<-"https://autos.mercadolibre.co.cr/autos-camionetas/"

remote_driver <-rsDriver(browser="firefox",
                         chromever=NULL,
                         verbose=F,
                         port=free_port())

remDr<- remote_driver$client
#remDr$open()
remDr$navigate(homepage)
Sys.sleep(2)
# maximizar la pagina 
remDr$maxWindowSize()


#inicializar un DF para almacenar la informacion 
cars <- data.frame(name = character(), 
                   price = character(),atributes = character(),
                   location = character(),year= character(),
                   kilometers= character(), stringsAsFactors = FALSE)

# Crear la funcion para la descarga de datos para llamarla cada vez que cargue una nueva pagina 

link ='https://autos.mercadolibre.co.cr/autos-camionetas/'
page = read_html(link)
name=page %>% html_nodes(".poly-component__title") %>% html_text() %>% trimws()


filldata<-function(homepage){
  link =unlist(homepage) 
  page = read_html(link)
  name=page %>% html_nodes(".poly-component__title") %>% html_text() %>% trimws()
  price=page %>% html_nodes(".poly-component__price") %>% html_text() %>% trimws()
  atributes=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws()
  location=page %>% html_nodes(".poly-component__location") %>% html_text() %>% trimws()
  year=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws()
  year=substr(year, 1, 4)
  kilometers=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws() %>% gsub("Km", "", .) 
  kilometers=substr(kilometers, 5, nchar(kilometers))%>% trimws()
  new_car <- data.frame(name, price,atributes,location,year,kilometers, stringsAsFactors = FALSE)
  cars<- rbind(cars, new_car)
  return(cars)
}

#https://stackoverflow.com/questions/20986631/how-can-i-scroll-a-web-page-using-selenium-webdriver-in-python

# cargamos los datos de primera pagina
cars<-filldata(homepage)

View( cars)

for (i in 1:3) {
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(5)
  nextpage <- remDr$findElement(using = 'link text', 'Siguiente')
  # Comprobar si el elemento 'Siguiente' está presente
  if (!is.null(nextpage)) {
    newhomepage <- nextpage$getElementAttribute('href')
    Sys.sleep(4)
    nextpage$clickElement()
    cars <-filldata(newhomepage)
    Sys.sleep(6)
  } else {
    cat("No se encontró el enlace 'Siguiente'. Terminando el bucle.\n")
    break  # Rompe el bucle si no se encuentra el enlace 'Siguiente'
  }
}

View(cars)

# apliquemos alguna limpiezas

cars <- cars %>% mutate(currency = ifelse(grepl("\\$", price), "dolares", "colones"))
cars$price <-gsub("[₡$,US]", "", cars$price)
cars$price<-as.double(cars$price)
View(cars)

cars <- separate(cars, location, into = c("canton", "provincia"), sep = "-", remove = FALSE)

cars <- cars %>% 
  mutate(name = gsub("[[:punct:] ]", " ", name))

# primeras dos palabras pero me gusta mas la siguiente
cars <- cars %>% 
  mutate(marca = str_extract(name, "^\\S+\\s+\\S+"))

cars <- cars %>% 
  mutate(marca = word(name, 1))

View(cars)

# viengo algunas graficas

generate_plot <- function(data, variable,flip = FALSE) {
  plot_data <- data.frame(table(data[[variable]]))
  gg<-ggplot(plot_data, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) + 
    labs(title = paste("Cantidad de elementos por", variable),
         x = variable,
         y = "Cantidad") +
    theme_minimal()
  if (flip) {
    gg <- gg + coord_flip()
  }
  
  print(gg)
}

View(cars)
generate_plot(cars, "year")
generate_plot(cars, "marca",TRUE)
names(cars)

cars %>% 
  group_by(name) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))

# general los histogramas

cars$price_millions <- cars$price / 1000000
hist(cars$price_millions, breaks = 80, xlab = "Precio", 
     ylab = "Frecuencia", 
     main = "Histograma de Precios", xaxt = "n")
axis(side = 1, at = pretty(cars$price_millions), labels = pretty(cars$price_millions))

# generar los boxplots

iqr <- IQR(cars$price_millions)
upper_limit <- quantile(cars$price_millions)[4] + 1.5 * iqr
ggplot(cars, aes(x = "", y = price_millions)) +
  geom_boxplot(fill = "green", color = "black", outlier.color = "red") +
  labs(title = "Boxplot de Precios con Valores Atípicos Resaltados",
       y = "Precio") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


# Navegar en los elementos encontrados
Sys.sleep(5)
remDr$goBack()
Sys.sleep(5)
remDr$goForward()

# usar los buscadores de las paginas 
search_box<-remDr$findElement(using = 'id','cb1-edit')
Sys.sleep(5)
search_box$sendKeysToElement(list('Mitsubichi Montero',key='enter'))

remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

Sys.sleep(5)

# hacer click en los filtros de la pagina / si fuera un combobox es la clase input
#pero en esta pagina no hay , lo que hay son link entonce se hace con las a

usefilter <- remDr$findElement(using = 'Xpath', '//a[@aria-label="Repuestos Autos y Camionetas"]')
#usefilter$getElementAttribute()  este sirve con otros componentes no con le  href
Sys.sleep(10)
usefilter$clickElement()
Sys.sleep(5)
remDr$goBack()

remote_driver$server$stop()
View(cars)

