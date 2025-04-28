# Limpiar el entorno y ajustar opciones
rm(list = ls())
cat("\014")
options(encoding = "utf-8")

# Cargar las librerías necesarias
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(tidytext)
library(textdata)

# Extraer datos de IMDb

# Definir la URL base y extraer datos de la primera página
link <- "https://www.imdb.com/search/title/?title_type=feature&genres=adventure&sort=user_rating,desc&num_votes=25000,"
page <- read_html(link)

# Extraer los datos de la primera página
name <- page %>% html_nodes(".ipc-title__text") %>% html_text()
synopsis <- page %>% html_nodes(".ipc-html-content-inner-div") %>% html_text()
rating <- page %>% html_nodes(".ipc-rating-star--rating") %>% html_text()
year <- page %>% html_nodes(".dli-title-metadata-item") %>% html_text()
votes <- page %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text() %>% str_extract("\\d+") %>% as.integer()

# Definir la longitud máxima para rellenar con NA
max_length <- max(length(name), length(year), length(rating), length(synopsis), length(votes))

# Asegurarse de que todas las columnas tengan la misma longitud, rellenando con NA
name <- c(name, rep(NA, max_length - length(name)))
year <- c(year, rep(NA, max_length - length(year)))
rating <- c(rating, rep(NA, max_length - length(rating)))
synopsis <- c(synopsis, rep(NA, max_length - length(synopsis)))
votes <- c(votes, rep(NA, max_length - length(votes)))

# Crear el dataframe con los datos obtenidos
movies <- data.frame(name, year, rating, synopsis, votes, stringsAsFactors = FALSE)

# Guardar los resultados en un archivo CSV y mostrarlo
write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(votes)

# Crear un dataframe vacío para almacenar todas las páginas de datos
movies_list <- list()

# Recorrer las páginas y extraer los datos de cada una
for (page_result in seq(from = 1, to = 201, by = 50)) {
  link <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=adventure&sort=user_rating,desc&num_votes=25000,", page_result, "&ref_=adv_nxt")
  page <- read_html(link)
  
  # Extraer los datos de cada página
  name <- page %>% html_nodes(".ipc-title-link-wrapper") %>% html_text()
  synopsis <- page %>% html_nodes(".ipc-html-content-inner-div") %>% html_text()
  rating <- page %>% html_nodes(".ipc-rating-star--base") %>% html_text()
  year <- page %>% html_nodes(".dli-title-metadata-item") %>% html_text()
  votes <- page %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  
  # Asegurarse de que las columnas tengan la misma longitud
  max_length_page <- max(length(name), length(year), length(rating), length(synopsis), length(votes))
  name <- c(name, rep(NA, max_length_page - length(name)))
  year <- c(year, rep(NA, max_length_page - length(year)))
  rating <- c(rating, rep(NA, max_length_page - length(rating)))
  synopsis <- c(synopsis, rep(NA, max_length_page - length(synopsis)))
  votes <- c(votes, rep(NA, max_length_page - length(votes)))
  
  # Acumular los resultados en una lista
  movies_list[[length(movies_list) + 1]] <- data.frame(name, year, rating, synopsis, votes, stringsAsFactors = FALSE)
  
  print(paste("Page:", page_result))  
}

# Combina todos los resultados en un único dataframe
movies <- do.call(rbind, movies_list)

# Ver los datos completos
View(movies)

# Limpieza de datos: convertir el año a numérico
movies <- movies %>% mutate(year = gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", year))
movies$year <- as.numeric(movies$year)

# Asegurarse de que 'rating' sea numérico
movies <- movies %>% mutate(rating = as.numeric(rating))

# Transformar los géneros y separarlos en filas
movies <- movies %>%
  mutate(gender = str_squish(str_replace_all(gender, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Gráfico de distribución de géneros
df_gender <- movies %>%
  group_by(gender) %>%
  summarise(total = n())

ggplot(data = df_gender, aes(x = gender, y = total)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  geom_text(aes(label = as.character(total), vjust = 0)) +
  theme(text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "", y = "Total", title = "Distribución por género de películas")

# Análisis de sentimiento usando el diccionario NRC
nrc <- get_sentiments("nrc")

# Transformar el texto a formato tidy
movies_tidy <- tibble(texto = movies$synopsis) %>%
  unnest_tokens(word, texto)

# Unir con los sentimientos NRC
sentimiento <- movies_tidy %>%
  inner_join(nrc, by = "word", multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Mostrar el análisis de sentimientos
View(sentimiento)

# Graficar el análisis de sentimientos
ggplot(sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de las películas") +
  theme(plot.title = element_text(hjust = 0.5))

# Generar una nube de palabras para los sinopsis de las películas con calificación mayor a 7.6
analisis <- movies %>% filter(rating >= 7.6)
texto <- analisis$synopsis
texto <- gsub(pattern = "\\W", replace = " ", texto)
texto <- gsub(pattern = "\\d", replace = " ", texto)
texto <- tolower(texto)
texto <- removeWords(texto, stopwords("english"))
texto <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", texto)
texto <- stripWhitespace(texto)
texto <- tm::removePunctuation(texto)

# Crear la nube de palabras
wordcloud(texto, min.freq = 2, random.order = FALSE, scale = c(4, 0.5), color = rainbow(3))

# Análisis de las películas de aventura con rating > 7
adventure_movies <- movies %>%
  mutate(gender = str_squish(str_replace_all(gender, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)


# Ver el número de películas antes y después del filtro
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

