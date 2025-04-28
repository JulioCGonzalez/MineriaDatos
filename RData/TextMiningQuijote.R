
# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#########################
# 1. Librerias necesarias
#########################

# Vamos a cargar las librerias necesarias
install.packages(c("pdftools","stopwords","tidytext",
                   "stringi","stringr","scales",
                   "tidyr","widyr","ggraph","igraph",
                   "quanteda","topicmodels","cvTools"))

library(pdftools)
library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools)

########################
# 2. Carga de datos
########################

# Nos ubicamos en nuestro directorio de trabajo
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

# Lectura de archivos de texto
texto01 <- pdftools::pdf_text("files/DONQUIJOTE_PARTE1.pdf")
texto02 <- pdftools::pdf_text("files/DONQUIJOTE_PARTE2.pdf")

# se refenciamos al titulo 
texto01[1]

# Tamaño de cada libro
length(texto01)
length(texto02)

texto <- c(texto01,texto02)

length(texto)

# Vamos a hacer un poco de limpieza de texto
# ------------------------------------------
texto <- gsub("\\r", " ", texto)
texto <- gsub("\\n", "", texto)
texto <- gsub("\\d\\K\\.(?=\\d)", "", texto, perl = TRUE)#  Los puntos de separador de mil, lo sustituimos por un espacio

# Juntamos todas las páginas del libros
texto<-paste(texto, collapse = '')
# ya no quedan 900 y resto si no solo 1
length(texto)

texto[1]

# Vamos a estructurar el texto
texto <- gsub("http://www.educa.jcyl.es","",texto)

# se va a considerar el punto como estructura de analisis
# osea se va a generar elementos por medio de frases que terminan en punto
vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}

# Lo convertimos a un dataframe
frases_texto<-as.data.frame(vector)
View(frases_texto)
# le cambiamos el titulo 
colnames(frases_texto)[1]<-"frase"

#####################################
# 3. Limpieza de texto y tokenizaci?n
#####################################

# Quitamos los espacios de encabezado y pie de página
frases_texto$frase<-trimws(frases_texto$frase, "l") # para la izquierda trimws(frase,'r')
# Convertimos a caracter
frases_texto$frase <-as.character(frases_texto$frase)

# Vamos a hacer un poco de limpieza de texto, retirando texto que no aporta
# como el titulo , pie de pagina , ect 
# ------------------------------------------
frases_texto$frase<-gsub("El Ingenioso Hidalgo Don Quijote de la Mancha","",frases_texto$frase)
frases_texto$frase<-gsub("PRIMERA PARTE","",frases_texto$frase)
frases_texto$frase<-gsub("Miguel de Cervantes Saavedra","",frases_texto$frase)
frases_texto$frase<-gsub("Portal Educativo EducaCYL","",frases_texto$frase)
View(frases_texto)


#####################################
# 4. Analisis exploratorio de texto
#####################################
# Nos creamos un lexicon de stopwords en espa?ol 
# el stopword es una lexicologia
library(tm)

lexiconSW<-stopwords("es")
# si queremos agregar palabraas al SW manualmente 
lexiconSW <- append(lexiconSW,c("capítulo","d"," "))


lexiconSW<-as.data.frame(lexiconSW) # convertimos a dataframe
names(lexiconSW)<-"word"
lexiconSW$word<-as.character(lexiconSW$word)
View(lexiconSW)

#para revisar si la palabra ingresada manualmentre si quedo en el SW
lexiconSW %>% filter(lexiconSW$word=="tuvieras")


df <- tibble::rowid_to_column(frases_texto, "ID") #  Generamos un ID para cada frase
View(df)

# 1.1. Algunos análisis básicos
# -----------------------------

review_words <- df %>%
  distinct(frase, .keep_all = TRUE) %>% # eliminar filas duplicadas basadas en frase
  unnest_tokens(word, frase, drop = FALSE) %>%# trabajar por tocket osea separar por la granularidad de la palabras
  distinct(ID, word, .keep_all = TRUE) %>% # se genera el desagregado por palabras
  anti_join(lexiconSW) %>% # devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x.
  filter(str_detect(word, "[^\\d]")) %>% # selececciona words con algun comentario
  group_by(word) %>%
  dplyr::mutate(word_total = n()) %>%
  ungroup() # agrega nuevas variables y conserva las existentes

# Contamos las palabras resultantes
# ---------------------------------
word_counts <- review_words %>%
  dplyr::count(word, sort = TRUE)

word_counts %>%
  head(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  geom_text(
    label=n,
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T
  )+
  labs(title = paste0("Palabras mas utilizadas"),
       subtitle = "Stopwords retiradas",
       x = "Palabra",
       y = "Numero de veces usada")


word_counts %>%
  head(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  geom_text(
    aes(label=as.character(n),fontface="bold",hjust=-0.25,size = 2), # Modificación en esta línea
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T
  )+
  labs(title = paste0("Palabras mas utilizadas"),
       subtitle = "Stopwords retiradas",
       x = "Palabra",
       y = "Numero de veces usada")

# Generamos nuestro WordCloud
# ---------------------------
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)


df_grouped_V <- review_words %>% group_by(word) %>% count(word) %>%  
  group_by(word) %>% mutate(frecuencia = n/dim(review_words)[1])
# el mutate lo que hace es establecer el peso de la palabra en la nube

# Generamos el wordcloud
wordcloud(words = df_grouped_V$word, freq = df_grouped_V$frecuencia,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# 1.2. Bigramas Son analisis en pares de palabras
# -------------
# A veces nos interesa entender la relaci?n entre palabras en una opini?n. 
View(review_bigrams)

review_bigrams <- df %>%
  unnest_tokens(bigram, frase, token = "ngrams", 
                n = 2) # separamos token 2 - grams

bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") # separamos word por bigrama

View(bigrams_separated)


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% lexiconSW$word) %>%
  filter(!word2 %in% lexiconSW$word) # eliminamos  stop words por bigrama
View(bigrams_filtered)

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE) # contamos la cantidad de words por bigrama
View(bigram_counts)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") # count bigrams cleaning
View(bigrams_united)

bigrams_united %>%
  dplyr::count(bigram, sort = TRUE)


# Podemos visualizarlo tambien
review_subject <- df %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)

my_stopwords <- data_frame(word = c(as.character(1:10)))

review_subject <- review_subject %>% 
  anti_join(my_stopwords)

title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)

# Nos generamos el listado de bigramas con las tuplas de mas de 100 frecuencias
listadoBigramas<-title_word_pairs[which(title_word_pairs$n>200),]


set.seed(1234)

title_word_pairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')


