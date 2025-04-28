---
  title: "LAB03 - Análisis de Presupuesto del Poder Judicial"
author: "Julio César González, Kendall Umaña"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Librerías necesarias
install.packages("janitor")
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(dplyr)

url <- "https://pjcrdatosabiertos.blob.core.windows.net/datosabiertos/PJCROD_PRESUPUESTO_V1/PJCROD_PRESUPUESTO_V1.csv"

# Leer el archivo usando coma como separador y codificación Latin1
datos <- read_csv(url, locale = locale(encoding = "Latin1"))

# Revisar estructura
glimpse(datos)

datos <- datos %>%
  rename(
    anio = `2015`,
    cod_dependencia = `926`,
    dependencia = `Dirección Administración y Otros Órganos de Apoyo`,
    cod_programa = `0`,
    categoria_programatica = Remuneraciones,
    partida = `00101`,
    descripcion = `Sueldos para cargos fijos`,
    monto = `15356701298`,
    subprograma = `00`
  )

glimpse(datos)
summary(select_if(datos, is.numeric))

datos <- datos %>% distinct()

datos <- datos %>%
  mutate(
    anio = as.factor(anio),
    cod_dependencia = as.factor(cod_dependencia),
    cod_programa = as.factor(cod_programa),
    partida = as.factor(partida),
    subprograma = as.factor(subprograma),
    monto = as.numeric(monto)
  )
colSums(is.na(datos))

ggplot(datos, aes(y = monto)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot del Monto", y = "Monto")

Q1 <- quantile(datos$monto, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$monto, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

lim_inf
lim_sup

# ¿Cuántos y cuáles?
outliers <- datos %>% filter(monto < lim_inf | monto > lim_sup)
nrow(outliers)
head(outliers)

ggplot(datos, aes(x = monto)) +
  geom_histogram(bins = 40, fill = "orange") +
  labs(title = "Distribución del Monto")

# Imputar valores NA (si hubiera)
datos <- datos %>%
  mutate(monto = ifelse(is.na(monto), mean(monto, na.rm = TRUE), monto))

# Opcional: filtrar outliers extremos
datos <- datos %>%
  filter(monto <= lim_sup)

graficar_categoricas <- function(df) {
  cat_vars <- df %>% select(where(is.factor)) %>% names()
  for (var in cat_vars) {
    print(
      ggplot(df, aes_string(x = var)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Distribución de", var)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
}

graficar_categoricas(datos)

# Correlación (solo una numérica en este caso)
cor(select_if(datos, is.numeric), use = "complete.obs")

# Escalado
datos_escalado <- as.data.frame(scale(select_if(datos, is.numeric)))
summary(datos_escalado)
