library(shiny)

shinyUI(fluidPage(
  titlePanel("Northwind Sales Data"),
  
  sidebarLayout(
    sidebarPanel(
      # Filtro para seleccionar el país
      selectInput("country", "Select Country:", choices = NULL, selected = NULL),
      
      # Caja de texto para filtrar por nombre de producto
      textInput("product", "Filter by Product:", value = ""),
      
      # Etiquetas de totales, colocadas debajo de la caja de texto
      textOutput("total_sales"),
      textOutput("total_sales_by_country")
    ),
    
    mainPanel(
      # Mostrar la tabla interactiva con los datos
      tableOutput("sales_table")
    )
  )
))