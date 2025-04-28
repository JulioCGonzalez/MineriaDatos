library(shiny)
library(dplyr)

# Cargar los datos preprocesados
load("sales_data.RData")

shinyServer(function(input, output, session) {
  
  # Actualizar el selector de país con las opciones disponibles (incluyendo "All")
  updateSelectInput(session, "country", choices = c("All", unique(sales_data$ShipCountry)))
  
  # Filtrar los datos según la selección de país y el filtro de producto ingresado
  filtered_data <- reactive({
    data <- sales_data
    
    # Filtrar por país si no es "All"
    if (!is.null(input$country) && input$country != "All") {
      data <- filter(data, ShipCountry == input$country)
    }
    
    # Filtrar por producto si se ha ingresado texto (búsqueda parcial, case insensitive)
    if (nzchar(input$product)) {
      data <- filter(data, grepl(input$product, ProductName, ignore.case = TRUE))
    }
    
    data
  })
  
  # Mostrar la tabla de datos filtrados con solo las columnas requeridas
  output$sales_table <- renderTable({
    filtered_data() %>% 
      select(ShipCountry, ProductName, UnitPrice, Quantity, TotalSales)
  })
  
  # Calcular y mostrar el total de ventas global
  output$total_sales <- renderText({
    total <- sum(filtered_data()$TotalSales, na.rm = TRUE)
    paste("Total Sales:", "$",round(total, 2))
  })
  
  # Calcular y mostrar el total de ventas por país, solo si se ha seleccionado un país específico
  output$total_sales_by_country <- renderText({
    if (is.null(input$country) || input$country == "All") {
      ""  # Si se selecciona "All", no muestra nada.
    } else {
      sales_by_country <- filtered_data() %>%
        group_by(ShipCountry) %>%
        summarize(Total = sum(TotalSales, na.rm = TRUE))
      
      result <- paste(sales_by_country$ShipCountry, ": $", round(sales_by_country$Total, 2), collapse = "; ")
      paste("Total Sales:", result)
    }
  })
})