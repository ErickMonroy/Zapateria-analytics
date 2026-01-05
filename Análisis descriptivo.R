library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(scales)

# Leer y preparar datos
datos <- read_csv("datos.txt")

# Crear columna de fecha completa para ordenamiento
meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo")
datos$mes_num <- match(datos$mes, meses_espanol)
datos$fecha <- as.Date(paste(datos$anio, datos$mes_num, "01", sep = "-"))

# Calcular métricas descriptivas básicas
datos <- datos %>%
  mutate(
    rotacion_inventario = ventas / inventario,
    dias_inventario = 30 / rotacion_inventario,
    diferencia = ventas - inventario,
    porcentaje_diferencia = (ventas / inventario - 1) * 100
  )

# Interfaz de usuario - Enfoque Descriptivo
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Descriptivo de Inventario y Ventas"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen General", tabName = "resumen", icon = icon("chart-bar")),
      menuItem("Distribuciones", tabName = "distribuciones", icon = icon("chart-area")),
      menuItem("Tendencias Temporales", tabName = "tendencias", icon = icon("chart-line")),
      menuItem("Comparativas", tabName = "comparativas", icon = icon("balance-scale")),
      menuItem("Tablas Detalladas", tabName = "tablas", icon = icon("table")),
      hr(),
      selectInput("tienda_id", "Filtrar por Tienda:", 
                  choices = c("Todas", unique(datos$id_tienda)), 
                  selected = "Todas"),
      selectInput("unidad_negocio", "Filtrar por Producto:",
                  choices = c("Todas", unique(datos$unidad_negocio)),
                  selected = "Todas"),
      selectInput("mes", "Filtrar por Mes:",
                  choices = c("Todos", meses_espanol),
                  selected = "Todos"),
      sliderInput("rango_ventas", "Rango de Ventas:",
                  min = min(datos$ventas),
                  max = max(datos$ventas),
                  value = c(min(datos$ventas), max(datos$ventas))),
      actionButton("aplicar_filtros", "Aplicar Filtros", icon = icon("filter"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Resumen General
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("total_registros"),
                valueBoxOutput("tiendas_unicas"),
                valueBoxOutput("productos_unicos"),
                valueBoxOutput("ventas_totales_box"),
                valueBoxOutput("inventario_promedio_box"),
                valueBoxOutput("rotacion_global_box")
              ),
              fluidRow(
                box(
                  title = "Resumen Estadístico",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("resumen_estadistico")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Tiendas por Ventas",
                  plotlyOutput("top_tiendas_ventas"),
                  width = 6
                ),
                box(
                  title = "Top 5 Productos por Ventas",
                  plotlyOutput("top_productos_ventas"),
                  width = 6
                )
              )
      ),
      
      # Distribuciones
      tabItem(tabName = "distribuciones",
              fluidRow(
                
              ),
              fluidRow(
                box(
                  title = "Distribución de Rotación de Inventario",
                  plotlyOutput("distribucion_rotacion"),
                  width = 6
                ),
                box(
                  title = "Boxplot de Ventas por Producto",
                  plotlyOutput("boxplot_ventas_producto"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Histograma de Días de Inventario",
                  plotlyOutput("histograma_dias_inventario"),
                  width = 12
                )
              )
      ),
      
      # Tendencias Temporales
      tabItem(tabName = "tendencias",
              fluidRow(
                box(
                  title = "Evolución Mensual de Ventas",
                  plotlyOutput("tendencia_ventas_mensual"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "Evolución Mensual de Inventario",
                  plotlyOutput("tendencia_inventario_mensual"),
                  width = 6
                ),
                box(
                  title = "Evolución Mensual de Rotación",
                  plotlyOutput("tendencia_rotacion_mensual"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Serie de Tiempo: Ventas vs Inventario",
                  plotlyOutput("serie_tiempo_comparativa"),
                  width = 12
                )
              )
      ),
      
      # Comparativas
      tabItem(tabName = "comparativas",
              fluidRow(
                box(
                  title = "Ventas por Tienda",
                  plotlyOutput("comparativa_tiendas_ventas"),
                  width = 6
                ),
                box(
                  title = "Inventario Promedio por Tienda",
                  plotlyOutput("comparativa_tiendas_inventario"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Ventas por Producto",
                  plotlyOutput("comparativa_productos_ventas"),
                  width = 6
                ),
                box(
                  title = "Inventario Promedio por Producto",
                  plotlyOutput("comparativa_productos_inventario"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Relación Ventas vs Inventario",
                  plotlyOutput("scatter_ventas_inventario"),
                  width = 12
                )
              )
      ),
      
      # Tablas Detalladas
      tabItem(tabName = "tablas",
              fluidRow(
                box(
                  title = "Datos Completos Filtrados",
                  width = 12,
                  DTOutput("tabla_datos_filtrados")
                )
              ),
              fluidRow(
                box(
                  title = "Resumen por Tienda",
                  width = 6,
                  DTOutput("tabla_resumen_tienda")
                ),
                box(
                  title = "Resumen por Producto",
                  width = 6,
                  DTOutput("tabla_resumen_producto")
                )
              ),
              fluidRow(
                box(
                  title = "Resumen por Mes",
                  width = 12,
                  DTOutput("tabla_resumen_mes")
                )
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Datos filtrados reactivos
  datos_filtrados <- eventReactive(input$aplicar_filtros, {
    datos_filtrados <- datos
    
    # Filtrar por tienda
    if (input$tienda_id != "Todas") {
      datos_filtrados <- datos_filtrados %>%
        filter(id_tienda == as.numeric(input$tienda_id))
    }
    
    # Filtrar por unidad de negocio
    if (input$unidad_negocio != "Todas") {
      datos_filtrados <- datos_filtrados %>%
        filter(unidad_negocio == input$unidad_negocio)
    }
    
    # Filtrar por mes
    if (input$mes != "Todos") {
      datos_filtrados <- datos_filtrados %>%
        filter(mes == input$mes)
    }
    
    # Filtrar por rango de ventas
    datos_filtrados <- datos_filtrados %>%
      filter(ventas >= input$rango_ventas[1] & ventas <= input$rango_ventas[2])
    
    return(datos_filtrados)
  })
  
  # Value boxes - Resumen General
  output$total_registros <- renderValueBox({
    datos <- datos_filtrados()
    valor <- nrow(datos)
    
    valueBox(
      formatC(valor, format = "d", big.mark = ","),
      "Registros Totales",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$tiendas_unicas <- renderValueBox({
    datos <- datos_filtrados()
    valor <- length(unique(datos$id_tienda))
    
    valueBox(
      valor,
      "Tiendas Únicas",
      icon = icon("store"),
      color = "green"
    )
  })
  
  output$productos_unicos <- renderValueBox({
    datos <- datos_filtrados()
    valor <- length(unique(datos$unidad_negocio))
    
    valueBox(
      valor,
      "Productos Únicos",
      icon = icon("boxes"),
      color = "yellow"
    )
  })
  
  output$ventas_totales_box <- renderValueBox({
    datos <- datos_filtrados()
    valor <- sum(datos$ventas)
    
    valueBox(
      formatC(valor, format = "d", big.mark = ","),
      "Ventas Totales",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$inventario_promedio_box <- renderValueBox({
    datos <- datos_filtrados()
    valor <- mean(datos$inventario)
    
    valueBox(
      formatC(round(valor), format = "d", big.mark = ","),
      "Inventario Promedio",
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  output$rotacion_global_box <- renderValueBox({
    datos <- datos_filtrados()
    valor <- mean(datos$rotacion_inventario, na.rm = TRUE)
    
    valueBox(
      round(valor, 2),
      "Rotación Promedio",
      icon = icon("exchange-alt"),
      color = "purple"
    )
  })
  
  # Tabla de resumen estadístico
  output$resumen_estadistico <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      summarise(
        `Mínimo Ventas` = min(ventas),
        `Promedio Ventas` = mean(ventas),
        `Mediana Ventas` = median(ventas),
        `Máximo Ventas` = max(ventas),
        `Desv. Estándar Ventas` = sd(ventas),
        `Mínimo Inventario` = min(inventario),
        `Promedio Inventario` = mean(inventario),
        `Máximo Inventario` = max(inventario),
        `Coef. Variación Ventas` = sd(ventas)/mean(ventas) * 100,
        `Coef. Variación Inventario` = sd(inventario)/mean(inventario) * 100
      ) %>%
      pivot_longer(everything(), names_to = "Métrica", values_to = "Valor") %>%
      mutate(Valor = round(Valor, 2))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatRound("Valor", 2)
  })
  
  # Gráfico: Top 10 tiendas por ventas
  output$top_tiendas_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    top_tiendas <- datos %>%
      group_by(id_tienda) %>%
      summarise(ventas_totales = sum(ventas), .groups = 'drop') %>%
      arrange(desc(ventas_totales)) %>%
      head(10)
    
    p <- ggplot(top_tiendas, aes(x = reorder(factor(id_tienda), ventas_totales), 
                                 y = ventas_totales)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Tiendas por Ventas Totales",
           x = "Tienda",
           y = "Ventas Totales") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Gráfico: Top 5 productos por ventas
  output$top_productos_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    top_productos <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(ventas_totales = sum(ventas), .groups = 'drop') %>%
      arrange(desc(ventas_totales)) %>%
      head(5)
    
    p <- ggplot(top_productos, aes(x = reorder(unidad_negocio, ventas_totales), 
                                   y = ventas_totales)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(title = "Top 5 Productos por Ventas Totales",
           x = "Producto",
           y = "Ventas Totales") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de ventas
  output$distribucion_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = ventas)) +
      geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
      geom_density(aes(y = ..count.. * (max(ventas)-min(ventas))/30), 
                   color = "darkblue", size = 1) +
      labs(title = "Distribución de Ventas",
           x = "Ventas",
           y = "Frecuencia") +
      scale_x_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de inventario
  output$distribucion_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = inventario)) +
      geom_histogram(fill = "forestgreen", bins = 30, alpha = 0.7) +
      geom_density(aes(y = ..count.. * (max(inventario)-min(inventario))/30), 
                   color = "darkgreen", size = 1) +
      labs(title = "Distribución de Inventario",
           x = "Inventario",
           y = "Frecuencia") +
      scale_x_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de rotación
  output$distribucion_rotacion <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = rotacion_inventario)) +
      geom_histogram(fill = "purple", bins = 30, alpha = 0.7) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = mean(datos$rotacion_inventario, na.rm = TRUE), 
                 linetype = "dashed", color = "blue", size = 1) +
      labs(title = "Distribución de Rotación de Inventario",
           subtitle = "Línea roja: Rotación = 1 | Línea azul: Promedio",
           x = "Rotación (Ventas/Inventario)",
           y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Boxplot de ventas por producto
  output$boxplot_ventas_producto <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = unidad_negocio, y = ventas, fill = unidad_negocio)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "Distribución de Ventas por Producto",
           x = "Producto",
           y = "Ventas") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # Histograma de días de inventario
  output$histograma_dias_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = dias_inventario)) +
      geom_histogram(fill = "orange", bins = 30, alpha = 0.7) +
      geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Distribución de Días de Inventario",
           subtitle = "Línea roja: 30 días (rotación = 1)",
           x = "Días de Inventario",
           y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tendencia mensual de ventas
  output$tendencia_ventas_mensual <- renderPlotly({
    datos <- datos_filtrados()
    
    tendencia <- datos %>%
      group_by(fecha, mes) %>%
      summarise(ventas_totales = sum(ventas), .groups = 'drop') %>%
      arrange(fecha)
    
    p <- ggplot(tendencia, aes(x = fecha, y = ventas_totales)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(color = "darkblue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = "Evolución Mensual de Ventas",
           x = "Mes",
           y = "Ventas Totales") +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tendencia mensual de inventario
  output$tendencia_inventario_mensual <- renderPlotly({
    datos <- datos_filtrados()
    
    tendencia <- datos %>%
      group_by(fecha, mes) %>%
      summarise(inventario_promedio = mean(inventario), .groups = 'drop') %>%
      arrange(fecha)
    
    p <- ggplot(tendencia, aes(x = fecha, y = inventario_promedio)) +
      geom_line(color = "forestgreen", size = 1.5) +
      geom_point(color = "darkgreen", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = "Evolución Mensual de Inventario",
           x = "Mes",
           y = "Inventario Promedio") +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tendencia mensual de rotación
  output$tendencia_rotacion_mensual <- renderPlotly({
    datos <- datos_filtrados()
    
    tendencia <- datos %>%
      group_by(fecha, mes) %>%
      summarise(rotacion_promedio = mean(rotacion_inventario, na.rm = TRUE), .groups = 'drop') %>%
      arrange(fecha)
    
    p <- ggplot(tendencia, aes(x = fecha, y = rotacion_promedio)) +
      geom_line(color = "purple", size = 1.5) +
      geom_point(color = "darkviolet", size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Evolución Mensual de Rotación",
           x = "Mes",
           y = "Rotación Promedio") +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Serie de tiempo comparativa
  output$serie_tiempo_comparativa <- renderPlotly({
    datos <- datos_filtrados()
    
    tendencia <- datos %>%
      group_by(fecha) %>%
      summarise(
        ventas_totales = sum(ventas),
        inventario_promedio = mean(inventario),
        .groups = 'drop'
      ) %>%
      arrange(fecha)
    
    # Normalizar para comparación
    tendencia <- tendencia %>%
      mutate(
        ventas_norm = (ventas_totales - min(ventas_totales)) / 
          (max(ventas_totales) - min(ventas_totales)),
        inventario_norm = (inventario_promedio - min(inventario_promedio)) / 
          (max(inventario_promedio) - min(inventario_promedio))
      )
    
    p <- ggplot(tendencia, aes(x = fecha)) +
      geom_line(aes(y = ventas_norm, color = "Ventas"), size = 1.5) +
      geom_line(aes(y = inventario_norm, color = "Inventario"), size = 1.5) +
      labs(title = "Comparativa Normalizada: Ventas vs Inventario",
           subtitle = "Ambas series normalizadas a escala 0-1 para comparación",
           x = "Mes",
           y = "Valor Normalizado",
           color = "Métrica") +
      scale_color_manual(values = c("Ventas" = "steelblue", "Inventario" = "forestgreen")) +
      scale_x_date(date_labels = "%b %Y") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Comparativa tiendas por ventas
  output$comparativa_tiendas_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    comparativa <- datos %>%
      group_by(id_tienda) %>%
      summarise(ventas_totales = sum(ventas), .groups = 'drop') %>%
      arrange(desc(ventas_totales))
    
    p <- ggplot(comparativa, aes(x = reorder(factor(id_tienda), ventas_totales), 
                                 y = ventas_totales)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      labs(title = "Ventas Totales por Tienda",
           x = "Tienda",
           y = "Ventas Totales") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Comparativa tiendas por inventario
  output$comparativa_tiendas_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    comparativa <- datos %>%
      group_by(id_tienda) %>%
      summarise(inventario_promedio = mean(inventario), .groups = 'drop') %>%
      arrange(desc(inventario_promedio))
    
    p <- ggplot(comparativa, aes(x = reorder(factor(id_tienda), inventario_promedio), 
                                 y = inventario_promedio)) +
      geom_col(fill = "forestgreen", alpha = 0.8) +
      labs(title = "Inventario Promedio por Tienda",
           x = "Tienda",
           y = "Inventario Promedio") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Comparativa productos por ventas
  output$comparativa_productos_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    comparativa <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(ventas_totales = sum(ventas), .groups = 'drop') %>%
      arrange(desc(ventas_totales))
    
    p <- ggplot(comparativa, aes(x = reorder(unidad_negocio, ventas_totales), 
                                 y = ventas_totales)) +
      geom_col(fill = "coral", alpha = 0.8) +
      coord_flip() +
      labs(title = "Ventas Totales por Producto",
           x = "Producto",
           y = "Ventas Totales") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Comparativa productos por inventario
  output$comparativa_productos_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    comparativa <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(inventario_promedio = mean(inventario), .groups = 'drop') %>%
      arrange(desc(inventario_promedio))
    
    p <- ggplot(comparativa, aes(x = reorder(unidad_negocio, inventario_promedio), 
                                 y = inventario_promedio)) +
      geom_col(fill = "goldenrod", alpha = 0.8) +
      coord_flip() +
      labs(title = "Inventario Promedio por Producto",
           x = "Producto",
           y = "Inventario Promedio") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Scatter plot: Ventas vs Inventario
  output$scatter_ventas_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = inventario, y = ventas, color = unidad_negocio)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Relación entre Ventas e Inventario",
           subtitle = "Línea roja: Ventas = Inventario (rotación = 1)",
           x = "Inventario",
           y = "Ventas",
           color = "Producto") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Tablas detalladas
  output$tabla_datos_filtrados <- renderDT({
    datos <- datos_filtrados() %>%
      select(id_tienda, unidad_negocio, mes, anio, inventario, ventas, 
             rotacion_inventario, dias_inventario)
    
    datatable(datos,
              options = list(
                pageLength = 15, 
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE,
              filter = 'top') %>%
      formatCurrency(c("inventario", "ventas"), currency = "", digits = 0) %>%
      formatRound(c("rotacion_inventario", "dias_inventario"), 2)
  })
  
  # Tabla resumen por tienda
  output$tabla_resumen_tienda <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      group_by(id_tienda) %>%
      summarise(
        `Ventas Totales` = sum(ventas),
        `Inventario Promedio` = mean(inventario),
        `Rotación Promedio` = mean(ventas/inventario),
        `Meses con Datos` = n(),
        `Ventas por Mes` = mean(ventas),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Ventas Totales`))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatCurrency(c("Ventas Totales", "Inventario Promedio", "Ventas por Mes"), 
                     currency = "", digits = 0) %>%
      formatRound("Rotación Promedio", 2)
  })
  
  # Tabla resumen por producto
  output$tabla_resumen_producto <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(
        `Ventas Totales` = sum(ventas),
        `Inventario Promedio` = mean(inventario),
        `Rotación Promedio` = mean(ventas/inventario),
        `Tiendas con Producto` = n_distinct(id_tienda),
        `Ventas por Tienda` = sum(ventas)/n_distinct(id_tienda),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Ventas Totales`))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatCurrency(c("Ventas Totales", "Inventario Promedio", "Ventas por Tienda"), 
                     currency = "", digits = 0) %>%
      formatRound("Rotación Promedio", 2)
  })
  
  # Tabla resumen por mes
  output$tabla_resumen_mes <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      group_by(mes, anio) %>%
      summarise(
        `Ventas Totales` = sum(ventas),
        `Inventario Promedio` = mean(inventario),
        `Rotación Promedio` = mean(ventas/inventario),
        `Tiendas Activas` = n_distinct(id_tienda),
        `Productos Activos` = n_distinct(unidad_negocio),
        .groups = 'drop'
      ) %>%
      arrange(anio, match(mes, meses_espanol))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatCurrency(c("Ventas Totales", "Inventario Promedio"), 
                     currency = "", digits = 0) %>%
      formatRound("Rotación Promedio", 2)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)