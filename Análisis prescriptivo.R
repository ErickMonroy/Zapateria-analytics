library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)

# Leer y preparar datos
datos <- read_csv("datos.txt")

# Crear columna de fecha completa para ordenamiento
meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo")
datos$mes_num <- match(datos$mes, meses_espanol)
datos$fecha <- as.Date(paste(datos$anio, datos$mes_num, "01", sep = "-"))

# Calcular métricas adicionales
datos <- datos %>%
  mutate(
    rotacion_inventario = ventas / inventario,
    dias_inventario = 30 / rotacion_inventario,
    diferencia = ventas - inventario,
    porcentaje_ventas_inventario = (ventas / inventario - 1) * 100
  )

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Prescriptivo de Inventario"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Principal", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis por Tienda", tabName = "tienda", icon = icon("store")),
      menuItem("Análisis por Producto", tabName = "producto", icon = icon("shoe-prints")),
      menuItem("Recomendaciones", tabName = "recomendaciones", icon = icon("lightbulb")),
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      hr(),
      selectInput("tienda_id", "Seleccionar Tienda:", 
                  choices = c("Todas", unique(datos$id_tienda)), 
                  selected = "Todas"),
      selectInput("unidad_negocio", "Seleccionar Producto:",
                  choices = c("Todas", unique(datos$unidad_negocio)),
                  selected = "Todas"),
      dateRangeInput("fechas", "Rango de Fechas:",
                     start = min(datos$fecha),
                     end = max(datos$fecha),
                     min = min(datos$fecha),
                     max = max(datos$fecha)),
      actionButton("actualizar", "Actualizar Análisis", icon = icon("sync"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tablero principal
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_ventas"),
                valueBoxOutput("total_inventario"),
                valueBoxOutput("rotacion_promedio")
              ),
              fluidRow(
                box(plotlyOutput("tendencia_ventas"), width = 6),
                box(plotlyOutput("tendencia_inventario"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("rotacion_por_producto"), width = 12)
              )
      ),
      
      # Análisis por tienda
      tabItem(tabName = "tienda",
              fluidRow(
                box(
                  title = "Métricas por Tienda",
                  DTOutput("tabla_tiendas"),
                  width = 12
                )
              ),
              fluidRow(
                box(plotlyOutput("comparativa_tiendas"), width = 12)
              )
      ),
      
      # Análisis por producto
      tabItem(tabName = "producto",
              fluidRow(
                box(
                  title = "Desempeño por Unidad de Negocio",
                  DTOutput("tabla_productos"),
                  width = 12
                )
              ),
              fluidRow(
                box(plotlyOutput("heatmap_rotacion"), width = 12)
              )
      ),
      
      # Recomendaciones prescriptivas
      tabItem(tabName = "recomendaciones",
              fluidRow(
                box(
                  title = "Recomendaciones de Inventario",
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("recomendaciones_tabla"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "Explicación de Recomendaciones",
                  status = "info",
                  solidHeader = TRUE,
                  htmlOutput("explicacion_recomendaciones"),
                  width = 12
                )
              )
      ),
      
      # Datos completos
      tabItem(tabName = "datos",
              box(
                title = "Datos Completos",
                width = 12,
                DTOutput("tabla_completa")
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Filtrar datos reactivamente
  datos_filtrados <- eventReactive(input$actualizar, {
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
    
    # Filtrar por fecha
    datos_filtrados <- datos_filtrados %>%
      filter(fecha >= input$fechas[1] & fecha <= input$fechas[2])
    
    return(datos_filtrados)
  })
  
  # Value boxes
  output$total_ventas <- renderValueBox({
    datos <- datos_filtrados()
    valor <- sum(datos$ventas)
    
    valueBox(
      formatC(valor, format = "d", big.mark = ","),
      "Ventas Totales",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$total_inventario <- renderValueBox({
    datos <- datos_filtrados()
    valor <- mean(datos$inventario)
    
    valueBox(
      formatC(valor, format = "d", big.mark = ","),
      "Inventario Promedio",
      icon = icon("boxes"),
      color = "blue"
    )
  })
  
  output$rotacion_promedio <- renderValueBox({
    datos <- datos_filtrados()
    valor <- mean(datos$rotacion_inventario, na.rm = TRUE)
    
    color <- ifelse(valor >= 1, "green", 
                    ifelse(valor >= 0.8, "yellow", "red"))
    
    valueBox(
      round(valor, 2),
      "Rotación Promedio",
      icon = icon("retweet"),
      color = color
    )
  })
  
  # Gráfico de tendencia de ventas
  output$tendencia_ventas <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- datos %>%
      group_by(fecha, unidad_negocio) %>%
      summarise(ventas = sum(ventas), .groups = 'drop') %>%
      ggplot(aes(x = fecha, y = ventas, color = unidad_negocio)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Tendencia de Ventas",
           x = "Mes",
           y = "Ventas",
           color = "Producto") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Gráfico de tendencia de inventario
  output$tendencia_inventario <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- datos %>%
      group_by(fecha, unidad_negocio) %>%
      summarise(inventario = sum(inventario), .groups = 'drop') %>%
      ggplot(aes(x = fecha, y = inventario, color = unidad_negocio)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Tendencia de Inventario",
           x = "Mes",
           y = "Inventario",
           color = "Producto") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Gráfico de rotación por producto
  output$rotacion_por_producto <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(rotacion_promedio = mean(rotacion_inventario, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(unidad_negocio, rotacion_promedio), 
                 y = rotacion_promedio,
                 fill = rotacion_promedio)) +
      geom_col() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      coord_flip() +
      labs(title = "Rotación de Inventario por Producto",
           x = "Unidad de Negocio",
           y = "Rotación Promedio") +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                           midpoint = 1) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Tabla de métricas por tienda
  output$tabla_tiendas <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      group_by(id_tienda) %>%
      summarise(
        Ventas_Totales = sum(ventas),
        Inventario_Promedio = mean(inventario),
        Rotacion_Promedio = round(mean(rotacion_inventario), 2),
        Dias_Inventario_Promedio = round(mean(dias_inventario), 1),
        .groups = 'drop'
      ) %>%
      mutate(
        Eficiencia = case_when(
          Rotacion_Promedio >= 1.1 ~ "Excelente",
          Rotacion_Promedio >= 0.9 ~ "Buena",
          Rotacion_Promedio >= 0.7 ~ "Regular",
          TRUE ~ "Mejorable"
        )
      ) %>%
      arrange(desc(Rotacion_Promedio))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatCurrency("Ventas_Totales", currency = "", digits = 0) %>%
      formatCurrency("Inventario_Promedio", currency = "", digits = 0)
  })
  
  # Tabla de productos
  output$tabla_productos <- renderDT({
    datos <- datos_filtrados()
    
    resumen <- datos %>%
      group_by(unidad_negocio) %>%
      summarise(
        Ventas_Totales = sum(ventas),
        Inventario_Promedio = mean(inventario),
        Rotacion_Promedio = round(mean(rotacion_inventario), 2),
        .groups = 'drop'
      ) %>%
      mutate(
        Recomendacion = case_when(
          Rotacion_Promedio >= 1.2 ~ "Reducir inventario",
          Rotacion_Promedio >= 0.95 ~ "Mantener inventario",
          Rotacion_Promedio >= 0.8 ~ "Aumentar ligeramente",
          TRUE ~ "Revisar estrategia"
        )
      ) %>%
      arrange(desc(Rotacion_Promedio))
    
    datatable(resumen,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatCurrency("Ventas_Totales", currency = "", digits = 0) %>%
      formatCurrency("Inventario_Promedio", currency = "", digits = 0)
  })
  
  # Heatmap de rotación
  output$heatmap_rotacion <- renderPlotly({
    datos <- datos_filtrados()
    
    heatmap_data <- datos %>%
      group_by(id_tienda, unidad_negocio) %>%
      summarise(rotacion = mean(rotacion_inventario), .groups = 'drop')
    
    p <- ggplot(heatmap_data, aes(x = factor(id_tienda), 
                                  y = unidad_negocio, 
                                  fill = rotacion)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(rotacion, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                           midpoint = 1) +
      labs(title = "Mapa de Calor: Rotación por Tienda y Producto",
           x = "Tienda",
           y = "Producto",
           fill = "Rotación") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Tabla de recomendaciones prescriptivas
  output$recomendaciones_tabla <- renderDT({
    datos <- datos_filtrados()
    
    # Calcular recomendaciones basadas en análisis histórico
    recomendaciones <- datos %>%
      group_by(id_tienda, unidad_negocio) %>%
      summarise(
        ventas_promedio = mean(ventas),
        inventario_promedio = mean(inventario),
        rotacion_promedio = mean(rotacion_inventario),
        .groups = 'drop'
      ) %>%
      mutate(
        # Cálculo MEJORADO del inventario óptimo (adaptativo)
        inventario_optimo = case_when(
          rotacion_promedio > 1.3 ~ ventas_promedio / 1.15,  # Alta rotación: más agresivo
          rotacion_promedio < 0.7 ~ ventas_promedio / 0.85,  # Baja rotación: más conservador
          TRUE ~ ventas_promedio / 1.05                     # Caso normal
        ),
        
        diferencia_actual = inventario_promedio - inventario_optimo,
        porcentaje_diferencia = (diferencia_actual / inventario_promedio) * 100,
        
        accion_recomendada = case_when(
          porcentaje_diferencia > 20 ~ paste("Reducir en", round(abs(diferencia_actual)), "unidades (", round(abs(porcentaje_diferencia)), "%)"),
          porcentaje_diferencia > 10 ~ paste("Reducir ligeramente en", round(abs(diferencia_actual)), "unidades (", round(abs(porcentaje_diferencia)), "%)"),
          porcentaje_diferencia < -20 ~ paste("Aumentar en", round(abs(diferencia_actual)), "unidades (", round(abs(porcentaje_diferencia)), "%)"),
          porcentaje_diferencia < -10 ~ paste("Aumentar ligeramente en", round(abs(diferencia_actual)), "unidades (", round(abs(porcentaje_diferencia)), "%)"),
          abs(porcentaje_diferencia) <= 10 ~ "Mantener nivel actual"
        ),
        
        prioridad = case_when(
          abs(porcentaje_diferencia) > 30 ~ "Alta",
          abs(porcentaje_diferencia) > 20 ~ "Media-Alta",
          abs(porcentaje_diferencia) > 10 ~ "Media",
          TRUE ~ "Baja"
        )
      ) %>%
      select(Tienda = id_tienda, 
             Producto = unidad_negocio, 
             `Rotación Actual` = rotacion_promedio,
             `Inventario Actual` = inventario_promedio,
             `Inventario Óptimo` = inventario_optimo,
             `Diferencia` = diferencia_actual,
             `Acción Recomendada` = accion_recomendada,
             Prioridad = prioridad) %>%
      arrange(Prioridad, desc(abs(`Diferencia`)))
    
    datatable(recomendaciones,
              options = list(pageLength = 15),
              rownames = FALSE) %>%
      formatCurrency(c("Inventario Actual", "Inventario Óptimo", "Diferencia"), 
                     currency = "", digits = 0) %>%
      formatRound("Rotación Actual", 2) %>%
      formatStyle(
        "Prioridad",
        backgroundColor = styleEqual(
          c("Alta", "Media-Alta", "Media", "Baja"),
          c("#FFCCCC", "#FFFFCC", "#CCFFCC", "#E6F3FF")
        )
      )
  })
  
  # Explicación de recomendaciones
  output$explicacion_recomendaciones <- renderUI({
    HTML("
    <h4>Metodología de Recomendaciones:</h4>
    <p>Las recomendaciones se basan en el análisis de la <strong>rotación de inventario</strong>, 
    que mide cuántas veces el inventario se vende y reemplaza en un período determinado.</p>
    
    <h5>Cálculo del Inventario Óptimo (Adaptativo):</h5>
    <ul>
      <li><strong>Alta rotación (>1.3)</strong>: Inventario Óptimo = Ventas Promedio / 1.15 (más agresivo)</li>
      <li><strong>Baja rotación (<0.7)</strong>: Inventario Óptimo = Ventas Promedio / 0.85 (más conservador)</li>
      <li><strong>Rotación normal (0.7-1.3)</strong>: Inventario Óptimo = Ventas Promedio / 1.05 (balanceado)</li>
    </ul>
    
    <h5>Interpretación de la Rotación:</h5>
    <ul>
      <li><strong>> 1.2</strong>: Excelente rotación. Posible riesgo de desabasto.</li>
      <li><strong>0.9 - 1.2</strong>: Rotación óptima. Balance entre disponibilidad y costo.</li>
      <li><strong>0.7 - 0.9</strong>: Rotación baja. Posible exceso de inventario.</li>
      <li><strong>< 0.7</strong>: Rotación crítica. Inventario estancado.</li>
    </ul>
    
    <h5>Acciones Recomendadas:</h5>
    <ul>
      <li><strong>Reducir inventario</strong>: Cuando el inventario actual supera al óptimo en más de 10%</li>
      <li><strong>Aumentar inventario</strong>: Cuando el inventario actual es menor al óptimo en más de 10%</li>
      <li><strong>Mantener</strong>: Cuando la diferencia está dentro del ±10%</li>
    </ul>
    ")
  })
  
  # Tabla de datos completa
  output$tabla_completa <- renderDT({
    datos <- datos_filtrados() %>%
      select(id_tienda, unidad_negocio, mes, anio, inventario, ventas, 
             rotacion_inventario, dias_inventario)
    
    datatable(datos,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatCurrency(c("inventario", "ventas"), currency = "", digits = 0) %>%
      formatRound(c("rotacion_inventario", "dias_inventario"), 2)
  })
  
  # Gráfico comparativo de tiendas
  output$comparativa_tiendas <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- datos %>%
      group_by(id_tienda) %>%
      summarise(rotacion_promedio = mean(rotacion_inventario), .groups = 'drop') %>%
      ggplot(aes(x = reorder(factor(id_tienda), rotacion_promedio), 
                 y = rotacion_promedio)) +
      geom_col(aes(fill = rotacion_promedio)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Comparativa de Rotación entre Tiendas",
           x = "Tienda",
           y = "Rotación Promedio") +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                           midpoint = 1) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
