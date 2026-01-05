# Instalar paquetes necesarios (si no los tienes)
# install.packages(c("shiny", "tidyverse", "lubridate", "forecast", "DT", "plotly", "shinydashboard", "tseries"))

library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)
library(DT)
library(plotly)
library(shinydashboard)
library(tseries)

# Cargar y preparar datos
datos <- read.csv("datos.txt")

# Convertir meses a formato fecha para serie temporal
meses_esp <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
               "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
meses_num <- 1:12
names(meses_num) <- meses_esp

datos <- datos %>%
  mutate(
    fecha = as.Date(paste(anio, meses_num[mes], "01", sep = "-")),
    mes_num = meses_num[mes],
    rotacion = ifelse(inventario > 0, ventas / inventario * 100, 0),
    unidad_negocio = as.factor(unidad_negocio),
    id_tienda = as.factor(id_tienda),
    periodo = paste(mes, anio)
  ) %>%
  arrange(id_tienda, unidad_negocio, fecha)

# Función mejorada para pronóstico con manejo de errores
pronosticar_futuro <- function(tienda_seleccionada, unidad_seleccionada, 
                               meses_adelante = 6, incluir_2026 = FALSE) {
  
  datos_filtrados <- datos %>%
    filter(id_tienda == tienda_seleccionada, 
           unidad_negocio == unidad_seleccionada) %>%
    arrange(fecha)
  
  if(nrow(datos_filtrados) < 3) {
    return(NULL)
  }
  
  # Crear serie temporal mensual
  ventas_ts <- ts(datos_filtrados$ventas, 
                  frequency = 12, 
                  start = c(2025, 1))
  
  # Probamos múltiples modelos con manejo de errores
  modelos <- list()
  metricas <- list()
  
  # 1. Modelo ARIMA
  tryCatch({
    modelo_arima <- auto.arima(ventas_ts, 
                               seasonal = TRUE,
                               stepwise = TRUE,
                               approximation = FALSE)
    modelos$arima <- modelo_arima
    # Crear un forecast de prueba para obtener métricas
    fc_arima <- forecast(modelo_arima, h = min(1, nrow(datos_filtrados)-1))
    metricas$arima <- accuracy(fc_arima)
  }, error = function(e) {
    message("Error en ARIMA: ", e$message)
  })
  
  # 2. Modelo ETS (Exponential Smoothing)
  tryCatch({
    modelo_ets <- ets(ventas_ts)
    modelos$ets <- modelo_ets
    fc_ets <- forecast(modelo_ets, h = min(1, nrow(datos_filtrados)-1))
    metricas$ets <- accuracy(fc_ets)
  }, error = function(e) {
    message("Error en ETS: ", e$message)
  })
  
  # 3. Modelo naive estacional (benchmark)
  tryCatch({
    if(nrow(datos_filtrados) > 12) {
      modelo_naive <- snaive(ventas_ts, h = min(meses_adelante, 12))
    } else {
      modelo_naive <- naive(ventas_ts, h = meses_adelante)
    }
    modelos$naive <- modelo_naive
    metricas$naive <- accuracy(modelo_naive)
  }, error = function(e) {
    message("Error en Naive: ", e$message)
  })
  
  # Seleccionar mejor modelo basado en MAPE
  mejor_modelo <- NULL
  mejor_modelo_nombre <- "ARIMA Default"
  
  if(length(metricas) > 0) {
    # Extraer MAPE de forma segura
    mapes <- c()
    for(nombre in names(metricas)) {
      m <- metricas[[nombre]]
      if(!is.null(m) && nrow(m) > 0 && "MAPE" %in% colnames(m)) {
        mapes[nombre] <- m[1, "MAPE"]
      }
    }
    
    if(length(mapes) > 0) {
      mejor_modelo_nombre <- names(which.min(mapes))
      mejor_modelo <- modelos[[mejor_modelo_nombre]]
    }
  }
  
  # Si no hay mejor modelo, usar uno por defecto
  if(is.null(mejor_modelo)) {
    tryCatch({
      mejor_modelo <- auto.arima(ventas_ts)
      mejor_modelo_nombre <- "ARIMA Auto"
    }, error = function(e) {
      mejor_modelo <- Arima(ventas_ts, order = c(0,1,0))
      mejor_modelo_nombre <- "Random Walk"
    })
  }
  
  # Generar pronóstico
  tryCatch({
    if(inherits(mejor_modelo, "forecast")) {
      pronostico_obj <- mejor_modelo
    } else {
      pronostico_obj <- forecast(mejor_modelo, h = meses_adelante)
    }
  }, error = function(e) {
    # Pronóstico simple basado en promedio
    pronostico_obj <- list(
      mean = rep(mean(datos_filtrados$ventas, na.rm = TRUE), meses_adelante),
      lower = matrix(rep(mean(datos_filtrados$ventas, na.rm = TRUE) * 0.8, meses_adelante * 2), 
                     ncol = 2),
      upper = matrix(rep(mean(datos_filtrados$ventas, na.rm = TRUE) * 1.2, meses_adelante * 2),
                     ncol = 2)
    )
    class(pronostico_obj) <- "forecast"
  })
  
  # Preparar datos históricos
  historico_extendido <- datos_filtrados %>%
    select(id_tienda, unidad_negocio, fecha, mes, anio, ventas, inventario) %>%
    mutate(Tipo = "Histórico")
  
  # Generar fechas futuras
  ultima_fecha <- max(datos_filtrados$fecha, na.rm = TRUE)
  fechas_futuras <- seq(ultima_fecha + months(1), 
                        by = "month", 
                        length.out = meses_adelante)
  
  # Calcular meses y años futuros de forma segura
  meses_futuros <- character(meses_adelante)
  años_futuros <- integer(meses_adelante)
  
  for(i in 1:meses_adelante) {
    fecha_i <- fechas_futuras[i]
    mes_i <- month(fecha_i)
    if(mes_i >= 1 && mes_i <= 12) {
      meses_futuros[i] <- meses_esp[mes_i]
    } else {
      meses_futuros[i] <- "Desconocido"
    }
    años_futuros[i] <- year(fecha_i)
  }
  
  # Asegurar que tenemos valores de pronóstico
  valores_pronostico <- as.numeric(pronostico_obj$mean)
  if(length(valores_pronostico) < meses_adelante) {
    valores_pronostico <- rep(valores_pronostico[1], meses_adelante)
  }
  
  # Extraer límites de forma segura
  n_pronosticos <- length(valores_pronostico)
  if(!is.null(pronostico_obj$lower) && nrow(pronostico_obj$lower) >= n_pronosticos) {
    limite_inf <- as.numeric(pronostico_obj$lower[1:n_pronosticos, 2])
    limite_sup <- as.numeric(pronostico_obj$upper[1:n_pronosticos, 2])
  } else {
    # Calcular límites aproximados (80-120% del pronóstico)
    limite_inf <- valores_pronostico * 0.8
    limite_sup <- valores_pronostico * 1.2
  }
  
  # Crear dataframe con pronósticos
  pronostico_df <- data.frame(
    fecha = fechas_futuras,
    mes = meses_futuros,
    anio = años_futuros,
    ventas = valores_pronostico,
    limite_inferior = limite_inf,
    limite_superior = limite_sup,
    Tipo = "Pronóstico"
  )
  
  # Estimación de inventario futuro basado en rotación histórica
  rotacion_historica <- mean(datos_filtrados$rotacion, na.rm = TRUE)
  if(is.na(rotacion_historica) || rotacion_historica == 0) {
    rotacion_historica <- 20  # Valor por defecto
  }
  
  pronostico_df$inventario_estimado <- pronostico_df$ventas / (rotacion_historica / 100)
  
  # Combinar histórico y pronóstico
  datos_completos <- bind_rows(
    historico_extendido,
    pronostico_df %>% 
      mutate(
        id_tienda = tienda_seleccionada,
        unidad_negocio = unidad_seleccionada,
        inventario = inventario_estimado
      ) %>%
      select(id_tienda, unidad_negocio, fecha, mes, anio, ventas, inventario, Tipo)
  )
  
  return(list(
    pronostico = pronostico_obj,
    datos_completos = datos_completos,
    pronostico_df = pronostico_df,
    modelo = mejor_modelo,
    modelo_nombre = mejor_modelo_nombre,
    metricas = metricas,
    datos_historicos = datos_filtrados,
    rotacion_promedio = rotacion_historica
  ))
}

# Función para análisis de tendencia con manejo de errores
analizar_tendencia <- function(tienda_seleccionada, unidad_seleccionada) {
  datos_filtrados <- datos %>%
    filter(id_tienda == tienda_seleccionada, 
           unidad_negocio == unidad_seleccionada) %>%
    arrange(fecha)
  
  if(nrow(datos_filtrados) < 3) return(NULL)
  
  tryCatch({
    # Regresión lineal simple para tendencia
    modelo_tendencia <- lm(ventas ~ as.numeric(fecha), data = datos_filtrados)
    pendiente <- coef(modelo_tendencia)[2] * 365  # Ventas por año
    
    # Cálculo de crecimiento mensual de forma segura
    if(nrow(datos_filtrados) > 1) {
      cambios <- diff(datos_filtrados$ventas)
      ventas_previas <- datos_filtrados$ventas[-nrow(datos_filtrados)]
      ratios <- ifelse(ventas_previas > 0, cambios / ventas_previas, 0)
      crecimiento_mensual <- mean(ratios, na.rm = TRUE) * 100
    } else {
      crecimiento_mensual <- 0
    }
    
    return(list(
      modelo = modelo_tendencia,
      pendiente_anual = ifelse(is.numeric(pendiente), pendiente, 0),
      crecimiento_mensual_pct = ifelse(is.numeric(crecimiento_mensual), crecimiento_mensual, 0),
      r_cuadrado = ifelse(!is.null(summary(modelo_tendencia)$r.squared), 
                          summary(modelo_tendencia)$r.squared, 0)
    ))
  }, error = function(e) {
    return(list(
      modelo = NULL,
      pendiente_anual = 0,
      crecimiento_mensual_pct = 0,
      r_cuadrado = 0
    ))
  })
}

# Interfaz de usuario simplificada pero funcional
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Predictivo"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pronóstico Principal", tabName = "pronostico", icon = icon("chart-line")),
      menuItem("Análisis 2026", tabName = "analisis_2026", icon = icon("calendar")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Datos Completos", tabName = "datos", icon = icon("table"))
    ),
    
    # Selectores
    selectInput("tienda", "Seleccionar Tienda:",
                choices = sort(unique(datos$id_tienda)),
                selected = "1"),
    
    selectInput("unidad", "Seleccionar Unidad de Negocio:",
                choices = unique(datos$unidad_negocio),
                selected = "CALZADO CASUAL HOMBRE"),
    
    # Parámetros de pronóstico
    sliderInput("meses_pronostico", "Meses a Pronosticar:",
                min = 1, max = 18, value = 12),
    
    checkboxInput("incluir_2026", "Extender a 2026", value = TRUE),
    
    actionButton("actualizar", "Generar Pronóstico", 
                 icon = icon("play"),
                 class = "btn-primary")
  ),
  
  dashboardBody(
    tabItems(
      # Tab Pronóstico Principal
      tabItem(tabName = "pronostico",
              fluidRow(
                box(
                  title = "Pronóstico de Ventas 2025-2026",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_pronostico_completo", height = 500),
                  width = 12,
                  footer = "Línea naranja indica inicio del pronóstico"
                )
              ),
              
              fluidRow(
                box(
                  title = "Detalles del Pronóstico Futuro",
                  status = "info",
                  solidHeader = TRUE,
                  DTOutput("tabla_pronostico_detallado"),
                  width = 8
                ),
                
                box(
                  title = "Resumen Predictivo",
                  status = "success",
                  solidHeader = TRUE,
                  uiOutput("resumen_predictivo"),
                  width = 4
                )
              )
      ),
      
      # Tab Análisis 2026
      tabItem(tabName = "analisis_2026",
              fluidRow(
                box(
                  title = "Pronóstico Anual 2026",
                  status = "warning",
                  solidHeader = TRUE,
                  plotOutput("grafico_2026", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Metas y Objetivos 2026",
                  status = "info",
                  solidHeader = TRUE,
                  uiOutput("metas_2026"),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Recomendaciones de Inventario 2026",
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("tabla_inventario_2026"),
                  width = 12
                )
              )
      ),
      
      # Tab Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("ventas_proyectadas_2026"),
                valueBoxOutput("crecimiento_esperado"),
                valueBoxOutput("inventario_promedio_2026")
              ),
              
              fluidRow(
                box(
                  title = "Tendencia Histórica",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_tendencia", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Rotación por Mes",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_rotacion", height = 400),
                  width = 6
                )
              )
      ),
      
      # Tab Datos
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Datos Históricos + Pronóstico",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("tabla_datos_completos"),
                  width = 12,
                  downloadButton("descargar_pronostico", "Descargar Pronóstico")
                )
              )
      )
    )
  )
)

# Servidor corregido
server <- function(input, output, session) {
  
  # Pronóstico reactivo mejorado
  pronostico_reactivo <- eventReactive(input$actualizar, {
    req(input$tienda, input$unidad, input$meses_pronostico)
    
    # Ajustar meses si se incluye 2026
    meses <- input$meses_pronostico
    if(input$incluir_2026 && meses < 12) {
      meses <- 12  # Asegurar que llegue a 2026
    }
    
    pronosticar_futuro(input$tienda, input$unidad, 
                       meses, input$incluir_2026)
  })
  
  # Análisis de tendencia
  tendencia_reactiva <- eventReactive(input$actualizar, {
    req(input$tienda, input$unidad)
    analizar_tendencia(input$tienda, input$unidad)
  })
  
  # Gráfico de pronóstico completo
  output$grafico_pronostico_completo <- renderPlotly({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico)) {
      return(plot_ly() %>%
               add_annotations(
                 text = "Datos insuficientes para pronóstico",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE,
                 font = list(size = 16)
               ) %>%
               layout(
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               ))
    }
    
    datos_plot <- pronostico$datos_completos %>%
      mutate(
        periodo = paste0(mes, " ", anio),
        es_pronostico = Tipo,
        tooltip = paste0(
          "Fecha: ", format(fecha, "%b %Y"), "\n",
          "Ventas: ", format(round(ventas), big.mark = ","), "\n",
          "Tipo: ", Tipo
        )
      )
    
    # Punto de inicio del pronóstico
    inicio_pronostico <- datos_plot %>%
      filter(Tipo == "Pronóstico") %>%
      slice(1) %>%
      pull(fecha)
    
    # Crear el gráfico de forma segura
    p <- plot_ly() 
    
    # Agregar datos históricos
    historico <- datos_plot %>% filter(Tipo == "Histórico")
    if(nrow(historico) > 0) {
      p <- p %>%
        add_trace(
          data = historico,
          x = ~fecha,
          y = ~ventas,
          name = "Histórico",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = 'blue', width = 2),
          marker = list(size = 8, color = 'blue'),
          text = ~tooltip,
          hoverinfo = 'text'
        )
    }
    
    # Agregar datos de pronóstico
    futuro <- datos_plot %>% filter(Tipo == "Pronóstico")
    if(nrow(futuro) > 0) {
      p <- p %>%
        add_trace(
          data = futuro,
          x = ~fecha,
          y = ~ventas,
          name = "Pronóstico",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = 'red', width = 2, dash = 'dash'),
          marker = list(size = 8, color = 'red'),
          text = ~tooltip,
          hoverinfo = 'text'
        )
    }
    
    # Configurar layout
    p <- p %>%
      layout(
        title = paste("Pronóstico de Ventas - Tienda", 
                      input$tienda, "-", input$unidad),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Ventas"),
        hovermode = 'x unified',
        showlegend = TRUE
      )
    
    # Agregar línea vertical si hay pronóstico
    if(nrow(futuro) > 0 && length(inicio_pronostico) > 0) {
      p <- p %>%
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = inicio_pronostico,
              x1 = inicio_pronostico,
              y0 = 0,
              y1 = max(datos_plot$ventas, na.rm = TRUE),
              line = list(color = "orange", width = 2, dash = "dot")
            )
          )
        )
    }
    
    p
  })
  
  # Tabla de pronóstico detallado
  output$tabla_pronostico_detallado <- renderDT({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      return(datatable(data.frame(Mensaje = "No hay datos de pronóstico disponibles")))
    }
    
    df <- pronostico$pronostico_df
    
    # Calcular crecimiento de forma segura
    if(nrow(df) > 1) {
      df$crecimiento_mensual <- c(NA, diff(df$ventas) / df$ventas[-nrow(df)] * 100)
    } else {
      df$crecimiento_mensual <- NA
    }
    
    df <- df %>%
      mutate(
        ventas = round(ventas),
        limite_inferior = round(limite_inferior),
        limite_superior = round(limite_superior),
        inventario_estimado = round(inventario_estimado),
        crecimiento_mensual = round(crecimiento_mensual, 1)
      ) %>%
      select(
        'Mes' = mes,
        'Año' = anio,
        'Ventas Pronosticadas' = ventas,
        'Límite Inferior' = limite_inferior,
        'Límite Superior' = limite_superior,
        'Inventario Estimado' = inventario_estimado,
        'Crecimiento %' = crecimiento_mensual
      )
    
    datatable(
      df,
      options = list(
        pageLength = 12,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Crecimiento %',
        backgroundColor = styleInterval(
          c(-10, 0, 10),
          c('#FF6B6B', '#FFE66D', '#A8E6CF', '#4ECDC4')
        )
      )
  })
  
  # Resumen predictivo
  output$resumen_predictivo <- renderUI({
    pronostico <- pronostico_reactivo()
    tendencia <- tendencia_reactiva()
    
    if(is.null(pronostico)) {
      return(tags$div(class = "alert alert-warning",
                      "No hay datos suficientes para el resumen."))
    }
    
    # Cálculos seguros
    if(!is.null(pronostico$pronostico_df) && nrow(pronostico$pronostico_df) > 0) {
      ventas_2025 <- pronostico$pronostico_df %>%
        filter(anio == 2025) %>%
        summarise(total = sum(ventas, na.rm = TRUE)) %>%
        pull(total)
      
      ventas_2026 <- pronostico$pronostico_df %>%
        filter(anio == 2026) %>%
        summarise(total = sum(ventas, na.rm = TRUE)) %>%
        pull(total)
    } else {
      ventas_2025 <- 0
      ventas_2026 <- 0
    }
    
    # Calcular crecimiento
    if(ventas_2025 > 0 && ventas_2026 > 0) {
      crecimiento_anual <- round((ventas_2026 / ventas_2025 - 1) * 100, 1)
    } else {
      crecimiento_anual <- 0
    }
    
    tagList(
      tags$h4("Resumen Predictivo", style = "color: #2c3e50;"),
      tags$div(class = "well",
               tags$p(tags$strong("Modelo utilizado: "), 
                      tags$span(style = "color: #3498db;", pronostico$modelo_nombre)),
               tags$hr(),
               tags$p(tags$strong("Ventas proyectadas 2025: "), 
                      tags$span(style = "color: #27ae60; font-weight: bold;",
                                format(round(ventas_2025), big.mark = ","))),
               tags$p(tags$strong("Ventas proyectadas 2026: "), 
                      tags$span(style = "color: #e74c3c; font-weight: bold;",
                                format(round(ventas_2026), big.mark = ","))),
               tags$p(tags$strong("Crecimiento anual esperado: "), 
                      tags$span(style = paste0("color: ", 
                                               ifelse(crecimiento_anual >= 0, "#27ae60", "#e74c3c"),
                                               "; font-weight: bold;"),
                                sprintf("%+.1f%%", crecimiento_anual))),
               tags$p(tags$strong("Rotación promedio: "), 
                      tags$span(style = "color: #9b59b6;",
                                round(pronostico$rotacion_promedio, 2), "%")),
               tags$p(tags$strong("Tendencia mensual: "), 
                      tags$span(style = paste0("color: ", 
                                               ifelse(tendencia$crecimiento_mensual_pct >= 0, 
                                                      "#27ae60", "#e74c3c")),
                                round(tendencia$crecimiento_mensual_pct, 1), "%"))
      )
    )
  })
  
  # Gráfico 2026
  output$grafico_2026 <- renderPlot({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      return(NULL)
    }
    
    datos_2026 <- pronostico$pronostico_df %>%
      filter(anio == 2026)
    
    if(nrow(datos_2026) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No hay datos de pronóstico para 2026",
                        size = 6) +
               theme_void())
    }
    
    # Asegurar orden de meses
    datos_2026$mes <- factor(datos_2026$mes, 
                             levels = meses_esp[1:min(12, nrow(datos_2026))])
    
    ggplot(datos_2026, aes(x = mes, y = ventas)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                    width = 0.2, color = "#e74c3c", size = 0.8) +
      geom_point(color = "#e74c3c", size = 3) +
      labs(
        title = "Pronóstico Mensual 2026",
        x = "Mes",
        y = "Ventas"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Metas 2026
  output$metas_2026 <- renderUI({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      return(tags$div(class = "alert alert-info",
                      "Genera un pronóstico para ver las metas de 2026."))
    }
    
    datos_2026 <- pronostico$pronostico_df %>%
      filter(anio == 2026)
    
    if(nrow(datos_2026) == 0) {
      return(tags$div(class = "alert alert-warning",
                      "No hay datos de pronóstico para 2026."))
    }
    
    total_2026 <- sum(datos_2026$ventas, na.rm = TRUE)
    promedio_mensual <- mean(datos_2026$ventas, na.rm = TRUE)
    mes_pico_idx <- which.max(datos_2026$ventas)
    mes_pico <- ifelse(length(mes_pico_idx) > 0, 
                       datos_2026$mes[mes_pico_idx], "N/A")
    valor_pico <- ifelse(length(mes_pico_idx) > 0,
                         datos_2026$ventas[mes_pico_idx], 0)
    
    tagList(
      tags$h4("Metas 2026", style = "color: #2c3e50;"),
      tags$div(class = "well",
               tags$p(tags$strong("Meta anual total: "), 
                      tags$span(style = "color: #27ae60; font-weight: bold;",
                                format(round(total_2026), big.mark = ","))),
               tags$p(tags$strong("Promedio mensual objetivo: "), 
                      tags$span(style = "color: #3498db;",
                                format(round(promedio_mensual), big.mark = ","))),
               tags$p(tags$strong("Mes de mayor venta esperado: "), 
                      tags$span(style = "color: #e74c3c; font-weight: bold;", mes_pico)),
               tags$p(tags$strong("Ventas pico estimadas: "), 
                      tags$span(style = "color: #e74c3c;",
                                format(round(valor_pico), big.mark = ","))),
               tags$hr(),
               tags$p(tags$em("Recomendaciones estratégicas:")),
               tags$ul(
                 tags$li("Reforzar stock en ", tags$strong(mes_pico)),
                 tags$li("Mantener rotación > ", 
                         round(pronostico$rotacion_promedio * 1.1, 1), "%"),
                 tags$li("Revisión trimestral de inventario"),
                 tags$li("Campañas promocionales en meses bajos")
               )
      )
    )
  })
  
  # Tabla inventario 2026
  output$tabla_inventario_2026 <- renderDT({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos_2026 <- pronostico$pronostico_df %>%
      filter(anio == 2026)
    
    if(nrow(datos_2026) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para 2026")))
    }
    
    df <- datos_2026 %>%
      mutate(
        ventas = round(ventas),
        inventario_estimado = round(inventario_estimado),
        nivel_servicio = ifelse(inventario_estimado > 0,
                                round(ventas / inventario_estimado * 100, 1),
                                0),
        punto_reorden = round(inventario_estimado * 0.3)  # 30% del inventario
      ) %>%
      select(
        Mes = mes,
        'Ventas Esperadas' = ventas,
        'Inventario Recomendado' = inventario_estimado,
        'Nivel Servicio %' = nivel_servicio,
        'Punto de Reorden' = punto_reorden
      )
    
    datatable(
      df,
      options = list(
        pageLength = 12,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Nivel Servicio %',
        backgroundColor = styleInterval(
          c(15, 25, 35),
          c('#FF6B6B', '#FFE66D', '#A8E6CF', '#4ECDC4')
        )
      )
  })
  
  # Value boxes
  output$ventas_proyectadas_2026 <- renderValueBox({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      valor <- 0
    } else {
      datos_2026 <- pronostico$pronostico_df %>%
        filter(anio == 2026)
      valor <- ifelse(nrow(datos_2026) > 0, 
                      sum(datos_2026$ventas, na.rm = TRUE), 0)
    }
    
    valueBox(
      value = format(round(valor), big.mark = ","),
      subtitle = "Ventas Proyectadas 2026",
      icon = icon("chart-line"),
      color = ifelse(valor > 0, "green", "red")
    )
  })
  
  output$crecimiento_esperado <- renderValueBox({
    tendencia <- tendencia_reactiva()
    
    if(is.null(tendencia)) {
      valor <- 0
    } else {
      valor <- tendencia$crecimiento_mensual_pct
    }
    
    color <- ifelse(valor > 5, "green", 
                    ifelse(valor > 0, "yellow", "red"))
    
    valueBox(
      value = paste0(round(valor, 1), "%"),
      subtitle = "Crecimiento Mensual",
      icon = icon(ifelse(valor > 0, "arrow-up", "arrow-down")),
      color = color
    )
  })
  
  output$inventario_promedio_2026 <- renderValueBox({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico) || is.null(pronostico$pronostico_df)) {
      valor <- 0
    } else {
      datos_2026 <- pronostico$pronostico_df %>%
        filter(anio == 2026)
      valor <- ifelse(nrow(datos_2026) > 0, 
                      mean(datos_2026$inventario_estimado, na.rm = TRUE), 0)
    }
    
    valueBox(
      value = format(round(valor), big.mark = ","),
      subtitle = "Inventario Promedio 2026",
      icon = icon("boxes"),
      color = "blue"
    )
  })
  
  # Gráfico de tendencia
  output$grafico_tendencia <- renderPlotly({
    pronostico <- pronostico_reactivo()
    tendencia <- tendencia_reactiva()
    
    if(is.null(pronostico) || is.null(tendencia)) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No hay datos de tendencia",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               ))
    }
    
    datos_plot <- pronostico$datos_historicos
    
    p <- plot_ly(datos_plot, x = ~fecha, y = ~ventas,
                 type = 'scatter', mode = 'lines+markers',
                 name = 'Ventas',
                 line = list(color = 'blue', width = 2),
                 marker = list(size = 8, color = 'blue'),
                 text = ~paste('Fecha:', format(fecha, "%b %Y"),
                               '<br>Ventas:', format(ventas, big.mark = ",")),
                 hoverinfo = 'text')
    
    # Agregar línea de tendencia si existe modelo
    if(!is.null(tendencia$modelo)) {
      fechas_numericas <- as.numeric(datos_plot$fecha)
      predicciones <- predict(tendencia$modelo)
      
      p <- p %>%
        add_trace(x = ~fecha, y = predicciones,
                  name = 'Tendencia',
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'red', width = 2, dash = 'dash'))
    }
    
    p <- p %>%
      layout(
        title = paste("Tendencia de Ventas - Tienda", input$tienda),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Ventas"),
        hovermode = 'x unified',
        showlegend = TRUE
      )
    
    p
  })
  
  # Gráfico de rotación
  output$grafico_rotacion <- renderPlotly({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico)) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No hay datos de rotación",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               ))
    }
    
    datos_plot <- pronostico$datos_historicos %>%
      mutate(
        periodo = paste0(mes, " ", anio),
        rotacion = round(rotacion, 1)
      )
    
    plot_ly(datos_plot, x = ~periodo, y = ~rotacion,
            type = 'bar',
            name = 'Rotación',
            marker = list(color = ~rotacion,
                          colorscale = 'Viridis',
                          showscale = TRUE),
            text = ~paste('Período:', periodo,
                          '<br>Rotación:', rotacion, '%'),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Rotación por Mes - Tienda", input$tienda),
        xaxis = list(title = "Período", tickangle = -45),
        yaxis = list(title = "Rotación (%)"),
        showlegend = FALSE
      )
  })
  
  # Tabla de datos completos
  output$tabla_datos_completos <- renderDT({
    pronostico <- pronostico_reactivo()
    
    if(is.null(pronostico)) {
      df <- datos
    } else {
      df <- pronostico$datos_completos
    }
    
    df <- df %>%
      mutate(
        rotacion = ifelse(inventario > 0, 
                          round(ventas / inventario * 100, 2),
                          0),
        ventas = round(ventas),
        inventario = round(inventario)
      ) %>%
      select(
        Tienda = id_tienda,
        'Unidad Negocio' = unidad_negocio,
        Fecha = fecha,
        Mes = mes,
        Año = anio,
        Ventas = ventas,
        Inventario = inventario,
        'Rotación %' = rotacion,
        Tipo = Tipo
      )
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        ),
        scrollX = TRUE
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Tipo',
        backgroundColor = styleEqual(
          c("Histórico", "Pronóstico"),
          c('#E3F2FD', '#FFF3E0')
        )
      )
  })
  
  # Descargar pronóstico
  output$descargar_pronostico <- downloadHandler(
    filename = function() {
      paste0("pronostico_tienda_", input$tienda, "_", 
             gsub("[^A-Za-z0-9]", "_", input$unidad), "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      pronostico <- pronostico_reactivo()
      if(!is.null(pronostico)) {
        write.csv(pronostico$datos_completos, file, 
                  row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(datos, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)