<div align="center">
  <img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/logo.png?raw=true" alt="Logo" width="220px" >
  
  <h3> Análisis logístico de inventario y rotación</h3>
  
<img src="https://img.shields.io/badge/R-4.4.3-276DC3?style=for-the-badge&logo=r&logoColor=white" alt="R 4.4.3"> 
<img src="https://img.shields.io/badge/Versión-1.0.0-blue?style=for-the-badge" alt="Versión">

<p align="right"><br>Última actualización: 10 de Enero del 2025</p> </div>

## Contexto del Proyecto
<div align="justify">

**Calzando a México** es una empresa con presencia nacional dedicada a la venta de calzado en sus diferentes categorías: caballero, dama, infantil, deportivo y accesorios. Actualmente, la empresa enfrenta un problema de sobreinventario que ha generado una rotación de inventario por debajo de los niveles óptimos, afectando la rentabilidad y la eficiencia operativa.

Este proyecto desarrolla una serie de dashboards y gráficas interactivas en R/Shiny para transformar datos históricos en análisis descriptivo, predictivo y prescriptivo, permitiendo a la empresa tomar decisiones basadas en datos para optimizar su gestión de inventario.

</div>

## Objetivos del proyecto

1. Diagnosticar el problema de sobreinventario mediante análisis descriptivo detallado.
2. Pronosticar la demanda futura para optimizar las compras y la planificación de inventario.
3. Prescribir acciones específicas para mejorar la rotación de inventario y reducir costos operativos.

## Tecnologías utilizadas

<img src="https://img.shields.io/badge/R-4.4.3-276DC3?style=for-the-badge&logo=r&logoColor=white" alt="R 4.4.3"> <br>

Librerías importadas:

*   `shiny`, `shinydashboard`:  Para la aplicación web interactiva.
*   `tidyverse` (dplyr, ggplot2, etc.): Para manipulación y gráficos base.
*   `plotly`: Para gráficos interactivos.
*   `forecast`: Para los modelos de pronóstico (ETS).
*   `DT`: Para las tablas interactivas.
*   `lubridate`:  Para el manejo de fechas.

## Interfaz del sistema

### Análisis Descriptivo
<div align="justify">
Propósito: Diagnosticar el estado actual del inventario y ventas mediante la obtención de la rotación y la comparación de estas variables.
</div>
<br>
<div align=center>

|Dashboard General|Distribución de rotación y ventas|Evolución de ventas|
|:---:|:----:|:---:|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/Resumen_General.png?raw=true" alt="Dashboards de resumen">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/Comportamiento_ventas.png?raw=true" alt="Gráficas de distribución">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/Distribuci%C3%B3n_ventas.png?raw=true" alt="Gráfica de evolución mensual de ventas">|
|**Evolución de rotación**|**Relación ventas/Inventario**|**Tablas detalladas**|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/inventario_rotaci%C3%B3n.png?raw=true" alt="Gráfica de evolución inventario/rotación">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/distribuci%C3%B3n_comparativo.png?raw=true" alt="Gráfica de comportamiento">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Descriptivo/Datos.png?raw=true" alt="Tabla de datos completos">|
</div>
<div align="justify">

> Este análisis nos permite conocer a detalle la situación actual de la empresa de acuerdo a los datos generales y permitiendo un filtrado de acuerdo a las necesidades o análisis de comportamiento individual por sucursal. Las gráficas  de distribución nos permite medir la rotación de inventario de acuerdo a las ventas de cada tipo de producto, además de que podemos medir el comportamiento de ventas que hemos tenido en el mercado, por otro lado la relación entre ventas e inventario nos permite conocer que existe un problema de sobreinventario a comparación de la cantidad de ventas para cada producto. Al final podemos consultar cada uno de los datos detallados mediante tablas para analizar la información de forma directa.

</div>

### Análisis Predictivo 

<div align="justify">
Propósito: Pronosticar el comportamiento de ventas de acuerdo a las temporadas y así anticiparse con mantener un inventario adecuado para cada producto, garantizando una buena rotación en las sucursales.
</div>
<br>

<div align=center>

|Pronóstico principal|Detalles de pronóstico|Pronóstico mensual y metas a cumplir|
|:---:|:----:|:---:|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Pronostico%20de%20ventas.png?raw=true" alt="Gráfica de pronosticos 2025-2026">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Resumen_predictivo.png?raw=true" alt="Detalles de pronóstico">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Pronostico%20de%20ventas_%20accion.png?raw=true" alt="gráfica de pronóstico mensual y plan de mejora">|
|**Recomendación de inventario**|**Pronóstico de rotación**|**Tablas detalladas**|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Recomendaci%C3%B3n_inventario.png?raw=true" alt="Tabla recomendación de inventario">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Pronostico_rotaci%C3%B3n.png?raw=true" alt="Pronóstico de rotación">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Predictivo/Datos%20general.png?raw=true" alt="Tabla de datos completos">|
</div>
<div align="justify">

> Realizar un pronóstico de ventas de acuerdo a las temporadas del año tiene como propósito conocer el comportamiento de rotación de cada tipo de producto, lo que nos permite realizar un aproximado de inventario que debemos tener para cada mes, cada uno de los datos están clasificados de tal manera que ayude a mantener un buen análisis y cumplir con los planes de acción (metas y objetivos).

</div>

### Análisis Prescriptivo

<div align="justify">
Propósito: Conocer los puntos de oportunidad que tiene la empresa, de esta manera se dan recomendaciones para cada sucursal, lo que permite una mejor distribución logística de acuerdo a lo que se debe hacer en ese momento.
</div>
<br>
<div align=center>

|Dashboard y tendencias ventas/inventario |Rotación de inventario por producto|Nivel de eficiencia por tienda|
|:---:|:----:|:---:|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Dashboard%20principal.png?raw=true" alt="Dashboard y tendencias">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Rotaci%C3%B3n%20de%20inventario%20por%20producto.png?raw=true" alt="Gráfica de rotación de inventario por producto">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Tabla%20de%20eficiencia.png?raw=true" alt="Tabla de métricas por tienda">|
|**Rotación comparativa entre tiendas**|**Desempeño por unidad de negocio**|**Rotación por tienda y producto**|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Rotaci%C3%B3n_tiendas.png?raw=true" alt="Gráfica de barras rotación entre tiendas">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Desempe%C3%B1o.png?raw=true" alt="Tabla de desempeño por unidad de negocio">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Mapa%20de%20calor.png?raw=true" alt="Tabla mapa de calor">|
|**Recomendación de inventario**|**Metodologías de las recomendaciones**|**Tablas detalladas**|
|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Recomendaci%C3%B3n_inventario.png?raw=true" alt="Tabla recomendación de inventario">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Explicaci%C3%B3n_recomendaciones.png?raw=true" alt="Explicación de recomendaciones">|<img src="https://github.com/ErickMonroy/Zapateria-analytics/blob/main/img%20README/Prescriptivo/Datos%20generales.png?raw=true" alt="Tabla de datos completos">|
</div>
<div align="justify">

> La representación de gráficas y tablas en el análisis prescriptivo nos permite detallar con claridad los planes de acción de acuerdo a la situación por la que está pasando cada sucursal, por ejemplo, las gráficas de tendencia permite conocer el comportamiento de cada uno de los tipos de productos, y así conocer nuestros producto estrella o producto vaca, qué obtendríamos de igual forma en una matriz BCG. La rotación es comparativa para determinar si existe un problema de manera visual, y a través de esto obtenemos una prioridad  de atención y una acción recomendada para cada tienda y producto que se necesite.
</div>

## Instalación de R

1. Descargar e instalar R desde su sitio oficial:<br>
https://cran.r-project.org/bin/windows/base/ (V4.4.3 o superior)

2. Instalar RStudio (recomendado):<br>
https://posit.co/download/rstudio-desktop/

## Instalación del Sistema 
1.  **Clonar el repositorio:**
    ```bash
    git clone https://github.com/ErickMonroy/Zapateria-analytics.git
    ```
2. Abrir el programa e instalar todas las dependencias necesarias desde RStudio
3. Haz clic en el botón "Run App" en la parte superior del editor 

### Recomendación
>Si abres el programa desde la opción de RStudio, la primera vez te aparecerá un recuadro en la parte superior para instalar todas las dependencias del programa, de esta manera nos evitamos instalar una por una.