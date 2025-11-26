# Dashboard de Calidad de Vida para Pacientes Oncológicos

## Descripción del Proyecto

Este dashboard interactivo fue desarrollado como parte de una Práctica Profesional de Ingeniería en Estadística de la Universidad Católica del Maule (UCM) en colaboración con el Hospital de Curicó, específicamente para el servicio de Oncología.

La herramienta permite visualizar, analizar y monitorear la calidad de vida de pacientes oncológicos mediante el procesamiento de cuestionarios EORTC (European Organisation for Research and Treatment of Cancer) estandarizados.

## Características Principales

### Funcionalidades del Dashboard

1. CARGA Y PROCESAMIENTO DE DATOS
   - Soporte para múltiples tipos de cuestionarios EORTC
   - Validación automática de estructura de datos
   - Conversión inteligente de formatos de fecha

2. VISTA INDIVIDUAL DE PACIENTES
   - Información demográfica completa
   - Scores individuales de escalas EORTC (0-100)
   - Historial completo de evaluaciones

3. ANÁLISIS GENERAL
   - Estadísticas descriptivas de la población
   - Promedios de escalas por tipo de cuestionario
   - Distribución de respuestas por pregunta

4. SEGUIMIENTO TEMPORAL
   - Evolución de pacientes individuales
   - Comparación de múltiples variables
   - Modo de visualización por preguntas o scores

### Cuestionarios EORTC Soportados

- EORTC QLQ-C30 (Core - General)
- EORTC QLQ-STO22 (Gástrico)
- EORTC QLQ-BR23/BR42 (Mama)
- EORTC QLQ-CR29 (Colorectal)
- EORTC QLQ-LC13/LC29 (Pulmón)
- EORTC QLQ-OV28 (Ovario)
- EORTC QLQ-PAN26 (Páncreas)
- EORTC QLQ-PR25 (Próstata)
- EORTC QLQ-H&N35 (Cabeza y Cuello)
- EORTC QLQ-ELD14 (Adulto Mayor)

## Instalación y Uso

### Requisitos del Sistema

Paquetes R requeridos:
install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", 
                   "ggplot2", "lubridate", "DT", "plotly", 
                   "purrr", "tidyr", "viridis", "shinyWidgets"))

### Ejecución

Ejecutar la aplicación:
shiny::runApp("Dashboard_oncology_QOL.R")

## Estructura de Datos Requerida

### Columnas Obligatorias (Metadatos)
1. Marca temporal (Fecha y hora de la evaluación)
2. RUT (Sin puntos ni dígito verificador)
3. Edad (En años)
4. Sexo
5. Nivel Educacional
6. Estado Civil
7. Ocupación actual

### Columnas de Preguntas
- A partir de la columna 8: Preguntas numeradas según el cuestionario EORTC
- Valores numéricos según escalas Likert (1-4 o 1-7)

## Cálculo de Scores EORTC

### Metodología de Scoring

1. Reverse Scoring: Preguntas invertidas según manual EORTC
2. Raw Score (RS): Promedio de items por escala
3. Transformación Lineal: Conversión a escala 0-100

Fórmulas:
- Escalas Funcionales: Score = (1 - ((RS - 1) / rango)) × 100
- Escalas de Síntomas: Score = ((RS - 1) / rango) × 100
---
