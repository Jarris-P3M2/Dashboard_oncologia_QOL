# app.R
# Instalación de paquetes si faltan
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("viridis")) install.packages("viridis")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("tibble")) install.packages("tibble")

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
library(purrr)
library(tidyr)
library(viridis)
library(shinyWidgets)
library(RColorBrewer)
library(tibble)

# ==============================================================================
# LÓGICA DE CÁLCULO (SCORING)
# ==============================================================================

# Función para calcular scores
# data_q: Dataframe que contiene SOLO las preguntas numéricas (sin columnas de metadatos)
calculate_score <- function(data_q, items_rel, type = "symptom", range_val = 3, reverse_rel = NULL) {
  
  # Validar que los índices solicitados existan en el dataframe de preguntas
  valid_items <- items_rel[items_rel <= ncol(data_q)]
  
  if (length(valid_items) == 0) return(rep(NA, nrow(data_q)))
  
  # Seleccionar las columnas de preguntas
  selected_data <- data_q[, valid_items, drop = FALSE]
  
  # Aplicar Reverse Scoring (Si aplica)
  # Para rango 3 (1-4), la inversión es: 5 - valor.
  # Para rango 6 (1-7), la inversión es: 8 - valor.
  max_score <- range_val + 1
  sum_reverse <- 1 + max_score
  
  if (!is.null(reverse_rel)) {
    # Filtrar solo los reverse que estén en los items seleccionados
    valid_reverse <- reverse_rel[reverse_rel %in% valid_items]
    
    for (idx in valid_reverse) {
      # Encontrar el nombre de columna correspondiente a este índice relativo
      col_name <- names(data_q)[idx]
      if (col_name %in% names(selected_data)) {
        selected_data[[col_name]] <- sum_reverse - selected_data[[col_name]]
      }
    }
  }
  
  # Calcular Raw Score (RS) = Promedio por fila
  rs <- rowMeans(selected_data, na.rm = TRUE)
  
  # Transformación Lineal a 0-100
  score <- rep(NA, length(rs))
  
  if (type == "functional") {
    # Funcional: 1 - ((RS - 1) / range) * 100
    score <- (1 - ((rs - 1) / range_val)) * 100
  } else {
    # Síntoma / Global: ((RS - 1) / range) * 100
    score <- ((rs - 1) / range_val) * 100
  }
  
  return(round(score, 2))
}

# ==============================================================================
# DICCIONARIO DE CONFIGURACIÓN DE ENCUESTAS
# ==============================================================================
# meta_cols: Cantidad de columnas de datos personales antes de que empiecen las preguntas.
# items: Índices RELATIVOS a la sección de preguntas (La pregunta 1 del excel es el índice 1).

survey_configs <- list(
  "Encuesta generica" = list(type = "generic", meta_cols = 7),
  
  # 1. EORTC QLQ-C30 (Core)
  "EORTC QLQ-C30 General" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Salud Global / QoL", items = 29:30, type = "global", range = 6),
      list(name = "Func. Física", items = 1:5, type = "functional"),
      list(name = "Func. de Rol", items = 6:7, type = "functional"),
      list(name = "Func. Emocional", items = 21:24, type = "functional"),
      list(name = "Func. Cognitiva", items = c(20, 25), type = "functional"),
      list(name = "Func. Social", items = 26:27, type = "functional"),
      list(name = "Fatiga", items = c(10, 12, 18), type = "symptom"),
      list(name = "Náuseas/Vómitos", items = 14:15, type = "symptom"),
      list(name = "Dolor", items = c(9, 19), type = "symptom"),
      list(name = "Disnea", items = 8, type = "symptom"),
      list(name = "Insomnio", items = 11, type = "symptom"),
      list(name = "Pérdida Apetito", items = 13, type = "symptom"),
      list(name = "Estreñimiento", items = 16, type = "symptom"),
      list(name = "Diarrea", items = 17, type = "symptom"),
      list(name = "Dificultad Financiera", items = 28, type = "symptom")
    )
  ),
  
  # 2. EORTC QLQ-STO22 (Gástrico)
  # Manual: 31-52. Excel: 1-22.
  "EORTC QLQ-STO22 (Gástrico)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Disfagia", items = 1:3, type = "symptom"),
      list(name = "Dolor", items = 4:7, type = "symptom"),
      list(name = "Reflujo", items = 8:10, type = "symptom"),
      list(name = "Comer", items = c(11:13, 16), type = "symptom"),
      list(name = "Ansiedad", items = c(17, 18, 20), type = "symptom"),
      list(name = "Boca Seca", items = 14, type = "symptom"),
      list(name = "Gusto", items = 15, type = "symptom"),
      list(name = "Imagen Corporal", items = 19, type = "symptom"),
      list(name = "Pérdida Cabello", items = 22, type = "symptom")
    )
  ),
  
  # 3. EORTC QLQ-BR23 (Mama Antigua)
  # Manual: 31-53. Excel: 1-23.
  "EORTC QLQ-BR23 (Mama)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 9:12, type = "functional"),
      list(name = "Func. Sexual", items = 14:15, type = "functional", reverse = 14:15),
      list(name = "Disfrute Sexual", items = 16, type = "functional", reverse = 16),
      list(name = "Perspectiva Futura", items = 13, type = "functional"),
      list(name = "Efectos Sec. Sist.", items = c(1:4, 6:8), type = "symptom"),
      list(name = "Síntomas Mama", items = 20:23, type = "symptom"),
      list(name = "Síntomas Brazo", items = 17:19, type = "symptom"),
      list(name = "Pérdida Cabello", items = 5, type = "symptom")
    )
  ),
  
  # 4. EORTC QLQ-BR42 (Mama Actualizada)
  # Manual: 31-72. Excel: 1-42.
  "EORTC QLQ-BR42 (Mama V.Nueva)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 9:12, type = "functional"),
      list(name = "Func. Sexual", items = 14:15, type = "functional", reverse = 14:15),
      list(name = "Satisfacción Mama", items = 41:42, type = "functional", reverse = 41:42), # Orig 71,72 -> 41,42
      list(name = "Perspectiva Futura", items = 13, type = "functional"),
      list(name = "Disfrute Sexual", items = 16, type = "functional", reverse = 16),
      list(name = "Quimio Efectos", items = c(1:6, 27:28), type = "symptom"), # Orig 31-36, 57-58 -> 1-6, 27-28
      list(name = "Sínt. Endocrinos", items = c(7:8, 24:26), type = "symptom"),
      list(name = "Sínt. Brazo", items = 17:19, type = "symptom"),
      list(name = "Sínt. Mama", items = 20:23, type = "symptom"),
      list(name = "Neuropatía", items = 29:32, type = "symptom"),
      list(name = "Esquelético", items = 33:36, type = "symptom"),
      list(name = "Sínt. Vaginales", items = 38:40, type = "symptom"),
      list(name = "Aumento Peso", items = 37, type = "symptom")
    )
  ),
  
  # 5. EORTC QLQ-CR29 (Colorectal)
  # Manual: 31-59. Excel: 1-29.
  "EORTC QLQ-CR29 (Colorectal)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 15:17, type = "functional"),
      list(name = "Ansiedad", items = 13, type = "functional"),
      list(name = "Peso", items = 14, type = "functional"),
      list(name = "Interés Sex. (H)", items = 26, type = "functional", reverse = 26),
      list(name = "Interés Sex. (M)", items = 28, type = "functional", reverse = 28),
      list(name = "Frec. Urinaria", items = 1:2, type = "symptom"),
      list(name = "Sangre Heces", items = 8:9, type = "symptom"),
      list(name = "Frec. Heces", items = 22:23, type = "symptom"),
      list(name = "Incontinencia Urin.", items = 3, type = "symptom"),
      list(name = "Disuria", items = 4, type = "symptom"),
      list(name = "Dolor Abdominal", items = 5, type = "symptom"),
      list(name = "Dolor Glúteo", items = 6, type = "symptom"),
      list(name = "Hinchazón", items = 7, type = "symptom"),
      list(name = "Boca Seca", items = 10, type = "symptom"),
      list(name = "Pérdida Cabello", items = 11, type = "symptom"),
      list(name = "Gusto", items = 12, type = "symptom"),
      list(name = "Flatulencia", items = 19, type = "symptom"),
      list(name = "Incont. Fecal", items = 20, type = "symptom"),
      list(name = "Piel Irritada", items = 21, type = "symptom"),
      list(name = "Vergüenza", items = 24, type = "symptom"),
      list(name = "Probl. Estoma", items = 25, type = "symptom"),
      list(name = "Impotencia", items = 27, type = "symptom"),
      list(name = "Dispareunia", items = 29, type = "symptom")
    )
  ),
  
  # 6. EORTC QLQ-ELD14 (Adulto Mayor)
  # Manual: 31-44. Excel: 1-14.
  "EORTC QLQ-ELD14 (Adulto Mayor)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Mantener Propósito", items = 11:12, type = "functional"),
      list(name = "Apoyo Familiar", items = 5, type = "functional"),
      list(name = "Movilidad", items = c(1, 3, 4), type = "symptom"),
      list(name = "Preocup. x Otros", items = 6:7, type = "symptom"),
      list(name = "Preocup. Futuro", items = 8:10, type = "symptom"),
      list(name = "Carga Enfermedad", items = 13:14, type = "symptom"),
      list(name = "Rigidez Articular", items = 2, type = "symptom")
    )
  ),
  
  # 7. EORTC QLQ-LC13 (Pulmón)
  # Manual: 31-42 (43 opcional). Excel: 1-12.
  "EORTC QLQ-LC13 (Pulmón)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Disnea", items = 3:5, type = "symptom"),
      list(name = "Tos", items = 1, type = "symptom"),
      list(name = "Hemoptisis", items = 2, type = "symptom"),
      list(name = "Boca Dolorida", items = 6, type = "symptom"),
      list(name = "Disfagia", items = 7, type = "symptom"),
      list(name = "Neuropatía", items = 8, type = "symptom"),
      list(name = "Alopecia", items = 9, type = "symptom"),
      list(name = "Dolor Pecho", items = 10, type = "symptom"),
      list(name = "Dolor Brazo", items = 11, type = "symptom"),
      list(name = "Dolor Otros", items = 12, type = "symptom")
    )
  ),
  
  # 8. EORTC QLQ-LC29 (Pulmón V.Nueva)
  # Manual: 31-59. Excel: 1-29.
  "EORTC QLQ-LC29 (Pulmón V.Nueva)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Tos", items = c(1, 22), type = "symptom"),
      list(name = "Falta Aire", items = 3:5, type = "symptom"),
      list(name = "Efectos Sec.", items = c(6:9, 13:18, 20, 23), type = "symptom"),
      list(name = "Miedo Progresión", items = c(19, 21), type = "symptom"),
      list(name = "Probl. Cirugía", items = 25:29, type = "symptom"),
      list(name = "Hemoptisis", items = 2, type = "symptom"),
      list(name = "Dolor Pecho", items = 10, type = "symptom"),
      list(name = "Dolor Brazo", items = 11, type = "symptom"),
      list(name = "Dolor Otro", items = 12, type = "symptom"),
      list(name = "Pérdida Peso", items = 24, type = "symptom")
    )
  ),
  
  # 9. EORTC QLQ-OV28 (Ovario) -> META COLS = 6
  # Manual: 31-58. Excel: 1-28.
  "EORTC QLQ-OV28 (Ovario)" = list(
    type = "defined", meta_cols = 6, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 20:21, type = "functional"),
      list(name = "Actitud Enferm.", items = 22:24, type = "functional"),
      list(name = "Sexualidad", items = 25:28, type = "functional", reverse = 25:27),
      list(name = "Sínt. Abdominales", items = 1:7, type = "symptom"),
      list(name = "Neuropatía", items = 11:13, type = "symptom"),
      list(name = "Quimio Efectos", items = c(8:10, 14:17), type = "symptom"),
      list(name = "Sínt. Hormonales", items = 18:19, type = "symptom")
    )
  ),
  
  # 10. EORTC QLQ-PAN26 (Páncreas)
  # Manual: 31-56. Excel: 1-26.
  "EORTC QLQ-PAN26 (Páncreas)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Satisfacción", items = 23:24, type = "functional", reverse = 23:24),
      list(name = "Sexualidad", items = 25:26, type = "functional"),
      list(name = "Dolor Pancreático", items = c(1, 3:5), type = "symptom"),
      list(name = "Hinchazón", items = 2, type = "symptom"),
      list(name = "Sínt. Digestivos", items = 6:7, type = "symptom"),
      list(name = "Gusto", items = 8, type = "symptom"),
      list(name = "Indigestión", items = 9, type = "symptom"),
      list(name = "Flatulencia", items = 10, type = "symptom"),
      list(name = "Pérdida Peso", items = 11, type = "symptom"),
      list(name = "Debilidad", items = 12, type = "symptom"),
      list(name = "Boca Seca", items = 13, type = "symptom"),
      list(name = "Sínt. Hepáticos", items = 14:15, type = "symptom"),
      list(name = "Hábito Intest.", items = 16:17, type = "symptom"),
      list(name = "Imagen Corporal", items = 18:19, type = "symptom"), # Symptom en PAN26
      list(name = "Efectos Sec.", items = 20, type = "symptom"),
      list(name = "Preocup. Futuro", items = 21, type = "symptom"),
      list(name = "Planificación", items = 22, type = "symptom")
    )
  ),
  
  # 11. EORTC QLQ-PR25 (Próstata) -> META COLS = 6
  # Manual: 31-55. Excel: 1-25.
  "EORTC QLQ-PR25 (Próstata)" = list(
    type = "defined", meta_cols = 6, range_global = 3,
    scales = list(
      list(name = "Actividad Sexual", items = 20:21, type = "functional", reverse = 20:21),
      list(name = "Func. Sexual", items = 22:25, type = "functional", reverse = 22),
      list(name = "Sínt. Urinarios", items = c(1:7, 9), type = "symptom"),
      list(name = "Ayuda Incont.", items = 8, type = "symptom"),
      list(name = "Sínt. Intestinales", items = 10:13, type = "symptom"),
      list(name = "Sínt. Hormonales", items = 14:19, type = "symptom")
    )
  ),
  
  # 12. EORTC QLQ-H&N35 (Cabeza y Cuello)
  # Manual: Preguntas 31 a 73.
  # Excel (Índices relativos): 1 a 43.
  "EORTC QLQ-HN43 (Cabeza/Cuello)" = list(
    type = "defined", 
    meta_cols = 7, 
    range_global = 3,
    scales = list(
      # Escalas Multi-item y Single-item
      list(name = "Dolor en la Boca", items = 1:4, type = "symptom"),        
      list(name = "Tragar", items = 5:8, type = "symptom"),                 
      list(name = "problemas con los Dientes", items = c(9, 10, 43), type = "symptom"),       
      list(name = "Abrir Boca", items = 11, type = "symptom"),           
      list(name = "Boca Seca", items = 12:13, type = "symptom"),             
      list(name = "problemas con los Sentidos", items = 14:15, type = "symptom"),
      list(name = "Tos", items = 16, type = "symptom"),                      
      list(name = "Habla", items = c(17, 25:28), type = "symptom"),          
      list(name = "Imagen Corporal", items = 18:20, type = "symptom"),      
      list(name = "Comer Social", items = 21:24, type = "symptom"),        
      list(name = "Contacto Social", items = 29, type = "symptom"),          
      list(name = "Sexualidad", items = 30:31, type = "symptom"),            
      list(name = "Problemas Hombro", items = 32:33, type = "symptom"),      
      list(name = "Hinchazón Cuello", items = 34, type = "symptom"),        
      list(name = "Problemas Piel", items = 35:37, type = "symptom"),        
      list(name = "Pérdida Peso", items = 38, type = "symptom"),             
      list(name = "Miedo al progreso", items = 39:40, type = "symptom"),      
      list(name = "Cicatrización Herida", items = 41, type = "symptom"),      
      list(name = "Problemas Neurológicos", items = 42, type = "symptom")    
    )
  )
)

# ==============================================================================
# UI MEJORADA
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("heart-pulse"), "Dashboard Calidad de Vida"),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Carga de Datos", tabName = "carga", icon = icon("upload")),
      menuItem("Vista Individual", tabName = "individual", icon = icon("user")),
      menuItem("Vista General (Radar)", tabName = "general", icon = icon("star")),
      menuItem("Evolución Temporal", tabName = "evolucion", icon = icon("chart-line")),
      menuItem("Estructura de Datos", tabName = "config", icon = icon("cog"))
    ),
    
    # Información adicional en el sidebar
    conditionalPanel(
      condition = "input.tabs != 'carga'",
      div(
        style = "padding: 15px; border-top: 1px solid #eee; margin-top: 20px;",
        h5("Información del Sistema", style = "font-weight: bold;"),
        uiOutput("system_info")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        /* Estilos generales mejorados */
        .content-wrapper, .right-side { 
          background-color: #f8f9fa; 
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        .box { 
          border-radius: 10px; 
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); 
          border-top: 3px solid #3c8dbc;
        }
        
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        
        .box.box-success {
          border-top-color: #00a65a;
        }
        
        .box.box-info {
          border-top-color: #00c0ef;
        }
        
        .box.box-warning {
          border-top-color: #f39c12;
        }
        
        .small-box { 
          border-radius: 8px; 
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .scrollable-table { 
          overflow-x: auto; 
        }
        
        /* Mejoras para value boxes */
        .small-box h3, .small-box p {
          font-weight: 600;
        }
        
        /* Mejoras para tablas */
        .dataTables_wrapper {
          border-radius: 8px;
        }
        
        /* Colores personalizados para gráficos */
        .functional-score { color: #00a65a; }
        .symptom-score { color: #dd4b39; }
        .global-score { color: #3c8dbc; }
        
        /* Estilos para títulos */
        .box-title {
          font-weight: 600;
          color: #2c3e50;
        }
        
        /* Mejoras en selectores */
        .selectize-input {
          border-radius: 6px;
        }
        
        /* Estilos específicos para heat map */
        .heatmap-container {
          margin: 20px 0;
        }
        
        /* Estilos para radar chart */
        .radar-chart {
          margin: 0 auto;
        }
      "))
    ),
    
    tabItems(
      # Pestaña CARGA MEJORADA
      tabItem(tabName = "carga",
              fluidRow(
                box(
                  title = span(icon("upload"), "Cargar y Configurar Datos"), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = FALSE,
                  fluidRow(
                    column(6,
                           selectizeInput("survey_type", "1. Selecciona el Tipo de Cuestionario:", 
                                          choices = names(survey_configs), 
                                          width = "100%",
                                          options = list(placeholder = 'Selecciona un cuestionario...'))
                    ),
                    column(6,
                           fileInput("file_upload", "2. Sube el Archivo Excel",
                                     accept = c(".xlsx", ".xls"), 
                                     buttonLabel = "Examinar...", 
                                     placeholder = "Ningún archivo seleccionado",
                                     width = "100%")
                    )
                  ),
                  hr(),
                  uiOutput("file_status_ui"),
                  br(),
                  h4("Vista Previa de Datos"),
                  DTOutput("data_preview")
                )
              )
      ),
      
      # Pestaña INDIVIDUAL MEJORADA
      tabItem(tabName = "individual",
              fluidRow(
                box(
                  title = span(icon("search"), "Seleccionar Paciente"), 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("rut_select", "Seleccionar RUT:", 
                              choices = NULL,
                              selectize = TRUE),
                  uiOutput("patient_details"),
                  hr()
                ),
                box(
                  title = span(icon("user"), "Información del Paciente"), 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  uiOutput("patient_info")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("chart-bar"), "Gráfico de Barras - Scores Individuales (0-100)"), 
                  status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("individual_scores_plot", height = "500px"),
                  br(),
                  p(strong("Interpretación:")),
                  p("Escalas de síntomas (rojo): alto score = mayor severidad"),
                  p("Escalas de síntomas (rojo): alto score = mayor severidad")
                ),
                box(
                  title = span(icon("star"), "Gráfico de Radar - Perfil Completo"), 
                  status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("individual_radar_plot", height = "500px"),
                  br(),
                )
              ),
              fluidRow(
                box(
                  title = span(icon("table"), "Historial de Respuestas"), 
                  width = 12, 
                  status = "info",
                  div(class="scrollable-table", 
                      DTOutput("individual_responses"))
                )
              )
      ),
      
      # Pestaña GENERAL MEJORADA CON RADAR (reemplaza el heatmap)
      tabItem(tabName = "general",
              fluidRow(
                valueBoxOutput("total_patients", width = 3),
                valueBoxOutput("total_encuestas", width = 3),
                valueBoxOutput("mean_age", width = 3),
                valueBoxOutput("radar_info", width = 3)
              ),
              
              fluidRow(
                box(
                  title = span(icon("star"), "Gráfico de Radar - Promedios Generales"), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("group_var_radar", "Variable para agrupar (opcional):",
                                       choices = c("Ninguna", "Sexo", "Nivel Educacional", 
                                                   "Estado Civil", "Ocupación actual"),
                                       selected = "Ninguna"),
                           actionButton("update_radar", "Actualizar Radar", 
                                        icon = icon("refresh"),
                                        class = "btn-primary")
                    ),
                    column(8,
                           div(class = "radar-container",
                               plotlyOutput("general_radar_plot", height = "600px")
                           )
                    )
                  ),
                  br(),
                )
              ),
              
              # Gráfico de línea para evolución temporal de promedios
              fluidRow(
                box(
                  title = span(icon("chart-line"), "Evolución Temporal de Promedios"), 
                  status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("general_line_plot", height = "400px"),
                  br(),
                )
              ),
              
              fluidRow(
                box(title = span(icon("database"), "Tabla de Datos Completa"), 
                    width = 12, collapsible = TRUE, collapsed = TRUE,
                    status = "primary",
                    div(class="scrollable-table", DTOutput("data_table")))
              )
      ),
      
      # Pestaña EVOLUCIÓN TEMPORAL - MEJORADA
      tabItem(tabName = "evolucion",
              fluidRow(
                box(title = span(icon("sliders-h"), "Configuración de Evolución"), 
                    status = "primary", solidHeader = TRUE, width = 3,
                    selectizeInput("rut_evolution", "Paciente:", 
                                   choices = NULL,
                                   options = list(placeholder = 'Selecciona un paciente...')),
                    radioButtons("evol_mode", "Modo de Visualización:", 
                                 choices = c("Scores EORTC" = "scores", 
                                             "Preguntas Individuales" = "preguntas"),
                                 selected = "scores"),
                    uiOutput("variable_evolution_ui"),
                ),
                box(title = span(icon("chart-line"), "Evolución Temporal"), 
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("evolution_plot", height = "700px")
                )
              )
      ),
      
      # Pestaña CONFIG MEJORADA
      tabItem(tabName = "config",
              fluidRow(
                box(title = span(icon("cogs"), "Estructura de Datos Detectada"), 
                    status = "info", width = 12, 
                    verbatimTextOutput("data_structure"))
              )
      )
    )
  )
)

# ==============================================================================
# SERVER MEJORADO
# ==============================================================================
server <- function(input, output, session) {
  
  # Variables Reactivas
  data_raw <- reactiveVal()       # Datos tal cual vienen del excel
  data_scores <- reactiveVal()    # Datos con scores 0-100 calculados
  survey_metadata <- reactiveVal() # Info de columnas
  radar_data <- reactiveVal()     # Datos procesados para radar general
  
  # Variable reactiva para compatibilidad
  data_reactive <- reactive({ data_raw() })
  
  # Información del sistema
  output$system_info <- renderUI({
    req(data_raw())
    tagList(
      strong("Cuestionario:"), br(), input$survey_type, br(), br(),
      strong("Pacientes:"), br(), length(unique(data_raw()[[2]])), br(), br(),
      strong("Evaluaciones:"), br(), nrow(data_raw())
    )
  })
  
  # Value box para información del radar
  output$radar_info <- renderValueBox({
    valueBox(
      "Radar", 
      "Perfiles de calidad de vida", 
      icon = icon("star"), 
      color = "orange"
    )
  })
  
  # ----------------------------------------------------------------------------
  # Lógica de Carga y Procesamiento
  # ----------------------------------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      # 1. Leer Excel
      df <- read_excel(input$file_upload$datapath)
      
      # Validar columnas mínimas (Primeras 7 fijas)
      if (ncol(df) < 8) stop("El archivo debe tener al menos 8 columnas (7 de metadatos + preguntas)")
      
      # 2. Convertir marca temporal a fecha - MANEJO MEJORADO
      first_col <- df[[1]]
      
      # Intentar diferentes formatos de fecha
      fecha_convertida <- tryCatch({
        if (inherits(first_col, "POSIXt") || inherits(first_col, "Date")) {
          # Ya es fecha
          first_col
        } else if (is.character(first_col)) {
          # Intentar diferentes formatos
          as.POSIXct(first_col, tryFormats = c(
            "%Y-%m-%d %H:%M:%S", 
            "%Y-%m-%d",
            "%d/%m/%Y %H:%M:%S", 
            "%d/%m/%Y",
            "%d-%m-%Y %H:%M:%S",
            "%d-%m-%Y"
          ))
        } else if (is.numeric(first_col)) {
          # Podría ser un número de Excel (días desde 1900)
          as.Date(first_col, origin = "1899-12-30")
        } else {
          # Si no se puede convertir, mantener como está
          first_col
        }
      }, error = function(e) {
        # Si falla la conversión, mantener original
        first_col
      })
      
      df[[1]] <- fecha_convertida
      
      # 3. Convertir preguntas a numérico - CON MANEJO DE ERRORES
      fixed_cols <- names(df)[1:7]
      question_cols <- names(df)[8:ncol(df)]
      
      for(col in question_cols) {
        # Intentar conversión con manejo de valores no numéricos
        suppressWarnings({
          df[[col]] <- as.numeric(as.character(df[[col]]))
        })
      }
      
      data_raw(df)
      
      # 4. Calcular SCORES según configuración
      config_name <- input$survey_type
      config <- survey_configs[[config_name]]
      
      if (config$type == "defined") {
        # Crear dataframe base con los metadatos
        scores_df <- df[, 1:7]
        
        # Iterar sobre cada escala definida
        for (scale in config$scales) {
          # Mapeo de items a nombres de columna reales
          valid_indices <- scale$items[scale$items <= length(question_cols)]
          
          if (length(valid_indices) > 0) {
            col_names <- question_cols[valid_indices]
            
            # Obtener nombres de columnas para reverse (si hay)
            reverse_cols <- NULL
            if (!is.null(scale$reverse)) {
              valid_rev <- scale$reverse[scale$reverse <= length(question_cols)]
              if (length(valid_rev) > 0) {
                reverse_cols <- question_cols[valid_rev]
              }
            }
            
            # Determinar rango (global o específico)
            rango_uso <- if (!is.null(scale$range)) scale$range else config$range_global
            
            # Calcular Score
            scores_df[[scale$name]] <- calculate_score(
              df[, question_cols, drop = FALSE], 
              scale$items, 
              type = scale$type, 
              range_val = rango_uso,
              reverse_rel = scale$reverse
            )
          }
        }
        data_scores(scores_df)
        showNotification(paste("Scores calculados para:", config_name), type = "message")
        
      } else {
        data_scores(NULL) # Modo genérico no calcula scores
      }
      
      # Actualizar metadatos
      survey_metadata(list(
        fixed = fixed_cols,
        questions = question_cols,
        scales = if(!is.null(data_scores())) names(data_scores())[8:ncol(data_scores())] else NULL
      ))
      
      # Actualizar UI Inputs
      ruts <- unique(df[[2]]) # Asumiendo RUT en col 2
      updateSelectInput(session, "rut_select", choices = ruts)
      updateSelectizeInput(session, "rut_evolution", choices = ruts, server = TRUE)
      
      # Procesar datos para radar general
      updateRadarData()
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Función para actualizar datos del radar general
  updateRadarData <- function() {
    req(data_scores())
    
    df_scores <- data_scores()
    
    # Crear variable de agrupación si se selecciona
    if (input$group_var_radar == "Ninguna") {
      # Sin agrupación - usar todos los datos
      df_grouped <- df_scores %>%
        mutate(Group = "Total")
    } else {
      # Usar variable categórica directamente
      group_col <- switch(input$group_var_radar,
                          "Sexo" = "Sexo",
                          "Nivel Educacional" = "Nivel Educacional",
                          "Estado Civil" = "Estado Civil",
                          "Ocupación actual" = "Ocupación actual")
      
      df_grouped <- df_scores %>%
        mutate(Group = .data[[group_col]])
    }
    
    # Obtener nombres de escalas (excluyendo columnas 1-7 de metadatos y "Group")
    scale_cols <- names(df_grouped)[8:ncol(df_grouped)]
    
    # Asegurarse de que scale_cols no incluya "Group"
    scale_cols <- setdiff(scale_cols, "Group")
    
    if (length(scale_cols) == 0) {
      radar_data(NULL)
      return()
    }
    
    # Calcular promedios por grupo - CORRECCIÓN: Excluir "Group" de scale_cols
    radar_df <- df_grouped %>%
      group_by(Group) %>%
      summarise(across(all_of(scale_cols), ~ mean(., na.rm = TRUE))) %>%
      ungroup()
    
    radar_data(radar_df)
  }
  
  # Observador para actualizar radar cuando cambian parámetros
  observeEvent(input$update_radar, {
    req(data_scores())
    updateRadarData()
  })
  
  # También actualizar al cargar datos
  observeEvent(data_scores(), {
    req(data_scores())
    updateRadarData()
  })
  
  # Estado del archivo
  output$file_status_ui <- renderUI({
    if (is.null(data_raw())) {
      div(class = "alert alert-warning", role = "alert",
          icon("exclamation-triangle"), 
          strong(" Esperando archivo..."),
          " Por favor, sube un archivo Excel para comenzar."
      )
    } else {
      div(class = "alert alert-success", role = "alert",
          icon("check-circle"), 
          strong(" Archivo cargado exitosamente!"),
          br(),
          paste("Registros:", nrow(data_raw()), "| Configuración:", input$survey_type)
      )
    }
  })
  
  output$data_preview <- renderDT({
    req(data_raw())
    datatable(
      head(data_raw(), 5), 
      options = list(
        dom = 't',
        scrollX = TRUE,
        pageLength = 5
      ),
      class = 'cell-border stripe hover'
    )
  })
  
  # ----------------------------------------------------------------------------
  # VISTA INDIVIDUAL MEJORADA
  # ----------------------------------------------------------------------------
  # Información del paciente seleccionado
  output$patient_info <- renderUI({
    req(input$rut_select, data_reactive())
    
    rut_col <- names(data_reactive())[2]
    paciente <- data_reactive() %>%
      filter(.data[[rut_col]] == input$rut_select)
    
    if (nrow(paciente) == 0) return(NULL)
    
    # Estadísticas del paciente
    evaluaciones <- nrow(paciente)
    
    # Obtener primera y última fecha
    fecha_col <- names(paciente)[1]
    
    if (inherits(paciente[[fecha_col]], "POSIXct") || inherits(paciente[[fecha_col]], "Date")) {
      primera_eval <- min(paciente[[fecha_col]], na.rm = TRUE)
      ultima_eval <- max(paciente[[fecha_col]], na.rm = TRUE)
      dias_seguimiento <- as.numeric(difftime(ultima_eval, primera_eval, units = "days"))
      
      fecha_info <- tagList(
        p(icon("calendar-plus"), strong("Primera evaluación:"), 
          if(!is.na(primera_eval)) format(primera_eval, "%d/%m/%Y %H:%M") else "No disponible"),
        p(icon("calendar-day"), strong("Última evaluación:"), 
          if(!is.na(ultima_eval)) format(ultima_eval, "%d/%m/%Y %H:%M") else "No disponible")
      )
    } else {
      fecha_info <- p(icon("calendar-check"), strong("Fecha no disponible en formato correcto"))
      dias_seguimiento <- NA
    }
    
    tagList(
      fluidRow(
        column(6,
               h4("📊 Estadísticas de Seguimiento"),
               p(icon("calendar-check"), strong("Total de evaluaciones:"), evaluaciones),
               p(icon("clock"), strong("Período de seguimiento:"), 
                 if(!is.na(dias_seguimiento) && evaluaciones > 1) paste(round(dias_seguimiento), "días") else "Evaluación única"),
               fecha_info
        ),
        column(6,
               h4("👤 Información Demográfica"),
               p(icon("user"), strong("RUT:"), input$rut_select),
               p(icon("birthday-cake"), strong("Edad:"), paciente$`Edad (en años)`[1], "años"),
               p(icon("venus-mars"), strong("Sexo:"), paciente$Sexo[1]),
               p(icon("graduation-cap"), strong("Educación:"), paciente$`Nivel Educacional`[1]),
               p(icon("heart"), strong("Estado Civil:"), paciente$`Estado Civil`[1]),
               p(icon("briefcase"), strong("Ocupación:"), paciente$`Ocupación actual`[1])
        )
      )
    )
  })
  
  # Gráfico de barras individual
  output$individual_scores_plot <- renderPlotly({
    req(input$rut_select, data_scores())
    
    # Filtrar datos del paciente seleccionado (última evaluación)
    rut_col <- names(data_scores())[2]
    df_pt <- data_scores() %>% 
      filter(.data[[rut_col]] == input$rut_select)
    
    if (nrow(df_pt) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "No hay datos disponibles para este paciente",
                   font = list(color = '#7f8c8d')
                 )
               ))
    }
    
    # Tomar la última evaluación
    fecha_col <- names(data_scores())[1]
    df_pt <- df_pt %>%
      arrange(desc(.data[[fecha_col]])) %>%
      slice(1)
    
    # Obtener columnas de escalas
    scale_cols <- names(df_pt)[8:ncol(df_pt)]
    scores <- as.numeric(df_pt[1, scale_cols])
    
    # Obtener tipos de escalas para colorear
    config <- survey_configs[[input$survey_type]]
    scale_types <- setNames(rep("Desconocido", length(scale_cols)), scale_cols)
    
    if (!is.null(config$scales)) {
      for(s in config$scales) {
        if (s$name %in% scale_cols) {
          scale_types[s$name] <- s$type
        }
      }
    }
    
    # Colores según tipo de escala
    color_map <- c(
      "functional" = "#2ecc71", 
      "global" = "#3498db", 
      "symptom" = "#e74c3c", 
      "Desconocido" = "#95a5a6"
    )
    
    # Crear dataframe para el gráfico
    plot_data <- data.frame(
      Escala = factor(scale_cols, levels = scale_cols),
      Score = scores,
      Tipo = factor(scale_types[scale_cols], levels = c("functional", "global", "symptom", "Desconocido"))
    )
    
    plot_ly(plot_data, x = ~Escala, y = ~Score, type = 'bar',
            color = ~Tipo, colors = color_map,
            text = ~paste("Score:", round(Score, 1)),
            textposition = 'auto',
            marker = list(line = list(color = 'rgba(0,0,0,0.2)', width = 1))) %>%
      layout(
        title = list(
          text = paste("Scores Individuales -", input$rut_select),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Escalas",
          tickangle = -45
        ),
        yaxis = list(
          title = "Score (0-100)",
          range = c(0, 100)
        ),
        margin = list(b = 100, t = 50),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        )
      )
  })
  
  # Gráfico de radar individual
  output$individual_radar_plot <- renderPlotly({
    req(input$rut_select, data_scores())
    
    # Filtrar datos del paciente seleccionado (última evaluación)
    rut_col <- names(data_scores())[2]
    df_pt <- data_scores() %>% 
      filter(.data[[rut_col]] == input$rut_select)
    
    if (nrow(df_pt) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "No hay datos disponibles para este paciente",
                   font = list(color = '#7f8c8d')
                 )
               ))
    }
    
    # Tomar la última evaluación
    fecha_col <- names(data_scores())[1]
    df_pt <- df_pt %>%
      arrange(desc(.data[[fecha_col]])) %>%
      slice(1)
    
    # Obtener columnas de escalas
    scale_cols <- names(df_pt)[8:ncol(df_pt)]
    scores <- as.numeric(df_pt[1, scale_cols])
    
    # Para el radar, necesitamos cerrar el polígono (repetir el primer punto al final)
    scores_radar <- c(scores, scores[1])
    escalas_radar <- c(scale_cols, scale_cols[1])
    
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers',
      r = scores_radar,
      theta = escalas_radar,
      fill = 'toself',
      fillcolor = 'rgba(52, 152, 219, 0.3)',
      line = list(color = 'rgb(52, 152, 219)', width = 3),
      marker = list(
        color = 'rgb(52, 152, 219)',
        size = 8,
        symbol = 'circle'
      ),
      name = input$rut_select
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            tickfont = list(size = 10)
          ),
          angularaxis = list(
            tickfont = list(size = 10),
            rotation = 90
          )
        ),
        title = list(
          text = paste("Perfil de Calidad de Vida -", input$rut_select),
          font = list(size = 16)
        ),
        margin = list(t = 50, b = 50, l = 50, r = 50),
        showlegend = FALSE
      )
  })
  
  output$individual_responses <- renderDT({
    req(input$rut_select, data_raw())
    rut_col <- names(data_raw())[2]
    df <- data_raw() %>% filter(.data[[rut_col]] == input$rut_select)
    
    datatable(
      df,
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
      class = 'cell-border stripe hover'
    )
  })
  
  # ----------------------------------------------------------------------------
  # VISTA GENERAL CON RADAR
  # ----------------------------------------------------------------------------
  # Value Boxes
  output$total_patients <- renderValueBox({
    req(data_raw())
    valueBox(
      length(unique(data_raw()[[2]])), 
      "Pacientes Únicos", 
      icon = icon("users"), 
      color = "aqua"
    )
  })
  
  output$total_encuestas <- renderValueBox({
    req(data_raw())
    valueBox(
      nrow(data_raw()), 
      "Total de Evaluaciones", 
      icon = icon("clipboard-list"), 
      color = "green"
    )
  })
  
  output$mean_age <- renderValueBox({
    req(data_raw())
    avg <- mean(as.numeric(data_raw()[[3]]), na.rm=TRUE) # Asumiendo Edad col 3
    valueBox(
      round(avg, 1), 
      "Edad Promedio", 
      icon = icon("calendar-alt"), 
      color = "purple"
    )
  })
  
  # Radar para vista general
  output$general_radar_plot <- renderPlotly({
    req(radar_data())
    
    radar_df <- radar_data()
    
    if (is.null(radar_df) || nrow(radar_df) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "No hay datos disponibles para generar el radar",
                   font = list(color = '#7f8c8d')
                 )
               ))
    }
    
    # Obtener columnas de escalas (todas excepto Group)
    scale_cols <- names(radar_df)[-1]
    
    # Limitar a un número razonable de escalas para el radar
    if (length(scale_cols) > 12) {
      scale_cols <- scale_cols[1:12]  # Tomar las primeras 12 escalas
    }
    
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    
    # Colores para diferentes grupos
    colors <- c('#e74c3c', '#3498db', '#2ecc71', '#f39c12', '#9b59b6', '#1abc9c', '#d35400')
    
    for (i in 1:nrow(radar_df)) {
      group_name <- radar_df$Group[i]
      scores <- as.numeric(radar_df[i, scale_cols])
      
      # Cerrar el polígono (repetir primer punto al final)
      scores_radar <- c(scores, scores[1])
      escalas_radar <- c(scale_cols, scale_cols[1])
      
      color <- colors[(i-1) %% length(colors) + 1]
      
      p <- p %>% add_trace(
        r = scores_radar,
        theta = escalas_radar,
        name = group_name,
        fillcolor = paste0('rgba(', col2rgb(color)[1], ',', col2rgb(color)[2], ',', col2rgb(color)[3], ',0.2)'),
        line = list(color = color, width = 2),
        marker = list(color = color, size = 6),
        fill = 'toself'
      )
    }
    
    p %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            tickfont = list(size = 10)
          ),
          angularaxis = list(
            tickfont = list(size = 10),
            rotation = 90
          )
        ),
        title = list(
          text = "Perfiles de Calidad de Vida - Promedios",
          font = list(size = 16)
        ),
        margin = list(t = 50, b = 50, l = 50, r = 50),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        )
      )
  })
  
  # Gráfico de línea para evolución temporal de promedios - CORREGIDO
  output$general_line_plot <- renderPlotly({
    req(data_scores())
    
    df_scores <- data_scores()
    
    # Agrupar por fecha - CORRECCIÓN
    fecha_col <- names(df_scores)[1]
    
    # Verificar si la columna de fecha es de tipo fecha
    if (inherits(df_scores[[fecha_col]], "POSIXct") || inherits(df_scores[[fecha_col]], "Date")) {
      df_temporal <- df_scores %>%
        mutate(Fecha = as.Date(.data[[fecha_col]])) %>%
        group_by(Fecha) %>%
        summarise(across(8:ncol(df_scores), ~ mean(., na.rm = TRUE))) %>%
        ungroup() %>%
        arrange(Fecha)
    } else {
      # Si no es fecha, crear un índice secuencial
      df_temporal <- df_scores %>%
        mutate(Fecha = row_number()) %>%
        group_by(Fecha) %>%
        summarise(across(8:ncol(df_scores), ~ mean(., na.rm = TRUE))) %>%
        ungroup() %>%
        arrange(Fecha)
    }
    
    if (nrow(df_temporal) < 2) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "Se necesitan al menos 2 puntos temporales",
                   font = list(color = '#7f8c8d')
                 )
               ))
    }
    
    # Seleccionar algunas escalas principales para mostrar
    scale_cols <- names(df_temporal)[2:ncol(df_temporal)]
    if (length(scale_cols) > 8) {
      scale_cols <- scale_cols[1:8]  # Limitar a 8 escalas para claridad
    }
    
    # Crear gráfico de líneas
    p <- plot_ly(df_temporal, x = ~Fecha)
    
    colors <- RColorBrewer::brewer.pal(min(8, length(scale_cols)), "Set2")
    
    for (i in seq_along(scale_cols)) {
      scale <- scale_cols[i]
      color <- colors[(i-1) %% length(colors) + 1]
      
      p <- p %>% add_trace(
        y = as.formula(paste0("~`", scale, "`")),
        name = scale,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = color, width = 3),
        marker = list(color = color, size = 8)
      )
    }
    
    p %>%
      layout(
        title = list(
          text = "Evolución Temporal de Scores Promedio",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Fecha"
        ),
        yaxis = list(
          title = "Score Promedio (0-100)",
          range = c(0, 100)
        ),
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        ),
        margin = list(t = 50, b = 100)
      )
  })
  
  # Tabla de datos completa
  output$data_table <- renderDT({
    req(data_raw())
    df_show <- if(!is.null(data_scores())) data_scores() else data_raw()
    datatable(
      df_show, 
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    )
  })
  
  # ----------------------------------------------------------------------------
  # EVOLUCIÓN TEMPORAL
  # ----------------------------------------------------------------------------
  output$variable_evolution_ui <- renderUI({
    req(survey_metadata(), input$evol_mode)
    
    if (input$evol_mode == "preguntas") {
      choices <- survey_metadata()$questions
    } else {
      req(data_scores())
      # Usar los nombres reales de las columnas de scores
      scale_cols <- names(data_scores())[8:ncol(data_scores())]
      choices <- setNames(scale_cols, scale_cols)
    }
    
    selectizeInput("var_evol", "Seleccionar Variables:", 
                   choices = choices, 
                   multiple = TRUE,
                   options = list(maxItems = 5, placeholder = 'Selecciona variables...'))
  })
  
  # Estadísticas de evolución
  output$evolution_stats <- renderUI({
    req(input$rut_evolution, data_scores())
    
    rut_col <- names(data_scores())[2]
    df_pt <- data_scores() %>% 
      filter(.data[[rut_col]] == input$rut_evolution)
    
    if (nrow(df_pt) < 2) {
      return(p("Se necesitan al menos 2 evaluaciones para calcular estadísticas de evolución."))
    }
    
    # Ordenar por fecha
    fecha_col <- names(data_scores())[1]
    df_pt <- df_pt %>%
      arrange(.data[[fecha_col]])
    
    # Calcular cambios promedio
    scale_cols <- names(df_pt)[8:ncol(df_pt)]
    
    changes <- sapply(scale_cols, function(scale) {
      vals <- df_pt[[scale]]
      if (length(vals) >= 2) {
        return(round(vals[length(vals)] - vals[1], 1))
      } else {
        return(NA)
      }
    })
    
    # Identificar las 3 escalas con mayor mejora y empeoramiento
    valid_changes <- changes[!is.na(changes)]
    
    if (length(valid_changes) == 0) {
      return(p("No se pudieron calcular cambios en las escalas."))
    }
    
    improvements <- sort(valid_changes, decreasing = TRUE)[1:min(3, length(valid_changes))]
    worsenings <- sort(valid_changes)[1:min(3, length(valid_changes))]
    
    tagList(
      h5("Resumen de Cambios:"),
      p(strong("Mayores mejoras:")),
      tags$ul(
        lapply(names(improvements), function(name) {
          tags$li(paste(name, ": ", improvements[name], " puntos"))
        })
      ),
      p(strong("Mayores empeoramientos:")),
      tags$ul(
        lapply(names(worsenings), function(name) {
          tags$li(paste(name, ": ", worsenings[name], " puntos"))
        })
      )
    )
  })
  
  # Gráfico de evolución temporal - CORREGIDO
  output$evolution_plot <- renderPlotly({
    req(input$rut_evolution, input$var_evol, input$evol_mode)
    
    tryCatch({
      # Seleccionar dataset correcto
      if(input$evol_mode == "scores") {
        df_source <- data_scores()
      } else {
        df_source <- data_raw()
      }
      
      req(df_source)
      
      # Filtrar por RUT
      rut_col <- names(df_source)[2]
      df_pt <- df_source %>% 
        filter(.data[[rut_col]] == input$rut_evolution)
      
      if(nrow(df_pt) == 0) {
        return(plotly_empty() %>%
                 layout(
                   title = list(
                     text = "No hay datos disponibles para este paciente",
                     font = list(color = '#7f8c8d')
                   )
                 ))
      }
      
      # Ordenar por fecha
      fecha_col <- names(df_source)[1]
      df_pt <- df_pt %>%
        arrange(.data[[fecha_col]])
      
      # Obtener valores de fecha
      x_vals <- df_pt[[fecha_col]]
      
      # Si no es fecha, convertir a secuencial
      if (!inherits(x_vals, "POSIXct") && !inherits(x_vals, "Date")) {
        x_vals <- seq_along(x_vals)
      }
      
      p <- plot_ly(df_pt, x = x_vals)
      
      # Paleta de colores para múltiples variables
      colors <- c('#e74c3c', '#3498db', '#2ecc71', '#f39c12', '#9b59b6')
      
      for(i in seq_along(input$var_evol)) {
        var <- input$var_evol[i]
        color <- colors[(i-1) %% length(colors) + 1]
        
        # Verificar que la variable existe en el dataframe
        if(var %in% names(df_pt)) {
          y_vals <- df_pt[[var]]
          
          p <- p %>% add_trace(y = y_vals, 
                               name = var, type = 'scatter', mode = 'lines+markers',
                               line = list(color = color, width = 3),
                               marker = list(color = color, size = 8))
        }
      }
      
      y_title <- if(input$evol_mode == "scores") "Score (0-100)" else "Respuesta"
      
      # Rango fijo de 0-100 para scores
      y_range <- if(input$evol_mode == "scores") c(0, 100) else NULL
      
      p %>% layout(
        title = list(
          text = paste("Evolución Temporal -", input$rut_evolution),
          font = list(size = 16)
        ),
        yaxis = list(
          title = y_title,
          range = y_range
        ),
        xaxis = list(title = if(inherits(x_vals, "POSIXct") || inherits(x_vals, "Date")) "Fecha" else "Evaluación"),
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.3
        )
      )
    }, error = function(e) {
      showNotification(paste("Error en gráfico de evolución:", e$message), type = "error")
      return(plotly_empty())
    })
  })
  
  # ----------------------------------------------------------------------------
  # CONFIGURACIÓN
  # ----------------------------------------------------------------------------
  output$data_structure <- renderPrint({
    req(data_raw())
    cat("=== ESTRUCTURA DE DATOS CRUDOS ===\n")
    str(data_raw())
    if(!is.null(data_scores())) {
      cat("\n\n=== ESTRUCTURA DE SCORES CALCULADOS ===\n")
      str(data_scores())
    }
  })
}

shinyApp(ui, server)

