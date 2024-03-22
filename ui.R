# ui.R -------------------------------------------------------------------------
# Description: Este script crea la interfaz de usuario de la aplicación de
# Shiny.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar la UI ------------------------------------------------------------
fluidPage(
  fluidRow(
    box(width = 8,
        background = "red",
        div(
          HTML(paste0(
            '<font color="black"><strong>',
            "Este tablero es producto del curso avanzado de R organizado por OPS en marzo de 2024.
    Los datos presentados en este tablero son completamente ficticios y han sido creados 
    únicamente con fines didácticos. Cualquier similitud con datos reales, personas o 
    eventos es pura coincidencia y no debe interpretarse como una representación exacta 
    de la realidad. Este contenido está diseñado para ilustrar conceptos y promover el 
    aprendizaje a través de ejemplos construidos para este fin.",
    '</strong></font>'
          )
          ))
    )
  ),
  titlePanel(span(img(src="GrupoTesoroLogo.png", height = 50), "DASHBOARD CAMPAÑA SARAMPIÓN 2024, YURUGUAY")),
  ## CSS -------------------------------------------------------------------------
  includeCSS("style.scss"),
  # Dashboard title ------------------------------------------------------------
  
  ## Inicializar dashboard -------------------------------------------------------
  dashboardPage(
    
    ## Header dashboard ------------------------------------------------------------
    dashboardHeader(title = paste0(
                          Sys.Date()
                          )
                    ),

    ## Sidebar dashboard -----------------------------------------------------------
    dashboardSidebar(
      sidebarMenu(
        # Modulo 1: Inicio
        menuItem(text = "Inicio",
                 tabName = "inicio",
                 icon = icon("home"),
                 selected = TRUE
        ),
        # Modulo 2: Justificacion
        menuItem(text = "Justificación",
                 tabName = "justificacion",
                 icon = icon("th-large")
        ),
        # Modulo 3: Avance Campaña
        menuItem(text = "Avance Campaña",
                 tabName = "avance_campana",
                 icon = icon("th-large")
        ),
        # Modulo 4: Georreferenciacion
        menuItem(text = "Georreferenciacion",
                 tabName = "georreferenciacion",
                 icon = icon("th-large")
        )
      )
    ),
    ## Cuerpo dashboard ------------------------------------------------------------
    dashboardBody(
      tabItems(
        ### Inicio -------------------------------------------------------------
        tabItem(tabName = "inicio",
                fluidRow(box(
                  width = 12,
                  title = HTML("<b>CAMPAÑA DE VACUNACIÓN SRP,<br/> YURUGUAY 2024</b>"),
                  textOutput(outputId = "inicio_textbox")
                )
                ),
        #Imagen
        fluidRow(
          box( width = 8, height = 800,
               imageOutput(outputId = "Imagen")),
          box(
            width = 4,
            uiOutput(outputId = "Descripcion_equipo")
          )
        )),
        
        ### Justificacion ------------------------------------------------------
        tabItem(tabName = "justificacion",
                fluidRow(
                  box(
                    width = 12,
                    title = HTML("<b>Justificación</b>"),
                    textOutput(outputId = "justificacion_textbox"))
                ),
                fluidRow(
                box(
                  width = 6, height = 500,
                  plotOutput (outputId = "grafica_susc")
                ),
                box(
                  width = 6, height = 500,
                  dataTableOutput (outputId = "tabla_susc")
                )
                )),
        ### Avance de campaña --------------------------------------------------
        tabItem(tabName = "avance_campana",
                fluidRow(
                  box(
                    width = 12,
                    title = HTML("<b>Avance de Campaña</b>"),
                    textOutput(outputId = "avance_campana_textbox")  
                  )),
                fluidRow(
                  box(
                    width = 6, height = 500,
                    h5(" "),
                    title = "Cobertura Nacional",
                    plotlyOutput(outputId = "grafica_nacional")
                  ),
                  box(
                    width = 6, 
                    selectInput(#selector
                      inputId = "selector_departamento",
                      label = "Departamento",
                      choices = unique(campana_departamento$departamento_res_mad)
                    ),
                    plotlyOutput(
                      outputId = "grafica_departamento"
                    )
                  ) 
                  )
        ),
        ### Georreferenciación -------------------------------------------------
        tabItem(tabName = "georreferenciacion",
                fluidRow(
                  box(
                    width = 12,
                    title = HTML("<b>Georreferenciación</b>"),
                    textOutput(outputId = "georreferenciacion_textbox")
                  )),
                fluidRow(
                  box(  #Caja mapa 1
                    width = 6, height = 600,
                    title ="Avance campaña",
                    selectInput(#selector
                      inputId = "selector_cobertura",
                      label = "Rango de cobertura",
                      choices = c("<=20%", 
                                  "20% - 40%",
                                  "40% - 60%", 
                                  "60% - 80%", 
                                  "> 80%"),
                      selected = "40% - 60%"
                    ),
                    leafletOutput(
                      outputId = "mapa"
                    )
                  ),
                  box( # caja mapa 2
                    width = 6, height = 600,
                    title ="No vacunados",
                    selectInput(#selector
                      inputId = "selector_no_vacunados",
                      label = "Departamento",
                      choices = c(unique(datos_map$ADM1_ISON)), 
                      multiple = TRUE,
                      selected = c(unique(datos_map$ADM1_ISON))
                    ),
                    leafletOutput(
                      outputId = "mapa_no_vacunados"
                    )
                    
                  ),
                  box(
                    width = 12,
                    textOutput(
                      outputId = "pie_pagina"
                    )
                  )
                )
                )
          
        ) # fin del tabItems
      ) # fin del dashboardBody
    )  # fin del dashboardPage
  )  # fin del fluidPage
