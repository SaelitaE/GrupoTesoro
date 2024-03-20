# server.R ---------------------------------------------------------------------
# Description: Este script crea un servidor, que representa una sesión de R
# que corre código y devuelve resultados (p. ej., una gráfica).
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar el servidor ------------------------------------------------------
shinyServer(function(input, output) {
  ## Elementos del UI ----------------------------------------------------------
  ### Inicio -------------------------------------------------------------------
  # Cuadro informativo para seccion de Inicio
  output$inicio_textbox <- renderText({
    "El equipo tesoro esta integrado por los paises de Mexico, Guatemala, Chile, Paraguay, y se encargan de analizar las coberturas de vacunación de los paises de Centroamerica"
  })

  
  #Imagen 
  output$logo_imagen <- renderImage({
    list(
      src = "GrupoTesoroLogo.png",
      contentType = "image/png",
      width = "auto",
      height = "100"
    )
  }, deleteFile = FALSE)
  
 

  

  #Imagen módulo de inicio
  output$Imagen <- renderImage({
    list(src = "a.png",
         contenType = "image/png",
         width = "100%",
         heigth = "100%"
    )})
  
  output$Descripcion_equipo <- renderUI({
    HTML(paste("Pamela Burgos - Chile",
    "Shaily Escobar - Guatemala",
    "Rodrigo Martinez - Paraguay",
    "Alfredo Zatarain - México",sep = "<br/>"))
  }
  )

  ### Justificacion ------------------------------------------------------------
  # Cuadro informativo para seccion de Justificacion
  output$justificacion_textbox <- renderText({
    "TExto de la justificación"
  })
  
  output$grafica_susc <- renderPlot({
    
    grafica <- ggplot(data = susc_anio, 
                      aes(x = anio,
                          y = susceptibles_acumulado)
                      
    )
    
    grafica <-  grafica +
      geom_bar(stat = "identity",
               fill = "#253494"
               )+
      theme_classic()+
          labs(x = "Año",
           y = "Cantidad de susceptibles"
      )
    
   grafica
    
  })
  
  output$tabla_susc <- renderDataTable({
    datatable(susc_anio, class = "compact")
  })
  
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"
  })
  
  output$grafica_nacional <- renderPlotly({
    grafica <- ggplot(data = tabla_grafica, 
                      aes(x = fecha_vac,
                          y = vacunados)
    )
    
    grafica <-  grafica +
      geom_bar(stat = "identity",
               fill = "#253494"
      )+
      theme_classic()+
      labs(x = "Fecha",
           y = "Cantidad de vacunados"
      )
    
    grafica <- grafica +
      geom_line(data = tabla_grafica,
                aes(x = fecha_vac,
                    y = cobertura_acumulada),
                inherit.aes = FALSE)
    
    grafica
    
    grafica_interactiva_nacional <- ggplotly (grafica) %>% 
      # Debemos agregar el segundo eje de nuevo, esta vez manualmente,
      # mediante la función add_lines de plotly
      add_lines(
        x = ~fecha_vac, y = ~cobertura_acumulada, data = tabla_grafica,
        yaxis = "y2"
      ) %>% 
      # hacemos algunas configuraciones al eje y secundario y a los márgenes,
      # para que nuestra gráfica se vea bien
      layout(
        # configuraciones al nuevo eje vertical
        yaxis2 = list(
          tickfont = list(size = 16),
          titlefont = list(size = 18),
          overlaying = "y",
          nticks = 10,
          side = "right",
          title = "Cobertura (%)",
          # limitamos el eje entre 0 y 100%
          range = c(0,100),
          showline = TRUE
        ),
        # agregamos un poco de margen a la derecha para que quepa el nuevo eje
        # vertical
        margin = list(r = 100)
      )
    
    grafica_interactiva_nacional
  })
  
  ## Por depto y selector ----
  
 departamento_reactive <- reactive({
    campana_departamento %>% 
      filter(departamento_res_mad == input$selector_departamento)
  })
  
    output$grafica_departamento <- renderPlotly({
    
      cobertura_acumulada_depto <-departamento_reactive() %>% 
        ungroup() %>% 
        arrange(fecha_vac) %>% 
        mutate(cob_acumulada= cumsum(cobertura))
      
      View(cobertura_acumulada_depto)
      
      grafica <- ggplot(data = cobertura_acumulada_depto, 
                        aes(x = fecha_vac,
                            y = vacunados)
      )
      
      grafica <-  grafica +
        geom_bar(stat = "identity",
                 fill = "#253494"
        )+
        theme_classic()+
        labs(x = "Fecha",
             y = "Cantidad de vacunados"
        )
      
      grafica <- grafica +
        geom_line(data = cobertura_acumulada_depto,
                  aes(x = fecha_vac,
                      y = cob_acumulada),
                  inherit.aes = FALSE)
      
      grafica
      
      grafica_interactiva_nacional <- ggplotly (grafica) %>% 
        # Debemos agregar el segundo eje de nuevo, esta vez manualmente,
        # mediante la función add_lines de plotly
        add_lines(
          x = ~fecha_vac, y = ~cob_acumulada, data = cobertura_acumulada_depto,
          yaxis = "y2"
        ) %>% 
        # hacemos algunas configuraciones al eje y secundario y a los márgenes,
        # para que nuestra gráfica se vea bien
        layout(
          # configuraciones al nuevo eje vertical
          yaxis2 = list(
            tickfont = list(size = 16),
            titlefont = list(size = 18),
            overlaying = "y",
            nticks = 10,
            side = "right",
            title = "Cobertura (%)",
            # limitamos el eje entre 0 y 100%
            range = c(0,100),
            showline = TRUE
          ),
          # agregamos un poco de margen a la derecha para que quepa el nuevo eje
          # vertical
          margin = list(r = 100)
        )
      
      grafica_interactiva_nacional
    
  })
  
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})
  