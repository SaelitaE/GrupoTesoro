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
    "El equipo Tesoro está integrado por colaboradores de los paises de México, Guatemala, Chile y Paraguay, su objetivo es evaluar las coberturas de vacunación durante la campaña de vacunación de SPR en Yuruguay"
  })

  #Titulo tablero
  # output$titulotablero <- renderText({
  #   "Título"
  #   })
  
  
  #Imagen 
  output$logo_imagen <- renderImage({
    list(
      src = "GrupoTesoroLogo.png",
      contentType = "image/png",
      width = "auto",
      height = 100
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
    HTML(paste("<b>Pamela Burgos - Chile</b>",
    "<b>Shaily Escobar - Guatemala</b>",
    "<b>Rodrigo Martinez - Paraguay</b>",
    "<b>Alfredo Zatarain - México</b>",sep = "<br/>"))
  }
  )

  ### Justificacion ------------------------------------------------------------
  # Cuadro informativo para seccion de Justificacion
  output$justificacion_textbox <- renderText({
    "Las estrategias recomendadas por OPS para mantener la eliminación del sarampión y rubéola establecen que es necesario asegurar la inmunidad de la población evaluando la cantidad de susceptibles y programando una campaña de seguimiento cada 4-5 años o cuando la cantidad de susceptibles es similar a una cohorte de nacidos vivos.
A partir de la recomendación de OPS, el Ministerio de Salud de Yuruguay calculó las cohortes de nacidos vivos susceptibles al sarampión y rubéola desde el año 2018. Como se muestra en el primer gráfico, donde el número de susceptibles al sarampión es de 30,989 niños y niñas, cifra que se acerca a la cohorte de nacimiento (31,188/2023), lo que justifica la implementación de la campaña."
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
  
  # tabla justif. -------------------------------
  output$tabla_susc <- renderDataTable({
    datatable(susc_anio_mun, 
              class = "compact",
              colnames = c("Año", "Municipio", "Población", "Total susceptibles"))
  })
  
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Desde el 04 de marzo a la fecha el avance de cobertura  a nivel nacional y por departamento es el siguiente:"
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
                    y = cobertura_acumulada * 60
                    ),
                color = "red",
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
                  color = "red",
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
    "A continuación se presenta en el primer mapa la cobertura de vacunación a nivel nacional por rango de cobertura. En el segundo mapa se puede observar el número de niños no vacunados por departamento."
    })
  
    # Selectores mapa -----------------------------------------------------------
    rango_cob_reactive <- reactive({
      datos_map %>% 
        filter(rango_cob == input$selector_cobertura)
    })
    
# Output del mapa ----------------------------------------------------
    output$mapa <- renderLeaflet ({

      # Mapa interactivo --------------------------------------------------------
      #breaks <- quantile(datos_map$cobertura, na.rm = T)
      
      breaks <- c(0, 20, 40, 60, 80, 100)
      
      pal <- colorBin(c("#24693D","#8CCE7D", "orange" ,"#EACF65", "#BF233C"),
                      reverse = T , domain = datos_map$cobertura, bins = breaks)
      
    # labels_cor <- sprintf("<b>%s", paste("Avance",datos_map$ADM2_ISON, datos_map$cobertura))       %>%      lapply(htmltools::HTML)

      
      map <- leaflet(rango_cob_reactive()) %>% 
        setView(-55.5, -32.5, zoom = 6) %>% 
        addProviderTiles("OpenStreetMap") %>% 
        addEasyButton(
          easyButton(
            icon = "fa-globe",
            title = "Zoom Inicial",
            onClick = JS("function(btn, map){ map.setZoom(6); }")
          )
        )
      
      map <-map %>% 
        addPolygons(
          fillColor = ~pal(cobertura),
          color = "lightgray",
          dashArray = "3",
          fillOpacity = 0.7
          #,          label = labels_cor
          )%>% 
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = ~cobertura,
          na.label = "Sin Dato",
          title = "Cobertura campaña")
    
      
      map
      
    })
    
    
    
    # Selectores mapa2 (NO VACUNADOS) -----------------------------------------------------------
    no_vac_reactive <- reactive({
      datos_map %>% 
        filter(ADM1_ISON %in% input$selector_no_vacunados) # Pendiente
    })
    
    # Output del mapa NO VACUNADOS ----------------------------------------------------
    output$mapa_no_vacunados <- renderLeaflet ({
      
      #breaks <- c(0, 500, 1000, 1500, 2000, 3500, 4000)
      breaks <- quantile(datos_map$no_vacunados, na.rm = T)
      
      pal <- colorBin(c("#24693D","#8CCE7D", "orange" ,"#EACF65", "#BF233C", "darkred"),
                      reverse = F , domain = datos_map$no_vacunados, bins = breaks)
      
      # labels_cor <- sprintf("<b>%s", paste("Avance",datos_map$ADM2_ISON, datos_map$cobertura))       %>%      lapply(htmltools::HTML)
   
      
      map_no_vacunados <- leaflet(no_vac_reactive()) %>% 
        setView(-55.5, -32.5, zoom = 6) %>% 
        addProviderTiles("OpenStreetMap") %>% 
        addEasyButton(
          easyButton(
            icon = "fa-globe",
            title = "Zoom Inicial",
            onClick = JS("function(btn, map){ map.setZoom(6); }")
          )
        )
      
      map_no_vacunados <-map_no_vacunados %>% 
        addPolygons(
          fillColor = ~pal(no_vacunados),
          color = "lightgray",
          dashArray = "3",
          fillOpacity = 0.7
          #,          label = labels_cor
        )%>% 
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = ~no_vacunados,
          na.label = "Sin Dato",
          title = "No vacunados </br>
          en campaña",
          bins = 5)

      
      map_no_vacunados
      
      
    })    

    output$pie_pagina <- renderText({
      "A la fecha hay un total de 2,718 niños y niñas que han recibido una dosis de SRP y no reportan lugar de residencia."
    })  

})
  