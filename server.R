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
    "Las estrategias recomendadas por OPS para mantener la eliminación del sarampión y rubéola establecen que es necesario asegurar la inmunidad de la población evaluando la cantidad de susceptibles y programando una campaña de seguimiento cada 4-5 años o cuando la cantidad de susceptibles es similar a una cohorte de nacidos vivos.
A partir de la recomendación de OPS, el Ministerio de Salud de Yuruguay calculó las cohortes de nacidos vivos susceptibles al sarampión y rubéola desde el año 2018. Como se muestra en el primer gráfico, donde el número de susceptibles al sarampión es de 225.163 niños y niñas, cifra que supera la cohorte de nacimiento (247.437/año), lo que justifica la implementación de la campaña."
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
    datatable(susc_anio_mun, class = "compact")
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
                    y = cobertura_acumulada * 60),
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
    "A partir del 04 de marzo hasta el 30 de marzo del 2024, todos los niños y niñas desde los 12 meses a los 4 años 11 meses 29 días deben recibir una dosis extra de vacuna SRP, independientemente de las dosis recibidas previamente. 
El propósito de la campaña es elevar el nivel de inmunidad de este grupo objetivo y mantener la eliminación del sarampión, la rubéola y SRC en Yuruguay.
El objetivo de la campaña es alcanzar una cobertura igual o mayor al 95% en la población definida en cada uno de los municipios del país."
  })
  
    # Selectores mapa -----------------------------------------------------------
    rango_cob_reactive <- reactive({
      datos_map %>% 
        filter(rango_cob == input$selector_cobertura)
    })
    
# Output del mapa ----------------------------------------------------
    output$mapa <- renderLeaflet ({
      
      # corropletas <- ggplot()+ 
      #   geom_sf(data = datos_map,
      #           aes(fill = rango_cob,
      #               geometry = geometry),
      #           color = '#969696',
      #           size = .9)+
      #   scale_fill_manual("Porcentaje de cobertura", 
      #                     values = c("<=20%" = "#2596be",
      #                                "20% - 40%"= "#51abcb",
      #                                "40% - 60%"= "#7cc0d8",
      #                                "60% - 80%"= "#a8d5e5",
      #                                "> 80%" = "#e9f5f9"
      #                     )
      #   ) +
      #   labs(title = "Porcentaje de avance campaña vacunación 2024",
      #        caption = "Fuente : MinSa Yuruguay")+
      #   theme_void()+ # Personalización adicional del tema del gráfico. 
      #   theme(title=element_text(face = "bold"), #Establece el estilo del título en negrita,  
      #         legend.position= c(.9, .3), 
      #         legend.justification='left', # la posición y la orientación de la leyenda,
      #         legend.direction='vertical',
      #         legend.text=element_text(size=10)) # y el tamaño del texto de la leyenda.

      # Mapa interactivo --------------------------------------------------------
      #breaks <- quantile(datos_map$cobertura, na.rm = T)
      breaks <- c(0, 20, 40, 60, 80, 100)
      pal <- colorBin(c("#24693D","#8CCE7D", "orange" ,"#EACF65", "#BF233C"),
                      reverse = T , domain = datos_map$cobertura, bins = breaks)
      
    # labels_cor <- sprintf("<b>%s", paste("Avance",datos_map$ADM2_ISON, datos_map$cobertura))       %>%      lapply(htmltools::HTML)
    labels_punt <- sprintf(paste("ID caso", rnve_original$ID))
      
      
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
      # 
      # map <- map %>% 
      #   addCircles(
      #     data = rnve_original,
      #     lng = ~longitude,
      #     lat = ~latitude,
      #     group = "Puntos",
      #     label = labels_punt,
      #     fillOpacity = 0.4) 
      
      map
      
      
      
    })
})
  