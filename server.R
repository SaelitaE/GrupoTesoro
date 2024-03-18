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
    "Descripción"
  })
<<<<<<< Updated upstream
  
  #Imagen 
  output$logo_imagen <- renderImage({
    list(
      src = "GrupoTesoroLogo.png",
      contentType = "image/png",
      width = "auto",
      height = "100"
    )
  }, deleteFile = FALSE)
  
  output$texto_encabezado <- renderText({
    "Grupo Tesorooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"  
  })
  

  
=======
  #Imagen módulo de inicio
  output$Imagen <- renderImage({
    list(src = "a.png",
         contenType = "image/png",
         width = "100%",
         heigth = "100%"
    )})
>>>>>>> Stashed changes
  ### Justificacion ------------------------------------------------------------
  # Cuadro informativo para seccion de Justificacion
  output$justificacion_textbox <- renderText({
    "Descripción"
  })
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"
  })
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})
  