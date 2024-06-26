# requirements.R ---------------------------------------------------------------
# Description: Contiene el listado de paquetes necesarios para la correcta
# ejecución de este proyecto.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# cargar pacman si no lo está
if (!require("pacman")) install.packages("pacman")

# cargar paquetes necesarios
pacman::p_load(
  shiny,
  shinydashboard, 
  tidyr,
  dplyr,
  lubridate,
  stringr,
  rio,
  ggplot2,
  bench,
  DT,
  plotly,
  leaflet,
  leaflet.extras,
  esquisse,
  sf
)
