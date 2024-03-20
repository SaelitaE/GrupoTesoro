# global.R ---------------------------------------------------------------------
# Description: Este script carga toda la información y paquetes necesarios
# para ejecutar el dashboard.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Requerimientos ---------------------------------------------------------------
source("requirements.R")

# Carga de datos --------------------------------------------------------------
registro_civil_original <- rio::import("./data/registro_civil.csv")
rnve_original <- rio::import("./data/RNVE.csv")

registro_civil <- registro_civil_original %>%
  filter(year(fecha_nac) <= year(today()) - 1)

rnve <- rnve_original %>% 
  filter(fecha_vac <= today()) %>% 
  select(ID, nombre, apellido, fecha_nac, dosis, fecha_vac, municipio_res_mad) %>% 
  rename(municipio = municipio_res_mad)

rc <- registro_civil %>% 
  select(ID, nombre, apellido, fecha_nac,cod_municipio, municipio_res_mad)

#rm(registro_civil_original)
#rm(rnve_original)

rc_rnve <- rc %>% 
  left_join(rnve, by = c("ID", "nombre", "apellido", "fecha_nac")) 

# Encuentra la población por año de nacimiento  --------------------------------------------------------------

pop_rc <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano) %>% 
  tally() %>% 
  mutate(anio = ano+1) %>% 
  filter(anio >= 2018 & anio <= 2022) %>% 
  rename(poblacion = n) 

# Susceptibles por año (año de nacimiento del niñ@) --------------------------------------------------------------

susc_anio <- rc_rnve %>%
  pivot_wider(
    id_cols = c(ID, nombre, apellido, fecha_nac),
    names_from = "dosis",
    values_from = "fecha_vac") %>%
  select(-`NA`) %>%
  mutate(SPR1 = if_all(5, ~ !is.na(.x))) %>%
  mutate(anio_nac = year(fecha_nac)) %>%
  group_by(anio_nac, SPR1)%>%
  tally() %>%
  pivot_wider(
    id_cols = c(anio_nac),
    names_from = "SPR1",
    values_from = "n") %>%
  rename(SPR1 = `TRUE`,
         no_inmunizado = `FALSE`) %>% 
  filter (anio_nac >= 2018 & anio_nac <=2022) %>% 
  rename(anio = anio_nac ) %>% 
  mutate(total_susc = no_inmunizado +round(SPR1*0.05,0)) %>% 
  ungroup() %>% 
  arrange(anio) %>% 
  mutate(susceptibles_acumulado = cumsum (total_susc)) %>% 
  left_join(., pop_rc, by =  c ("anio")) %>% 
  select(anio, poblacion, SPR1, no_inmunizado, total_susc,  susceptibles_acumulado )




# Susceptibles por año y municipio (año de nacimiento del niñ@) --------------------------------------------------------------


susc_anio_mun <- rc_rnve %>%
  pivot_wider(
    id_cols = c(ID, nombre, apellido, fecha_nac, municipio_res_mad),
    names_from = "dosis",
    values_from = "fecha_vac") %>% 
  select(-`NA`) %>%
  mutate(SPR1 = if_all(6, ~ !is.na(.x))) %>%
  mutate(anio_nac = year(fecha_nac)) %>%
  group_by(anio_nac, municipio_res_mad, SPR1)%>%
  tally() %>%
  pivot_wider(
    id_cols = c(anio_nac,municipio_res_mad),
    names_from = "SPR1",
    values_from = "n") %>%
  rename(SPR1 = `TRUE`,
         no_inmunizado = `FALSE`) %>% 
  filter (anio_nac >= 2018 & anio_nac <=2022) %>% 
  rename(anio = anio_nac ) %>% 
  mutate(total_susc = no_inmunizado +round(SPR1*0.05,0))

