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

rnve <- rnve_original %>% filter(fecha_vac <= today())

rc <- registro_civil %>% 
  select(ID, nombre, apellido, fecha_nac)

rnve_2 <- rnve %>% 
  select(ID, nombre, apellido, fecha_nac, dosis, fecha_vac, municipio_res_mad)



rc_rnve <- rc %>%
  left_join(rnve_2, by = c("ID", "nombre", "apellido", "fecha_nac")) %>%
  pivot_wider(
    id_cols = c(ID, nombre, apellido, fecha_nac),
    names_from = "dosis",
    # Esta es la columna que contiene la fecha de vacunación, para cada dosis
    values_from = "fecha_vac") %>% 
  # Eliminar columna NA
  select(-`NA`) %>% 
  mutate(Completo = if_all(5, ~ !is.na(.x))) %>% 
  # Obtener año de nacimiento para asignar a cada persona a una cohorte.
  mutate(ano_nac = year(fecha_nac)) %>% 
  # Agrupamos por ano_nac y la variable esquema completo
  group_by(ano_nac, Completo)%>% 
  # Contamos. 
  tally() %>% 
  pivot_wider(
    # Estas son las columnas que no vamos a mover
    id_cols = c(ano_nac),
    # Esta es la columna que contiene el estado de esquema completo
    names_from = "Completo",
    # Esta es la columna que contiene el número de personas en cada grupo
    values_from = "n") %>% 
  # Renombrar variables
  rename(SPR1 = `TRUE`,
         no_SPR1 = `FALSE`) 


susceptibles <- rc_rnve %>% 
  filter( ano_nac >= 2018 & ano_nac <= 2022) %>% 
  mutate(No_vacunados = no_SPR1) %>% 
  mutate(falla_primaria = round(SPR1* 0.05, 0)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_susc = No_vacunados + falla_primaria) %>% 
  ungroup() %>% 
  arrange(ano_nac) %>% 
  mutate(susceptibles_acumulado = cumsum (total_susc))


pop_LT1_rn <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano) %>% 
  tally() %>% 
  mutate(ano_nac = ano+1) %>% 
  filter(ano_nac >= 2018 & ano_nac <= 2022) %>% 
  mutate(Poblacion = n)


tabla_final <- susceptibles %>% 
  left_join(., pop_LT1_rn, by =  c ("ano_nac")) %>% 
  select(ano_nac, Poblacion, total_susc,  susceptibles_acumulado )



##  Por municipio -----------------------------------------
# primeras dosis administradas por municipio (todos los años)
rc_rnve_mun_SPR1 <- rc %>%
  left_join(rnve_2, by = c("ID", "nombre", "apellido", "fecha_nac")) %>% 
  filter(dosis == "Primera") %>% 
  group_by(anomunicipio_res_mad) %>% 
  tally()
  

  