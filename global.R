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

#por año de nacimiento
pop_rc <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano) %>% 
  tally() %>% 
  mutate(anio = ano+1) %>% 
  filter(anio >= 2018 & anio <= 2022) %>% 
  rename(poblacion = n) 

#Por municipio

pop_rc_mun <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano, municipio_res_mad) %>% 
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

# Población por año y Departamento: 
susc_anio_mun <- susc_anio_mun %>% 
  left_join(.,pop_rc_mun, by = c("municipio_res_mad","anio")) %>% 
  select(anio, municipio_res_mad, poblacion, total_susc)



# Calculos de cobertura por municipio (avance diario)- ----------------------------

fecha_campana <- as.Date("2024-03-04", "%Y-%m-%d")
fecha_edad_minima <- fecha_campana %m-% months(12)
fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1

# Población objetivo
pop_campana_adm1 <- registro_civil_original %>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  group_by(departamento_res_mad) %>% 
  tally(name = "poblacion") 

# Cobertura
campana_departamento <- rnve_original %>% 
  left_join(., registro_civil_original, by = c("ID", "nombre", "apellido", "fecha_nac")) %>% 
  filter(dosis == "Campaña") %>% 
  group_by(fecha_vac, departamento_res_mad) %>% 
  summarise(
    vacunados = n()
  ) %>%
  left_join(., pop_campana_adm1, by = "departamento_res_mad") %>% 
  mutate(cobertura = round((vacunados / poblacion * 100),1))

campana_nacional_dia <- campana_departamento %>% 
  group_by(fecha_vac) %>% 
  summarise(
    vacunados = sum(vacunados)
  )

poblacion_nacional <- sum(pop_campana_adm1$poblacion)

cobertura_nacional_dia <- campana_nacional_dia %>% 
mutate(poblacion = poblacion_nacional) %>% 
  mutate(cobertura = round(vacunados/poblacion_nacional*100,1)) %>% 
  ungroup() %>% 
  arrange(fecha_vac) %>% 
  mutate(vacunados_acumulados = cumsum(vacunados)) %>% 
  mutate(cobertura_acumulada = round(vacunados_acumulados/poblacion_nacional*100,1)) %>% 
  select (fecha_vac, cobertura_acumulada)

tabla_grafica <- campana_nacional_dia %>% 
  left_join(., cobertura_nacional_dia, by = c("fecha_vac"))


# Cobertura acumulada por depto

tabla_grafica_depto <-campana_departamento %>%
  group_by(departamento_res_mad) %>%
  arrange(fecha_vac) %>%
  mutate(cobertura_acumulada = cumsum(cobertura))


# Carga de datos para mapa -----------------------------------------------

mun <- read_sf("data\\Anterior_URYMixed\\URY_ADM2_Anterior.shp")

grafica_mapa <- ggplot()+
  geom_sf(data = mun)

pop_campana_adm2 <- registro_civil_original %>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  group_by(cod_municipio, municipio_res_mad) %>% 
  tally(name = "total_pob") %>%
  rename(GIS_CODE = cod_municipio)

poblacion <- pop_campana_adm2 %>% 
  full_join(., mun, by = c("GIS_CODE")) %>% 
  rename(municipio = municipio_res_mad)


vacunados <- rnve %>% 
  filter(dosis == "Campaña") %>% 
  select(municipio) %>% 
  group_by(municipio) %>% 
  summarise(total_vac = n())

datos_map <- full_join(poblacion, vacunados, by = "municipio") %>%
  mutate(cobertura = round(total_vac/total_pob  * 100,1)) %>% 
  mutate(no_vacunados = total_pob-total_vac)


## Creacion de variables en el shp ####
datos_map <- datos_map %>% 
  mutate(rango_cob = case_when(cobertura <= 20 ~ "<=20%", 
                               cobertura > 20  & cobertura <= 40  ~ "20% - 40%",
                               cobertura > 40  & cobertura <= 60  ~ "40% - 60%", 
                               cobertura > 60  & cobertura <= 80  ~ "60% - 80%", 
                               cobertura > 80 ~ "> 80%", 
                               TRUE ~ "Sin Dato"),
         rango_cob = factor(rango_cob, levels = c("<=20%", 
                                                  "20% - 40%",
                                                  "40% - 60%",
                                                  "60% - 80%",
                                                  "> 80%",
                                                  "Sin Dato")) 
        ) %>% 
  st_as_sf()



