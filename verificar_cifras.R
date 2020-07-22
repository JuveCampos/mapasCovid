################################################
## Chequeo de cifras (defunciones por estado) ##
################################################

# Librerias ----
library(tidyverse)
library(rebus)
library(curl)

# Setup ---- 
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

# Bases de datos ----
# Descarga automatica de los datos de hoy: 
# Nombre del archivo (correr despues de las 7 p.m.)
descarga <- function(){
  archivo <<- Sys.Date() %>%
    as.character() %>%
    as_tibble() %>%
    mutate(anio = "20",
           mes = str_remove_all(string = str_extract(value,
                                                     pattern = "\\-\\d\\d\\-"),
                                pattern = "\\-"),
           dia = str_extract(value,
                             pattern = "\\d\\d$"),
           fecha = paste0(anio, mes, dia, "COVID19MEXICO.csv")) %>% 
    pull(fecha)
  
  # Descargo el archivo
  curl::curl_download(url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", 
                      destfile = "www/datosHoyCovid.zip")
  print("Ya se descargó el archivo más reciente")
  # Deszipeo el archivo  
  zip::unzip("www/datosHoyCovid.zip", 
             exdir = "www/Estados")
  print("Ya se descomprimió en la carpeta www/")
}

# Descargamos los datos
descarga()

# Info de mapa ----
muni <- st_read("www/Shapes/mpios.geojson")

# Info de casos ----
list.files("www/Estados") # Ver archivos en la carpeta 
casos <- read_csv(paste0("www/Estados/", sort(list.files("www/Estados"))[length(list.files("www/Estados"))]))

# PROCESAMIENTO DE LA INFORMACIÓN ----
# Casos por municipio de residencia ----
casos <- casos %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) 

# Datos Por Municipio ----

# Casos registrados tipo 1.
enfermos <- casos %>% 
  filter(RESULTADO == 1) %>% 
  group_by(CVEGEO) %>% 
  count(RESULTADO) %>% 
  rename("Casos" = n)

sum(enfermos$Casos) # Checar que salga igual que el de las noticias

# Igual al de la DGEp.

# Fallecimientos por municipio 
fallecimientos <- casos %>%
  filter(RESULTADO == 1) %>% 
  filter(!is.na(FECHA_DEF)) %>% # Presentan fecha de fallecimiento
  ungroup() %>% 
  group_by(CVEGEO) %>% 
  count() %>% 
  rename("Fallecimientos" = n)

sum(fallecimientos$Fallecimientos) # Checar que salga igual que el de las noticias

# Igual al de la DGEp.

# Pegamos los datos ----
mapa <- merge(muni, enfermos, by = "CVEGEO", all.x = TRUE) %>% 
  merge(fallecimientos, by = "CVEGEO", all.x = TRUE) %>%
  mutate(Casos = case_when(is.na(Casos) ~ 0L,
                           TRUE ~ Casos),
         Fallecimientos = case_when(is.na(Fallecimientos) ~ 0L,
                                    TRUE ~ Fallecimientos))

## Aguascalientes

mapa %>%
  filter(NOM_ENT == "Aguascalientes") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Aguascalientes") %>%
  summarise(total_defu = sum(Fallecimientos))

## Baja California

mapa %>%
  filter(NOM_ENT == "Baja California") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Baja California") %>%
  summarise(total_defu = sum(Fallecimientos))

## Baja California Sur

mapa %>%
  filter(NOM_ENT == "Baja California Sur") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Baja California Sur") %>%
  summarise(total_defu = sum(Fallecimientos))

## Campeche

mapa %>%
  filter(NOM_ENT == "Campeche") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Campeche") %>%
  summarise(total_defu = sum(Fallecimientos))

## Chiapas

mapa %>%
  filter(NOM_ENT == "Chiapas") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Chiapas") %>%
  summarise(total_defu = sum(Fallecimientos))

## Chihuahua

mapa %>%
  filter(NOM_ENT == "Chihuahua") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Chihuahua") %>%
  summarise(total_defu = sum(Fallecimientos))

## CDMX

mapa %>%
  filter(NOM_ENT == "Ciudad de México") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Ciudad de México") %>%
  summarise(total_defu = sum(Fallecimientos))

## Coahuila de Zaragoza

mapa %>%
  filter(NOM_ENT == "Coahuila de Zaragoza") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Coahuila de Zaragoza") %>%
  summarise(total_defu = sum(Fallecimientos))

## Colima

mapa %>%
  filter(NOM_ENT == "Colima") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Colima") %>%
  summarise(total_defu = sum(Fallecimientos))

## Durango

mapa %>%
  filter(NOM_ENT == "Durango") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Durango") %>%
  summarise(total_defu = sum(Fallecimientos))

## Estado de México

mapa %>%
  filter(NOM_ENT == "México") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "México") %>%
  summarise(total_defu = sum(Fallecimientos))

## Guanajuato

mapa %>%
  filter(NOM_ENT == "Guanajuato") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Guanajuato") %>%
  summarise(total_defu = sum(Fallecimientos))

## Guerrero

mapa %>%
  filter(NOM_ENT == "Guerrero") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Guerrero") %>%
  summarise(total_defu = sum(Fallecimientos))

## Hidalgo

mapa %>%
  filter(NOM_ENT == "Hidalgo") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Hidalgo") %>%
  summarise(total_defu = sum(Fallecimientos))

## Jalisco

mapa %>%
  filter(NOM_ENT == "Jalisco") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Jalisco") %>%
  summarise(total_defu = sum(Fallecimientos))

## Michoacán de Ocampo

mapa %>%
  filter(NOM_ENT == "Michoacán de Ocampo") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Michoacán de Ocampo") %>%
  summarise(total_defu = sum(Fallecimientos))

## Morelos

mapa %>%
  filter(NOM_ENT == "Morelos") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Morelos") %>%
  summarise(total_defu = sum(Fallecimientos))

## Nayarit

mapa %>%
  filter(NOM_ENT == "Nayarit") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Nayarit") %>%
  summarise(total_defu = sum(Fallecimientos))

## Nuevo León

mapa %>%
  filter(NOM_ENT == "Nuevo León") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Nuevo León") %>%
  summarise(total_defu = sum(Fallecimientos))

## Oaxaca

mapa %>%
  filter(NOM_ENT == "Oaxaca") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Oaxaca") %>%
  summarise(total_defu = sum(Fallecimientos))

## Puebla

mapa %>%
  filter(NOM_ENT == "Puebla") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Puebla") %>%
  summarise(total_defu = sum(Fallecimientos))

## Querétaro

mapa %>%
  filter(NOM_ENT == "Querétaro") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Querétaro") %>%
  summarise(total_defu = sum(Fallecimientos))

## Quintana Roo

mapa %>%
  filter(NOM_ENT == "Quintana Roo") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Quintana Roo") %>%
  summarise(total_defu = sum(Fallecimientos))

## San Luis Potosí

mapa %>%
  filter(NOM_ENT == "San Luis Potosí") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "San Luis Potosí") %>%
  summarise(total_defu = sum(Fallecimientos))

## Sinaloa

mapa %>%
  filter(NOM_ENT == "Sinaloa") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Sinaloa") %>%
  summarise(total_defu = sum(Fallecimientos))

## Sonora

mapa %>%
  filter(NOM_ENT == "Sonora") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Sonora") %>%
  summarise(total_defu = sum(Fallecimientos))

## Tabasco

mapa %>%
  filter(NOM_ENT == "Tabasco") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Tabasco") %>%
  summarise(total_defu = sum(Fallecimientos))

## Tamaulipas

mapa %>%
  filter(NOM_ENT == "Tamaulipas") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Tamaulipas") %>%
  summarise(total_defu = sum(Fallecimientos))

## Tlaxcala

mapa %>%
  filter(NOM_ENT == "Tlaxcala") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Tlaxcala") %>%
  summarise(total_defu = sum(Fallecimientos))

## Veracruz de Ignacio de la Llave

mapa %>%
  filter(NOM_ENT == "Veracruz de Ignacio de la Llave") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Veracruz de Ignacio de la Llave") %>%
  summarise(total_defu = sum(Fallecimientos))

## Yucatán

mapa %>%
  filter(NOM_ENT == "Yucatán") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Yucatán") %>%
  summarise(total_defu = sum(Fallecimientos))


## Zacatecas

mapa %>%
  filter(NOM_ENT == "Zacatecas") %>%
  summarise(total_casos = sum(Casos))

mapa %>%
  filter(NOM_ENT == "Zacatecas") %>%
  summarise(total_defu = sum(Fallecimientos))



