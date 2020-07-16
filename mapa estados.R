# Generacion de mapas de COVID ----

# Librerias ----
library(tidyverse)
library(rebus)
library(curl)
library(sf)
library(leaflet)

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
casos <- read_csv(sort(list.files("www/Estados"))[length(list.files("www/Estados"))])


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

# Fallecimientos por municipio 
fallecimientos <- casos %>%
  filter(RESULTADO == 1) %>% 
  filter(!is.na(FECHA_DEF)) %>% # Presentan fecha de fallecimiento
  ungroup() %>% 
  group_by(CVEGEO) %>% 
  count() %>% 
  rename("Fallecimientos" = n)

sum(fallecimientos$Fallecimientos) # Checar que salga igual que el de las noticias

# Pegamos los datos ----
mapa <- merge(muni, enfermos, by = "CVEGEO", all.x = TRUE) %>% 
  merge(fallecimientos, by = "CVEGEO", all.x = TRUE) %>%
  mutate(Casos = case_when(is.na(Casos) ~ 0L,
                           TRUE ~ Casos),
         Fallecimientos = case_when(is.na(Fallecimientos) ~ 0L,
                                    TRUE ~ Fallecimientos))

# hacer mapa
edos <- mapa$NOM_ENT %>% unique() %>% as.character() %>% sort()

# Datos de prueba de la funcion de abajo 
cat <- "Fallecimientos"
estado <- "Morelos"

mapa_edos <- function(estado, cat = "Fallecimientos"){
  # Filtramos el estado
  edo <- mapa %>% 
    filter(NOM_ENT == estado)
  
  # Generamos paleta de colores
  max_fallecimientos <-  max(edo$Fallecimientos, na.rm = TRUE)
  min_fallecimientos <-  min(edo$Fallecimientos, na.rm = TRUE)
  
  max_casos <-  max(edo$Casos, na.rm = TRUE)
  min_casos <-  min(edo$Casos, na.rm = TRUE)
  
  pal_casos <- colorNumeric(palette = "magma",
                            rev = TRUE, 
                            domain = c(min_casos, 
                                       max_casos))
  
  pal_fallecimientos <- colorNumeric(palette = "magma", 
                                     rev = TRUE, 
                                     domain = c(min_fallecimientos, 
                                                max_fallecimientos))
  
  if(cat == "Fallecimientos"){
    valores = edo$Fallecimientos
    func <- pal_fallecimientos
    pal <- func(valores)
    label <- paste0("Fallecimientos: ", edo$Fallecimientos)
  } else {
    valores = edo$Casos
    func <- pal_casos
    pal <- func(valores)
    label <- paste0("Casos: ", edo$Casos)
  }
  
  # Mapa ---- 
  edo %>% 
    leaflet() %>% 
    addPolygons(fillColor = pal,
                fillOpacity = 1,
                label = label, 
                weight = 0.5, 
    ) %>% 
    addLegend(position = "bottomright", 
              pal = func, values = valores)
}

# Aplicacion de la funcion para hacer mapas por estado
mapa_edos(estado = "Hidalgo", cat = "Casos")
mapa_edos(estado = "Chiapas", cat = "Fallecimientos")
mapa_edos(estado = "Ciudad de México")

