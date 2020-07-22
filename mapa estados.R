##################################################
# Defunciones de COVID-19 por estado (municipios)#
##################################################

# Código elaborado por: Juvenal Campos y Ami Sosa 

# Librerias ----
library(tidyverse)
library(rebus)
library(curl)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

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

# hacer mapa
edos <- mapa$NOM_ENT %>% unique() %>% as.character() %>% sort()

# Titulo ----

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }"))

title <- tags$div(tag.map.title, HTML("Defunciones por COVID-19"))  

# Rosa de los vientos

norte <- function(mapa_leaflet, 
                  ancho = 40, 
                  position = 'topleft', 
                  direccion_img_norte = "http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-north-arrow-2.png"){
  # 1. Descargamos la imagen
  
  north.arrow.icon <- paste0("<img src='", 
                             direccion_img_norte,
                             "' style='width:",
                             as.character(ancho), "px;'>")
  # Lo incluimos en una funcion de RLeaflet
  if (!require("leaflet")) install.packages("leaflet") # Asegurarnos que este instalado Leaflet
  addControl(mapa_leaflet, 
             html = north.arrow.icon, position = "topright", 
             className = "fieldset {
             border: 0;}") 
}


# Mapa y función molde - Morelos 
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
              pal = func, values = valores, 
              title = "Defunciones<br>por municipio",
              opacity = 1) %>% 
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions
                (maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>% 
    norte() %>% 
    addProviderTiles(providers$CartoDB.Positron, options = tileOptions(opacity = 0.6)) %>%
    addControl(title, position = "topleft", className="map-title")
}

# Defunciones estados ---- 

# Colima, Nayarit y Yucatán con tratamiento especial de datos para eliminar islas (en siguiente parte del código)

## Aguascalientes 
mapa_edos(estado = "Aguascalientes", cat = "Fallecimientos")

## Baja California
mapa_edos(estado = "Baja California", cat = "Fallecimientos")

## Baja California Sur
mapa_edos(estado = "Baja California Sur", cat = "Fallecimientos")

## Campeche
mapa_edos(estado = "Campeche", cat = "Fallecimientos")

## Chiapas
mapa_edos(estado = "Chiapas", cat = "Fallecimientos")

## Chihuahua
mapa_edos(estado = "Chihuahua", cat = "Fallecimientos")

## CDMX
mapa_edos(estado = "Ciudad de México", cat = "Fallecimientos")

## Coahuila de Zaragoza
mapa_edos(estado = "Coahuila de Zaragoza", cat = "Fallecimientos")

## Durango
mapa_edos(estado = "Durango", cat = "Fallecimientos")

## Estado de México 
mapa_edos(estado = "México", cat = "Fallecimientos")

## Guanajuato
mapa_edos(estado = "Guanajuato", cat = "Fallecimientos")

## Guerrero
mapa_edos(estado = "Guerrero", cat = "Fallecimientos")

## Hidalgo
mapa_edos(estado = "Hidalgo", cat = "Fallecimientos")

## Jalisco
mapa_edos(estado = "Jalisco", cat = "Fallecimientos")

## Michoacán de Ocampo
mapa_edos(estado = "Michoacán de Ocampo", cat = "Fallecimientos")

## Morelos
mapa_edos(estado = "Morelos", cat = "Fallecimientos")

## Nuevo León
mapa_edos(estado = "Nuevo León", cat = "Fallecimientos")

## Oaxaca
mapa_edos(estado = "Oaxaca", cat = "Fallecimientos")

## Puebla
mapa_edos(estado = "Puebla", cat = "Fallecimientos")

## Querétaro
mapa_edos(estado = "Querétaro", cat = "Fallecimientos")

## Quintana Roo
mapa_edos(estado = "Quintana Roo", cat = "Fallecimientos")

## San Luis Potosí
mapa_edos(estado = "San Luis Potosí", cat = "Fallecimientos")

## Sinaloa
mapa_edos(estado = "Sinaloa", cat = "Fallecimientos")

## Sonora
mapa_edos(estado = "Sonora", cat = "Fallecimientos")

## Tabasco
mapa_edos(estado = "Tabasco", cat = "Fallecimientos")

## Tamaulipas
mapa_edos(estado = "Tamaulipas", cat = "Fallecimientos")

## Tlaxcala
mapa_edos(estado = "Tlaxcala", cat = "Fallecimientos")

## Veracruz de Ignacio de la Llave
mapa_edos(estado = "Veracruz de Ignacio de la Llave", cat = "Fallecimientos")

## Zacatecas
mapa_edos(estado = "Zacatecas", cat = "Fallecimientos")

# Eliminar islas de Colima, Nayarit y Yucatán ----

## Colima
colima <- mapa %>% filter(NOM_ENT == "Colima")

# Coordenadas extremas Colima Territorial
maxX = -103.47499
minX = -104.76983
maxY = 19.563769
minY = 18.65329

cuadro <- expand.grid(x = c(minX,
                            maxX),
                      y = c(minY,
                            maxY))  %>%
  as.matrix()

# Ordenamos de manera cíclica el cuadro, y lo cerramos
cuadro <- cuadro[c(1,2,4,3,1),]

# Lo convertimos en lista
cuadro <- list(cuadro)

# Lo convertimos en un objeto geométrico poligonal
cuadro <- st_polygon(cuadro) %>% st_sfc(crs = 4326)

# Exploramos como se ve:
leaflet(colima) %>%
  addTiles() %>%
  addPolygons(opacity = 1) %>%
  addPolygons(data = cuadro,
              color = "red")

# Ahora si, le extirpamos al objeto el corazon de Colima:
colima_sin_islas <- st_intersection(colima, cuadro)
plot(colima_sin_islas)

# Y lo regresamos al shape original.
# Le sacamos el colima con islas
mapa <- mapa %>%
  filter(NOM_ENT != "Colima")

# Le metemos la nueva Colima, sin Islas.
mapa <- rbind(mapa, colima_sin_islas)

# Guardamos nuestro nuevo mapa:
st_write(mapa, "www/Shapes/mpios_sin_revillagigedo.geojson")

## Función para quiar islas en más estados 
# Datos de entrada: Las coordenadas del Cuadro y el shape a recortar.

recorte_cuadro <- function(shp, 
                           minX, 
                           maxX,
                           minY,
                           maxY){
  cuadro <- expand.grid(x = c(minX,
                              maxX),
                        y = c(minY,
                              maxY))  %>%
    as.matrix()
  
  cuadro <- cuadro[c(1,2,4,3,1),]
  cuadro <- list(cuadro)
  cuadro <- st_polygon(cuadro) %>% 
    st_sfc(crs = 4326)
  
  edo_sin_islas <- st_intersection(shp, cuadro)
  
  # Lo que queremos que retorne la funcion: 
  return(edo_sin_islas)
}

## Nayarit 
nayarit <- mapa %>% 
  filter(NOM_ENT == "Nayarit")

nayarit_sin_islas_marias <- recorte_cuadro(shp = nayarit, 
                                           minX = -105.7765, 
                                           maxX = -103.7209, 
                                           minY = 20.60322, 
                                           maxY = 23.0845)

# Funciona? 
plot(nayarit_sin_islas_marias, max.plot = 1)

## Yucatán 
yucatan <- mapa %>% filter(NOM_ENT == "Yucatán")
yucatan_sin_arrecife_alacranes <- recorte_cuadro(shp = yucatan, 
                                                 minX = -90.620039,
                                                 maxX = -87.414154,
                                                 minY = 19.584267, 
                                                 maxY = 21.731110)

# Funciona? 
plot(yucatan_sin_arrecife_alacranes, max.plot = 1)

## Generamos nueva capa de datos, ahora sin las islas de Yucatán ni las de Nayarit
mapa <- mapa %>% 
  # Quitamos los viejos
  filter(!(NOM_ENT %in% c("Yucatán", "Nayarit"))) %>% 
  # Metemos los nuevos
  rbind(yucatan_sin_arrecife_alacranes, nayarit_sin_islas_marias) 

# Vemos que haya funcionado todo: 
plot(mapa, max.plot = 1)

# Guardamos el archivo resultante
st_write(mapa, "www/Shapes/mpios_con_menos_islas_aun.geojson")

# Colima, Nayarit, Yucatán mapas COVID sin islas ----

## Colima 
mapa_edos(estado = "Colima", cat = "Fallecimientos")

## Nayarit 
mapa_edos(estado = "Nayarit", cat = "Fallecimientos")

## Yucatán 
mapa_edos(estado = "Yucatán", cat = "Fallecimientos")
