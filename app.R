#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Resumen comandos Shiny https://raw.githubusercontent.com/rstudio/cheatsheets/main/translations/spanish/shiny_es.pdf
# Widgets disponibles https://shiny.rstudio.com/gallery/widget-gallery.html
#Codigos paises https://www.iban.com/country-codes
#Autor David Perez Martorell davidperezmartorell@gmail.com
#Patrocinado por www.ecosistemaglobal.org y Asociacion Focazul

# Librerias ---------------------------------------------------------------
#Carga de librerias
#install.packages("countrycode")
#install.packages("shinyWidgets")
#install.packages("gdal")
library(shinythemes)
library(ggplot2)
library(countrycode)
library(shiny)
library(shinyjs)
library(sf)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rgdal)
library(sp)
library(htmlwidgets)
library(htmltools)
library(shinyWidgets)
#install.packages("geodata", repos = "https://cloud.r-project.org/")
#update.packages("geodata")
library(geodata) #Libreria para descargar datos, mapas..etc..
library(httr) #PAra leer http y conocer si el archivo de mapa existe
library(jsonlite) #PAra leer contenido del http
library(profvis)
library(rnoaa) # Libreria para el clima actualizado
library(ggmap) #Libreria para conocer geolocalizacion
# Variables iniciales -----------------------------------------------------
#CArga datos de paises
paises <- read.csv("inst/paises.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
regiones <- NULL #Inicio lista de recinoes en un principio vacia
provincias <- NULL #Inicio lista de privincias en un principio vacia
comarcas <- NULL #Inicio lista de privincias en un principio vacia
localidad <- NULL #Inicio lista de privincias en un principio vacia
seleccion_pais <- NULL #Inicio
seleccion_region <- NULL #Inicio
nivel_solicitado <- 1
fillRows <- "NAME_1" #Variable que define el nivel d la leyenda en funcion del contenido

MapaBase <-readRDS("inst/maps/gadm41_AFG_1_pk.rds")%>% st_as_sf()
React_MapaBase_Elevaciones <- reactiveVal() #Variable Mapabase de clima necesaria  cuando agregamos la capa de clima
React_MapaBase_Rios <- reactiveVal() #Variable Mapabase de rios
React_NumRows <- reactiveVal(34) #Variable que contiene el numero de filas para escoger los colores. 34 es el valor por defecto de AFG
esta_mapa_local <- "vacio" #Se usa en descarga_mapa como variable de control al buscar si elarchivo esta en local
React_crs_mapabase <- reactiveVal(st_crs(MapaBase)) #Contiene CRQ de GADM seguramente generico

Sys.setlocale(category = "LC_ALL", locale = "es_ES.utf8") # Establecer la configuración regional
options(encoding = "UTF-8") # Establecer la codificación de caracteres


# Menu --------------------------------------------------------------------
ui <- fluidPage(
  
  useShinyjs(), # Agregar esta función para usar shinyjs
  tags$head( #Encabezado del navegador
    tags$title("Mapas de todo el mundo")
  ),
  
  tags$style(
    HTML(" 
    h3 {
      color: red;
      font-size: 24px;
      font-weight: bold;
    }
    h6 {
      color: blue;
      font-size: 16px;
      font-weight: bold;
    }
    select, .selectize-input, .selectize-dropdown {
      border: 1px solid gray;
      padding: 2px;
      font-size: 12px;
      font-family: Arial;
      border-radius: 2px;
      #width: 70%;
      margin-top: 1px;margin-left: 1px;margin-right: 1px;margin-bottom: 1px;
    }
    .sidebar {
       text-align: left;
       max-width: 100px;
    }
    .well {
     width: 80%;
    }
  ")
  )
  
  ,
  
  
  #Encabezado del codigo con un titulo
  titlePanel(HTML("<h3>Mapas de todo el mundo patrocinado por https://ecosistemaglobal.org/ y Asociacion Focazul</h3>")),
  sidebarLayout(#Panel de herramientas lateral
     sidebarPanel(
      width = 4, 
      #Aqui escogemos el pais segun datos de atchivo y tabla paises
       selectInput("seleccion_pais", choices = paises$name, textOutput("result_pais"), width = "auto"),
       fluidRow(
        column(width = 6, selectInput("seleccion_region", choices = c("region", regiones[, 2]),
                                      textOutput("result_region"),width = "100%") ),
        column(width = 6, selectInput("seleccion_provincia",choices = c("provincia", provincias[, 2]),
                                      textOutput("result_provincia"), width = "100%") )
      ),
      fluidRow(
        column(width = 6, selectInput("seleccion_comarca", choices = c("comarca", comarcas[, 2]),
                                      textOutput("result_comarca"),width = "100%") ),
        column(width = 6, selectInput("seleccion_localidad",choices = c("localidad", localidad[, 2]),
                                      textOutput("result_localidad"), width = "100%") )
      ),
      # #Aqui escogemos si queremos pintar o el clima
      # selectInput("seleccion_clima", "Variable BIO:", 
      #             choices = c("Elevaciones" = "elev",
      #                         "Precipitacion media (mm)" = "prec",
      #                         "Altitud" = "alt",
      #                         "average monthly mean temperature °C * 10" = "tmean",
      #                         "average monthly minimum temperature °C * 10" = "tmin",
      #                         "average monthly maximum temperature °C * 10" = "tmax", 
      #                         "otras indicadores bioclimaticos" = "bio", 
      #                         selected = "elev")),
      radioButtons(
        inputId = "opcion_bio",
        label = "Opciones:",
        choices = c("Colours", "Legend", "Elevations","Rivers", "Clean"),
        selected = "Colours"
      ),
      #Boton para resetear contenido
      actionButton("reset", "reset")
    ), #Fin de sidebarPanel
    
    # Carga cuerpo ------------------------------------------------------------
    mainPanel(
      htmlOutput("TittleMap"),
      plotOutput("MapaBase"),
      footer() 
    ) #Fin de sidebarLayout
  )# Fin del main pannel
) #Fin de fluidPage


#CONEXION A LUCAS MAPAS DE SUELOS
#Instale el paquete "RODBC" en R utilizando el comando install.packages("RODBC")
#Cargue el paquete "RODBC" en R con el comando library(RODBC)
#Defina la conexión a la base de datos utilizando la función odbcConnect con los parámetros necesarios. Por ejemplo, para conectarse a la base de datos del ESDAC, puede utilizar la siguiente sintaxis:
#  bash
#Copy code
#con <- odbcConnect("ESDAC", uid="usuario", pwd="contraseña", 
#                   believeNRows=FALSE, rows_at_time=500)


##################################
#   Fin de interfaz de usuario   #
##################################
# Definimos options_regiones fuera de la función run_code


#Esta funcion es para modificar el valor del fillRows que servirá para la leyenda
changeFillRows <- function(nivel){
  if (nivel_solicitado==1){fillRows=="NAME_1"}
  if (nivel_solicitado==2){fillRows=="NAME_2"}
  if (nivel_solicitado==3){fillRows=="NAME_3"}
  if (nivel_solicitado==4){fillRows=="NAME_4"}
}

server <- function(input, output,session){
  
  
  # Esta es la variable reactiva que contiene las regiones del país seleccionado
  observeEvent(input$seleccion_pais, {
    
    # Escoge el nombre del país según el código
    options <- paises$name 
    values <- paises$iso3
    index_pais <- match(input$seleccion_pais, options)
    pais_seleccionado2 <- values[index_pais]
    nivel_solicitado=1
    changeFillRows(nivel_solicitado) # La leyenda depende de este valor
    MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
    lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
    regiones <- unique(MapaBase$NAME_1)
    renderMap(input,output, MapaBase,"NAME_1",n_filas) #Llma a la funcion de plotear el mapa
    # Actualiza las opciones de seleccion_region
    choices <- c("region", regiones)
    updateSelectInput(session, "seleccion_region", choices = choices)
    
  })
  
  # Definimos una variable reactiva "provincias_pais" que contiene las provincias del país y la región seleccionadas
  observeEvent(input$seleccion_region, {
    
    if (!is.null(input$seleccion_pais) && input$seleccion_pais != "Selecciona un país" && 
        !is.null(input$seleccion_region) && input$seleccion_region != "region") {
      options <- paises$name 
      values <- paises$iso3
      index_pais <- match(input$seleccion_pais, options)
      pais_seleccionado2 <- values[index_pais]
      nivel_solicitado=2
      changeFillRows(nivel_solicitado) # La leyenda depende de este valor
      
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
      MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)

      lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
      regiones <- unique(MapaBase$NAME_2)
      
      renderMap(input,output, MapaBase,"NAME_2",n_filas) #Llma a la funcion de plotear el mapa
      # Actualiza las opciones de seleccion_region
      choices <- c("provincia", regiones)
      updateSelectInput(session, "seleccion_provincia", choices = choices)
      
    }})
  
    # Definimos una variable reactiva "comarcas_pais" que contiene las comarcas del país y la región seleccionadas
  observeEvent(input$seleccion_provincia, {
    
    if (!is.null(input$seleccion_pais) && input$seleccion_pais != "Selecciona un país" && 
        !is.null(input$seleccion_region) && input$seleccion_region != "region" &&
        !is.null(input$seleccion_provincia) && input$seleccion_provincia != "provincia") {
      options <- paises$name 
      values <- paises$iso3
      index_pais <- match(input$seleccion_pais, options)
      pais_seleccionado2 <- values[index_pais]
      nivel_solicitado=3
      #Esta parte del programa es para descargar pero contiene un if para poder usar el mejor mapa con mayor detalle dscargado
      MapaBase_anterior <- MapaBase # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )

      # Verificar si MapaBase tiene la columna NAME_2 para poder dar mas detalle
      if ("NAME_2" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay

        regiones <- unique(MapaBase$NAME_3)
        renderMap(input,output, MapaBase,"NAME_3",n_filas) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        choices <- c("comarca", regiones)
        updateSelectInput(session, "seleccion_comarca", choices = choices)
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior

        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- unique(MapaBase$NAME_2) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=2 #Modifico a nivel=2 porque no ha podido encontrarlo
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor     
        browser()
        renderMap(input,output, MapaBase,"NAME_2",n_filas) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        #choices <- c("comarca", regiones)
        #updateSelectInput(session, "seleccion_comarca", choices = choices)
      }
    }})
  
    # Definimos una variable reactiva "localidad_pais" que contiene las comarcas del país y la región seleccionadas
  observeEvent(input$seleccion_comarca, {
    
    if (!is.null(input$seleccion_pais) && input$seleccion_pais != "Selecciona un país" && 
        !is.null(input$seleccion_region) && input$seleccion_region != "region" &&
        !is.null(input$seleccion_provincia) && input$seleccion_provincia != "provincia" && 
        !is.null(input$seleccion_comarca) && input$seleccion_comarca != "comarca") {
      
      options <- paises$name 
      values <- paises$iso3
      index_pais <- match(input$seleccion_pais, options)
      pais_seleccionado2 <- values[index_pais]
      nivel_solicitado=4
      
      #Esta parte del programa es para descargar pero contiene un if para poder usar el mejor mapa con mayor detalle dscargado
      MapaBase_anterior <- MapaBase # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )

      browser()
      if ("NAME_3" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- MapaBase
        regiones <- unique(regiones$NAME_4)
        renderMap(input,output, MapaBase,"NAME_4",n_filas) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        choices <- c("localidad", regiones)
        updateSelectInput(session, "seleccion_localidad", choices = choices)
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- MapaBase
        regiones <- unique(regiones$NAME_3) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=3 #Modifico a nivel=2 porque no ha podido encontrarlo
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor              
        renderMap(input,output, MapaBase,"NAME_3",n_filas) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        #choices <- c("comarca", regiones)
        #updateSelectInput(session, "seleccion_comarca", choices = choices)
      }
    }})
  
    # Definimos una variable reactiva "localidad_pais" que contiene las comarcas del país y la región seleccionadas
  observeEvent(input$seleccion_localidad, {
    
    if (!is.null(input$seleccion_pais) && input$seleccion_pais != "Selecciona un país" && 
        !is.null(input$seleccion_region) && input$seleccion_region != "region" &&
        !is.null(input$seleccion_provincia) && input$seleccion_provincia != "provincia" && 
        !is.null(input$seleccion_comarca) && input$seleccion_comarca != "comarca" && 
        !is.null(input$seleccion_localidad) && input$seleccion_localidad != "localidad") {
      
      options <- paises$name 
      values <- paises$iso3
      index_pais <- match(input$seleccion_pais, options)
      pais_seleccionado2 <- values[index_pais]
      nivel_solicitado=4
      
      #Esta parte del programa es para descargar pero contiene un if para poder usar el mejor mapa con mayor detalle dscargado
      MapaBase_anterior <- MapaBase # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
      #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
      browser()
      if ("NAME_4" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        MapaBase <- filter(MapaBase,MapaBase$NAME_4 == input$seleccion_localidad)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        renderMap(input,output, MapaBase,"NAME_4",n_filas) #Llma a la funcion de plotear el mapa
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- MapaBase
        regiones <- unique(regiones$NAME_3) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=3 #Modifico a nivel=2 porque no ha podido encontrarlo
        changeFillRows(nivel_solicitado) # La leyenda depende de este valor              
        renderMap(input,output, MapaBase,"NAME_4",n_filas) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        #choices <- c("comarca", regiones)
        #updateSelectInput(session, "seleccion_comarca", choices = choices)
      }
    }})
  
# Call the function with the required arguments
  observeResetButton(session, input)
  
# aquí va el código para que acctualice
  run_code <- function() {
    flush.console()
    
    cat("seleccion : ", input$seleccion_pais, " y el pasis2", pais_seleccionado2)
  }
  
# Función para renderizar el título del mapa
  renderTittleMap(input, output)

# Función para renderizar el mapa
  renderMap(input,output, MapaBase,fillRows,n_filas)

} #Aqui acaba el server <- function(input, output,session){


#Vincula entrada de usuario con servidor
shinyApp(ui, server)




