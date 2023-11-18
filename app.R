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
library(shinydashboard)
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
MapaBase <-readRDS("inst/maps/gadm41_AFG_1_pk.rds")%>% st_as_sf()
React_MapaBase <- reactiveVal(MapaBase) #Variable Mapabase necesaria en todos los espacios
React_MapaBase_Elevaciones <- reactiveVal() #Variable Mapabase de clima necesaria  cuando agregamos la capa de clima
React_MapaBase_Rios <- reactiveVal() #Variable Mapabase de rios
React_FillRows <- reactiveVal("NAME_1") #Variable que define el nivel d la leyenda en funcion del contenido
React_NumRows <- reactiveVal(34) #Variable que contiene el numero de filas para escoger los colores. 34 es el valor por defecto de AFG
esta_mapa_local <- "vacio" #Se usa en descarga_mapa como variable de control al buscar si elarchivo esta en local
React_crs_mapabase <- reactiveVal(st_crs(MapaBase)) #Contiene CRQ de GADM seguramente generico

Sys.setlocale(category = "LC_ALL", locale = "es_ES.utf8") # Establecer la configuración regional
options(encoding = "UTF-8") # Establecer la codificación de caracteres

#################################
#             Menu              #
#################################
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
      footer() #Call the footer screen
    
##################################
#   Fin de interfaz de usuario   #
##################################
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



#Esta funcion es para modificar el valor del FILL_ROWS que servirá para la leyenda
cambia_fill_rows <- function(nivel){
  if (nivel==1){React_FillRows("NAME_1")}
  if (nivel==2){React_FillRows("NAME_2")}
  if (nivel==3){React_FillRows("NAME_3")}
  if (nivel==4){React_FillRows("NAME_4")}
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
    cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
    MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
    lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
    regiones <- unique(MapaBase$NAME_1)
    render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
    # Actualiza las opciones de seleccion_region
    choices <- c("region", regiones)
    updateSelectInput(session, "seleccion_region", choices = choices)
    
  })
  
  
  #Aqui actualizamos la lista de provincias
  # Definimos una variable reactiva "provincias_pais" que contiene las provincias del país y la región seleccionadas
  observeEvent(input$seleccion_region, {
    
    if (!is.null(input$seleccion_pais) && input$seleccion_pais != "Selecciona un país" && 
        !is.null(input$seleccion_region) && input$seleccion_region != "region") {
      options <- paises$name 
      values <- paises$iso3
      index_pais <- match(input$seleccion_pais, options)
      pais_seleccionado2 <- values[index_pais]
      nivel_solicitado=2
      cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
      
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
      MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
      React_MapaBase(MapaBase)
      
      lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
      regiones <- unique(MapaBase$NAME_2)
      render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
      # Actualiza las opciones de seleccion_region
      choices <- c("provincia", regiones)
      updateSelectInput(session, "seleccion_provincia", choices = choices)
      
    }})
  
  #Aqui actualizamos la lista de comarcas
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
      MapaBase_anterior <- React_MapaBase() # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )

      # Verificar si MapaBase tiene la columna NAME_2 para poder dar mas detalle
      if ("NAME_2" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        React_MapaBase(MapaBase)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay

        regiones <- unique(MapaBase$NAME_3)
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        choices <- c("comarca", regiones)
        updateSelectInput(session, "seleccion_comarca", choices = choices)
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior

        browser()
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- unique(MapaBase$NAME_2) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=2 #Modifico a nivel=2 porque no ha podido encontrarlo
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor              
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        #choices <- c("comarca", regiones)
        #updateSelectInput(session, "seleccion_comarca", choices = choices)
      }
    }})
  
  #Aqui actualizamos la lista de localidades
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
      MapaBase_anterior <- React_MapaBase() # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )

      
      if ("NAME_3" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- React_MapaBase()
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        React_MapaBase(MapaBase)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- React_MapaBase()
        regiones <- unique(regiones$NAME_4)
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        choices <- c("localidad", regiones)
        updateSelectInput(session, "seleccion_localidad", choices = choices)
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior
        MapaBase <- React_MapaBase()
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        React_MapaBase(MapaBase)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- React_MapaBase()
        regiones <- unique(regiones$NAME_3) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=3 #Modifico a nivel=2 porque no ha podido encontrarlo
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor              
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
        # Actualiza las opciones de seleccion_region
        #choices <- c("comarca", regiones)
        #updateSelectInput(session, "seleccion_comarca", choices = choices)
      }
    }})
  
  
  #Aqui actualizamos la lista de localidades
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
      MapaBase_anterior <- React_MapaBase() # Copiar el valor actual de MapaBase en una variable temporal para devisar cuando no trae mapas con mas detalle
      MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
      #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
      
      if ("NAME_4" %in% colnames(MapaBase)) { #Aqui entra si existe mas detalle
        #Tras solicitar un nuevo mapa, lo descargamos, lo filtramos y actualizamos variable general MapaBase
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
        MapaBase <- React_MapaBase()
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        MapaBase <- filter(MapaBase,MapaBase$NAME_4 == input$seleccion_localidad)
        React_MapaBase(MapaBase)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
      } else { #Aqui entra si NO existe mas detalle
        # Si no tiene la columna, asignar el valor anterior a MapaBase
        MapaBase<-MapaBase_anterior
        MapaBase <- React_MapaBase()
        MapaBase <- filter(MapaBase,MapaBase$NAME_1 == input$seleccion_region)
        MapaBase <- filter(MapaBase,MapaBase$NAME_2 == input$seleccion_provincia)
        MapaBase <- filter(MapaBase,MapaBase$NAME_3 == input$seleccion_comarca)
        React_MapaBase(MapaBase)
        lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
        regiones <- React_MapaBase()
        regiones <- unique(regiones$NAME_3) #Modifico a NAME2 porque no ha encontrado un nivel con mas detalle
        nivel_solicitado=3 #Modifico a nivel=2 porque no ha podido encontrarlo
        cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor              
        render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
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
  }
  
  
  
  
  #Aqui imprimimos el titulo del mapa
  output$TittleMap <- renderText({
    
    # Impresion del título ----------------------------------------------------
    title <- paste0("<strong>Country </strong> ", input$seleccion_pais)
    if (input$seleccion_region != "region") {
      title <- paste0(title, "<strong> Region </strong>", input$seleccion_region)
      if(input$seleccion_provincia != "provincia"){
        title <- paste0(title, "<strong> Provincia </strong>", input$seleccion_provincia)
        if(input$seleccion_comarca != "comarca"){
          title <- paste0(title, "<strong> Comarca </strong>", input$seleccion_comarca)
          if(input$seleccion_localidad != "localidad"){
            title <- paste0(title, "<strong> Localidad </strong>", input$seleccion_localidad)
            #if(input$opcion_bio == TRUE){
            #  title <- paste0("<strong>Country </strong> ", input$seleccion_pais, " clima ",input$seleccion_clima)
            #}
          }
        }
      }
    }
    HTML(paste0("<h3 style='color: green;'>", title, "</h3>"))    }) 
  
  
  # Función para renderizar el mapa
  render_mapa <- function(MapaBase) {
    #Aqui imprimimos el mapa
    output$MapaBase <- renderPlot({
      
      fill_rows <- React_FillRows()  #Importamos la leyenda
      n_filas = React_NumRows()
      
      #Opciones para plotear
      plot_type <- switch(input$opcion_bio, 
                          "Colours" = "colores",
                          "Legend" = "leyenda",
                          "Elevations" = "elevaciones",
                          "Rivers" = "rios",
                          "Clean" = "limpio"
      )
      
      if (plot_type == "colores") {
        #Código para dibujar el mapa sin colores
        ggplot(MapaBase) + 
          geom_sf(aes(fill = .data[[fill_rows]])) +
          scale_fill_discrete(name = "Provincias") +
          theme_void()
      } else if (plot_type == "leyenda") {
        #Código para dibujar el mapa con leyenda
        ggplot(MapaBase) + 
          geom_sf(data = MapaBase) +
          geom_sf(fill = "white") +
          geom_sf_text(aes(label = !!as.name(fill_rows)), 
                       size = 4.5, hjust = 1, maxsize = 4) + 
          scale_fill_discrete(name = "Regiones") +
          theme(axis.text = element_text(size = 8), 
                axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0))) 
        
      } #Mapa de elevaciones
      else if (plot_type == "elevaciones") {
        
        #Solicita mapa. En funcion de la localizacion, se escogera un archivo con mas o menos detalle
        MapaBase_Elevaciones<-downloadElevationsMap(MapaBase,input$seleccion_pais,input$seleccion_region, input$seleccion_provincia, input$seleccion_comarca,  input$seleccion_localidad)

        #Preparo los datos de reprsentacion del mapa
        #Colores para plotear
        #elevation_colours = c("#00A600", "#E6E600", "#EAB64E", "#EEB99F", "#F2F2F2")
        #elevation_colours = c("green","green" "yellow","orange", "brown")
        #Altura máxima
        max_altura <- max(MapaBase_Elevaciones$Altura)
        #Intervalos altura
        intervalos_altura <- seq(0, max_altura, length.out = 5)
        
        # Plotear el mapa utilizando los datos unidos
        ggplot() + 
          geom_raster(data = MapaBase_Elevaciones, aes(x, y, fill = Altura)) + 
          scale_fill_gradient(limits = c(0, max_altura), breaks = intervalos_altura) +
          labs(fill = input$seleccion_clima) +
          scale_fill_gradient(low = "yellow", high = "brown") +
          geom_sf(data = MapaBase, alpha = 0.5, linetype = "dotted", size = 1.5) 
        
        #scale_colour_manual(values = c("#00A600", "#E6E600", "#EAB64E", "#EEB99F", "#F2F2F2"))
      } else if (plot_type == "rios") {# Graficar mapas de rios y límites administrativos
        MapaBase_Rios <- downloadRiverMap(MapaBase, st_crs(MapaBase))
        ggplot() + 
          geom_sf(data = MapaBase) +
          geom_sf(data = MapaBase_Rios , color = "blue") 
        
      } else if (plot_type == "limpio") {
        #Código por defecto si no se selecciona ninguna opción válida
        ggplot(MapaBase) + 
          geom_sf(data = MapaBase) +
          geom_sf(fill = "white")
        
      }
      
      
      
    }) #Aqui acaba plotear mapa
    
  } #Fin renderizacion imagen
  

} #Aqui acaba el server <- function(input, output,session){



#Vincula entrada de usuario con servidor
shinyApp(ui, server)




